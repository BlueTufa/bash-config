#!/usr/bin/env -S scala-cli shebang

// -*- mode: scala -*-

//> using scala 2.13.15
//> using toolkit typelevel:0.1.29
//> using dep com.softwaremill.sttp.client::core:2.2.7
//> using dep org.apache.commons:commons-compress:1.20
//> using dep commons-codec:commons-codec:1.15
//> using dep org.scala-lang.modules::scala-xml:2.4.0

import org.apache.commons.compress.compressors.bzip2.{
  BZip2CompressorOutputStream => BZip2OutputStream
}

import org.apache.commons.codec.binary.Base64OutputStream
import sttp.client.quick._
import cats.implicits._
import cats.effect._
import scala.xml.{XML, Node}
import java.io.{
  OutputStream,
  PrintWriter,
  ByteArrayOutputStream
}

import cats.effect.unsafe.implicits.global

final case class Emoji(glyph: String, shortcut: String) {
  def show: String = s"$shortcut $glyph"
}

def download: IO[String] =
  IO {
    quickRequest
      .get(uri"https://raw.githubusercontent.com/warpling/Macmoji/master/emojiSubstitutions.plist")
      .send()
      .body
  }

def byteArrayOutputStream: Resource[IO, ByteArrayOutputStream] =
  Resource.fromAutoCloseable(IO(new ByteArrayOutputStream))

def base64OutputStream(outputStream: OutputStream): Resource[IO, Base64OutputStream] =
  Resource.fromAutoCloseable(IO(new Base64OutputStream(
    outputStream,
    true,             // encode
    -1,               // no line breaks
    Array.empty[Byte] // line separators ignored
  )))

def compressedOutputStream(outputStream: OutputStream): Resource[IO, BZip2OutputStream] =
  Resource.fromAutoCloseable(IO(new BZip2OutputStream(outputStream)))

def printWriter(outputStream: OutputStream): Resource[IO, PrintWriter] =
  Resource.fromAutoCloseable(IO(new PrintWriter(outputStream)))

def encodedWriter(byteArrayOutputStream: ByteArrayOutputStream): Resource[IO,PrintWriter] =
  for {
    base64 <- base64OutputStream(byteArrayOutputStream)
    compressed <- compressedOutputStream(base64)
    writer <- printWriter(compressed)
  } yield writer

def parse(unparsedXML: String): IO[Node] = IO {
  val factory = javax.xml.parsers.SAXParserFactory.newInstance()

  // disable DTD validation
  factory.setValidating(false)
  factory.setFeature("http://xml.org/sax/features/validation", false)
  factory.setFeature("http://apache.org/xml/features/nonvalidating/load-dtd-grammar", false)
  factory.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false)
  factory.setFeature("http://xml.org/sax/features/external-general-entities", false)
  factory.setFeature("http://xml.org/sax/features/external-parameter-entities", false)

  XML.withSAXParser(factory.newSAXParser).loadString(unparsedXML)
}

def extractDefinitions(root: Node): IO[Seq[Emoji]] =
  IO((root \ "array" \ "dict").map { entry =>
    (entry \ "string").map(_.text) match {
      case Seq(glyph, shortcut) => Emoji(glyph, shortcut)
    }
  }).map(_ ++ Seq(
    Emoji("₀", ":subscript_0:"),
    Emoji("₁", ":subscript_1:"),
    Emoji("₂", ":subscript_2:"),
    Emoji("₃", ":subscript_3:"),
    Emoji("₄", ":subscript_4:"),
    Emoji("₅", ":subscript_5:"),
    Emoji("₆", ":subscript_6:"),
    Emoji("₇", ":subscript_7:"),
    Emoji("₈", ":subscript_8:"),
    Emoji("₉", ":subscript_9:"),

    Emoji("⁰", ":superscript_0:"),
    Emoji("¹", ":superscript_1:"),
    Emoji("²", ":superscript_2:"),
    Emoji("³", ":superscript_3:"),
    Emoji("⁴", ":superscript_4:"),
    Emoji("⁵", ":superscript_5:"),
    Emoji("⁶", ":superscript_6:"),
    Emoji("⁷", ":superscript_7:"),
    Emoji("⁸", ":superscript_8:"),
    Emoji("⁹", ":superscript_9:"),

    Emoji("±", ":plus_minus:"),
   
    Emoji("⅓", ":frac_one_third:"),
    Emoji("⅔", ":frac_two_thirds:"),

    Emoji("¼", ":frac_one_quarter:"),
    Emoji("½", ":frac_one_half:"),
    Emoji("¾", ":frac_three_quarters:"),

    Emoji("⅕", ":frac_one_fifth:"),
    Emoji("⅖", ":frac_two_fifths:"),
    Emoji("⅗", ":frac_three_fifths:"),
    Emoji("⅘", ":frac_four_fifths:"),

    Emoji("⅛", ":frac_one_eighth:"),
    Emoji("⅜", ":frac_three_eighths:"),
    Emoji("⅝", ":frac_five_eighths:"),
    Emoji("⅞", ":frac_seven_eighths:"),
  ))

def truncateForTesting(emojis: Seq[Emoji]): IO[Seq[Emoji]] =
  IO(emojis.take(5))

def writeDefinitions(dest: PrintWriter, definitions: Seq[Emoji]): IO[Unit] =
  IO {
    definitions.foreach { emoji =>
      dest.println(emoji.show)
    }
    dest.flush()
  }

def retrieveResults(byteArrayOutputStream: ByteArrayOutputStream): IO[String] =
  IO {
    byteArrayOutputStream.toString()
  }

def compressEmojiDefinitions: IO[String] =
  for {
    unparsed <- download
    parsed <- parse(unparsed)
    definitions <- extractDefinitions(parsed)
    results <- byteArrayOutputStream.use { byteOutputStream =>
      for {
        _ <- encodedWriter(byteOutputStream).use { writer =>
          writeDefinitions(writer, definitions)
        }
        r <- retrieveResults(byteOutputStream)
      } yield r
    }
  } yield results

def buildBashScript(encodedEmojiDefs: String): IO[String] =
  IO {
    s"""|#!/bin/bash
        |
        |ME=$$(basename "$${BASH_SOURCE[0]}")
        |DIR=$$(cd "$$(dirname "$${BASH_SOURCE[0]}")" &>/dev/null && pwd)
        |
        |PREVIEW_COMMAND="$$DIR/$$ME render {+}"
        |
        |function split () {
        |    printf '%s\\n' "$$@"
        |}
        |
        |function render () {
        |    cut -f2 -d' ' |
        |        tr -d '\\'n |
        |        perl -CS -pe 's/..\\K(?=.)/\\N{U+200D}/g'
        |}
        |
        |function main () {
        |    if [ "$$1" = 'render' ]; then
        |        shift
        |        split "$$@"
        |    else
        |        base64 --decode <<EOF | bzcat | fzf -0 -m --preview="$$PREVIEW_COMMAND"
        |$encodedEmojiDefs
        |EOF
        |    fi
        |}
        |
        |main "$$@" | render""".stripMargin
  }

def run: IO[ExitCode] = for {
  encodedEmojiDefs <- compressEmojiDefinitions
  bashScript <- buildBashScript(encodedEmojiDefs)
} yield {
  println(bashScript)
  ExitCode.Success
}


run.unsafeRunSync

