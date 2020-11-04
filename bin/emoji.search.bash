#!/bin/bash

ME=$(basename "${BASH_SOURCE[0]}")
DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" &>/dev/null && pwd)

PREVIEW_COMMAND="$DIR/$ME render {+}"

function split () {
    printf '%s\n' "$@"
}

function render () {
    cut -f2 -d' ' |
        tr -d '\'n |
        perl -CS -pe 's/..\K(?=.)/\N{U+200D}/g'
}

function emoji-definitions () {
    base64 --decode <<EOF | bzcat
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hDAK3dVgYPRvwFEE5xYB7gSfRFVVRVYwFESWeisA8fDpFvere3i19+uufn5zrx0QBAIqYdn1mvq/MU4Ot49zg333wAERCItlGv1D05a3NZ6trXW+uugREAeH7hHHsT6zzrrUc99a3zroepgAAXfCD90mOTxrxTxzrW+Kl4nvhmYBEDBkRGDE9bS2tb5sfeym6669eU4vqXXr0REXYMiEp0V2vcpz6qmMY8d3h3456W2CHDIwYM4WiiZ5u3jrrjU0rX3+8875wgAZmZkbIpWaX1SaYzratPvnIqqEUWKLFgoqqoLFgqkAYMzBW1CtqpbCXtbSZTDda0YBAzMGQIwQMbbYMaTaaFVu/jOuudn147c9d+uuhB022xViiigqkFWCigCIvrv09fX27+PM8ePb19fX17fZesPZRBGKiAjEFVUQYMjNMolr3xansplMZR/aEQGEjFI5bes0xnMfOmpkiLHKIvM8RS08yxbMMpkv2mS5JisErW1UtfGf3AEQAnz4MIRZCRSACkgCMiwHEGWvL4Ec2layDCe1rWf3ggH6SKM82pqaZvdM58+d9M9rO9SnuHv73is823rrfvEqvw74SLJmJS9z1Tvjxne9BgDDHXtM56M3veZda8986Tmt9500gQKAEI5zHKdnlEp4TD861trdxIgRcpt+HPpr8YIj1qiyIc5fet5Y8i5EIAgW6A377dts3DbPXHftzyB479SqEgGC2IENAYl6zi4ELZky0xe+7+mu+t8kCIsAEO/cN1uOhacQBrCJvYp0rO3WECIyIFMihO0rXo8iKVhO2ZXvbGWSTUyAqQE4svGjKYzEgKzvrOZoxIgpVedWAiq69FzvnF739siWKEQHi93UdbECAfmmMZzjAiCVFsbPhNcCApSIpfl8OjEiGY9PniEnRiQrRbfKXxhErCEToA38uI0g4EJwp8C12397vGda21X7xciDpO3O+kpEEKc3nCW4npIiEYRm+jiACQrKtM2xhJkRBrqR9nVJwALWNV7vrGrJ45dbLAF5DIwfnbwCne8W5Xe1nY1rW3TsotSfGmYLgCjnU00mdZ0uvxTtYEXbtYliTyFrIms21hDlp+XNdXMCIqZo3Nn4xibJkN47QSaAV3yli98nfOEintkQIrECGEepYBTv8GeLUSt16znUNHcgA9mYTo4EH3hnyy9a6y886616C5Fd2pzik4ApT3VErj21RBYVPzZ7gQp7E0hi+MZym4ebBjZ1wyjgKyheaHXGes71eFqWvNhAWxLEYwSdKtwY42o9PZgregccXAXBgnmmm30uwQT0tNsuefkHTpMokjciro7m0hTWlxPa+z1fpf9feevPbH/bV/0+Zev2OP1oluhco2uRFKVqWUEZrnCW0xVVcPq7nzWr9/9KbCISfhf835va/4/U+pgQ9kghBYRISCDAOn06BWYPfNAIjB0s6DMlAB4a1PN2O8BjySzR5ZZtE3R/q/XqgFsQ8Ovo0U6tjg1K2xnSQAS22uqQIbLrvmoPPLpcZAIMttNtMsRgRQh8esY61O2v67Y925AXvm05zz92IKMYok8UpS9eYsRAVrSlKTnOIK0pfulLl5RIhDEGTaBJrfuxtaMf2yIhelL9yvOc5EQlKV70vXvN/lXvfnnJEQxi1rVrSlKdUAJK1RKpXXm1qSSGYxcCK1n0c1rY92hqM+503jFyF72rSc5ZnvxQgQpSlMVrWtLeaEAKapSk55z1IixJ77WcCq9/ddxzWUhKLpsmyUqPnDcRKUu5RxGspAF/GckIAooQIqBFAiCoqKoAiIoqxEVAkRkkJIskAEUhsY2MBNpJLKv/59C+Ovza8/jxjTT/8XckU4UJDxuhGHA=
EOF
}

function main () {
    case "$1" in
        render)
            shift
            split "$@" | render
            ;;
        debug)
            emoji-definitions
            ;;
        *)
      emoji-definitions | fzf -0 -m --preview="$PREVIEW_COMMAND" | render
    esac
}

main "$@"