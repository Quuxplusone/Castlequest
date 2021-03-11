# Castlequest (1980)

By Michael S. Holtzman and Mark Kershenblatt.


## Provenance

The U.S. Copyright Office has a deposit related to this game:
[TXu000091366](https://cocatalog.loc.gov/cgi-bin/Pwebrecon.cgi?Search_Arg=TXu000091366&Search_Code=REGS&CNT=10&HIST=1)

On 2021-03-02, Mark Kershenblatt received 78 pages of paper copies
from the USCTO. He scanned them in and sent the scans to Arthur O'Dwyer,
in the form of two PDFs. Arthur rotated and concatenated the PDFs
into the single 78-page PDF in this repository, `castlequest.pdf`.

Arthur O'Dwyer manually transcribed the PDF into the plain text
file in this repository, `castlequest.ocr.txt`. (If you find any
places where the transcription differs from the original PDF,
Arthur will pay a "bug bounty" of $5 per error! Open a pull request
on this repository or send me an email.)

The `src` directory contains `.f` and `.dat` files that have
been mechanically extracted from `castlequest.ocr.txt` using
command-line tools such as `cut -b 17-88`.


## How to compile and play

I'm still deciding how to organize the patches in the long term,
so these instructions may change. For now, my patches are in a
separate git branch named `patches`:

    git checkout patches
    cd src
    make
    ./cquest | asa

In order for `make` to work, you'll need to have either `f77` or
`gfortran` in your path.

In order for your input to be recognized, you'll need to enter
all your text in ALL CAPS. I recommend turning on CAPS LOCK while
you play.

The game's output uses "[carriage control](https://en.wikipedia.org/wiki/ASA_carriage_control_characters)":
when it prints the character `0` in column 1, it's expecting that
the printer hardware will turn that into an extra newline.
Naturally, modern terminals don't do that. But many POSIX systems
(including Mac OSX) come with a utility program named `asa` that
can interpret those carriage-control characters for you. If your
computer lacks `asa`, you can hack it together in a couple lines
of your favorite scripting language; or, just deal with the extra
`0` characters and fewer newlines.
