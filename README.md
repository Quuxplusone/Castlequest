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

The `Makefile` in the `src` directory does not yet actually work,
because the Fortran code uses several Fortran IV-isms such as
treating strings as integers, and because it depends on VS Fortran
intrinsics such as `CMS` to open and read the record-structured
data files. Arthur hopes to get something working eventually.
Patches welcome.
