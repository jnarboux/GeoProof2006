#!/bin/bash
for toto in *.svg
    do
        inkscape -f "$toto" -w 500 -B -e "png/$toto.png"&
        pngtopnm -mix "png/$toto.png" | pnmtops -nocenter -equalpixels -dpi 72 -noturn -rle > "png/$toto.ps"
    done

exit 0