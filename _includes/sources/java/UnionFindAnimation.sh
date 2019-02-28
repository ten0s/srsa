#!/bin/bash -e

make run CLASS=UnionFindAnimation

for dotfile in  $(find . -name 'uf-*.dot' | sort); do
    cmdfile=${dotfile%.dot}.cmd
    pngfile=${dotfile%.dot}.png
    bigfile=${pngfile%.png}-big.png
    endfile=${pngfile%.png}-end.png
    dot -Tpng $dotfile -o $pngfile
    convert $pngfile -gravity center -extent 1000x250 $bigfile
    convert $bigfile -fill blue -gravity northwest -pointsize 30 -annotate 0 @$cmdfile $endfile
done

convert -delay 120 -loop 0 "*-end.png" uf.gif

rm -f *.cmd *.dot *.png
