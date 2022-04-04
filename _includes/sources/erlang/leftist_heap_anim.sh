#!/bin/bash -e

# If it fails with 'convert-im6.q16: not authorized'
# $ sudo mv /etc/ImageMagick-6/policy.xml /etc/ImageMagick-6/policy.xml.bak

erlc leftist_heap.erl
escript leftist_heap_anim.erl

for dotfile in  $(find . -name 'leftist_heap-*.dot' | sort); do
    cmdfile=${dotfile%.dot}.cmd
    pngfile=${dotfile%.dot}.png
    bigfile=${pngfile%.png}-big.png
    endfile=${pngfile%.png}-end.png
    dot -Tpng $dotfile -o $pngfile
    convert $pngfile -gravity center -background white -extent 400x450 $bigfile
    convert $bigfile -fill blue -gravity northwest -pointsize 30 -annotate 0 @$cmdfile $endfile
done

convert -delay 120 -loop 0 "*-end.png" leftist_heap.gif
rm *.cmd *.dot *.png
