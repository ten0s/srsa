#!/bin/bash -e

DITAA="java -jar /home/ten0s/emacs/lib/java/ditaa-0.11.0-standalone.jar"

$DITAA - tower-of-hanoi-01.png <<EOF
       +-|-+                 |                   |
       +-|-+                 |                   |
     +---|---+               |                   |
     +---|---+               |                   |
   +-----|-----+             |                   |
   +-----|-----+             |                   |
+--------|--------+          |                   |
+--------|--------+          |                   |
         A                   B                   C
EOF

$DITAA - tower-of-hanoi-02.png <<EOF
         |                   |                   |
         |                   |                   |
         |                   |                 +-|-+
         |                   |                 +-|-+
         |                   |               +---|---+
         |                   |               +---|---+
+--------|--------+          |             +-----|-----+
+--------|--------+          |             +-----|-----+
         A                   B                   C
EOF

$DITAA - tower-of-hanoi-03.png <<EOF
         |                   |                   |
         |                   |                   |
         |                   |                 +-|-+
         |                   |                 +-|-+
         |                   |               +---|---+
         |                   |               +---|---+
         |          +--------|--------+    +-----|-----+
         |          +--------|--------+    +-----|-----+
         A                   B                   C
EOF

$DITAA - tower-of-hanoi-04.png <<EOF
         |                 +-|-+                 |
         |                 +-|-+                 |
         |               +---|---+               |
         |               +---|---+               |
         |             +-----|-----+             |
         |             +-----|-----+             |
         |          +--------|--------+          |
         |          +--------|--------+          |
         A                   B                   C
EOF

for pngfile in $(find . -name 'tower-of-hanoi-*.png' | sort); do
    convert $pngfile -gravity west -extent 630x182 $pngfile
done
convert -delay 80 -loop 0 "tower-of-hanoi-*.png" tower-of-hanoi.gif

rm -f *.png
