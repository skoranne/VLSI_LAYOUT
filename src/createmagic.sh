#!/bin/bash
#Given a directory full of .bin files, conver them nicely into a Magic file
#
#magic
#tech sky130A
#timestamp 1780872228
#<< metal1 >>
#KLBIN foo.bin
#<< end >>
outFile=$1
echo "magic" > $outFile
echo "tech sky130A" >> $outFile
echo "timestamp 1780872228" >> $outFile

#echo "magic" > $outFile

for file in $(ls *.bin); do
    # Skip if no matching files are found
    [[ -e "$file" ]] || continue
    [[ -s "$file" ]] || continue
    # Use a regex to match the prefix and the layer pattern (e.g., L70_D16)
    # ^(.*) captures the prefix (like SMALL_SDT)
    # _(L[0-9]+_D[0-9]+) captures the layer sequence
    # \.bin$ matches the extension at the end
    if [[ "$file" =~ ^(.*)_(L[0-9]+_D[0-9]+)\.bin$ ]]; then
        prefix="${BASH_REMATCH[1]}"
        layer="${BASH_REMATCH[2]}"
        echo "<< $layer >>" >> $outFile
	rp=$(realpath $file)
	echo "KLBIN $rp" >> $outFile
        echo "$prefix has layer $layer"
    fi
done
echo "<< end >>" >> $outFile
echo "" >> $outFile
