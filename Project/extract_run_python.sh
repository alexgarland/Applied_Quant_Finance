directory="~/Applied_Quant_Finance/27"
cd ~/Applied_Quant_Finance/27
find . -name "*.bz2" | while read filename; do bzip2 -d "`dirname "$filename"`" "$filename"; done;
