directory= '~/Applied_Quant_Finance/27'
cd ~/Applied_Quant_Finance/27

#Create the relevant json files and run the relevant python file
find . -name "*.bz2" | while read filename; do
  json_name="${filename%.*}"
  bzip2 -d "`dirname "$filename"`" "$filename";
  python "../Project/relevant_tweets.py" $json_name
done;
