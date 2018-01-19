# Convert Temperly Corpus for Humdrum
# TO DO
## Swap Their Key Annotation for Ours

echo '**harm' | cat - $1 > temp && mv temp $1 |
sed 's/\|/\=/g' $1 |
sed 's/\[/\*/g' | 
sed 's/\]/\:/g'|
tr " " "\n" 
