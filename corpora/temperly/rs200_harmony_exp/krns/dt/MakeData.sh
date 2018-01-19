for i in *.schema
do
grep -v '\*\*' $i | 
grep -v "=" | 
grep -v "\." | 
grep -v '\*' >> TemperlyData.csv
done
