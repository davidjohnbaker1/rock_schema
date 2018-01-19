for i in *.krn
do
echo $i 
grep -v R $i |
harm2kern -r | 
sed 's/root/kern/'| 
semits | 
./sint > $i.schema
done
