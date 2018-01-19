# Make Humdrum Synatx ###############################################
#
# Use this file on the harm directory to swap out and make hudrumable
#####################################################################

for i in *.harm
do
sed 's/b/-/g' $i | 		# Swaps to Flats
sed 's/a/+/g' |			# Swaps a's for augmented
sed 's/h+rm/harm/g' |		# hacky fix to problem
sed 's/\#9//g' |		# Remove Sharp 9
sed 's/d[0-9]/7/g' |		# Dominant sevens as just 7
sed 's/d/7/g' |			# Rest of Domaints
sed 's/h7/o7/g' |		# half dimished sevenths
sed 's/x7/oD7/g' |		# Full dim seven 
#########################################################
# INVERSIONS 
sed 's/s4//g' |
sed 's/s2//g' |
sed 's/64/c/g' |		# Inverts Second Inv Triads
sed '/c/ s/$/c/' |       	 # adds C to End
sed 's/c//'	|		# Removes first c 
sed 's/65/b/g' |		# Second Inversion Seventh
sed 's/6/b/g' |			# Inverts First Inv Triads
sed '/b/ s/$/b/' |       	 # adds B to End
sed 's/b//'	|		# Removes first b 
sed 's/43/c/g' |		# Second Inversion Seventh
sed '/c/ s/$/c/' |       	 # adds C to End
sed 's/c//'	|		# Removes first c
sed 's/42/d/g'  | 
sed '/d/ s/$/d/' |       	 # adds D to End
sed 's/d//' > $i.humdrum   # Third Inversion Seventh
rename 's/\.harm\.humdrum/\.krn/' *.humdrum
done 
				
## Problem is that we still have seventh chords
##
