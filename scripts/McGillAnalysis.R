#======================================================================================================
# McGill Billboard Dataset
#--------------------------------------------------
# This script creates the analysis and graphs for Gran and Baker's Pop Music Analysis
#======================================================================================================
# Jacob Gran and David Baker 
# February 8th, 2018 (Last Edit)
#======================================================================================================
# Libraries
#--------------------------------------------------
library(ggplot2)
library(data.table)
library(readr)
library(stringr)
#======================================================================================================
# Import Dataset 
setwd("data/")
# Billboard Dataset 
#--------------------------------------------------
# Chord by chord is csv file taken from original McGill Corpus 
# http://ddmal.music.mcgill.ca/research/billboard
# But got file from here 
# https://github.com/corpusmusic/bb-cluster

chord_by_chord <- read_csv("chord_by_chord_nospace.csv", col_names = FALSE)

rock <- data.table(chord_by_chord)
setnames(rock, c("X1", "X2", "X3", "X4", "X5", "X6", "X7","X8"),
         c("Song","Artist","Year","Time","Chord","RelativeTonic","RomanNumeral","Quality"))

#======================================================================================================
# Create Data in McGill 
#--------------------------------------------------
# Check number of songs with NonHarmonic, Remove
# Used bash/terminal to take chord and create absolute tonic variable
rock
rock[, .(Names = unique(RelativeTonic))]
rock[RelativeTonic == "NonHarmonic"]
629/30712 # About 2% are non harmonic, remove
rock <- rock[RelativeTonic != "NonHarmonic"]
rock[, RelativeTonic := as.numeric(RelativeTonic)]
unique(rock$Song)
# Only interested in movements
rock[1:20]
# Add Root motion Colum
rock[, RootMotion := diff(RelativeTonic)]
# Convert all to Positive Intergers
rock
rock$PosRootMotion <- with(rock, ifelse(RootMotion >= 0 , RootMotion , RootMotion + 12))

table(rock$PosRootMotion) ## MAKE A TABLE OF THIS , Most likely next chord in ascending semitones from the root we are now standing on
#======================================================================================================
# Create Data in "Rock Corpus"
#--------------------------------------------------
# Import David Temperly Dataset

rs200 <- read.csv("TemperlyData2.csv", header = FALSE)
rs200 <- data.table(rs200)
setnames(rs200,"V1","RootMotionA")
rs200$RootMotion <- as.numeric(rs200$RootMotion)

# Get rid of negative root motion 
rs200$PosRootMotion <-  with(rs200, ifelse(RootMotion >= 0 , RootMotion , RootMotion + 12 ))

temperley.root.plot.data <- table(rs200$PosRootMotion)
temperley.root.plot.data <- data.table(temperley.root.plot.data)

temperley.root.plot.data[, V1 := as.numeric(V1)]

temperley.root.plot.data[, Percent := N/sum(N)]



# Root Motion in RS200 Plot FREQUENCY
ggplot(temperley.root.plot.data, aes(x = reorder(V1,V1), y = N)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  labs(x = "Root Motion in Ascending Semitones", y ="Frequencey of Occurance in RS200", title = "RS200 Root Motion") +
  theme_bw()

# PERCENT
ggplot(temperley.root.plot.data, aes(x = reorder(V1,V1), y = Percent)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  labs(x = "Root Motion in Ascending Semitones", y ="Frequencey of Occurance in RS200 in %", title = "RS200 Root Motion") +
  theme_bw()


rock.root.plot.data <- table(rock$PosRootMotion)
rock.root.plot.data <- data.table(rock.root.plot.data)
rock.root.plot.data <- rock.root.plot.data[V1 != 0,]
rock.root.plot.data[, V1 := as.numeric(V1)]

#--------------------------------------------------
# Root Motion in McGill 
ggplot(rock.root.plot.data, aes(x = reorder(V1,V1), y = N)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  labs(x = "Root Motion in Ascending Semitones", y ="Frequencey of Occurance in McGill", title = "McGill Root Motion") +
  theme_bw()

rock.root.plot.data[, Percent := N/sum(N)]


# PERCENT
ggplot(rock.root.plot.data, aes(x = reorder(V1,V1), y = Percent)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  labs(x = "Root Motion in Ascending Semitones", y ="Frequencey of Occurance in McGill in %", title = "McGill Root Motion") +
  theme_bw()
#--------------------------------------------------
# Make Bach Plot

Bach_Semitones2 <- read.csv("Bach_Semitones2.csv", header= FALSE)
setnames(Bach_Semitones2, "V1", "RootMotion")
Bach_Semitones2$PosRootMotion <-  with(Bach_Semitones2, ifelse(RootMotion >= 0 , RootMotion , RootMotion + 12 ))

Bach_Semitones2.graph <- data.table(table(Bach_Semitones2$PosRootMotion))

Bach_Semitones2.graph[, V1 := as.numeric(V1) ]

Bach_Semitones2.graph[, Percent := N/sum(N)]
ggplot(Bach_Semitones2.graph[V1 != 0], aes(x = V1, y = Percent)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  labs(x = "Root Motion in Ascending Semitones", y ="Frequencey of Occurance", title = "Root Motion in Bach 371 Chorales") +
  theme_bw() + scale_x_continuous(breaks=1:11)

#############################



# ROCK PIE 

pie.chart.data.rock <- rock[PosRootMotion != 0 ]
# find.schema(pie.chart.data.rock, 5)
# This is broken

#======================================================================================================
# Find Overlap in the two sets
billboardNames <- data.table(unique(rock$Song))
DTnames <- read.csv("SongsInDT.csv", header = FALSE)
billboardNames

billboardNames$V1 <- str_to_lower(billboardNames$V1)
billboardNames$V1 <- str_replace_all(billboardNames$V1,"[:punct:]","")

DTnames$V1 <- str_replace_all(DTnames$V1,"dt.krn","")
DTnames$V1 <- str_replace_all(DTnames$V1,"_","")

intersect(DTnames$V1, billboardNames$V1)
# Remove the 20 duplicates out of billboard dataset 
rock <-  rock[Song != "Help!"][Song != "BornToBeWild"][Song != "BeMyBaby"][Song != "EightMilesHigh"][Song != "EleanorRigby"][Song != "GoodVibrations"][Song != "HonkyTonkWomen"][Song != "ISawHerStandingThere"][Song != "IWantYouBack"][Song != "InTheMidnightHour"][Song != "MaggieMay"][Song != "NotFadeAway"][Song != "One"][Song != "PeopleGetReady"][Song != "StandByMe"][Song != "SummertimeBlues"][Song != "SunshineofYourLove"][Song != "TheSoundsOfSilence"][Song != "WillYouLoveMeTomorrow"][Song != "WithOrWithoutYou"]

#======================================================================================================D
# Descriptives 
unique(rock$Song)
# 710 Different Songs in Database
# DOUBLE CHECK THE OVERLAP, DAVE 

#--------------------------------------------------
# Plots of Datasets
plot(table(rs200$PosRootMotion))
plot(table(rock$PosRootMotion))

#======================================================================================================
# Find Schemas in Temperly Corpus 
#======================================================================================================
rs200 <- rs200[PosRootMotion != 0]

rs1 <- rs200[  , .I[PosRootMotion == "1"]]
rs1.two <- rs1 + 1
rs1.1 <- table(rs200$PosRootMotion[rs1.two])

rs2 <- rs200[  , .I[PosRootMotion == "2"]]
rs2.two <- rs2 + 1
rs1.2 <- table(rs200$PosRootMotion[rs2.two])

rs3 <- rs200[  , .I[PosRootMotion == "3"]]
rs3.two <- rs3 + 1
rs1.3 <- table(rs200$PosRootMotion[rs3.two])

rs4 <- rs200[  , .I[PosRootMotion == "4"]]
rs4.two <- rs4 + 1
rs1.4 <- table(rs200$PosRootMotion[rs4.two])

rs5 <- rs200[  , .I[PosRootMotion == "5"]]
rs5.two <- rs5 + 1
rs1.5 <- table(rs200$PosRootMotion[rs5.two])

rs6 <- rs200[  , .I[PosRootMotion == "6"]]
rs6.two <- rs6 + 1
rs1.6 <- table(rs200$PosRootMotion[rs6.two])

rs7 <- rs200[  , .I[PosRootMotion == "7"]]
rs7.two <- rs7 + 1
rs1.7 <- table(rs200$PosRootMotion[rs7.two])

rs8 <- rs200[  , .I[PosRootMotion == "8"]]
rs8.two <- rs8 + 1
rs1.8 <- table(rs200$PosRootMotion[rs8.two])

rs9 <- rs200[  , .I[PosRootMotion == "9"]]
rs9.two <- rs9 + 1
rs1.9 <- table(rs200$PosRootMotion[rs9.two])

rs10 <- rs200[  , .I[PosRootMotion == "10"]]
rs10.two <- rs10 + 1
rs1.10 <- table(rs200$PosRootMotion[rs10.two])

rs11 <- rs200[  , .I[PosRootMotion == "11"]]
rs11.two <- rs11 + 1
rs1.11 <- table(rs200$PosRootMotion[rs11.two])


rs1.1 <- data.table(rs1.1)
rs1.2 <- data.table(rs1.2)
rs1.3 <- data.table(rs1.3)
rs1.4 <- data.table(rs1.4 )
rs1.5 <- data.table(rs1.5 )
rs1.6 <- data.table(rs1.6 )
rs1.7 <- data.table(rs1.7 )
rs1.8 <- data.table(rs1.8 )
rs1.9 <- data.table(rs1.9 )
rs1.10 <- data.table(rs1.10 )
rs1.11 <- data.table(rs1.11)

rs1.1$Prior <- "1"
rs1.2$Prior <- "2"
rs1.3$Prior <- "3"
rs1.4$Prior <- "4"
rs1.5$Prior <- "5"
rs1.6$Prior <- "6"
rs1.7$Prior <- "7"
rs1.8$Prior <- "8"
rs1.9$Prior <- "9"
rs1.10$Prior <- "10"
rs1.11$Prior <- "11"


bigCount <- rbind(rs1.1, rs1.2,rs1.3, rs1.4, rs1.5, rs1.6, rs1.7, rs1.8, rs1.9, rs1.10, rs1.11)
bigCount[,Schema:=paste0(Prior, "-", V1)]

bigCount[order(-N)][1:30]
top30 <- bigCount[order(-N)][1:30]
top30[, RealIndex := .I]
#--------------------------------------------------
# Make Chart, ungrouped

bigCount$Prior <- as.numeric(bigCount$Prior)
bigCount$V1 <- as.factor(bigCount$V1)
p <- ggplot(bigCount, aes(x = Prior, y = N, fill = V1))
p + geom_bar(stat = "identity") + 
  labs(x = "Initial Interval", y = "Frequency Count", title = "Temperley Schema Counts") 

#--------------------------------------------------
# Remove Involutions
str(bigCount)
bigCount$N <- as.numeric(bigCount$N)
bigCount$V1 <- as.numeric(as.character(bigCount$V1))
bigCount[, Added := Prior + V1]
bigCount[Added == 12]

bigCountNoInvo <- bigCount[Added != 12]
bigCountNoInvo[order(-N)][1:100]
#======================================================================================================
bigCountNoInvo
str(bigCountNoInvo)
#bigCountNoInvo$V1 <- as.integer(bigCount$V1)
#bigCountNoInvo$V1 <- as.factor(bigCount$V1)
q <- ggplot(bigCountNoInvo, aes(x = Prior, y = N, fill = V1))
q + geom_bar(stat = "identity") + 
  labs(x = "Initial Interval", y = "Frequency Count", title = "Temperley Schema Counts, No Involution") 

# Make legend so that all 6s are the same

all.mcgill <- bigCount[order(-N)]
all.mcgill[, RealIndex := .I]

# ggplot(bigCountNoInvo[N > 100], aes(x = V1, y = Prior, size = N)) + geom_point()

pie(bigCountNoInvo[Prior == 10])

ggplot(data=top30, aes(x=RealIndex, y=N)) + geom_bar(stat="identity") 
ggplot(data=all.mcgill, aes(x=RealIndex, y=N)) + 
  geom_bar(stat="identity") + 
  theme_bw() + labs(title = "Three Chord Schemas in McGill Billboard", x = "Schemas Ordered by Frequency", y =  "Frequency in Corpus")



tester <- table(rocky$PosRootMotion[rownameswhererootequalstenOne])
tester1 <- table(rocky$PosRootMotion[rownameswhererootequalstenOne])
rbind(tester, tester1)

round(table(rocky$PosRootMotion[rownameswhererootequalstenOne])/sum(table(rocky$PosRootMotion[rownameswhererootequalstenOne])),2)

pie(table(rocky$PosRootMotion[rownameswhererootequalstenOne]))


#======================================================================================================
# Make Pie Charts for McGill
rock[PosRootMotion == 10]

tentable <- rock[PosRootMotion == 10]
pie(table(tentable$RomanNumeral))

# Get the row names that match 10 
# Save those
# Add 1 or 2 or whatever to those row names, make sure to have original row name
# Print the original data frame with those row names 

rocky <- rock

rownameswhererootequalsten <- rocky[  , .I[PosRootMotion == "10"]]
rownameswhererootequalstenOne <- rownameswhererootequalsten + 1

# Table of things that happen after ten !!!!!!!!!!!!!!!!!!!!!! THIS IS GOOOOOOD 
pie(table(rocky$PosRootMotion[rownameswhererootequalstenOne]))
#======================================================================================================
# Pie chart that shows the distributions of Roman Numerals 

# Rows that occur after a 10 
rowafter10 <- rocky[rownameswhererootequalstenOne]
foursandsevensoften <-rowafter10[PosRootMotion == 7]

fourseventenDT <- data.table(table(foursandsevensoften$RomanNumeral))
fourseventenDT$NAME <- data.table(RN = c("I","II","IV (Blues Cadence)","V","bIII","bV","bVI","bVII (Double Plagal)"))


pie(fourseventenDT$N, labels = fourseventenDT$NAME)
#======================================================================================================
# Got Schemas from DT, looking for Roman Numeral breakdowns with McGill

# Given each schema (10-7) show roman numerals 
# if consecutive 10 then 7 (after 0s removed), print RN for the first one 

#======================================================================================================
# TOP SCHEMAS

# SCHEMA AND N 
# MAKE THIS A BEAUTIFUL TABLE, SEND TO JACOB

stargazer::stargazer(bigCountNoInvo[, Schema, N][order(-N)][1:15], summary = FALSE)

bigCountNoInvo[order(-N)][1:15]

rock <- rock[PosRootMotion != 0]
rs1.mg <- rock[  , .I[PosRootMotion == "10"]]
rs1.two.mg <- rs1.mg + 1
initial.ten <- rock[rs1.two.mg]
pie(table(initial.ten[PosRootMotion == 7]$RomanNumeral), main = "10 - 7 Schema")
# What the The roman numeral is shoing is what second chord would be 
# Remove labels on everything but the main blobs ("Double Plagal (I-bVII-IV)" and "Blues Cadence (V-IV-I)") 

rs1.1.mg <- table(rock$RomanNumeral[rs1.two.mg])
pie(rs1.1.mg)
# We think this would clarify itself at a four chord level 

rs5.mg <- rock[  , .I[PosRootMotion == "5"]]
rs5.two.mg <- rs5.mg + 1
initial.five <- rock[rs5.two.mg]
pie(table(initial.five[PosRootMotion == 5]$RomanNumeral), main = "5 - 5 Schema")

rs1.1.mg <- table(rock$RomanNumeral[rs1.two.mg])
pie(rs1.1.mg)

rs7.mg <- rock[  , .I[PosRootMotion == "7"]]
rs7.two.mg <- rs7.mg + 1
initial.seven <- rock[rs7.two.mg]
pie(table(initial.seven[PosRootMotion ==10]$RomanNumeral), main = "7 - 10 Schema")

# MAKE TWO VERSIONS OF TEH OVERLEAF TABLE FOR DRAMATIC EFFECT

# Print top six with their sisters 
# Temperley juxtaposition chart !
# Reproduce chart in overleaf

# Start on Four Chord Progressions?!?!?!
#======================================================================================================
# Save that shit
write.csv(rock[,c(10,7)], "McGillBillBoard_PosRootMotion.krn")
#======================================================================================================

#======================================================================================================
# Pop Docket 
# * Make Figure Folder in github, send to Jacob
#======================================================================================================
# Put in a null token whenever song starts to eliminate boundry problem
