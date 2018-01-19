#======================================================================================================
# Billboard Dataset
#======================================================================================================
# Jacob Gran and David Baker 
# July 24th, 2017 (Last Edit)
#======================================================================================================
# Libraries
library(ggplot2)
library(data.table)
library(readr)
library(stringr)
#======================================================================================================
# Import Datasets 
## Import Billboard Dataset 
setwd("/Users/davidjohnbaker/Desktop/projects/relational_network/corpora/mcgillBillboard/McGill-Billboard")
chord_by_chord <- read_csv("~/Desktop/projects/relational_network/corpora/mcgillBillboard/McGill-Billboard/chord_by_chord.csv",
                           col_names = FALSE)
rock <- data.table(chord_by_chord)
setnames(rock, c("X1", "X2", "X3", "X4", "X5", "X6", "X7","X8"),
         c("Song","Artist","Year","Time","Chord","AbsoluteTonic","RomanNumeral","Quality"))

#======================================================================================================
# Create Data in McGill 

# Add Root motion Colum
rock[, RootMotion := diff(AbsoluteTonic)]
# Convert all to Positive Intergers
str(rock)

rock$PosRootMotion <- with(rock, ifelse(RootMotion >= 0 , RootMotion , RootMotion + 12))

table(rock$PosRootMotion) ## MAKE A TABLE OF THIS , Most likely next chord in ascending semitones from the root we are now standing on
#======================================================================================================

# Import David Temperly Dataset
setwd("/Users/davidjohnbaker/Desktop/projects/relational_network/corpora/temperly/rs200_harmony_exp/krns/dt")

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
setwd("/Users/davidjohnbaker/Desktop/projects/relational_network/")
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
find.schema(pie.chart.data.rock, 5)


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

all.mcgill <- bigCount[order(-N)]
all.mcgill[, RealIndex := .I]


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
# Save that shit
write.csv(rock[,c(10,7)], "McGillBillBoard_PosRootMotion.krn")
#======================================================================================================

#======================================================================================================
# Pop Docket 
#======================================================================================================
# Put in a null token whenever song starts to eliminate boundry problem
