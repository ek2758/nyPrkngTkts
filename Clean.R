library(stringr)
library(data.table)
library(bit64)
library(plyr)

# Scan file from Department of Finance per line
tktsScan <- scan("Data/DOFOriginal.csv", character(0), sep="\n")
head(tktsScan)
tktsScan[856314] # Sample problem row

# Count commas in each line
commaN <- str_count(tktsScan, ",")
head(commaN)
commaN[856314:856320]
which(commaN>10) # Messed up rows have only 11 commas
which(commaN<10) # No rows with less than 10 commas
commaN[19156253] # Lop off this row -- no data

badRows <- tktsScan[which(commaN>10)] # Split bad rows from good rows
goodRows <- tktsScan[which(commaN==10)]

# For loop to replace renegade extra commas with nothing in badRows
for (i in 1:length(badRows)) {
    substr(badRows[i], # Third occurrence of a comma
           as.integer(str_locate_all(badRows[i],",")[[1]][3,1]), # Start
           as.integer(str_locate_all(badRows[i],",")[[1]][3,2])) <- "" # End and replace
    }

# Count commas in badRows to check they were fixed
str_count(badRows, ",")

# Append datasets back together to create fixed CSV
tkts <- append(goodRows, badRows)

# Properly write CSV table without header. write.csv doesn't work.
write.table(tkts, file = "Data/tkts.csv", quote = FALSE, sep = ",", row.names = FALSE, col.names = FALSE)

# Create data table of tickets -- takes less than 12 seconds. Awesome.
tktsDT <- fread("Data/tkts.csv", sep = ",", header=FALSE, verbose=TRUE)

# # These methods takes for.ever. and don't reasonably work
# tktsDF <- colsplit(tkts, ",", c("smmsNo","lcnsPlt","vhclMk","vhclBdy","vhclExp",
#                                   "violDt","violTime","violCd","violAddrss","violStrt","fine"))
# tktsDF <- read.csv(textConnection(tkts), header=FALSE)

# Rename columns
colnames(tktsDT) <- c("smmsNo","lcnsPlt","vhclMk","vhclBdy","vhclExp",
                      "violDt","violTime","violCd","violAddrss","violStrt","fine")

# Check for extra spaces and delete them
head(tktsDT)
summary(tktsDT)

# ncharMin <- function(x) min(nchar(x))
# ncharMax <- function(x) max(nchar(x))
# 
# colwise(ncharMin)(tktsDT)

max(sapply(tktsDT$smmsNo, nchar)) # S/B 10
tktsDT$smmsNo <- str_trim(tktsDT$smmsNo, side="right")

max(sapply(tktsDT$lcnsPlt, nchar)) # S/B 2

min(sapply(tktsDT$vhclMk, nchar)) # S/B <5
tktsDT$vhclMk <- str_trim(tktsDT$vhclMk)

max(sapply(tktsDT$vhclBdy, nchar)) # S/B 4 ? invalid multibyte string
tktsDT$vhclBdy <- str_trim(tktsDT$vhclBdy)

max(sapply(tktsDT$vhclExp, nchar)) # S/B 8
nchar(tktsDT$violDt[1]) # S/B 8

nchar(tktsDT$violTime[1]) # Expecting 5
min(sapply(tktsDT$violTime, nchar)) # Guessing this is 3 or 4... oh! It's 5 -- because of trailing spaces. Deleted!
tktsDT$violTime <- str_trim(tktsDT$violTime, side="right")

max(sapply(tktsDT$violCd, nchar)) # S/B 2

tktsDT$violAddrss <- str_trim(tktsDT$violAddrss)
tktsDT$violStrt <- str_trim(tktsDT$violStrt)

max(sapply(tktsDT$fine, nchar)) # S/B >2
min(sapply(tktsDT$fine, nchar)) # S/B 0
tktsDT$fine <- str_trim(tktsDT$fine)

# Check whole dataset
summary(tktsDT)

# Verify lcnsPlt
ddply(tktsDT, .(lcnsPlt), "nrow") # Sum by license plate state; 69 codes?
    # Do these codes need to be fixed? Verified? 99 code is missing?

# Verify vhclMk
ddply(tktsDT, .(vhclMk), "nrow") # Wow, this one is going to need some cleaning. >7000 unique types.

# Verify vhclBdy
ddply(tktsDT, .(vhclBdy), "nrow") # Includes "taxi"; 1832 types.

# Verify violDt
ddply(tktsDT, .(violDt), "nrow") 
    # Most appear to be YYYYMMDD
    # But some appear to be YYYYDDMM -- Is this true? Or am I seeing something?
    # And some appear to be YYYYMM?? -- Days are not realistic

# Verify violTime
ddply(tktsDT, .(violTime), "nrow") # Includes both military time and AM/PM!

# Verify violCd
violCds <- ddply(tktsDT, .(violCd), "nrow") # 1.4M red light-running violations

# Verify violStrt
redLightStreets <- ddply(subset(tktsDT, violCd==20), .(violStrt), "nrow")
    # Needs some serious regex cleaning. Yay!    

# Verify fine
redLightFines <- ddply(subset(tktsDT, violCd==20), .(fine), "nrow")

# Subset to red light-running violations
redLights <- subset(tktsDT, violCd==20)
nrow(redLights) # 1,371,785 red light-running violations caugh on Camera in Manhattan from 1 June 2005 - 31 July 2012

head(redLights)
redLightPlts <- ddply(redLights, .(lcnsPlt), "nrow")
redLightMakes <- ddply(redLights, .(vhclMk), "nrow") # Looks like there are some models in here.
redLightBdy <- ddply(redLights, .(vhclMk, vhclBdy), "nrow")
