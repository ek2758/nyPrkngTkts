# Load dataset from encrypted folder
load("/Volumes/Data/tktsClean.RData")

library(stringr)
library(data.table)
library(bit64)
library(plyr)
library(foreach)

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

# Check whole dataset
summary(tktsDT)
head(tktsDT)

# Correct classes
tktsDT$violCd <- as.character(tktsDT$violCd)
tktsDT$violDt2 <- as.Date(as.character(tktsDT$violDt), format="%Y%m%d") # Create dates
invalidDts <- subset(tktsDT, is.na(violDt2)) 
unique(invalidDts$violDt)
    # 20120231 <- 20120302
    # 20120575 <- 20120714
    # 20090931 <- 20091001
    # 20092011 <- 20100811
    # 20120370 <- 20120509

# ncharMin <- function(x) min(nchar(x))
# ncharMax <- function(x) max(nchar(x))
# 
# colwise(ncharMin)(tktsDT)

max(sapply(tktsDT$smmsNo, nchar)) # S/B 10
tktsDT$smmsNo <- str_trim(tktsDT$smmsNo)

max(sapply(tktsDT$lcnsPlt, nchar)) # S/B 2
tktsDT$lcnsPlt <- toupper(tktsDT$lcnsPlt)

min(sapply(tktsDT$vhclMk, nchar)) # S/B <5
tktsDT$vhclMk <- str_trim(tktsDT$vhclMk)
tktsDT$vhclMk <- toupper(tktsDT$vhclMk)

max(sapply(tktsDT$vhclBdy, nchar)) # S/B 4 ? invalid multibyte string
tktsDT$vhclBdy <- str_trim(tktsDT$vhclBdy)
tktsDT$vhclBdy <- toupper(tktsDT$vhclBdy)
tktsDT$vhclBdy[1393178] <- "BUS" # Error at this line; hard-code with correct text; grep doesn't work
tktsDT$vhclBdy[1567482] <- "BUS" # Something about unicode something or other?
tktsDT$vhclBdy[1672408] <- "BUS"
tktsDT$vhclBdy[1672413] <- "BUS"

max(sapply(tktsDT$vhclExp, nchar)) # S/B 8
nchar(tktsDT$violDt[1]) # S/B 8

nchar(tktsDT$violTime[1]) # Expecting 5
min(sapply(tktsDT$violTime, nchar)) # Guessing this is 3 or 4... oh! It's 5 -- because of trailing spaces. Deleted!
tktsDT$violTime <- str_trim(tktsDT$violTime)

max(sapply(tktsDT$violCd, nchar)) # S/B 2

tktsDT$violAddrss <- str_trim(tktsDT$violAddrss)
tktsDT$violAddrss <- toupper(tktsDT$violAddrss)

tktsDT$violStrt <- str_trim(tktsDT$violStrt)
tktsDT$violStrt <- toupper(tktsDT$violStrt)

max(sapply(tktsDT$fine, nchar)) # S/B >2
min(sapply(tktsDT$fine, nchar)) # S/B 0
tktsDT$fine <- str_trim(tktsDT$fine)

# Verify lcnsPlt
ddply(tktsDT, .(lcnsPlt), "nrow") # Sum by license plate state; 69 codes?
    # Do these codes need to be fixed? Verified? 99 code is missing ?

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
ddply(tktsDT, .(violTime), "nrow") # Includes both military time and AM/PM?!
timeAP <- str_extract(tktsDT$violTime, "\\D$") # Extract AM/PM designation
unique(timeAP) # A, P, and... B??
which(timeAP=="B") # Just one row: 1428182
tktsDT[1428182,] # Fire hydrant parking -> PM
time <- str_extract(tktsDT$violTime, "\\d{4}") # Extract 4-digit times (all have 4 digits)
t <- as.data.frame(cbind(time, timeAP))
max(nchar(time)) # 4
min(nchar(time)) # 2 <- these are NAs
sum(is.na(time)) # 432,674 with no times
t$hr <- as.numeric(str_sub(t$time,1,2)) # Separate hours
t$min <- as.numeric(str_sub(t$time,3,4)) # from minutes
t$timeAP[1428182] <- "P" # Fix invalid AM/PM designation

adjust.hr <- function(p, h) { # If hrs are off, typically off by multiple of 12
    x <- ifelse(is.na(p) & h<24, h,
                ifelse(p=="P" & h<12, h+12,
                       ifelse(h>=24 & h<=35, h-12,
                              ifelse(h>=36 & h<=47, h-24,
                                     ifelse(h>=48 & h<60, h-36,
                                            ifelse(h>=60 & h<72, h-48,
                                                   ifelse(h>=72 & h<84, h-60,
                                                          ifelse(h>=84 & h<96, h-72, h))))))))
    return(x)
}

t$hr.adj <- adjust.hr(t$timeAP, t$hr) # Apply hour adjustments
t$hr[which(t$hr.adj>=25)] # Check for validity

adjust.min <- function(m) { # Turn minutes into double-digits, if not already
    x <- ifelse(str_length(m)==1, str_c("0", m), m) 
    return(x)
}

t$min.adj <- adjust.min(t$min) # Apply minute adjustments
min(sapply(t$min.adj, nchar)) # Should be 2

# Concatenate column to dataset
tktsDT$time2 <- str_c(as.character(t$hr.adj), as.character(t$min.adj), sep=":")
head(tktsDT)

# Create POSIXct date/time column
tktsDT$timeEDT <- as.POSIXct(strptime(str_c(tktsDT$violDt2, tktsDT$time2, sep=" "), "%Y-%m-%d %H:%M"))

# Verify violCd
violCds <- ddply(tktsDT, .(violCd), "nrow")
violCds <- arrange(violCds, as.numeric(violCd)) # Invalid codes 1,2,3 ?? Focus on fines instead?

redLight <- as.data.frame(subset(tktsDT, violCd==7))

busLane <- subset(tktsDT, violCd %in% c(18,19))
nrow(busLane)
head(busLane)
busLane.fine <- ddply(busLane, .(violCd, fine), "nrow")
busLane.fine115 <- as.data.frame(subset(busLane, fine==115))
busLane.fineX115 <- as.data.frame(subset(busLane, fine!=115))
# Started bus lane camera program on Monday, 22 November 2010
    busLane.window <- as.data.frame(subset(busLane, violDt2 %between% c("2010-11-21","2010-11-23")))
    busLane.window <- arrange(busLane.window, violDt2)
    busLane.post22Nov2010 <- as.data.frame(subset(busLane, violDt2 > "2010-11-22"))
    busLane.post22Nov2010.strt <- ddply(busLane.post22Nov2010, .(violStrt), "nrow")
subset(busLane.post22Nov2010, violDt2>"2012-01-01" & violCd=="18")

# Verify fines
violFines <- ddply(tktsDT, .(fine), "nrow")
violFines0 <- ddply(subset(tktsDT, fine==0), .(violCd), "nrow")

# Bring in DOF violation code info
tktsCds <- read.csv("Data/DOF_Parking_Violation_Codes.csv")
colnames(tktsCds) <- c("cd","definition","mnhttn96below","otherAreas")
tktsCds$fine <- str_extract(tktsCds$mnhttn96below, perl("\\d{2,3}")) # Pull out fine for 1st offense
unique(tktsCds$fine)

tkt.ctgry <- function(f) { # Assign code for range of fee amount -- proxy for "seriousness"
    x <- ifelse(f %in% c("50","60","65"), "A",
                ifelse(f %in% c("95","100","115"), "B",
                       ifelse(f %in% c("165","180"), "C",
                              ifelse(f=="265", "D", NA))))
    return(x)
}
tktsCds$ctgry <- foreach(i=1:nrow(tktsCds)) %do% tkt.ctgry(tktsCds$fine[i])

subset(tktsCds, ctgry=="D", select=c("definition"))
# A: parking infraction
# B: idling/blocking/stopping/standing
# C: block handicap zone/pedestrian ramp
# D: overnight tractor-trailer parking

# Verify vehicle makes
violMks <- ddply(tktsDT, .(vhclMk), "nrow")

# Verify street names

# Fix weird strings
# violStreets$violStrt2 <- sapply(violStreets$violStrt,function(row) iconv(row,to='UTF-8'))

tktsDT$violStrt[245165] <- "CENTRAL PK WEST"
tktsDT$violStrt[709688] <- "WILLIAMS ST"
tktsDT$violStrt[1441019] <- "SPRING ST"
tktsDT$violStrt[1584755] <- "ARL AVE"
tktsDT$violStrt[2207648] <- "E/S OF PEARL ST 40"
tktsDT$violStrt[2417516] <- "RIVERSD DR PARK"
tktsDT$violStrt[2417535:2417536] <- "RIVERSD DR PARK"
tktsDT$violStrt[2530299] <- "PARK AVE"
tktsDT$violStrt[2722843] <- "CATHERINE SLIP"

violStreets <- ddply(tktsDT, .(violStrt), "nrow")


