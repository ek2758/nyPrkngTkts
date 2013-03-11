library(stringr)
library(plyr)
library(reshape2)

# Scan file from Department of Finance per line
tktsScan <- scan("DOFOriginal.csv", character(0), sep="\n")
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
tktsData <- colsplit(tkts, ",", c())
