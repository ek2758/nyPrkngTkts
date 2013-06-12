library(stringr)
library(reshape2)

subModels <- read.csv("nyData/subModelsEdmunds.csv", header=FALSE, stringsAsFactors=FALSE)
head(subModels)
colnames(subModels) <- c("makeId","subModel","body","styleIds")

# Find max number of commas 
subModels$styleIds[which((str_count(subModels$styleIds, perl("\\,")))==31)]

# Unload styleIds to run through API again for Edmunds true market values
styleIds.long <- foreach(i=1:nrow(subModels)) %do% {
    melted <- melt(str_split(subModels$styleIds[i], "\\, "))
    cbind(subModels$makeId[i], melted)
}
styleIds.long <- ldply(styleIds.long)
styleIds.long <- styleIds.long[,1:2]
colnames(styleIds.long)[1:2] <- c("makeId","styleId")
styleIds.long$styleId <- str_extract(styleIds.long$styleId, perl("\\d+"))


write.table(styleIds.long[,2], file="nyData/styleIdsEdmunds.csv", row.names=FALSE, col.names=FALSE, sep=",")