makesEdmunds <- read.csv("nyData/makesEdmunds2.csv", header=FALSE, stringsAsFactors=FALSE)
colnames(makesEdmunds) <- c("ids","niceNames")

gwDF.vehType2A <- as.data.frame(ddply(subset(gwDT, vehType2!="business"), .(vehType2), "nrow"))

makesEdmunds <- merge(makesEdmunds, gwDF.vehType2A, by.x="niceNames", by.y="vehType2", all.x=TRUE)

makesEdmunds <- na.omit(makesEdmunds)
write.table(makesEdmunds[,2], file="nyData/makesEdmundsTktSubset.csv", row.names=FALSE, col.names=FALSE, sep=",")