library(plyr)
library(stringr)
library(ggplot2)

head(gv)
sapply(gv, class)

# Add footage of street lengths
footage <- foreach(i=1:nrow(gv)) %do% {
    ifelse(!is.na(gv$park[i]),
           ifelse(gv$park[i]=="pDumb",
                  ifelse(gv$stNames[i]=="7TH AVE S", 2350,
                         ifelse(gv$stNames[i]=="6TH AVE", 2160,
                                ifelse(gv$stNames[i]=="JONES ST", 430,
                                       ifelse(gv$stNames[i]=="GREENWICH AVE", 1040)))),
                  ifelse(gv$park[i]=="pSmart",
                         ifelse(gv$stNames[i]=="7TH AVE S", 1315,
                                ifelse(gv$stNames[i]=="6TH AVE", 1785,
                                       ifelse(gv$stNames[i]=="W 4 ST",1000,
                                              ifelse(gv$stNames[i]=="CARMINE ST", 180,
                                                     ifelse(gv$stNames[i]=="CORNELIA ST", 400,
                                                            ifelse(gv$stNames[i]=="JONES ST", 430,
                                                                   ifelse(gv$stNames[i]=="GROVE ST", 220,
                                                                          ifelse(gv$stNames[i]=="BLEECKER ST", 325,
                                                                                 ifelse(gv$stNames[i]=="GREENWICH AVE", 110))))))))))
                  ), NA)
}
footage.DF <- ldply(footage)
gv <- cbind(gv, footage.DF)
colnames(gv)[ncol(gv)] <- "footage"

# Create triState variable
gv <- within(gv, {
    triState <- ifelse(!is.na(lcnsPlt) & lcnsPlt %in% c("NY","NJ","CT"),"Y","N")
    parkDate <- ifelse(violDt2>="2008-10-01","Post","Pre")
})
             
ddply(gv, .(parkDate, park), summarise,
      tktsN = sum(!is.na(smmsNo)),
      maxDate = max(violDt2),
      minDate = min(violDt2),
      tktsNdaily = sum(!is.na(smmsNo))/as.numeric((max(violDt2)-min(violDt2))),
      meanFoot = sum(unique(footage)),
      tktsPerFoot = length(smmsNo)/sum(unique(footage)),
      meanFine = mean(as.numeric(fine)),
      personal.share = sum(ifelse(!is.na(vehType) & vehType=="personal",1,0))/sum(!is.na(vehType)),
      triState.share = sum(ifelse(triState=="Y",1,0))/length(smmsNo))