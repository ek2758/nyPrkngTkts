head(levsDF.StT)
head(tktsDT)
library(data.table)
library(stringr)
library(plyr)
library(ggplot2)
library(xts)

# Check for NAs in vehicle make assignments -- should get nothing here
subset(levsDF.T, is.na(niceNames))

# Join up vehicle makes
tkts <- merge(tktsDT, subset(levsDF.T, select=c("vhclMk","niceNames")), by="vhclMk", all.x=TRUE)

# Join up street names
tkts <- merge(tkts, subset(levsDF.StT, select=c("violStrt","stNames")), by="violStrt", all.x=TRUE)

violMks.bodyTyps <- ddply(tkts, .(niceNames, vhclBdy), "nrow")
head(violMks.bodyTyps)

# Join up body types
tkts <- merge(tkts, subset(vhclBodies, select=c("vhclBdy","bodyNames")), by="vhclBdy", all.x=TRUE)

# Organize variables
summary(tkts)
sapply(tkts, class)
tkts$violAddrss[which(str_detect(tkts$violAddrss, perl("[a-zA-Z]")))]
tkts[105599:105603,]

# Subset parking tickets in Greenwich Village
gv <- subset(tkts, !is.na(stNames) & !is.na(violAddrss))

# Subset ParkSmart blocks
gv$addrssNum <- as.numeric(str_extract(gv$violAddrss, perl("\\d*")))
gv <- within(gv, {
    park <- ifelse(stNames=="7TH AVE S" & addrssNum %in% 1:60|
                     stNames=="7TH AVE S" & addrssNum %in% 113:149 & addrssNum %% 2==0 |
                     stNames=="7TH AVE S" & addrssNum %in% 157:199|
                     stNames=="6TH AVE" & addrssNum %in% 424:525|
                     stNames=="GREENWICH AVE" & addrssNum %in% 33:68|
                     stNames=="JONES ST" & addrssNum %in% 2:52 & addrssNum %% 2==0,
                 "pDumb",
                 ifelse(stNames=="7TH AVE S" & addrssNum %in% 70:150 & addrssNum %% 2==0|
                            stNames=="7TH AVE S" & addrssNum %in% 87:99 & addrssNum %% 2!=0|
                            stNames=="6TH AVE" & addrssNum %in% 241:281 & addrssNum %% 2!=0|
                            stNames=="6TH AVE" & addrssNum %in% 301:333 & addrssNum %% 2!=0|
                            stNames=="6TH AVE" & addrssNum %in% 322:354 & addrssNum %% 2==0|
                            stNames=="6TH AVE" & addrssNum %in% 347:403 & addrssNum %% 2!=0|
                            stNames=="W 4 ST" & addrssNum %in% 122:150 & addrssNum %% 2==0|
                            stNames=="W 4 ST" & addrssNum %in% 159:193 & addrssNum %% 2!=0|
                            stNames=="W 4 ST" & addrssNum %in% 225:229 & addrssNum %% 2!=0|
                            stNames=="CARMINE ST" & addrssNum %in% 5:13 & addrssNum %% 2!=0|
                            stNames=="CORNELIA ST" & addrssNum %in% 5:37|
                            stNames=="JONES ST" & addrssNum %in% 3:31 & addrssNum %% 2!=0|
                            stNames=="GROVE ST" & addrssNum %in% 49:61|
                            stNames=="BLEECKER ST" & addrssNum %in% 316:346 & addrssNum %% 2==0|
                            stNames=="GREENWICH AVE" & addrssNum %in% 1:5 & addrssNum %% 2!=0,
                        "pSmart", NA))
})

gv$vehType <- ifelse(is.na(gv$bodyNames), NA,
                       ifelse(gv$bodyNames %in% c("bus","commercial","garbage","limo","taxi","van"), "commercial","personal"))

gv$vehType2 <- ifelse(is.na(gv$bodyNames), NA,
                       ifelse(gv$bodyNames %in% c("bus","commercial","garbage","limo","taxi","van"), "commercial",
                              as.character(gv$niceNames)))

gv <- subset(gv, select=c("smmsNo","lcnsPlt","violAddrss","fine","violDt2","park","niceNames","stNames","bodyNames","vehType"))

# Save Workspace here: tktsClean.RData
save.image("~/Documents/Rich Bad Behavior/nyPrkngTkts/nyData/tktsClean.RData")

# Create condensed workspace with cleaned datasets
rm(list=ls()[!ls() %in% c("gv", "tkts")])

# Save workspace here: tktsCleaned.RData
save.image("~/Documents/Rich Bad Behavior/nyPrkngTkts/nyData/tktsCleaned.RData")