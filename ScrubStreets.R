library(plyr)
library(stringr)
library(RecordLinkage)
library(foreach)

head(violStreets)
violStreets <- arrange(violStreets, desc(nrow))
violStreets[1:20,]

levsDF.St <- levsDF.St[,1:7]

# ------------------
# Clean street names
# ------------------
pttrn.slash <- "\\b\\D/\\D\\b"
pttrn.NYC <- "\\bNYC\\b"
pttrn.punct <- "^[[:punct:]] ?"

for (i in 1:nrow(violStreets)) {
    detect.punct <- str_detect(violStreets$violStrt[i], perl(pttrn.punct))
    detect.slash <- str_detect(violStreets$violStrt[i], perl(pttrn.slash))
    
    replace.punct <- str_trim(str_replace(violStreets$violStrt[i], perl(pttrn.punct),""))
    replace.slash <- str_trim(str_replace(violStreets$violStrt[i], perl(pttrn.slash), ""))
    replace.NYC <- str_trim(str_replace(violStreets$violStrt[i], perl(pttrn.NYC), ""))
    
    violStreets$violStrt2[i] <- ifelse(detect.punct==TRUE, replace.punct, 
                                       ifelse(detect.slash==TRUE, replace.slash, replace.NYC))
}

# Start again
violStreets <- violStreets[,1:3]

# ----------------
# Fix street names
# ----------------
pttrn.6thAve <- "^[\\D ]{0,}(6|SIX) ?T?\\D{0,2} {0,2}([AS] ?V ?E?([NU][NU]E)?|[AV]E|VAE|AEV)"
pttrn.AveAmer <- "AVE? ?N?(UE)?S? {0,2}([IO]F|AT)? ?(T[EH][EH])? ?AME[NR]"
pttrn.7thAve <- "^[\\D ]{0,}(7|SEVEN) ?\\D{0,3} {0,2}(A ?[V\\.][EW]?(NU ?E)?|[AV]E|AEV|VAE|AV\\.?)"
pttrn.7thSouth <- "^7TH S$"
pttrn.Bleecker <- "BLEE?C?KE?RS?|BLKR"
pttrn.Street <- "SX?T(REEE?T)?\\.?$"
pttrn.West <- "^W[\\. ]?[(EST)\\d ]"
pttrn.Nmbr <- "\\d{1,3}( \\d{1,3})?"

for (i in 1:nrow(violStreets)) {
    detect.6thAve <- str_detect(violStreets$violStrt2[i], perl(pttrn.6thAve))
    detect.AveAmer <- str_detect(violStreets$violStrt2[i], perl(pttrn.AveAmer))
    detect.7thAve <- str_detect(violStreets$violStrt2[i], perl(pttrn.7thAve))
    detect.7thSouth <- str_detect(violStreets$violStrt2[i], perl(pttrn.7thSouth))
    detect.Bleecker <- str_detect(violStreets$violStrt2[i], perl(pttrn.Bleecker))
    detect.West <- str_detect(violStreets$violStrt2[i], perl(pttrn.West))
    detect.Street <- str_detect(violStreets$violStrt2[i], perl(pttrn.Street))
    
    replace.Street <- str_replace(violStreets$violStrt2[i], perl(pttrn.Street), "ST")
    
    if(detect.6thAve==TRUE | detect.AveAmer==TRUE) {
        violStreets$violStrt3[i] <- "6TH AVE"
    } else if(detect.7thAve==TRUE | detect.7thSouth==TRUE) {
        violStreets$violStrt3[i] <- "7TH AVE"
    } else if(detect.Bleecker==TRUE) {
        violStreets$violStrt3[i] <- "BLEECKER ST"
    } else if(detect.West==TRUE & detect.Street==TRUE) {
        violStreets$violStrt3[i] <- str_c("W", str_extract(violStreets$violStrt2[i], perl(pttrn.Nmbr)), "ST", sep=" ")
    } else {
        violStreets$violStrt3[i] <- replace.Street
    }
}


# -------------------------------
# Calculate Levenshtein distances
# -------------------------------
levenshtein.St <- function(x) {
    foreach(i=1:nrow(violStreets), .combine="cbind") %do% levenshteinSim(violStreets$violStrt3[i], x)
}

build.levenshteins.St <- function(x) {
    lev.6thAve <- levenshtein.St("6TH AVE")
    lev.7thAve <- levenshtein.St("7TH AVE")
    lev.Carmine <- apply(levenshtein.St(c("CARMINE","CARMINE ST")), 2, max)
    lev.Cornelia <- apply(levenshtein.St(c("CORNELIA","CORNELIA ST")), 2, max)
    lev.Jones <- apply(levenshtein.St(c("JONES","JONES ST")), 2, max)
    lev.W4th <- levenshtein.St("W 4 ST")
    lev.Bleecker <- apply(levenshtein.St(c("BLEECKER","BLEECKER ST")), 2, max)
    lev.Grove <- apply(levenshtein.St(c("GROVE","GROVE ST")), 2, max)
    lev.Greenwich <- apply(levenshtein.St(c("GREENWICH","GREENWICH AVE")), 2, max)
    
    x <- rbind(lev.6thAve, lev.7thAve, lev.Carmine, lev.Cornelia, lev.Jones,
               lev.W4th, lev.Bleecker, lev.Grove, lev.Greenwich)
    
    x <- as.data.frame(t(x))
    
    return(x)
}

levsDF.St <- build.levenshteins.St(levs)
colnames(levsDF.St) <- c("lev.6thAve", "lev.7thAve", "lev.Carmine", "lev.Cornelia", "lev.Jones",
                         "lev.W4th", "lev.Bleecker", "lev.Grove", "lev.Greenwich")

# -----------------------------------------------
# Search for maximum calculated Levenshtein score
# -----------------------------------------------
levsDF.St <- get.levenshteins(levsDF.St)

# Attach dirty names to levenshtein calculations
levsDF.St <- cbind(levsDF.St, violStreets$violStrt)
colnames(levsDF.St)[ncol(levsDF.St)] <- "violStrt"

# Check length of columns --> s/b 70499
apply(levsDF.St, 2, length) # 2 indicates by column


# ----------------------
# Browse classifications
# ----------------------

# These appear legit
subset(levsDF.St, lev.name=="lev.6thAve" & lev.scores>=0.9, select=c("violStrt","lev.name","lev.scores"))
subset(levsDF.St, lev.name=="lev.7thAve" & lev.scores>=0.9, select=c("violStrt","lev.name","lev.scores"))
subset(levsDF.St, lev.name=="lev.W4th" & lev.scores>=0.9, select=c("violStrt","lev.name","lev.scores"))

subset(levsDF.St, lev.name=="lev.Carmine" & lev.scores>=0.83, select=c("violStrt","lev.name","lev.scores"))

subset(levsDF.St, lev.name=="lev.Jones" & lev.scores>=0.8, select=c("violStrt","lev.name","lev.scores"))
subset(levsDF.St, lev.name=="lev.Grove" & lev.scores>=0.8, select=c("violStrt","lev.name","lev.scores"))

subset(levsDF.St, lev.name=="lev.Cornelia" & lev.scores>=0.75, select=c("violStrt","lev.name","lev.scores"))
subset(levsDF.St, lev.name=="lev.Bleecker" & lev.scores>=0.75, select=c("violStrt","lev.name","lev.scores"))

subset(levsDF.St, lev.name=="lev.Greenwich" & lev.scores>=0.73, select=c("violStrt","lev.name","lev.scores"))

# Assign 1 to column translate for vehicle make corrections
translate.St <- function(x) {
    foreach(i=1:nrow(x)) %do%
        if(x$lev.name[i] %in% c("lev.6thAve","lev.7thAve","lev.W4th") & x$lev.scores[i]>=0.9) {
            return(1)
        } else if(x$lev.name[i] %in% "lev.Carmine" & x$lev.scores[i]>=0.83) {
            return(1)
        } else if(x$lev.name[i] %in% c("lev.Jones","lev.Grove") & x$lev.scores[i]>=0.8) {
            return(1)
        } else if(x$lev.name[i] %in% c("lev.Cornelia","lev.Bleecker") & x$lev.scores[i]>=0.75) {
            return(1)
        } else if(x$lev.name[i] %in% "lev.Greenwich" & x$lev.scores[i]>=0.73) {
            return(1)
        } else {
            return(0)
        }
}
translations.St <- ldply(translate.St(levsDF.St))
levsDF.St <- cbind(levsDF.St, translations.St)
colnames(levsDF.St)[ncol(levsDF.St)] <- "translate"
levsDF.StT <- subset(levsDF.St, translate==1, select=c("violStrt","lev.result","lev.name","translate"))

# ------------------------
# Assign nice street names
# ------------------------
for (i in 1:nrow(levsDF.StT)) {
    n <- levsDF.StT$lev.name[i]
    
    if(n == "lev.6thAve") {
        levsDF.StT$stNames[i] <- "6TH AVE"
    } else if(n == "lev.7thAve") {
        levsDF.StT$stNames[i] <- "7TH AVE"
    } else if(n == "lev.Bleecker") {
        levsDF.StT$stNames[i] <- "BLEECKER ST"
    } else if(n == "lev.Carmine") {
        levsDF.StT$stNames[i] <- "CARMINE ST"
    } else if(n == "lev.Cornelia") {
        levsDF.StT$stNames[i] <- "CORNELIA ST"
    } else if(n == "lev.Greenwich") {
        levsDF.StT$stNames[i] <- "GREENWICH AVE"
    } else if(n == "lev.Grove") {
        levsDF.StT$stNames[i] <- "GROVE ST"
    } else if(n == "lev.Jones") {
        levsDF.StT$stNames[i] <- "JONES ST"
    } else if(n == "lev.W4th") {
        levsDF.StT$stNames[i] <- "W 4 ST"
    } else {
        levsDF.StT$stNames[i] <- NA
    }
}

# Make a numeric join key (i.e., row number)
violStreets$rn <- rownames(violStreets)
levsDF.StT$rn <- str_extract(levsDF.StT$lev.result,"\\d+")

violStreets <- join(violStreets, subset(levsDF.StT, select=c("rn","stNames")), by="rn")