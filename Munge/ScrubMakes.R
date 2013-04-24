library(plyr)
library(stringr)
library(RecordLinkage)
library(foreach)
library(ggplot2)

# Listing of Edmunds vehicle makes with DOF vehicle make translations
makesEdmunds <- read.csv("Data/makesEdmunds2.csv", header=TRUE, stringsAsFactors=FALSE)
head(makesEdmunds)

vhclMk2 <- ldply(foreach(i=1:nrow(violMks)) %do% 
    str_replace(violMks$vhclMk[i], perl("^[[:punct:]] ?|[[:punct:]]$"),""))
violMks <- cbind(violMks, vhclMk2)
colnames(violMks) <- c("vhclMk","nrow","vhclMk2")


# --------------------------------------------------
# Fix names -- alternative spellings, usually models
# --------------------------------------------------
pttrn.audi <- "^QUAT$" # Quattro
pttrn.bent <- "^AZURE$" # Azure
pttrn.chvy <- "^TRAI?LE?$" # Trailblazer
pttrn.honda <- "^ACC[AO]$|^ACD$" # Accord
pttrn.hyun <- "^AZERA$" # Azera
pttrn.mazda <- "MAZBA" # Prevent mis-spelling as Maybach classification
pttrn.mbenz <- "^BE?N[XZ]$|^M[ .]BE?N?$|^MCBZ$|^MER-B$"
pttrn.nissan <- "^MAXMA$" # Maxima
pttrn.olds <- "^ALER[ADO]?$" # Alero
pttrn.pont <- "^AZTE[CR]$|^SOLS?[ET]E?$" # Aztec, Solstice
pttrn.rover <- "^L[AD]?N?[/ -] ?RO?V?$|^LAN[DOR]/$|^LDR?O?V?R?$|^\\d?ROVE?$|^R.? ROV?$|^R[AGN]?[./ ] ?RO?V?$|^RANGE?/?$|^RGR[OV][VR]$|^RNG ?RO?$"
pttrn.toyo <- "^SOLER?O$" # Solero
pttrn.volks <- "^VOL[LK][OS]$|^VO?L?[./ ]WA?G?$|^VW$|^JETT[ES]?$" # Jetta

# Initial clean on some vehicles makes, standardize brands
for (i in 1:nrow(violMks)) {
    detect.audi <- str_detect(violMks$vhclMk2[i], perl(pttrn.audi))
    detect.bent <- str_detect(violMks$vhclMk2[i], perl(pttrn.bent))
    detect.chvy <- str_detect(violMks$vhclMk2[i], perl(pttrn.chvy))
    detect.honda <- str_detect(violMks$vhclMk2[i], perl(pttrn.honda))
    detect.hyun <- str_detect(violMks$vhclMk2[i], perl(pttrn.hyun))
    detect.nissan <- str_detect(violMks$vhclMk2[i], perl(pttrn.nissan))
    detect.mbenz <- str_detect(violMks$vhclMk2[i], perl(pttrn.mbenz))
    detect.olds <- str_detect(violMks$vhclMk2[i], perl(pttrn.olds))
    detect.pont <- str_detect(violMks$vhclMk2[i], perl(pttrn.pont))
    detect.rover <- str_detect(violMks$vhclMk2[i], perl(pttrn.rover))
    detect.toyo <- str_detect(violMks$vhclMk2[i], perl(pttrn.toyo))
    detect.volks <- str_detect(violMks$vhclMk2[i], perl(pttrn.volks))
    
    if(detect.audi==TRUE) {
        violMks$vhclMk3[i] <- "AUDI"
    } else if(detect.bent==TRUE) {
        violMks$vhclMk3[i] <- "BENTL"
    } else if(detect.chvy==TRUE) {
        violMks$vhclMk3[i] <- "CHEVR"
    } else if(detect.honda==TRUE) {
        violMks$vhclMk3[i] <- "HONDA"
    } else if(detect.hyun==TRUE) {
        violMks$vhclMk3[i] <- "HYUND"
    } else if(detect.nissan==TRUE) {
        violMks$vhclMk3[i] <- "NISSA"
    } else if(detect.mbenz==TRUE) {
        violMks$vhclMk3[i] <- "ME/BE"
    } else if(detect.olds==TRUE) {
        violMks$vhclMk3[i] <- "OLDSM"
    } else if(detect.pont==TRUE) {
        violMks$vhclMk3[i] <- "PONTI"
    } else if(detect.rover==TRUE) {
        violMks$vhclMk3[i] <- "ROVER"
    } else if(detect.toyo==TRUE) {
        violMks$vhclMk3[i] <- "TOYOT"
    } else if(detect.volks==TRUE) {
        violMks$vhclMk3[i] <- "VOLKS"
    } else {
        violMks$vhclMk3[i] <- NA
    }
}

# Collapse columns
violMks$vhclMk4 <- ifelse(!is.na(violMks$vhclMk3), violMks$vhclMk3, violMks$vhclMk2)

# -------------------------------
# Calculate Levenshtein distances
# -------------------------------
levenshtein <- function(x) {
    foreach(i=1:nrow(violMks), .combine="cbind") %do% levenshteinSim(violMks$vhclMk4[i], x)
}

build.levenshteins <- function(x) {
    lev.acura <- levenshtein("ACURA")
    lev.alfar <- levenshtein("ALFAR")
    lev.audi <- levenshtein("AUDI")
    lev.bmw <- levenshtein("BMW")
    lev.bentl <- levenshtein("BENTL")
    lev.buick <- levenshtein("BUICK")
    lev.cadil <- levenshtein("CADIL")
    lev.chevr <- levenshtein("CHEVR")
    lev.chrys <- levenshtein("CHRYS")
    lev.daewo <- levenshtein("DAEWO")
    lev.dodge <- levenshtein("DODGE")
    lev.fiat <- levenshtein("FIAT")
    lev.ferra <- levenshtein("FERRA")
    lev.ford <- levenshtein("FORD")
    lev.gmc <- levenshtein("GMC")
    lev.geo <- levenshtein("GEO")
    lev.humme <- levenshtein("HUMME")
    lev.honda <- levenshtein("HONDA")
    lev.hyund <- levenshtein("HYUND")
    lev.infin <- levenshtein("INFIN")
    lev.isuzu <- levenshtein("ISUZU")
    lev.jagua <- levenshtein("JAGUA")
    lev.jeep <- levenshtein("JEEP")
    lev.kia <- levenshtein("KIA")
    lev.lambo <- levenshtein("LAMBO")
    lev.rover <- levenshtein("ROVER")
    lev.lexus <- levenshtein("LEXUS")
    lev.linco <- levenshtein("LINCO")
    lev.lotus <- levenshtein("LOTUS")
    lev.mini <- levenshtein("MINI")
    lev.mayba <- levenshtein("MAYBA")
    lev.mazda <- levenshtein("MAZDA")
    lev.mebe <- levenshtein("ME/BE")
    lev.mercu <- levenshtein("MERCU")
    lev.mitsu <- levenshtein("MITSU")
    lev.nissa <- levenshtein("NISSA")
    lev.oldsm <- levenshtein("OLDSMO")
    lev.plymo <- levenshtein("PLYMO")
    lev.ponti <- levenshtein("PONTI")
    lev.porsc <- levenshtein("PORSC")
    lev.rolls <- levenshtein("ROLLS")
    lev.srt <- levenshtein("SRT")
    lev.saab <- levenshtein("SAAB")
    lev.satur <- levenshtein("SATUR")
    lev.scion <- levenshtein("SCION")
    lev.subar <- levenshtein("SUBAR")
    lev.suzuk <- levenshtein("SUZUK")
    lev.tesla <- levenshtein("TESLA")
    lev.toyot <- levenshtein("TOYOT")
    lev.volks <- levenshtein("VOLKS")
    lev.volvo <- levenshtein("VOLVO")
    lev.smart <- levenshtein("SMART")
    
    x <- rbind(lev.acura,lev.alfar,lev.audi,lev.bmw,lev.bentl,lev.buick,lev.cadil,lev.chevr,lev.chrys,
               lev.daewo,lev.dodge,lev.fiat,lev.ferra,lev.ford,lev.gmc,lev.geo,lev.humme,lev.honda,
               lev.hyund,lev.infin,lev.isuzu,lev.jagua,lev.jeep,lev.kia,lev.lambo,lev.rover,lev.lexus,
               lev.linco,lev.lotus,lev.mini,lev.mayba,lev.mazda,lev.mebe,lev.mercu,lev.mitsu,lev.nissa,
               lev.oldsm,lev.plymo,lev.ponti,lev.porsc,lev.rolls,lev.srt,lev.saab,lev.satur,lev.scion,
               lev.subar,lev.suzuk,lev.tesla,lev.toyot,lev.volks,lev.volvo,lev.smart)
    
    x <- as.data.frame(t(x))
    
    return(x)
}
levsDF <- build.levenshteins(levs)
colnames(levsDF) <- c("lev.acura","lev.alfar","lev.audi","lev.bmw","lev.bentl","lev.buick","lev.cadil","lev.chevr","lev.chrys",
                              "lev.daewo","lev.dodge","lev.fiat","lev.ferra","lev.ford","lev.gmc","lev.geo","lev.humme","lev.honda",
                              "lev.hyund","lev.infin","lev.isuzu","lev.jagua","lev.jeep","lev.kia","lev.lambo","lev.rover","lev.lexus",
                              "lev.linco","lev.lotus","lev.mini","lev.mayba","lev.mazda","lev.mebe","lev.mercu","lev.mitsu","lev.nissa",
                              "lev.oldsm","lev.plymo","lev.ponti","lev.porsc","lev.rolls","lev.srt","lev.saab","lev.satur","lev.scion",
                              "lev.subar","lev.suzuk","lev.tesla","lev.toyot","lev.volks","lev.volvo","lev.smart")
# Start again
levsDF <- levsDF[,1:52]

# -----------------------------------------------
# Search for maximum calculated Levenshtein score
# -----------------------------------------------
get.levenshteins <- function(x) {
    lev.max.score <- apply(x, 1, max, na.rm=TRUE) # Get maximum scores
    lev.max <- apply(x, 1, function(y) which(y == max(y)))
    lev.max.names <- lapply(lev.max, names) # Get names of highest Levenshtein scores (tied and winners)
    lev.max.namesN <- lapply(lev.max.names, length) # Count number of returned names
    
    lev.scores <- ldply(lev.max.score) # Attach scores
    lev.namesN <- ldply(lev.max.namesN) # Attach numbers of DF
    
    x <- cbind(x, lev.scores[,1], lev.scores[,2], lev.namesN[,2])
    colnames(x)[(ncol(x)-2):ncol(x)] <- c("lev.result","lev.scores","lev.namesN")
    
    levWin <- foreach(i=1:nrow(x)) %do% if(x$lev.namesN[i]==1) { # Assign names
        return(lev.max.names[[i]])
    } else {
        return(NA)
    }
    
    lev.name <- ldply(levWin)
    
    x <- cbind(x, lev.name[,1]) # Attach winning Levenshtein code
    colnames(x)[ncol(x)] <- "lev.name" # Correct column names
    
    return(x)  
}

levsDF <- get.levenshteins(levsDF)

# Attach dirty names to levenshtein calculations
levsDF <- cbind(levsDF, violMks$vhclMk)
colnames(levsDF)[ncol(levsDF)] <- "vhclMk"

# ----------------------
# Browse classifications
# ----------------------

# These appear legit
subset(levsDF, lev.scores>=0.75, select=c("vhclMk","lev.name","lev.scores"))

# These appear legit
subset(levsDF, lev.scores>=0.65 & lev.scores<0.75 &
           !(lev.name %in% c("lev.bmw","lev.gmc","lev.geo","lev.kia","lev.srt")), 
       select=c("vhclMk","lev.name","lev.scores"))

# Don't like these classifications
subset(levsDF, lev.scores>=0.6 & lev.scores<0.65 &
           !(lev.name %in% c("lev.bmw","lev.gmc","lev.geo","lev.kia","lev.srt")), 
       select=c("vhclMk","lev.name","lev.scores"))

# Pretty bad -- initial spellings are not great to start
subset(levsDF, lev.scores>=0.6 & lev.scores<0.65, select=c("vhclMk","lev.name","lev.scores"))

# Assign 1 to column translate for vehicle make corrections
translate <- function(x) {
    foreach(i=1:nrow(x)) %do%
        if(x$lev.name[i] %in% c("lev.bmw","lev.gmc","lev.geo","lev.kia","lev.srt") & 
               x$lev.scores[i]>=0.75) {
            return(1)
            } else if(!(x$lev.name[i] %in% c("lev.bmw","lev.gmc","lev.geo","lev.kia","lev.srt")) & 
                          x$lev.scores[i]>=0.65) {
                return(1)
                } else {
                    return(0)
                }
}
translations <- ldply(translate(levsDF))
levsDF <- cbind(levsDF, translations)
colnames(levsDF)[ncol(levsDF)] <- "translate"

# ----------------------------
# Assign Edmunds.com niceNames
# ----------------------------
levsDF <- join(levsDF, subset(makesEdmunds, select=c("lev.name","niceNames")),
               by="lev.name",
               type="left") # Assign vehicle name
levsDF <- arrange(levsDF, as.numeric(str_extract(lev.result,"\\d+")))
violMks <- cbind(violMks, subset(levsDF, select=c("lev.scores","translate","niceNames")))