library(plyr)
library(stringr)

vhclBodies <- arrange(vhclBodies, desc(nrow))
vhclBodies[1:20,]

# Start again
vhclBodies <- vhclBodies[,1:2]

# SUBN: station wagons, SUVs, hearses, ambulances (per NY V&T law)
pttrn.sedan <- "^4(?!(X|\\d))|4D|SEDA|^SE?DN$|FOUR|^DSD$"
pttrn.wagon <- "^WAGO?$|^WGN?$|STA[KT]|HATC"
pttrn.coupe <- "^D?2(?!\\d)|^(?!\\d)2|^\\D*2$|TWOD|CO[MU]P"
pttrn.conv <- "^CONV?$|^CV$"
pttrn.trck <- "^TRU?CK?$|^TRUK?$|^TK$|^REG$|PICK|^P/U$|^P-U$"
pttrn.suv <- "^U[ITL][ILT]?[CILT]?$|JEEP|^SUV$|^SWT?$|^SUB[ANU]$"
pttrn.van <- "MIN[IV]|^M?VAN$|^VN$"
pttrn.other <- "P/SH|S/SP|SN/P"

pttrn.taxi <- "^TA?XI$|^OMT$|^TLC$|^CAB$"
pttrn.moto <- "^MY?CY?L?$|^MO[TY][CEOR]?$|MORT|^M[CT][CRY]?[CY]$|^MT[RY]$|^CYCL?$|DUCA"
pttrn.scoot <- "SC[OT][OR]|^MOP[DE]?$|^VESP?$"
pttrn.bike <- "BIKE|PEDI|BICY"
pttrn.bus <- "^BU?S$|^BU$|SCHO|^OM[LFNRSV]$"
pttrn.comm2 <- "^DEL[IV]?$|^RE?[FI][EGIR]?$|^TRA?[CK]$|^TR?/T$|^TR?A?[LT][LR]?$|^LTR$|^FLA?T$|FLEE|MOBL|^TR[AR]I?$|OMNI|^FR[EHI][EGIT]?$|^FRT$|^T[RT][LT]?$|^INT[EL]?$|VAN[HT]|UHA[LU]|^[DU]HL$|WORK|MACK|ARMO|POST|^TOWT?$|TANK|SEMI|^MCI$|^UPS$|H/TR|DEMA|^BOXT?$|^TR?/[CE]R?$|R/RD|W/DR|CUST|^APP$|^EMVR$|^COMM?$|^TCK$"
pttrn.garb <- "GARB|DUMP"
pttrn.limo <- "^LIMO?$"
pttrn.govt <- "^GO?V$|^SR[FN]$"
pttrn.hrse <- "HRSE" #hearse
pttrn.campr <- "CAMP|^RV$|^BOAT$|H/WH|TRAV|WINN"

for (i in 1:nrow(vhclBodies)) {
    
    # Passenger vehicle body types
    detect.sedan <- str_detect(vhclBodies$vhclBdy[i], perl(pttrn.sedan))
    detect.wagon <- str_detect(vhclBodies$vhclBdy[i], perl(pttrn.wagon))
    detect.coupe <- str_detect(vhclBodies$vhclBdy[i], perl(pttrn.coupe))
    detect.conv <- str_detect(vhclBodies$vhclBdy[i], perl(pttrn.conv))
    detect.trck <- str_detect(vhclBodies$vhclBdy[i], perl(pttrn.trck))
    detect.suv <- str_detect(vhclBodies$vhclBdy[i], perl(pttrn.suv))
    detect.van <- str_detect(vhclBodies$vhclBdy[i], perl(pttrn.van))
    
    # Other non-passenger vehicle types
    detect.taxi <- str_detect(vhclBodies$vhclBdy[i], perl(pttrn.taxi))
    detect.moto <- str_detect(vhclBodies$vhclBdy[i], perl(pttrn.moto))
    detect.scoot <- str_detect(vhclBodies$vhclBdy[i], perl(pttrn.scoot))
    detect.bike <- str_detect(vhclBodies$vhclBdy[i], perl(pttrn.bike))
    detect.bus <- str_detect(vhclBodies$vhclBdy[i], perl(pttrn.bus))
    detect.comm2 <- str_detect(vhclBodies$vhclBdy[i], perl(pttrn.comm2))
    detect.garb <- str_detect(vhclBodies$vhclBdy[i], perl(pttrn.garb))
    detect.limo <- str_detect(vhclBodies$vhclBdy[i], perl(pttrn.limo))
    detect.govt <- str_detect(vhclBodies$vhclBdy[i], perl(pttrn.govt))
    detect.hrse <- str_detect(vhclBodies$vhclBdy[i], perl(pttrn.hrse))
    detect.campr <- str_detect(vhclBodies$vhclBdy[i], perl(pttrn.campr))
    
    if(detect.sedan==TRUE) {
        vhclBodies$vhclBdy2[i] <- "sedan"
    } else if(detect.wagon==TRUE) {
        vhclBodies$vhclBdy2[i] <- "wagon"
    } else if(detect.coupe==TRUE) {
        vhclBodies$vhclBdy2[i] <- "coupe"
    } else if(detect.conv==TRUE) {
        vhclBodies$vhclBdy2[i] <- "convertible"
    } else if(detect.trck==TRUE) {
        vhclBodies$vhclBdy2[i] <- "truck"
    } else if(detect.suv==TRUE) {
        vhclBodies$vhclBdy2[i] <- "suv"
    } else if(detect.van==TRUE) {
        vhclBodies$vhclBdy2[i] <- "van"
    } else {
        vhclBodies$vhclBdy2[i] <- NA
    }
        
    if(detect.taxi==TRUE) {
        vhclBodies$other[i] <- "taxi"
    } else if(detect.moto==TRUE) {
        vhclBodies$other[i] <- "motorcycle"
    } else if(detect.scoot==TRUE) {
        vhclBodies$other[i] <- "scooter"
    } else if(detect.bike==TRUE) {
        vhclBodies$other[i] <- "bicycle"
    } else if(detect.bus==TRUE) {
        vhclBodies$other[i] <- "bus"
    } else if(detect.comm2==TRUE) {
        vhclBodies$other[i] <- "commercial"
    } else if(detect.garb==TRUE) {
        vhclBodies$other[i] <- "garbage"
    } else if(detect.limo==TRUE) {
        vhclBodies$other[i] <- "limo"
    } else if(detect.govt==TRUE) {
        vhclBodies$other[i] <- "government"
    } else if(detect.hrse==TRUE) {
        vhclBodies$other[i] <- "hearse"
    } else if(detect.campr==TRUE) {
        vhclBodies$other[i] <- "camper"
    } else {
        vhclBodies$other[i] <- NA
    }
}

vhclBodies.left <- subset(vhclBodies, is.na(vhclBdy2) & is.na(other))
vhclBodies.left <- arrange(vhclBodies.left, desc(nrow)) # are the rest just commercial vehicles?

head(vhclBodies)
vhclBodies$bodyNames <- ifelse(is.na(vhclBodies$vhclBdy2), vhclBodies$other, vhclBodies$vhclBdy2)

