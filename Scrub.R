library(stringr)
library(plyr)

# Sort streets by frequency
redLightStreets <- arrange(redLightStreets, desc(nrow))

# Write regular expressions and add fix column in redLightStreets dataframe
redLightStreets$violStrt[11284] <- "SPRING ST" # Another weird unicode error

# Initial scrub
redLightStreets$violStrt <- str_replace(redLightStreets$violStrt, perl("WE?ST?"),"W")
redLightStreets$violStrt <- str_replace(redLightStreets$violStrt, "EAST","E")
redLightStreets$violStrt <- str_replace(redLightStreets$violStrt, perl(" NYC?$"),"")
redLightStreets$violStrt <- str_replace(redLightStreets$violStrt, perl(" TRANSVERSE"),"")
redLightStreets$violStrt <- str_replace(redLightStreets$violStrt, perl("\\(?\\bREAR\\b\\)? ?(OF )?"),"")
redLightStreets$violStrt <- str_replace(redLightStreets$violStrt, perl("[RSWECNIF]/[SOWEN]/?C? "),"")
redLightStreets$violStrt <- str_replace(redLightStreets$violStrt, perl("^OF "),"")

# Re-run from beginning
redLightStreets <- redLightStreets[,1:2]

# Extract zipcodes, standardize syntax of avenues and streets, validate
pttrn.Zip <- "100\\d{2}"
pttrn.Ave <- "\\d{1,3} {0,2}\\w{0,3} {0,3}(AVE(NUE)?|AV|AE|VE)" # Find any avenue
pttrn.VAve <- "^\\d{1,3} {0,2}[A-Z]{0,3} {0,3}(AVE3?(NUE)?|AV|AE|VE)\\.?$" # Validate avenues
pttrn.Nmbr <- "\\d{1,3}"
pttrn.VSt <- "^[EW] ?\\d{1,3} ?((ST)|(RD)|(TH)|(ND))? ?S?T?(REET)?$" # Validate streets
pttrn.VSt2 <- "^R ?\\d{1,3} ?((ST)|(RD)|(TH)|(ND))? ?S?T?(REET)?$" # Typo for East side
pttrn.Side <- "^\\w"
redLightStreets$violStrt[which(str_detect(redLightStreets$violStrt, perl(pttrn.VAve)))]
str_extract(str_extract(redLightStreets$violStrt[1489], perl(pttrn.VAve)), perl(pttrn.Nmbr))
    for (i in 1:nrow(redLightStreets)) {
        detect.Zip <- str_detect(redLightStreets$violStrt[i], perl(pttrn.Zip))
        extract.Zip <- str_extract(redLightStreets$violStrt[i], perl(pttrn.Zip))
        detect.VAve <- str_detect(redLightStreets$violStrt[i], perl(pttrn.VAve))
        detect.Ave <- str_extract(str_extract(redLightStreets$violStrt[i], perl(pttrn.Ave)), perl(pttrn.Nmbr))
        extract.Ave <- str_extract(redLightStreets$violStrt[i], perl(pttrn.Nmbr))
        detect.VSt <- str_detect(redLightStreets$violStrt[i], perl(pttrn.VSt))
        detect.VSt2 <- str_detect(redLightStreets$violStrt[i], perl(pttrn.VSt2))
        extract.St <- str_extract(redLightStreets$violStrt[i], perl(pttrn.Nmbr))
        extract.Side <- str_extract(redLightStreets$violStrt[i],perl(pttrn.Side))
        if (detect.Zip==TRUE) {
            redLightStreets$Zip[i] <- extract.Zip
        } else {
            redLightStreets$Zip[i] <- ""
        }
        if (detect.Ave==TRUE) {
            redLightStreets$fixStrt[i] <- str_c(extract.Ave, "AVE", sep=" ")
        } else if (detect.St==TRUE) {
            redLightStreets$fixStrt[i] <- str_c(extract.Side, extract.St, "ST", sep=" ")
        } else if (detect.St2==TRUE) {
            redLightStreets$fixStrt[i] <- str_c("E", extract.St, "ST", sep=" ")
        }
        else {
            redLightStreets$fixStrt[i] <- ""
        }
    }
# Update data to run on fixed syntax
redLightStreets$update <- ifelse(redLightStreets$Zip!="", redLightStreets$Zip,
                                 ifelse(redLightStreets$fixStrt!="",
                                        redLightStreets$fixStrt,
                                        redLightStreets$violStrt))

# Check spelling
# Mercer St
pttrn.Mrcr <- "((1/2 )|(S/E/C )|(D?S ))?\\bMER[GCKE]?[AEO][IRSTW]?[STY]?\\b ?[BW]?((S?([CS]T)?\\.?R?(EE)?T?3?)|(AVE))?$"
pttrn.Mrcr2 <- "\\bMER[CEV][ERUY]\\b ?(ST)?$"
# str_replace(redLightStreets$violStrt[which(str_detect(redLightStreets$violStrt, perl(pttrn.Mrcr2)))], perl(pttrn.Mrcr2), "MERCER ST")

# Greene St
pttrn.Grn <- "(FT |W )?GREEN[ENS]?( S?ST?\\.?(REET)? ?| AVE?| P[LT]| ON| SWT| T)( LN|WT)?"
# str_replace(redLightStreets$violStrt[which(str_detect(redLightStreets$violStrt, perl(pttrn.Grn)))], perl(pttrn.Grn), " GREENE ST")

# Crosby St
pttrn.Crsby <- "(\\bCROS\\b|([SN]W |B )?CRO?[BS][BS]YS?T? {0,2}((ST[\\.|\\+]?(REEE?)?T?)|AVE|SIDE|&)?)"
# str_replace(redLightStreets$violStrt[which(str_detect(redLightStreets$violStrt, perl(pttrn.Crsby)))], perl(pttrn.Crsby), "CROSBY ST")

# Grand St
pttrn.Grnd <- "([DEFK] {1,2})?O?GRA?ND ?((S?ST?(REET)?)|CONCOURSE|AVE)?"
# str_replace(redLightStreets$violStrt[which(str_detect(redLightStreets$violStrt, perl(pttrn.Grnd)))], perl(pttrn.Grnd), "GRAND ST ")

# 10th Ave
pttrn.10Ave <- "((\\b10\\b)|\\b0\\b) AVE.*"
# str_replace(redLightStreets$violStrt[which(str_detect(redLightStreets$violStrt, perl(pttrn.10Ave)))], perl(pttrn.10Ave), "10 AVE")

# York Ave
pttrn.Yrk <- "((^YORK$)|(?<!NEW )(YORK AVE?(NUE)?))"
# str_replace(redLightStreets$violStrt[which(str_detect(redLightStreets$violStrt, perl(pttrn.Yrk)))], perl(pttrn.Yrk), "YORK AVE")

# Madison Ave
pttrn.Mdsn <- "((MADISON$)|(E )?MADISON {0,2}A? ?VE?N?(UE)?( ON)?$)"
# str_replace(redLightStreets$violStrt[which(str_detect(redLightStreets$violStrt, perl(pttrn.Mdsn)))], perl(pttrn.Mdsn), "MADISON AVE")

# Prince St
pttrn.Prnc <- "(\\bPRIN\\b|([ANS][EW]? ?)?PRI?NCES? ?S?[CS]?T?\\.?(REET)?)"
# str_replace(redLightStreets$violStrt[which(str_detect(redLightStreets$violStrt, perl(pttrn.Prnc)))], perl(pttrn.Prnc), "PRINCE ST ")

# Bleecker St
pttrn.Blkr <- "BLEEKE?RS? ?(ST)?T?(REET)?"
# str_replace(redLightStreets$violStrt[which(str_detect(redLightStreets$violStrt, perl(pttrn.Blkr)))], perl(pttrn.Blkr), "BLEEKER ST")

# Wooster St
pttrn.Wstr <- "(A )?WO?O?STE?R ?(S[TZ]?[IRT]?\\.?|AVE)?"
# str_replace(redLightStreets$violStrt[which(str_detect(redLightStreets$violStrt, perl(pttrn.Wstr)))], perl(pttrn.Wstr), "WOOSTER ST ")

# Ludlow St
pttrn.Ldlw <- "LUDL[OW][OW]( ST\\.?(REET)?| LO| SST| S(TS)?)?$"
# str_replace(redLightStreets$violStrt[which(str_detect(redLightStreets$violStrt, perl(pttrn.Ldlw)))], perl(pttrn.Ldlw), "LUDLOW ST")

# Riverside Dr
pttrn.Rvrsd <- "(W/SOF)?RI?VE?RS[DI]?[DI][AE]?N? ?(DR\\.?|DRIVE|\\bPA?R?K?\\b|AVE?(NUE)?|\\bPLZ?\\b|RD|CIR)"
# str_replace(redLightStreets$violStrt[which(str_detect(redLightStreets$violStrt, perl(pttrn.Rvrsd)))], perl(pttrn.Rvrsd), "RIVERSIDE DR")
pttrn.Rvrsd2 <- "\\bRI?VE?RSI?DE?\\b$"
# str_replace(redLightStreets$violStrt[which(str_detect(redLightStreets$violStrt, perl(pttrn.Rvrsd2)))], perl(pttrn.Rvrsd2), "RIVERSIDE DR")
# Riverside Blvd
pttrn.RvrsdB <- "RIVERSIDES? BL?(VD)?\\d?"
# str_replace(redLightStreets$violStrt[which(str_detect(redLightStreets$violStrt, perl(pttrn.RvrsdB)))], perl(pttrn.RvrsdB), "RIVERSIDE BLVD")

# Mott Ave
pttrn.Mott <- "[BNW]?W MOTT|MOTTS ?(ST)?(REET)?"
# str_replace(redLightStreets$violStrt[which(str_detect(redLightStreets$violStrt, perl(pttrn.Mott)))], perl(pttrn.Mott), "MOTT ST")

# Elizabeth St
pttrn.Liz <- "\\bELIZ\\b|ELIZA?BB?ET?H?I? ?&?ST\\.?(REET)?|(NW )?ELIZABET?H? ?(&)?(AVE)?"
# str_replace(redLightStreets$violStrt[which(str_detect(redLightStreets$violStrt, perl(pttrn.Liz)))], perl(pttrn.Liz), "ELIZABETH ST")

# Columbus Ave 
pttrn.Clmbs <- "^[EW]? ?COLU?[MN]BUS( AV?E?(NUE)?)?$"
# str_replace(redLightStreets$violStrt[which(str_detect(redLightStreets$violStrt, perl(pttrn.Clmbs)))], perl(pttrn.Clmbs), "COLUMBUS AVE")

# W Broadway
pttrn.WBway <- "[WV]((EEST)|\\.)? {0,2}B(ROAD)?WAY/?\\d{0,2}( ST)?"
# str_replace(redLightStreets$violStrt[which(str_detect(redLightStreets$violStrt, perl(pttrn.WBway)))], perl(pttrn.WBway), "W BROADWAY")
# E Broadway -- whole of street is same zip code (see intersection)

# Broadway -- this one is such a mess
pttrn.Bway <- "\\bB(ROAD)?WAY?\\b"
# redLightStreets$violStrt[which(str_detect(redLightStreets$violStrt, perl(pttrn.Bway)))]

# Adam Clayton Powell
pttrn.ACP <- "(W )?ADAM ?(C(L?[AE]?YTON)?|POWELL) ?(POWELL)? ?(BL?(VD)?)?"
# -- replace with "ADAM CLAYTON POWELL"

# Frederick Douglass Blvd
pttrn.FD <- "^F?R?E?D?E?(RICK)? ?DOUGLASS? ?[BC]?[LI]?[VR]?D?$"
# str_replace(redLightStreets$violStrt[which(str_detect(redLightStreets$violStrt, perl(pttrn.FD)))], perl(pttrn.FD), "FREDERICK DOUGLASS BLVD")

# Macdougal St
pttrn.Mac <- "^MA?C ?[CDGJO][ADLUO][ALNUORWY]?[FU]?[GS]?[ADEHLOU][ACDEL]?(L|S|(SY)|(GE))? ?W?[PS]?[LT]?\\.?(REET)? ?(NY)?$"
# str_replace(redLightStreets$violStrt[which(str_detect(redLightStreets$violStrt, perl(pttrn.Mac)))], perl(pttrn.Mac), "MACDOUGAL ST")

# Henry Hudson Pkwy
pttrn.HHud <- "HUDSON PKW?Y$"
# -- replace with "HENRY HUDSON PKWY"

# FDR Dr
pttrn.FDR <- "(((CO?RNE?R )|A |(E/? ?))?\\b(F[\\.\\s]?[DO](EOL)?[\\.\\s]?R?)|FRANKLIN D ROOSEVELT)\\b {0,2}((DR)|(CON))?((S?E?R?/?[UV]?I?(CE)?)|D?D?R?I?(\\.|V|(VE))R?)? ?(S|(RD)|(AVE))?$"
# str_replace(redLightStreets$violStrt[which(str_detect(redLightStreets$violStrt, perl(pttrn.FDR)))], perl(pttrn.FDR), "FDR DR")

# Central Park West
pttrn.CPKW <- "CENTRAL PA?[RK]?[RK]? ?(W|(WES)T?|(AVE))?$"
# str_replace(redLightStreets$violStrt[which(str_detect(redLightStreets$violStrt, perl(pttrn.CPKW)))], perl(pttrn.CPKW), "CENTRAL PARK WEST")

# Harlem River Drive
pttrn.HRD <- "\\b(\\d{2})?HRD[AR3N/]?S?\\b$|HA?RLE?M RI?VE?R ?[DPRP]?[DRL]?\\.?[IY]?V?E?R? ?(RAMP|AVE)?$|^HRD DR$|^HRD AVE$"
# str_replace(redLightStreets$violStrt[which(str_detect(redLightStreets$violStrt, perl(pttrn.HRD)))], perl(pttrn.HRD), "HARLEM RIVER DR")


# Macdougal Alley

# Correct spelling
for (i in 1:nrow(redLightStreets)) {
    detect.Mrcr <- str_detect(redLightStreets$update[i], perl(pttrn.Mrcr))
    correct.Mrcr <- str_replace(redLightStreets$update[i], perl(pttrn.Mrcr), "MERCER ST")
    detect.Mrcr2 <- str_detect(redLightStreets$update[i], perl(pttrn.Mrcr2))
    correct.Mrcr2 <- str_replace(redLightStreets$update[i], perl(pttrn.Mrcr2), "MERCER ST")
    detect.Spr <- str_detect(redLightStreets$update[i], perl(pttrn.Spr))
    correct.Spr <- str_replace(redLightStreets$update[i], perl(pttrn.Spr), "SPRING ST")
    detect.Grn <- str_detect(redLightStreets$update[i], perl(pttrn.Grn))
    correct.Grn <- str_replace(redLightStreets$update[i], perl(pttrn.Grn), " GREENE ST")
    detect.Crsby <- str_detect(redLightStreets$update[i], perl(pttrn.Crsby))
    correct.Crsby <- str_replace(redLightStreets$update[i], perl(pttrn.Crsby), " CROSBY ST")
    detect.Grnd <- str_detect(redLightStreets$update[i], perl(pttrn.Grnd))
    correct.Grnd <- str_replace(redLightStreets$update[i], perl(pttrn.Grnd), "GRAND ST ")
    detect.10Ave <- str_detect(redLightStreets$update[i], perl(pttrn.10Ave))
    correct.10Ave <- str_replace(redLightStreets$update[i], perl(pttrn.10Ave), "10 AVE")
    detect.Yrk <- str_detect(redLightStreets$update[i], perl(pttrn.Yrk))
    correct.Yrk <- str_replace(redLightStreets$update[i], perl(pttrn.Yrk), "YORK AVE")
    detect.Mdsn <- str_detect(redLightStreets$update[i], perl(pttrn.Mdsn))
    correct.Mdsn <- str_replace(redLightStreets$update[i], perl(pttrn.Mdsn), "MADISON AVE")
    detect.Prnc <- str_detect(redLightStreets$update[i], perl(pttrn.Prnc))
    correct.Prnc <- str_replace(redLightStreets$update[i], perl(pttrn.Prnc), "PRINCE ST ")
    detect.Blkr <- str_detect(redLightStreets$update[i], perl(pttrn.Blkr))
    correct.Blkr <- str_replace(redLightStreets$update[i], perl(pttrn.Blkr), "BLEEKER ST")
    detect.Wstr <- str_detect(redLightStreets$update[i], perl(pttrn.Wstr))
    correct.Wstr <- str_replace(redLightStreets$update[i], perl(pttrn.Wstr), "WOOSTER ST ")
    detect.Ldlw <- str_detect(redLightStreets$update[i], perl(pttrn.Ldlw))
    correct.Ldlw <- str_replace(redLightStreets$update[i], perl(pttrn.Ldlw), "LUDLOW ST")
    detect.Rvrsd <- str_detect(redLightStreets$update[i], perl(pttrn.Rvrsd))
    correct.Rvrsd <- str_replace(redLightStreets$update[i], perl(pttrn.Rvrsd), "RIVERSIDE DR")
    detect.Rvrsd2 <- str_detect(redLightStreets$update[i], perl(pttrn.Rvrsd2))
    correct.Rvrsd2 <- str_replace(redLightStreets$update[i], perl(pttrn.Rvrsd2), "RIVERSIDE DR")
    detect.RvrsdB <- str_detect(redLightStreets$update[i], perl(pttrn.RvrsdB))
    correct.RvrsdB <- str_replace(redLightStreets$update[i], perl(pttrn.RvrsdB), "RIVERSIDE BLVD")
    detect.Mott <- str_detect(redLightStreets$update[i], perl(pttrn.Mott))
    correct.Mott <- str_replace(redLightStreets$update[i], perl(pttrn.Mott), "MOTT ST")
    detect.Liz <- str_detect(redLightStreets$update[i], perl(pttrn.Liz))
    correct.Liz <- str_replace(redLightStreets$update[i], perl(pttrn.Liz), "ELIZABETH ST")
    detect.Clmbs <- str_detect(redLightStreets$update[i], perl(pttrn.Clmbs))
    correct.Clmbs <- str_replace(redLightStreets$update[i], perl(pttrn.Clmbs), "COLUMBUS AVE")
    detect.WBway <- str_detect(redLightStreets$update[i], perl(pttrn.WBway))
    correct.WBway <- str_replace(redLightStreets$update[i], perl(pttrn.WBway), "W BROADWAY")
    detect.ACP <- str_detect(redLightStreets$update[i], perl(pttrn.ACP))
    replace.ACP <- "ADAM CLAYTON POWELL"
    detect.FD <- str_detect(redLightStreets$update[i], perl(pttrn.FD))
    correct.FD <- str_replace(redLightStreets$update[i], perl(pttrn.FD), "FREDERICK DOUGLASS BLVD")
    detect.Mac <- str_detect(redLightStreets$update[i], perl(pttrn.Mac))
    correct.Mac <- str_replace(redLightStreets$update[i], perl(pttrn.Mac), "MACDOUGAL ST")
    detect.HHud <- str_detect(redLightStreets$update[i], perl(pttrn.HHud))
    replace.HHud <- "HENRY HUDSON PKWY"
    detect.FDR <- str_detect(redLightStreets$update[i], perl(pttrn.FDR))
    correct.FDR <- str_replace(redLightStreets$update[i], perl(pttrn.FDR), "FDR DR")
    detect.CPKW <- str_detect(redLightStreets$update[i], perl(pttrn.CPKW))
    correct.CPKW <- str_replace(redLightStreets$update[i], perl(pttrn.CPKW), "CENTRAL PARK WEST")
    detect.HRD <- str_detect(redLightStreets$update[i], perl(pttrn.HRD))
    correct.HRD <- str_replace(redLightStreets$update[i], perl(pttrn.HRD), "HARLEM RIVER DR")
    

    if (detect.Mrcr==TRUE) {
        redLightStreets$spStrt[i] <- correct.Mrcr
    } else if (detect.Mrcr2==TRUE) {
        redLightStreets$spStrt[i] <- correct.Mrcr2
    } else if (detect.Spr==TRUE) {
        redLightStreets$spStrt[i] <- correct.Spr
    } else if (detect.Grn==TRUE) {
        redLightStreets$spStrt[i] <- correct.Grn
    } else if (detect.Crsby==TRUE) {
        redLightStreets$spStrt[i] <- correct.Crsby
    } else if (detect.Grnd==TRUE) {
        redLightStreets$spStrt[i] <- correct.Grnd
    } else if (detect.10Ave==TRUE) {
        redLightStreets$spStrt[i] <- correct.10Ave
    } else if (detect.Yrk==TRUE) {
        redLightStreets$spStrt[i] <- correct.Yrk
    } else if (detect.Mdsn==TRUE) {
        redLightStreets$spStrt[i] <- correct.Mdsn
    } else if (detect.Prnc==TRUE) {
        redLightStreets$spStrt[i] <- correct.Prnc
    } else if (detect.Blkr==TRUE) {
        redLightStreets$spStrt[i] <- correct.Blkr
    } else if (detect.Wstr==TRUE) {
        redLightStreets$spStrt[i] <- correct.Wstr
    } else if (detect.Ldlw==TRUE) {
        redLightStreets$spStrt[i] <- correct.Ldlw
    } else if (detect.Rvrsd==TRUE) {
        redLightStreets$spStrt[i] <- correct.Rvrsd
    } else if (detect.Rvrsd2==TRUE) {
        redLightStreets$spStrt[i] <- correct.Rvrsd2
    } else if (detect.RvrsdB==TRUE) {
        redLightStreets$spStrt[i] <- correct.RvrsdB
    } else if (detect.Mott==TRUE) {
        redLightStreets$spStrt[i] <- correct.Mott
    } else if (detect.Liz==TRUE) {
        redLightStreets$spStrt[i] <- correct.Liz
    } else if (detect.Clmbs==TRUE) {
        redLightStreets$spStrt[i] <- correct.Clmbs
    } else if (detect.WBway==TRUE) {
        redLightStreets$spStrt[i] <- correct.WBway
    } else if (detect.ACP==TRUE) {
        redLightStreets$spStrt[i] <- replace.ACP
    } else if (detect.FD==TRUE) {
        redLightStreets$spStrt[i] <- correct.FD
    } else if (detect.Mac==TRUE) {
        redLightStreets$spStrt[i] <- correct.Mac
    } else if (detect.HHud==TRUE) {
        redLightStreets$spStrt[i] <- replace.HHud
    } else if (detect.FDR==TRUE) {
        redLightStreets$spStrt[i] <- correct.FDR
    } else if (detect.CPKW==TRUE) {
        redLightStreets$spStrt[i] <- correct.CPKW
    } else if (detect.HRD==TRUE) {
        redLightStreets$spStrt[i] <- correct.HRD
    } else {
        redLightStreets$spStrt[i] <- ""
    }
}

redLightStreets$check <- ifelse(redLightStreets$Zip!="", redLightStreets$Zip,
                                  ifelse(redLightStreets$spStrt!="",
                                         redLightStreets$spStrt,
                                         redLightStreets$fixStrt))
redLightStreets$update2 <- ifelse(redLightStreets$Zip!="", redLightStreets$Zip,
                                ifelse(redLightStreets$spStrt!="",
                                       redLightStreets$spStrt,
                                       redLightStreets$update))

redLightStreets2 <- subset(redLightStreets, check=="")

# Find avenue intersections
pttrn.X <- " ((AND)|&|(ST)|-|(ON)) .{2,}"
redLightStreets$update[which(str_detect(redLightStreets$update, perl(pttrn.X)))]
    # Riverside
    pttrn.XRsd <- " \\d{2} ?((AND)|&|(ST)) RIVERSI?DE?( P[AK])?$"
    pttrn.XRsd2 <- " \\d{3} ?((AND)|&|(ST)) RIVERSI?DE?( P[AK])?$"
    pttrn.XRsd3 <- "B(ROAD)?WAY RI?VE?RSI?DE?"
    # West End
    pttrn.XWEnd <- "^W END( AVE)? .{0,3} \\d{2}"
    pttrn.XWEnd2 <- "((AND)|&) W END( AVE)?$"
    # Broadway
    pttrn.XBway <- "\\b\\d{2}\\b ?((TH)|(AND)|&|(ST)|@) B(ROAD)?WAY$"
    pttrn.XBway2 <- "\\b\\d{3}\\b ?((AND)|&|(ST)|-).{0,2}B(ROAD)?WAY$"
    pttrn.XBway3 <- "^B(ROAD)?WAY( ON)? W \\d{2} ST$"
    pttrn.XBway4 <- "^B(ROAD)?WAY( ON)? W \\d{3} ST$"
    pttrn.XBway5 <- "7 AVE.{0,5}BROADWAY"
    pttrn.XBway6 <- "[WV]((EEST)|\\.)? {0,2}B(ROAD)?WAY.*((SPRING)|(PRINCE))"
    pttrn.XBway7 <- "E\\.? {0,2}B(ROAD)?WAY"
    pttrn.XBway8 <- "OLD B(ROAD)?WAY"
    # Amsterdam
    pttrn.XAmst <- " \\d{2} ST (& )?AMST([AE]RDA?M)?$"
    pttrn.XAmst2 <- " \\d{3} ST AMST([AE]RDA?M)?$"
    # 12th Ave
    pttrn.X12Ave <- "\\d{2,3}(TH)? ((AND)|&|(ST))?( BTW)? 12TH( AVE)?$"
    # 11th Ave -- none
    # 10th Ave
    pttrn.X10Ave <- "((AND)|&|(ST)) 10TH$"
    # 9th Ave
    pttrn.X9Ave <- "((AND)|&|(ST)) 9(TH)? ?(AV)?E?$"
    # 8th Ave
    pttrn.X8Ave <- "((AND)|&|(ST)) 8(TH)? ?(AVE)?$"
    # 7th Ave
    pttrn.X7Ave <- " \\d{2,3} ?((TH)|(ST))? ((AND)|&|(ST))? 7(TH)? ?(AVE)?$"
    # 6th Ave
    pttrn.X6Ave <- "((AND)|&|(ST)) 6(TH)? ?(AVE)?$"
    # 5th Ave
    pttrn.X5Ave <- "((AND)|&|(ST)) 5(TH)? ?(AVE)?$"
    pttrn.X5Ave2 <- "^5TH AVE .*ST$"
    # Madison Ave
    pttrn.XMAve <- "((AND)|&|(ST)) MADISON ?(AVE)?$"
    # Park Ave
    pttrn.XPAve <- "((AND)|&|(ST)) PARK ?(AVE)$"
    # Lexington Ave -- none
    # 3rd Ave -- none
    # 2nd Ave -- none
    # 1st Ave
    pttrn.X1Ave <- "((AND)|&|(ST)) 1(ST)? ?(AVE)$"
    # York Ave
    pttrn.XYAve <- "((AND)|&|(ST)) YORK ?(AVE)$"
    # Columbus Ave NYCHA
    pttrn.ClmbsNYCHA <- "COLU?[MN]BUS( AVE)? ((DOUGLAS?)|(NYCHA))"
    pttrn.ClmbsNYCHA2 <- "^((DOUGLAS)|(NYCH))( \\w+)?( \\w+)? COLU?[MN]BUS( AVE)?"
    pttrn.XMrcr <- "((SPRING)|(PRINCE)|131).{1,3}MERCE"
    pttrn.XMrcr2 <- "((GRAND)|(1/2)).{1,3}MERCE"
    pttrn.Hnry <- "HENRY"
    pttrn.Chrry <- "CHERRY"
    pttrn.1CPK <- "(1|(ONE)|67) CENTRAL"
    pttrn.CPKE <- "E( \\d{2}) ?(ST)? ?CENTRAL"
    pttrn.CPKS <- "CENTRAL P(AR)?K ((200?)|(SO?(UTH)?))"
    pttrn.HRP <- "HARLEM RIVE?R PK"
pttrn.Spr <- "\\bS[TP]RI(NG)?\\b"
pttrn.Grn <- "\\b(\\d{2})?GREEN[ENS]?T?\\b [^W]"
pttrn.Crsby <- "CRO?[BS][BS]Y"
pttrn.Grnd <- "GRA?ND"
pttrn.Mrcr <- "\\bMERCER([TY]|(ST))?\\b"
redLightStreets$update[which(str_detect(redLightStreets$update, perl(pttrn.Grnd)))]

    for (i in 1:nrow(redLightStreets)) {
        detect.Rsd <- str_detect(redLightStreets$update[i], perl(pttrn.XRsd))
        detect.Rsd2 <- str_detect(redLightStreets$update[i], perl(pttrn.XRsd2))
        detect.Rsd3 <- str_detect(redLightStreets$update[i], perl(pttrn.XRsd3))
        detect.WEnd <- str_detect(redLightStreets$update[i], perl(pttrn.XWEnd))
        detect.WEnd2 <- str_detect(redLightStreets$update[i], perl(pttrn.XWEnd2))
        detect.Bway <- str_detect(redLightStreets$update[i], perl(pttrn.XBway))
        detect.Bway2 <- str_detect(redLightStreets$update[i], perl(pttrn.XBway2))
        detect.Bway3 <- str_detect(redLightStreets$update[i], perl(pttrn.XBway3))
        detect.Bway4 <- str_detect(redLightStreets$update[i], perl(pttrn.XBway4))
        detect.Bway5 <- str_detect(redLightStreets$update[i], perl(pttrn.XBway5))
        detect.Bway6 <- str_detect(redLightStreets$update[i], perl(pttrn.XBway6))
        detect.Bway7 <- str_detect(redLightStreets$update[i], perl(pttrn.XBway7))
        detect.Bway8 <- str_detect(redLightStreets$update[i], perl(pttrn.XBway8))
        detect.Amst <- str_detect(redLightStreets$update[i], perl(pttrn.XAmst))
        detect.Amst2 <- str_detect(redLightStreets$update[i], perl(pttrn.XAmst2))
        detect.12Ave <- str_detect(redLightStreets$update[i], perl(pttrn.X12Ave))
        detect.10Ave <- str_detect(redLightStreets$update[i], perl(pttrn.X10Ave))
        detect.9Ave <- str_detect(redLightStreets$update[i], perl(pttrn.X9Ave))
        detect.8Ave <- str_detect(redLightStreets$update[i], perl(pttrn.X8Ave))
        detect.7Ave <- str_detect(redLightStreets$update[i], perl(pttrn.X7Ave))
        detect.6Ave <- str_detect(redLightStreets$update[i], perl(pttrn.X6Ave))
        detect.5Ave <- str_detect(redLightStreets$update[i], perl(pttrn.X5Ave))
        detect.5Ave2 <- str_detect(redLightStreets$update[i], perl(pttrn.X5Ave2))
        detect.MAve <- str_detect(redLightStreets$update[i], perl(pttrn.XMAve))
        detect.PAve <- str_detect(redLightStreets$update[i], perl(pttrn.XPAve))
        detect.1Ave <- str_detect(redLightStreets$update[i], perl(pttrn.X1Ave))
        detect.YAve <- str_detect(redLightStreets$update[i], perl(pttrn.XYAve))
        detect.ClmbsNYCHA <- str_detect(redLightStreets$update[i], perl(pttrn.ClmbsNYCHA))
        detect.ClmbsNYCHA2 <- str_detect(redLightStreets$update[i], perl(pttrn.ClmbsNYCHA2))
        detect.Mrcr <- str_detect(redLightStreets$update[i], perl(pttrn.XMrcr))
        detect.Mrcr2 <- str_detect(redLightStreets$update[i], perl(pttrn.XMrcr2))
        detect.Hnry <- str_detect(redLightStreets$update[i], perl(pttrn.Hnry))
        detect.Chrry <- str_detect(redLightStreets$update[i], perl(pttrn.Chrry))
        detect.1CPK <- str_detect(redLightStreets$update[i], perl(pttrn.1CPK))
        detect.CPKE <- str_detect(redLightStreets$update[i], perl(pttrn.CPKE))
        detect.CPKS <- str_detect(redLightStreets$update[i], perl(pttrn.CPKS))
        detect.Spr <- str_detect(redLightStreets$update[i], perl(pttrn.Spr))
        detect.Grn <- str_detect(redLightStreets$update[i], perl(pttrn.Grn))
        detect.Crsby <- str_detect(redLightStreets$update[i], perl(pttrn.Crsby))
        detect.Grnd <- str_detect(redLightStreets$update[i], perl(pttrn.Grnd))
        detect.HRP <- str_detect(redLightStreets$update[i], perl(pttrn.HRP))
        if (detect.Rsd==TRUE) {
            redLightStreets$addy[i] <- str_c("350 W ",str_extract(redLightStreets$update[i],"\\d{2}"))
        } else if (detect.Rsd2==TRUE) {
            redLightStreets$addy[i] <- str_c("620 W ",str_extract(redLightStreets$update[i],"\\d{3}"))
        } else if (detect.Rsd3==TRUE) {
            redLightStreets$addy[i] <- str_c("620 W ",str_extract(redLightStreets$update[i],"\\d{3}"))
        } else if (detect.WEnd==TRUE) {
            redLightStreets$addy[i] <- str_c("240 W ",str_extract(redLightStreets$update[i],"\\d{2}"))
        } else if (detect.WEnd2==TRUE) {
            redLightStreets$addy[i] <- str_c("240 W ",str_extract(redLightStreets$update[i],"\\d{2}"))
        } else if (detect.Bway==TRUE) {
            redLightStreets$addy[i] <- str_c("50 W ",str_extract(redLightStreets$update[i],"\\d{2}"))
        } else if (detect.Bway2==TRUE) {
            redLightStreets$addy[i] <- str_c("550 W ",str_extract(redLightStreets$update[i],"\\d{3}"))
        } else if (detect.Bway3==TRUE) {
            redLightStreets$addy[i] <- str_c("150 W ",str_extract(redLightStreets$update[i],"\\d{2}"))
        } else if (detect.Bway4==TRUE) {
            redLightStreets$addy[i] <- str_c("650 W ",str_extract(redLightStreets$update[i],"\\d{3}"))
        } else if (detect.Bway5==TRUE) {
            redLightStreets$Zip[i] <- "10036"
        } else if (detect.Bway6==TRUE) {
            redLightStreets$Zip[i] <- "10012"
        } else if (detect.Bway7==TRUE) {
            redLightStreets$Zip[i] <- "10002"
        } else if (detect.Bway8==TRUE) {
            redLightStreets$Zip[i] <- "10027"
        } else if (detect.Amst==TRUE) {
            redLightStreets$addy[i] <- str_c("150 W ",str_extract(redLightStreets$update[i],"\\d{2}"))
        } else if (detect.Amst2==TRUE) {
            redLightStreets$addy[i] <- str_c("500 W ",str_extract(redLightStreets$update[i],"\\d{3}"))
        } else if (detect.12Ave==TRUE) {
            redLightStreets$addy[i] <- str_c("650 W ",str_extract(redLightStreets$update[i],"\\d{1,3}"))
        } else if (detect.10Ave==TRUE) {
            redLightStreets$addy[i] <- str_c("550 W ",str_extract(redLightStreets$update[i],"\\d{1,3}TH"))
        } else if (detect.9Ave==TRUE) {
            redLightStreets$addy[i] <- str_c("450 W ",str_extract(redLightStreets$update[i],"\\d{1,3} ?ST"))
        } else if (detect.8Ave==TRUE) {
            redLightStreets$addy[i] <- str_c("350 W ",str_extract(redLightStreets$update[i],"\\d{3}"), " ST")
        } else if (detect.7Ave==TRUE) {
            redLightStreets$addy[i] <- str_c("250 W ",str_extract(redLightStreets$update[i],"\\d{2,3}"), " ST")
        } else if (detect.6Ave==TRUE) {
            redLightStreets$addy[i] <- str_c("150 W ",str_extract(redLightStreets$update[i],"\\d{2,3}"), " ST")
        } else if (detect.5Ave==TRUE) {
            redLightStreets$addy[i] <- str_c("50 W ",str_extract(redLightStreets$update[i],"\\d{2,3}"), " ST")
        } else if (detect.5Ave2==TRUE) {
            redLightStreets$addy[i] <- str_c("50 W ",str_extract(redLightStreets$update[i],"\\d{2}"), " ST")
        } else if (detect.MAve==TRUE) {
            redLightStreets$addy[i] <- str_c("20 E ",str_extract(redLightStreets$update[i],"\\d{2,3}"), " ST")
        } else if (detect.PAve==TRUE) {
            redLightStreets$addy[i] <- str_c("100 E ",str_extract(redLightStreets$update[i],"\\d{2,3}"), " ST")
        } else if (detect.1Ave==TRUE) {
            redLightStreets$addy[i] <- str_c("350 E ",str_extract(redLightStreets$update[i],"\\d{2,3}"), " ST")
        } else if (detect.YAve==TRUE) {
            redLightStreets$addy[i] <- str_c("450 E ",str_extract(redLightStreets$update[i],"\\d{2,3}"), " ST")
        } else if (detect.ClmbsNYCHA==TRUE) {
            redLightStreets$Zip[i] <- "10025"
        } else if (detect.ClmbsNYCHA2==TRUE) {
            redLightStreets$Zip[i] <- "10025"
        } else if (detect.Mrcr==TRUE) {
            redLightStreets$Zip[i] <- "10012"
        } else if (detect.Mrcr2==TRUE) {
            redLightStreets$Zip[i] <- "10013"
        } else if (detect.Hnry==TRUE) {
            redLightStreets$Zip[i] <- "10002"
        } else if (detect.Chrry==TRUE) {
            redLightStreets$Zip[i] <- "10002"
        } else if (detect.1CPK==TRUE) {
            redLightStreets$Zip[i] <- "10023"
        } else if (detect.CPKE==TRUE) {
            redLightStreets$Zip[i] <- "10021"
        } else if (detect.CPKS==TRUE) {
            redLightStreets$Zip[i] <- "10019"
        } else if (detect.Spr==TRUE & detect.Grn==TRUE) {
            redLightStreets$Zip[i] <- "10012"
        } else if (detect.HRP==TRUE) {
            redLightStreets$Zip[i] <- "10035"
        } else {
            redLightStreets$addy[i] <- ""
        }
    }

redLightStreets$fixed <- ifelse(redLightStreets$Zip!="",
                                redLightStreets$Zip,
                                ifelse(redLightStreets$spStrt!="",
                                       redLightStreets$spStrt,
                                       ""))

# Find street intersections
# "W 148TH ST HENRY HUD"

# Stuff that doesn't make sense <- Discount from dataset
    # 4-digit streets
    pttrn.4dig <- "\\d{4} ((ST)|&)"
    redLightStreets$update[which(str_detect(redLightStreets$update, perl(pttrn.4dig)))]



# Inspect street names ending in numbers (no street/drive/ave designation?)

# Any full street addresses already?
pttrn.addy <- "^\\d{1,4} \\w+ (\\d{1,3})?((ST)|(RD)|(DR)|(TH)|(AVE))$"
unique(redLightStreets$update[which(str_detect(redLightStreets$update, perl(pttrn.addy)))])

# Outer join to full dataset to add fixed street names

# Scrub vehicle makes and verify with body types, to be fixed
    # Look at taxis and delivery trucks separately
