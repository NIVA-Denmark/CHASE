library("dplyr")
library("tidyr")

#===============================================================================
# function Assessment
Assessment <- function(assessmentdata, 
                       summarylevel = NA, 
                       doConfidence=T) {
  
  # Confidence Penalty Criteria (minimum numbers of indicators)
  CountHM <- 2
  CountOrg <- 3
  PenaltyHM <- 0.5
  PenaltyOrg <- 0.5
  
  # relative weights of confidence components
  ConfWeightTemp = 0.1
  ConfWeightSpatial = 0.25
  ConfWeightAcc = 0.25
  ConfWeightMethod = 0.25
  ConfWeightThresh = 0.15
  
  
  # Get column names from the input data
  cnames <- names(assessmentdata)
  
  requiredcols <- c("Matrix","Substance","Type",
                    "ConfTemp","ConfSpatial","ConfAcc","ConfMethod","ConfThresh")
  
  # If CR is not included, then Threshold and Status are required columns
  if ('CR' %in% toupper(cnames)) {
    UserCR = TRUE
  } else{
    UserCR = FALSE
    requiredcols <- c(requiredcols,"Threshold","Status")
  }
  
  extracols <- c("Waterbody","Response","nStations","nDat")
  
  #Check column names in the imported data
  nimp = ncol(assessmentdata)
  nreq = length(requiredcols)
  nextra = length(extracols)
  
  ok <- rep(0, nreq)
  okextra <- rep(0, nextra)
  foundresponse = FALSE
  
  
  for (i in 1:nimp) {
    for (j in 1:nreq) {
      if (toupper(requiredcols[j]) == toupper(cnames[i])) {
        names(assessmentdata)[i] <- requiredcols[j]
        ok[j] = 1
      }
    }
    for (j in 1:nextra) {
      if (toupper(extracols[j]) == toupper(cnames[i])) {
        names(assessmentdata)[i] <- extracols[j]
        okextra[j] = 1
      }
    }
  }
  
  for (j in 1:nextra) {
    if (okextra[j] == 0) {
      if (regexpr("Confidence", extracols[j], ignore.case = TRUE) > 0) {
        assessmentdata[[extracols[j]]] <- 'Low'
      } else{
        assessmentdata[[extracols[j]]] <- 1
      }
    }
  }
  
  n <- sum(ok, na.rm = TRUE)
  
  if (n < nreq) {
    # The required columns were not found in the input data
    message("Error in CHASE Assessment. Required column(s) were not found in the input data:")
    for (j in 1:nreq) {
      if (ok[j] != 1) {
        cat(paste0("    ", requiredcols[j]), "\n")
      }
    }
    return(NA)
  } else{
    # The required columns are present - do the assessment
    
    
    # Change order of matrices factors
    mat1 <- data.frame(unique(assessmentdata$Matrix))
    names(mat1)[1] <- 'Matrix'
    mat1$char <- as.character(mat1$Matrix)
    mat1$len <- nchar(mat1$char)
    mat1 <- arrange(mat1, len)
    
    assessmentdata$Matrix <-
      factor(assessmentdata$Matrix, levels = mat1$char)
    
    # All combinations of matrices and waterbodies
    # This is used to ensure that a NA is returned 
    # where the combinations are missing
    waterbodies <- unique(assessmentdata$Waterbody)
    matrices <- unique(assessmentdata$Matrix)
    matrices <- expand.grid(waterbodies, matrices)
    names(matrices)[1] <- 'Waterbody'
    names(matrices)[2] <- 'Matrix'
    
    if (UserCR == FALSE) {
      assessmentdata$CR <-
        ContaminationRatio(assessmentdata$Threshold,
                           assessmentdata$Status,
                           assessmentdata$Response)
    }
    
    # group by 
    
    # calculate indicator confidence
    
    assessmentdata$ConfScoreTemp <- mapply(ConfValue, assessmentdata$ConfTemp)
    assessmentdata$ConfScoreSpatial <- mapply(ConfValue, assessmentdata$ConfSpatial)
    assessmentdata$ConfScoreAcc <- mapply(ConfValue, assessmentdata$ConfAcc)
    assessmentdata$ConfScoreMethod <- mapply(ConfValue, assessmentdata$ConfMethod)
    assessmentdata$ConfScoreThresh <- mapply(ConfValue, assessmentdata$ConfThresh)
    
    assessmentdata <- assessmentdata %>%
      mutate(ConfScore = ConfWeightTemp*ConfScoreTemp + 
               ConfWeightSpatial*ConfScoreSpatial +
               ConfWeightAcc*ConfScoreAcc +
               ConfWeightMethod*ConfScoreMethod +
               ConfWeightThresh*ConfScoreThresh)
    
    assessmentdata <- assessmentdata %>% 
      select(-c(ConfScoreTemp,ConfScoreSpatial,ConfScoreAcc,ConfScoreMethod,ConfScoreThresh))
 
    # Add columns specifying if the inidcator is an organic or heavy metal. Used in confidence penalty calculations
    assessmentdata$HM<-mapply(IsHeavyMetal,assessmentdata$Type)
    assessmentdata$Org<-mapply(IsOrganic,assessmentdata$Type)
    
    MatchListBioEffects <-
      c(
        "bioeffect",
        "bioeffects",
        "bio effects",
        "bio effect",
        "biological effects",
        "biological effect"
      )
    MatchListBiota <- c("biota", "biot")
    MatchListSed <- c("sediment", "sed", "sedi")
    
    # counting substances in different matrices as distinct (used for Org)
    IndicatorCountByMatrix <- assessmentdata %>%
      filter(!is.na(Type), !is.na(CR)) %>%
      group_by(Waterbody, Matrix, Type, Substance) %>%
      summarise() %>%
      ungroup %>%
      group_by(Waterbody, Matrix, Type) %>%
      summarise(Count = n()) %>%
      group_by(Waterbody, Type) %>%
      summarise(Count = sum(Count,na.rm=T)) %>%
      pivot_wider(names_from="Type",values_from="Count")  %>%
      select(Waterbody,Org)
      
    # not counting same substances in different matrices as extra (used for HM)
    IndicatorCount <- assessmentdata %>%
      filter(!is.na(Type), !is.na(CR)) %>%
      group_by(Waterbody, Type, Substance) %>%
      summarise() %>%
      ungroup  %>%
      group_by(Waterbody, Type) %>%
      summarise(Count = n()) %>%
      pivot_wider(names_from="Type",values_from="Count") %>%
      select(Waterbody,HM)
    
    # calculate the confidence penalties where counts of Org and HM are below the limits
    QEtypeCount <-  IndicatorCountByMatrix %>% 
      left_join(IndicatorCount,by="Waterbody") %>%
      mutate(HM=ifelse(is.na(HM),0,HM),
             Org=ifelse(is.na(Org),0,Org)) %>%
      mutate(PenaltyHM=ifelse(HM<CountHM,PenaltyHM,0),
             PenaltyOrg=ifelse(Org<CountOrg,PenaltyOrg,0)) %>%
      mutate(Penalty=(1-PenaltyOrg)*(1-PenaltyHM)) %>%
      select(Waterbody,HM,Org,Penalty)
    
    
    
    QEdata <- assessmentdata %>%
      filter(!is.na(CR)) %>%
      group_by(Waterbody, Matrix) %>%
      summarise(
        sumCR = sum(CR, na.rm = TRUE),
        sumConf = sum(ConfScore, na.rm = TRUE),
        countHM = sum(HM, na.rm = TRUE),
        countOrg = sum(Org, na.rm = TRUE),
        Count = n()
      ) %>%
      ungroup()
    
    
    QEdata$IsBio <-
      ifelse(tolower(QEdata$Matrix) %in% MatchListBioEffects,
             TRUE,
             FALSE)
    QEdata$IsBiota <-
      ifelse(tolower(QEdata$Matrix) %in% MatchListBiota, TRUE, FALSE)
    QEdata$IsSed <-
      ifelse(tolower(QEdata$Matrix) %in% MatchListSed, TRUE, FALSE)
    QEdata$ConSum <-
      QEdata$sumCR / ifelse(QEdata$IsBio, QEdata$Count, sqrt(QEdata$Count))
    
    QEdata$ConfScore <- QEdata$sumConf / QEdata$Count
    
    QEdata$Confidence <-
      mapply(ConfidenceStatus, QEdata$ConfScore, TRUE)
    QEdata$MultiplierHM <- NULL
    QEdata$MultiplierOrg <- NULL
    QEdata$sumConf <- NULL
    QEdata$IsBio <- NULL
    QEdata$sumCR <- NULL
    QEdata$Count <- NULL
    
    QEspr <- QEdata %>%
      select(Waterbody, Matrix, ConSum) %>%
      spread(Matrix, ConSum)
    
    QEdata$QEStatus <- CHASEStatus(QEdata$ConSum, 2)
    QEdata <- left_join(matrices, QEdata, c('Waterbody', 'Matrix'))
    QEdata <- arrange(QEdata, Waterbody, Matrix)
    QEdataOut <- QEdata %>%
      select(Waterbody, Matrix, ConSum, QEStatus, ConfScore, Confidence)
    
    CHASE <- QEdata %>%
      group_by(Waterbody) %>%
      summarise(
        ConSum = max(ConSum, na.rm = TRUE),
        Sed = sum(IsSed, na.rm = TRUE),
        Biota = max(IsBiota, na.rm = TRUE),
        ConfScore = mean(ConfScore, na.rm = TRUE)
      )
    
    CHASEQE <- QEdata %>%
      select(Waterbody, Matrix, ConSum, QEStatus) %>%
      inner_join(CHASE, by = c("Waterbody" = "Waterbody", "ConSum" = "ConSum"))
    
    CHASEQE <- rename(CHASEQE, Status = QEStatus, Worst = Matrix)
    
    CHASEQE$ConfMultiplier <-
      ifelse(CHASEQE$Sed + CHASEQE$Biota > 0, 1, 0.5)
    CHASEQE$ConfScore <- CHASEQE$ConfScore * CHASEQE$ConfMultiplier
    
    CHASEQE$Confidence <- NA
    
    CHASEQE <- CHASEQE %>%
      select(Waterbody, Worst, ConSum, Status, ConfScore, Confidence) %>%
      left_join(QEtypeCount, by = c("Waterbody" = "Waterbody"))
    
    CHASEQE$ConfScore <- CHASEQE$ConfScore * (1 - CHASEQE$Penalty)
    CHASEQE$Confidence <-
      mapply(ConfidenceStatus, CHASEQE$ConfScore, TRUE)
    CHASEQE$Penalty <- scales::percent(CHASEQE$Penalty)
    
    assessmentdata$HM <- NULL
    assessmentdata$Org <- NULL
    if (!'RESPONSE' %in% toupper(cnames)) {
      assessmentdata$Response <- NULL
    }
    
    assessmentdata <- assessmentdata %>%
      left_join(
        rename(
          QEdataOut,
          QEConfidence = Confidence,
          QEConfScore = ConfScore
        ),
        c('Waterbody', 'Matrix')
      )
    
    QEspr <- inner_join(QEspr, CHASEQE, 'Waterbody')
    
    if(is.na(summarylevel)){
      # return a list with results at 4 summary levels
      CHASE <- list(
        "Indicator"=assessmentdata,
        "Matrix by column"=QEspr,
        "Matrix by row"=QEdataOut,
        "Waterbody"=CHASEQE
      )
      return(CHASE)
    } else if(summarylevel == 1) {
      return(assessmentdata)
    } else if (summarylevel == 2) {
      return(QEspr)
    } else if (summarylevel == 3) {
      return(QEdataOut)
    } else {
      return(CHASEQE)
    }
    #
  }
}


#===============================================================================
# function ContaminationRatio
ContaminationRatio <- function(threshold, status, response = 1) {
  # If response is not specified, it will be assumed to be positive
  # i.e. ContaminationRatio increases (worsens) with increasing status value
  if (missing(response)) {
    response = 1
  }
  response <- ifelse(is.na(response), 1, response)
  
  # ContaminationRatio calculated depending on Response direction
  cr <- ifelse(response > 0, status / threshold, threshold / status)
  return(cr)
}

#===============================================================================
#Function CHASEStatus
CHASEStatus <- function(CRsum, nCat = 5) {
  if (nCat == 5) {
    status <- ifelse(CRsum > 0.5, "Good", "High")
    status <- ifelse(CRsum > 1, "Moderate", status)
    status <- ifelse(CRsum > 5, "Poor", status)
    status <- ifelse(CRsum > 10, "Bad", status)
  } else{
    status <- ifelse(CRsum > 1, "Not good", "Good")
  }
  return(status)
}

CHASEStatus1 <- function(CRsum) {
  status <- ifelse(CRsum > 0.5, "Good", "High")
  status <- ifelse(CRsum > 1, "Moderate", status)
  status <- ifelse(CRsum > 5, "Poor", status)
  status <- ifelse(CRsum > 10, "Bad", status)
  return(status)
}

# Colours associated with Status classes - used by Shiny App
AddColours <- function(CRsum) {
  co <- ifelse(CRsum > 0.5, '#66FF66', '#3399FF')
  co <- ifelse(CRsum > 1, '#FFFF66', co)
  co <- ifelse(CRsum > 5, '#FF9933', co)
  co <- ifelse(CRsum > 10, '#FF6600', co)
  return(co)
}
