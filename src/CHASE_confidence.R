# help functions for the CHASE confidence assessment


#===============================================================================


# Function to calculate numeric confidence from string --------------------------------------------------------------------
# The function will always return a numeric value 
# between 0 and 1, depending on the argument sConf
# Given a numeric argument between 0 and 1, the function
# returns the same value
# e.g. ConfValue(0.37) returns a value of 0.37
# Passing a numeric argument less than 0, the function
# returns a value of 0
# Passing a numeric argument greater than 1, the function 
# returns a value of 1
# 
# The function recognizes the following words and returns 
# the respective values:
#    High = 1.0
#    Intermediate = 0.5
#    Medium = 0.5
#    Moderate = 0.5
#    Low = 0.0
# 
# The function is case-insensitive.
# Starting from the leftmost character, the function 
# recognizes any part of the key words
#
# e.g. "H", "hi", "hig" will all result in a value of 1.0
#      "med", "m", "int", "I", "in" will all return values of 0.5
#      "lo", "l" will all give a value of 0.0
#
# Any other argument passed to the function will give a result 
# equal to the argument NAvalue (default=0)

ConfValue <- function(sConf, ncat=3, NAvalue = NA) {
  if (is.numeric(sConf)) {
    return(sConf)
  } else{
    sConf <-str_to_lower(str_trim(sConf))
    l <- nchar(sConf)
    if(l<1) {
      return(NAvalue)
    } else{
      if(ncat==5){
        desc <- c("very low","v.low","v. low","low","mid","medium","moderate","high")
        value <- c(0, 0, 0, 0.25, 0.5, 0.5, 0.75, 1)
      }else{
        desc <- c("low","intermediate","medium","moderate","high")
        value <- c(0, 0.5, 0.5, 0.5, 1)
      }
      desc <- substr(desc,1,l)
      nc <- match(sConf,desc)
      nc <- value[nc]
      return(nc)
      }
    }
  }


Confidence <- function(Confidence1, Confidence2, Confidence3) {
  n1 <- lapply(Confidence1, ConfValue)
  if (missing(Confidence2)) {
    n <- n1
  } else{
    n2 <- lapply(Confidence2, ConfValue)
    if (missing(Confidence3)) {
      n <- mapply(ConfAvg, n1, n2)
    } else{
      n3 <- lapply(Confidence3, ConfValue)
      n <- mapply(ConfAvg, n1, n2, n3)
    }
  }
  return(n)
}

ConfAvg <- function(C1, C2, C3) {
  if (missing(C3)) {
    n = (C1 + C2) / 2
  } else{
    n = (C1 + C2 + C3) / 3
  }
  return(n)
}

ConfidenceS <- function(arrayConf) {
  arrayConf <- sapply(arrayConf, ConfValue)
  return(arrayConf)
}

ConfidenceStatus <- function(Score, Roman = FALSE) {
  Status <- ifelse(
    Roman == TRUE,
    ifelse(
      Score < 0.5,
      "Class III",
      ifelse(Score < 0.75, "Class II", "Class I")
    ),
    ifelse(Score < 0.5, "Low",
           ifelse(Score < 0.75, "Medium", "High"))
  )
  return(Status)
}



IsHeavyMetal <- function(sType, NAvalue = NA) {
  n <- NAvalue
  if (regexpr("hm", sType, ignore.case = TRUE) > 0) {
    n <- 1
  }
  if (regexpr("heavy", sType, ignore.case = TRUE) > 0) {
    n <- 1
  }
  return(n)
}

IsOrganic <- function(sType, NAvalue = NA) {
  n <- NAvalue
  if (regexpr("organ", sType, ignore.case = TRUE) > 0) {
    n <- 1
  }
  if (regexpr("org", sType, ignore.case = TRUE) > 0) {
    n <- 1
  }
  return(n)
}
