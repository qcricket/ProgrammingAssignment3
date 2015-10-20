best <- function(state, outcome) {
  
  ## Read outcome data
  outcome.DF <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  stateList <- unique(as.character(outcome.DF[,7]))
  if(match(state, stateList, nomatch=-1) == -1){
  stop ("invalid state")
  }
  
  validOutcome <- c("heart attack", "heart failure", "pneumonia")
  if(match(outcome, validOutcome, nomatch=-1) == -1){
  stop ("invalid outcome")
  }
    else if(outcome == "heart attack"){
      mortDataCol <- 11
    }
    else if(outcome == "heart failure"){
      mortDataCol <- 17
    }
    else ## Pneumonia
      mortDataCol <-23
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  stateLoc <- 7
  stateOutcomeUnordered <- outcome.DF[outcome.DF[,stateLoc] == state & outcome.DF[,mortDataCol]!="Not Available", ]
  stateOutcomeOrdered <- stateOutcomeUnordered[order(as.numeric(stateOutcomeUnordered[,mortDataCol]), stateOutcomeUnordered[,"Hospital.Name"]), ]
  stateOutcomeOrdered[1,2] 
  ## [,11] = heart attack mortality rate
  ## [,17] = heart failure mortality rate
  ## [,23] = pneumonia mortality rate
}
