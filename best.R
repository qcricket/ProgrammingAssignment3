best <- function(state, outcome) {
  
  ## Read outcome data
  outcome.DF <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  stateList <- unique(as.character(outcome.DF[,7]))
  if(match(state, stateList, nomatch=-1) == -1){
  #if (is.na(match(state, stateList))){
  stop ("invalid state")
  }
  
  validOutcome <- c("heart attack", "heart failure", "pneumonia")
  if(match(outcome, validOutcome, nomatch=-1) == -1){
  stop ("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  statesubset <- outcome.DF[outcome.DF$State == state, ]
  if(outcome == "heart attack"){
    stateInOrder <-statesubset[order(statesubset$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, na.last = TRUE, decreasing = FALSE),]
  }
  else if(outcome == "heart failure"){
    stateInOrder <-statesubset[order(statesubset$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, na.last = TRUE, decreasing = FALSE),]
  }
  else if(outcome == "pneumonia"){
    stateInOrder <-statesubset[order(statesubset$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, na.last = TRUE, decreasing = FALSE),]
  }
  stateInOrder[1,2] 
  ## [,11] = heart attack mortality rate
  ## [,17] = heart failure mortality rate
  ## [,23] = pneumonia mortality rate
}
