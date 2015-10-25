rankall <- function(outcome, num = "best") {
  
  ## Read outcome data
  outcome.DF <- 
    read.csv("outcome-of-care-measures.csv", colClasses = "character", 
             na.strings = "Not Available")
  
  ## Check that outcome is valid
  validOutcome <- c("heart attack", "heart failure", "pneumonia")
  if(match(outcome, validOutcome, nomatch=-1) == -1){
    stop ("invalid outcome")
  }
  
  ## Cut out useless data
  df <- outcome.DF[,c(2,7,11,17,23)]
  
  ## Rename columns in new data frame
  names(df)[1:5] <- c("Name","State","HA","HF","PN")
  
  if(outcome=="heart attack"){
    colID <- "HA"
  }
  else if(outcome=="heart failure"){
    colID <- "HF"
  }
  else 
    colID <- "PN"
  
  ## Initialize final output data frame
  stateList <- sort(unique(df$State))
  answer <- data.frame(matrix(nrow = length(stateList), ncol = 2))
  colnames(answer) <- c("hospital", "state")
  rownames(answer) <- stateList

  ## Ignore Not Available observations & re-order based on condition & hospital
  ## name
  
  for(i in seq_along(stateList)){
    ## Examine each state's set of observations
    df.State <- df[df$State==stateList[i], ]
    
    ## Order that state's observations
    df.State.Ordered <- df.State[order(as.numeric(df.State[[colID]]), df.State[["Name"]], na.last = NA) ,]
  
    ## Assign observation with desired rank to data frame
    if(num=="best")
      index <-1
    else if(num=="worst")
      index <- nrow(df.State.Ordered)
    else
      index <- num
      answer[i, ] <- c(df.State.Ordered$Name[index], 
                     stateList[i])
  }
  answer
}