rankall <- function(outcome, num = "best") {
  ## Set working directory to the folder with the data files
  setwd("~/Documents/Data Science/Coursera/R Programming/Programming Assignments/ProgrammingAssignment3")
  
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
  
  ## Cut out useless data
  df <- outcome.DF[,c(2,7,11,17,23)]
  
  ## Rename columns in new data frame
  names(df)[1:5] <- c("Name","State","HA","HF","PN")
  
  answer <- data.frame()
  colnames(answer) <- c("hospital", "state")
  
  ## Discard Not Available observations & re-order based on condition & hospital name
  
  ## Case 1 = Heart Attack
  if(outcome=="heart attack"){
    df.HA <- df[df$HA != "Not Available", ]    
    ## Rank in ascending order by mortality rate
    df.HA.Ordered <- lapply(x function(x) x[order(x$HA, x$Name), ])
    
    ## Generate output data frame    
      if(num=="best"){
        for(i in 1:54){
        answer <- rbind(c(df.HA.Ordered[[i]]$Name[1], df.HA.Ordered[[i]]$State[1]))
        }
      }
      else if(num =="worst"){
        for(i in 1:54){
          answer <- rbind(c(df.HA.Ordered[[i]]$Name[nrow(df.HA.Ordered[[i]])], df.HA.Ordered[[i]]$State[nrow(df.HA.Ordered[[i]])]))
        }
      }
      else 
        for(i in 1:54){
            answer <- rbind(c(df.HA.Ordered[[i]]$Name[nrow(df.HA.Ordered[[i]])], df.HA.Ordered[[i]]$State[nrow(df.HA.Ordered[[i]])]))
        }
    }
  ## Case 2 = Heart Failure
  else if(outcome=="heart failure"){
    df.HF <- df[df$HF != "Not Available", ]    
    ## Rank in ascending order by mortality rate
    df.HF.Ordered <- lapply(x function(x) x[order(x$HF, x$Name), ])
          
    ## Generate output data frame
      if(num=="best"){
        for(i in 1:54){
            answer <- rbind(c(df.HF.Ordered[[i]]$Name[1], df.HF.Ordered[[i]]$State[1]))
        }
        else if(num =="worst"){
          for(i in 1:54){
            answer <- rbind(c(df.HF.Ordered[[i]]$Name[nrow(df.HF.Ordered[[i]])], df.HF.Ordered[[i]]$State[nrow(df.HF.Ordered[[i]])]))
            }
        }
        else 
          for(i in 1:54){
            answer <- rbind(c(df.HF.Ordered[[i]]$Name[nrow(df.HF.Ordered[[i]])], df.HF.Ordered[[i]]$State[nrow(df.HF.Ordered[[i]])]))
            }
        }
  }
  ## Case 3 = Pneumonia
  else ## (outcome=="pneumonia"){
    df.PN <- df[df$PN != "Not Available", ]    
    ## Rank in ascending order by mortality rate
    df.PN.Ordered <- lapply(x function(x) x[order(x$PN, x$Name), ])
    
    ## Generate output data frame    
    if(num=="best"){
      for(i in 1:54){
        answer <- rbind(c(df.PN.Ordered[[i]]$Name[1], df.PN.Ordered[[i]]$State[1]))
      }
    }
    else if(num =="worst"){
      for(i in 1:54){
        answer <- rbind(c(df.PN.Ordered[[i]]$Name[nrow(df.PN.Ordered[[i]])], df.PN.Ordered[[i]]$State[nrow(df.PN.Ordered[[i]])]))
      }
    }
    else 
      for(i in 1:54){
        answer <- rbind(c(df.PN.Ordered[[i]]$Name[nrow(df.PN.Ordered[[i]])], df.PN.Ordered[[i]]$State[nrow(df.PN.Ordered[[i]])]))
      }
    
  ## For each state, find the hospital of the given rank
  answer