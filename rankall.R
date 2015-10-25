rankall <- function(outcome, num = "best") {
  ## Set working directory to the folder with the data files
  setwd("~/Documents/Data Science/Coursera/R Programming/Programming Assignments/ProgrammingAssignment3")
  
  ## Read outcome data
  outcome.DF <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  ## Check that outcome is valid
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
    df <- df[df$HA != "Not Available", ]    
    ## Rank in ascending order by mortality rate
    df.Ordered <- lapply(x, function(x) x[order(x$HA, x$Name), ])
  }
  
  ## Case 2 = Heart Failure
  else if(outcome=="heart failure"){
    df <- df[df$HF != "Not Available", ]    
    ## Rank in ascending order by mortality rate
    df.Ordered <- lapply(x, function(x) x[order(x$HF, x$Name), ])
  }
  
  ## Case 3 = Pneumonia
  else ## (outcome=="pneumonia"){
    df <- df[df$PN != "Not Available", ]    
    ## Rank in ascending order by mortality rate
    df.Ordered <- lapply(x, function(x) x[order(x$PN, x$Name), ])
      
## Generate output data frame    
  if(num=="best"){
    for(i in 1:54){
      answer <- rbind(c(df.Ordered[[i]]$Name[1], df.Ordered[[i]]$State[1]))
      }
    }
      else if(num =="worst"){
        for(i in 1:54){
          answer <- rbind(c(df.Ordered[[i]]$Name[nrow(df.Ordered[[i]])], df.Ordered[[i]]$State[nrow(df.Ordered[[i]])]))
        }
      }
      else 
        for(i in 1:54){
            answer <- rbind(c(df.Ordered[[i]]$Name[nrow(df.Ordered[[i]])], df.Ordered[[i]]$State[nrow(df.Ordered[[i]])]))
        }

## For each state, find the hospital of the given rank
answer
}