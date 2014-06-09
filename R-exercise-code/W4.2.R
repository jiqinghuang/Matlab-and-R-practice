data.set <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
data.set[, 11] <- as.numeric(data.set[, 11])
data.set[, 17] <- as.numeric(data.set[, 17])
data.set[, 23] <- as.numeric(data.set[, 23])


rankhospital <- function(state, outcome, num = "best"){
  ### read the outcome data
  ### check that state and outcome are vaild
  ## return hospital name in that state with the given rank
  ## 30-day death rate
  if( sum( data.set$State == state ) == 0){
    stop("invalid state")
  }
  else if( outcome == "heart attack"){
    total.col <- data.set[, 11]
    }
  else if( outcome == "heart failure"){
    total.col <- data.set[, 17]
    }
  else if( outcome == "pneumonia"){
    total.col <- data.set[, 23]
    }
  else{
    stop("invalid outcome")
  }
  state.index <- which(data.set$State == state)
  required.inf <- cbind( state.index,   total.col[state.index] )
  required.inf <-  required.inf[complete.cases(required.inf), ]
  hosp.name <- (data.set$Hospital.Name)[ required.inf[ , 1] ]
  result.inital <- data.frame( index = required.inf[, 1],  Hospital.Name =  hosp.name, 
                               Rate = required.inf[, 2] )
  ### now rank
  ### step 1 order, just take alphabetical order
  result.inital <-  result.inital[order(result.inital$Hospital.Name), ]
  ### step 1 order, order the rank
  result.inital <-  result.inital[order(result.inital$Rate), ]
  
  result <- result.inital
  result$Rank <- 1:length(result[ ,1])
  ### adjust the num params
  if ( num == "best" ){
    num <- 1
  }
  if ( num == "worst" ){
    num <- length(result[ ,1])
  }
  final <- as.character(result$Hospital.Name)
  return( final[num] )
}
