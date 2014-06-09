data.set <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
data.set[, 11] <- as.numeric(data.set[, 11])
data.set[, 17] <- as.numeric(data.set[, 17])
data.set[, 23] <- as.numeric(data.set[, 23])


best <- function(state, outcome){
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  a1 <- "heart attack"
  a2 <- "heart failure"
  a3 <- "pneumonia"
  if( sum( data.set$State == state ) == 0){
    stop("invalid state")
  }
  else if( outcome == a1){
    total.col <- data.set[, 11]
    state.index <- which(data.set$State == state)
    required.inf <- cbind( state.index,   total.col[state.index] )
    find.min <- which( required.inf[ , 2] == min( required.inf[ , 2] , na.rm = T) )
    true.index <- required.inf[find.min, 1]
    result <- (data.set$Hospital.Name)[true.index]
  }
  else if( outcome == a2){
    total.col <- data.set[, 17]
    state.index <- which(data.set$State == state)
    required.inf <- cbind( state.index,   total.col[state.index] )
    find.min <- which( required.inf[ , 2] == min( required.inf[ , 2] , na.rm = T) )
    true.index <- required.inf[find.min, 1]
    result <- (data.set$Hospital.Name)[true.index]
  }
  else if( outcome == a3){
    total.col <- data.set[, 23]
    state.index <- which(data.set$State == state)
    required.inf <- cbind( state.index,   total.col[state.index] )
    find.min <- which( required.inf[ , 2] == min( required.inf[ , 2] , na.rm = T) )
    true.index <- required.inf[find.min, 1]
    result <- (data.set$Hospital.Name)[true.index]
  }
  else{
    stop("invalid outcome")
  }
  final <- sort(result) 
  return(final[1])
}















