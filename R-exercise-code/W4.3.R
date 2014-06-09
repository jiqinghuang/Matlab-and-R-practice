data.set <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
data.set[, 11] <- as.numeric(data.set[, 11])
data.set[, 17] <- as.numeric(data.set[, 17])
data.set[, 23] <- as.numeric(data.set[, 23])

rankall <- function(outcome, num = "best"){
  ## read the outcome data
  ## check the state and outcome are vaild
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  if( outcome == "heart attack"){
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
  required.data <- data.frame( index = 1:length(total.col),  hosp = data.set$Hospital.Name, 
                               Rate = total.col, state = data.set$State)
  
  required.data <- required.data[ complete.cases(required.data),  ]
  s <- split(required.data, required.data$state)
  all.state <- character(0)
  all.hospital <- character(0)
  for (i in 1:54){
    this.data <- s[[i]]
    all.state[i] <- as.character((this.data$state)[1])
    index <- num
    if ( num == "best"){
      index <- 1
    }
    if ( num == "worst" ){
      index <- length( this.data$Rate )
    }
    # print(index)
    this.data <- this.data[order(this.data$Rate, this.data$hosp), ]
    all.hospital[i] <- as.character((this.data$hosp)[index])
    }
   data.frame( hospital = all.hospital, state = all.state)
}


















