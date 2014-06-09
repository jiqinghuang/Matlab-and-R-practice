complete <- function(directory, id = 1:332){
  getmonitor <- function(id, directory, summarize=FALSE){
    # first deal problems with id
    if (is.character(id) == T){
      id <- as.numeric(id)
    }
    if( id < 10){
      id <- paste("00", id, sep="")
    }
    else if( id >= 10 && id <= 99){
      id <- paste("0", id, sep="")
    }
    else {id <- as.character(id)}
    # read file
    filename <- paste(directory, "/", id, ".csv", sep="") 
    mydata <- read.csv(filename)
    if (summarize == TRUE){
      print(summary(mydata))
    }
    return(mydata)
  }
  
  step1 <- lapply(id, getmonitor, directory = directory)
  step2 <- lapply(step1, complete.cases)
  number <- sapply(step2, sum)
  data.frame(id, nobs=number)
}














