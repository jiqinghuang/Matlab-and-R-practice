corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
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
  
  complete <- function(directory, id = 1:332){
    step1 <- lapply(id, getmonitor, directory = directory)
    step2 <- lapply(step1, complete.cases)
    number <- sapply(step2, sum)
    data.frame(id, nobs=number)
  }
  
  
  all.cases <- complete("specdata", 1:332)
  index <- which(all.cases$nobs > threshold)
  if ( length(index)!=0 ){
  required.list <- lapply(index, getmonitor, directory = directory)
  required.logic <- lapply(required.list, complete.cases)
  result <- numeric(0)
  for (i in 1:length(index) ){
    a <- required.list[[i]]
    b <- required.logic[[i]]
    result[i] <-  cor((a[b, ])$sulfate, (a[b, ])$nitrate)
  }
  final <- result
  }
  else {
  final <- numeric(0)
  }
  return(final)
}

# cr <- corr("specdata", 150)
# head(cr)
# summary(cr)
# cr <- corr("specdata", 400)
# head(cr)
# summary(cr)
# cr <- corr("specdata", 5000)
# summary(cr)
# length(cr)
# cr <- corr("specdata")
# summary(cr)
# length(cr)














