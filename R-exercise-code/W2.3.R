pollutantmean <- function(directory,  pollutant, id = 1:332){

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
  step2 <- c()
  for (i in 1:length(id)){
    if (pollutant == "sulfate"){
      step2 <- c(step2, step1[[i]]$sulfate)
    }
    if (pollutant == "nitrate"){
      step2 <- c(step2, step1[[i]]$nitrate)
    }
  }
  mean(step2, na.rm  =T)
}



# mydata <- getmonitor(2, "specdata")
# head(mydata)
# mydata <- getmonitor(101, "specdata", TRUE)
# head(mydata)
# mydata <- getmonitor("200", "specdata", TRUE)
