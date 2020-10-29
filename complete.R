# Part 2
# Write a function that reads a directory full of files and reports the number
# of completely observed cases in each data file. The function should return 
# a data frame where the first column is the name of the file and the second 
# column is the number of complete cases. 

complete <- function(directory,id = 1:332){
  ## 'directory' is a character vector of length 1 idicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  files <- dir(directory)   #---- list of files in filesystem
  df <- data.frame(id = numeric(),nobs = numeric())
  cont <- 1
  
  for(i in id)
  {
    fileID <- files[i]
    
    path.directory <- paste(directory,"/",fileID,sep = "") 
    
    fileData <- read.csv(path.directory)
    
    validData <- complete.cases(fileData)
    valD <- length(validData[validData == TRUE])
    df[cont,] <- list(i,valD)
    cont <- cont + 1
  }
  df
  
}