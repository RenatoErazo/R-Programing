# Part 3
# Write a function that takes a directory of data files and 
# a threshold for complete cases and calculates the correlation 
# between sulfate and nitrate for monitor locations where the
# number of completely observed cases (on all variables) is
# greater than the threshold. The function should return a 
# vector of correlations for the monitors that meet the 
# threshold requirement. If no monitors meet the threshold
# requirement, then the function should return a numeric vector 
# of length 0. A prototype of this function follows


corr <- function(directory, threshold = 0){
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observations (on all variables) required 
  ## to compute the correlation between nitrate and sulfate;
  ## the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  source("complete.R")
  
  df <- complete(directory)
  filesTH <- df$id[df$nobs > threshold]
  
  # Define Right function 
  right = function(text, num_char) {
    substr(text, nchar(text) - (num_char-1), nchar(text))
  }
  
  correlations <- numeric()
  
  for(i in filesTH)
  {
    fileID <- i
    fileID <-  right(paste("00",fileID,sep = ""),3)  # Format to file name (001:332)
    path.directory <- paste(directory,"/",fileID,".csv",sep = "") 
    
    fileData <- read.csv(path.directory)
    
    validData <- complete.cases(fileData)
    valS <- fileData$sulfate[validData]
    valN <- fileData$nitrate[validData]
    
    correlations <- c(correlations,cor(valS,valN))
    
  }
  correlations
}