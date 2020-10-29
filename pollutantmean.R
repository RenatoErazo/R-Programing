
# Part 1
# Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) across a specified list of monitors. 
# The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. Given a vector monitor ID numbers, 'pollutantmean' 
# reads that monitors' particulate matter data from the directory specified in the 'directory' argument and returns the mean of the
# pollutant across all of the monitors, ignoring any missing values coded as NA. A prototype of the function is as follows



pollutantmean <- function(directory,pollutant,id = 1:332){
  ## 'directory' is a character vector of length 1 idicting
  ## the location of the CSV file
  
  ## 'pollutant' is a character vector of length 1 didicating 
  ## the name of the pullutant for which we will calculate the 
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' the mean of the pullutant across all monitors listsour 
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  
  num_data <- numeric() #init numeric vector
  
  # Define Right function 
  right = function(text, num_char) {
    substr(text, nchar(text) - (num_char-1), nchar(text))
  }
  
  
  for(i in id)
  {
    fileID <- i
    fileID <-  right(paste("00",fileID,sep = ""),3)  # Format to file name (001:332)
    path.directory <- paste(directory,"/",fileID,".csv",sep = "") 
    
    df <- read.csv(path.directory)
    
    if (pollutant == "sulfate"){
      num_data <- c(num_data,df$sulfate[!is.na(df$sulfate)])  
    }else 
    {
      if (pollutant != "nitrate") break #Break function for other case
      num_data <- c(num_data,df$nitrate[!is.na(df$nitrate)])  
    }  
  }
  
  
  mean(num_data)
  
}