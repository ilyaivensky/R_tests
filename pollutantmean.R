pollutantmean <- function(directory, pollutant, id = 1:332)
{
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  total <- 0
  count <- 0
  
  for (i in id)
  {
    file <- paste(directory, "/", formatC(i, width=3, flag="0"), ".csv", sep="")
    data <- read.csv(file)     
    bad <- is.na(data[pollutant])
    pollutamt_data <- data[pollutant][!bad]
    total <- total + sum(pollutamt_data)
    count <- count + length(pollutamt_data)
  }
  
  total / count
}