corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations
  
  correlation <- numeric()
  
  ## Obtain list of files having extension '.csv'
  files <- list.files(directory, pattern = "\\.csv$")
  
  for (file in files) 
  {
    ## Obtain numeric id of the file by removing extension '.csv'
    # id <- as.numeric(sub("^([^.]*).*", "\\1", file))
    
    ## Read that file
    data <- read.csv(paste(directory, "/", file, sep="")) 
    
    good <- complete.cases(data)
    if (sum(good) > threshold)
    {
      correlation <- c(correlation, cor(data['sulfate'], data['nitrate'], use="complete.obs"))
    }   
  }
  
  ## Return correlation
  correlation
}
