# Part 1 - Mean of pollutant accross a specified list of monitors
pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## Directory specify the location of the CSV files
  ## Polutant idicates the name of the pollutant ("sufalte" or "nitrate")
  ## ID numbers to be used
  ## Set directory
  path <- paste(getwd(), "/", directory, sep = "")
  ## Get files names
  files <- list.files(path)
  ## Select column
  if(identical(pollutant, "sulfate")) column <- 2
  if(identical(pollutant, "nitrate")) column <- 3
  ## Get data
  data <- vector()
  for(i in id) {
    file <- paste(path, "/", files[i], sep = "")
    interested_data <- read.csv(file)[,column]
    data <- c(data, interested_data[!is.na(interested_data)])
  }
  ## Mean
  mean(data)
}

# Part 2 - Reports the number of completely observed cases
complete <- function(directory, id = 1:332) {
  ## number of complete cases
  results <- data.frame(id=numeric(0), nobs=numeric(0))
  ## Set directory
  path <- paste(getwd(), "/", directory, sep = "")
  ## Get files names
  files <- list.files(path)
  for(i in id) {
    data <- read.csv(paste(path, "/", files[i], sep = ""))
    data <- data[(!is.na(data$sulfate)),]
    data <- data[(!is.na(data$nitrate)),]
    nobs <- nrow(data)
    results <- rbind(results, data.frame(id=i, nobs=nobs))
  }
  results
}

# Part 3 - Correlation between sulfate and nitrate
corr <- function(directory, threshold = 0) {
  # Get data
  correlation <- vector()
  complete_cases <- complete(directory)
  complete_cases <- complete_cases[complete_cases$nobs>=threshold, ]
  id = complete_cases[,1]
  ## Set directory
  path <- paste(getwd(), "/", directory, sep = "")
  ## Get files names
  files <- list.files(path)
  for(i in id) {
    data <- read.csv(paste(path, "/", files[i], sep = ""))
    data <- data[(!is.na(data$sulfate)),]
    data <- data[(!is.na(data$nitrate)),]
    sulfate <- data["sulfate"]
    nitrate <- data["nitrate"]
    correlation <- c(correlation, cor(sulfate, nitrate))
  }
  correlation
}