# Part 1 Alternate Solution

pollutantmean <- function(directory, pollutant, id= 1:332){
  
  ## Create an empty vector of pollutants
  pollutants = c()
  
  ## Get a list of filenames
  filenames = list.files(directory)
  
  ## For each .csv file in id
  for(i in id){
    
    ## Concatinate the directory and filename
    ## e.g. directory = "C:/folder", filenames = vector("001.csv", "002.csv", ...), filepath="C:/folder/001.csv"
    filepath=paste(directory,"/" ,filenames[i], sep="")
    
    ## read in each file and store it in data
    data = read.csv(filepath, header = TRUE)
    
    ##Concatinate the vectors from each file of the pollutant('sulfate' or 'nitrate') column to pollutants vector
    pollutants = c(pollutants, data[,pollutant])
    
  }
  ## Get the mean of the pollutants and remove NA values
  pollutants_mean = mean(pollutants, na.rm=TRUE)
  
  ## Return the mean 'pollutants_mean'
  pollutants_mean
}

pollutantmean(directory = '~/Desktop/Week2/specdata', pollutant = 'sulfate', id = 1:10)
pollutantmean(directory = '~/Desktop/Week2/specdata', pollutant = 'nitrate', id = 70:72)

# Part 2 Alternate Solution

complete <- function(directory, id= 1:332){
  
  ## Create an empty vector of id's
  ids = c()
  
  ## Create an empty vector of nobs
  nobss = c()
  
  ## Get a list of filenames
  filenames = list.files(directory)
  
  ## For each .csv file in id
  for(i in id){
    
    ## Concatinate the directory and filename
    ## e.g. directory = "C:/folder", filenames = vector("001.csv", "002.csv", ...), filepath="C:/folder/001.csv"
    filepath=paste(directory,"/" ,filenames[i], sep="")
    
    ## read in each file and store it in data
    data = read.csv(filepath, header = TRUE)
    
    ##Get a subset of all rows with complete data meaning no NA's
    ##completeCases = subset(data, !is.na(Date) & !is.na(sulfate) & !is.na(nitrate) & !is.na(id),select = TRUE )
    completeCases = data[complete.cases(data), ]
    
    ids =  c(ids, i)                    ## We can use i for id and concatinate a vector of id's
    nobss = c(nobss, nrow(completeCases) )## Concatinates the number of completed rows from the subset into a vector
    
  }
  ## Return the data frame
  data.frame(id=ids, nobs=nobss)
}

#Example usage
complete("~/Desktop/Week2/specdata", c(2, 4, 8, 10, 12))

# Part 3 Alternate Solution
corr <- function(directory, threshold = 0){
  
  completes = complete(directory, 1:332)
  completes_above_threshold = subset(completes, nobs > threshold )
  
  ## Initialize empty vector variable
  correlations <- vector()
  
  ## Get a list of filenames
  filenames = list.files(directory)
  
  ## For each .csv file in id
  for(i in completes_above_threshold$id){
    
    ## Concatinate the directory and filename
    ## e.g. directory = "C:/folder", filenames = vector("001.csv", "002.csv", ...), filepath="C:/folder/001.csv"
    filepath=paste(directory,"/" ,filenames[i], sep="")
    
    ## read in each file and store it in data
    data = read.csv(filepath, header = TRUE)
    
    ## Calculate and store the number of completed cases
    completeCases = data[complete.cases(data),]
    count = nrow(completeCases)
    
    ## Calculate and store the count of complete cases
    ## if threshhold is reached
    if( count >= threshold ) {
      correlations = c(correlations, cor(completeCases$nitrate, completeCases$sulfate) )
    }
  }
  correlations
}

# Example Usage
cr <- corr("~/Desktop/Week2/specdata", 150)
head(cr)