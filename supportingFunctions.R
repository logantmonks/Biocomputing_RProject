# Biocomputing R Project
# hjeon
# supportingFunctions.R

# set workspace
# Note: I have unzipped the Rproject2022.zip and I just set the working directory to the unzipped
setwd("/Users/hyesooclarejeon/Desktop/Biocomputing_RProject/Rproject2022")

################## Task 1: convert all files ending in txt into comma-separated value files ################

# define the function parseFiles
parseFiles <- function() {
  # add all full file names that are txt formatted into list
  txtFiles <- list.files(path = "countryY", pattern = "*.txt", full.names = TRUE)
  
  for (fileName in txtFiles) {
    # use read.delim because it is suited specifically for
    # tab-delimited/space-delimited data
    screenData <- read.delim(fileName, header = TRUE, sep = " ")
    # overwrite same file but this time with separator of comma
    write.table(screenData, fileName, sep = ",", row.names=FALSE)
  }
}

################## Task 2: convert all files ending in txt into comma-separated value files ################

# define the function combinCsv
combineCsv <- function(preferenceNumber) {
  # prompt user to choose in which method they would like to combine csv files
  cat("Enter a number based on your choice of combining csv files ...\n")
  cat("1: remove rows with NA’s in any columns\n")
  cat("2: include NAs in the compiled data but be warned of their presence\n")
  cat("3: include NAs in the compiled data without a warning\n")
  
  # save number of choice
  combineChoice = as.integer(readline())
  
  # collect all csv files in countryX directory
  countryXFiles <- list.files(path = "countryX", pattern = "*.csv", full.names = TRUE)
  
  # collect all csv files in countryY directory
  countryYFiles <- list.files(path = "countryY", pattern = "*.csv", full.names = TRUE)
  
  # define new comprehensive csv filename
  combinedFileName <- "allData_myversion.csv"
  
  # define header/column names
  header <- list("gender", "age", "marker01",	"marker02",	
  "marker03",	"marker04",	"marker05",	"marker06",	"marker07",	
  "marker08",	"marker09",	"marker10",	"country",	"dayofYear")
  
  # add header to combinedFileName
  write.table(header, file = combinedFileName) 
  
  for (fileName in countryXFiles) {
    # use read.csv to read each fileName
    screenData <- read.csv(fileName, header = TRUE)
    # combine two new columns - country and dayofYear - to existing 12 columns
    # check if NA for all 10 markers - if so, continue to next iteration
    for (i in 1:nrow(screenData)) {
      # save data for markers1 - 10 for this patient
      patient <- screenData[i, c(3, 10)]
      if (combineChoice == 1 && !is.element('1', patient)) {
        next
      }
      # if 1's are found in patient -> this is NOT an NA instance, so we would append to new csv file
      # extract dayOfYear using regex
      dayOfYear <- regmatches(fileName, regexpr("[0-9]+", fileName))
      
      # append 2 new columns to patient
      patient <- screenData[i, ]
      patient <- append(patient, list("X", dayOfYear))
      
      # append this patient + 2 new columns in new csv
      write.table(patient, file = combinedFileName, sep = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
    }
  }
}

combineCsv()
