# Biocomputing R project -- functions
# Isabella Gimon, Carol de Souza Moreira, Gretchen Andreasen
# Dec 14 2022

# First Function
Converting_txt_to_csv <- function(){
  Working_directory = readline(prompt = "Type the path to the directory with files: ")
  print(Working_directory)
  setwd(Working_directory)
  New_directory = readline(prompt= "Type the path to a new directory for converted files: ")
  dir.create(New_directory)
  filelist = list.files(pattern = ".txt")
  for (i in 1:length(filelist)){
    input <- filelist[i]
    output <- paste0(gsub("\\.txt$", "", input), ".csv")
    print(paste("Processing the file: ", input))
    data = read.table(input, header = TRUE)   
    setwd(New_directory)
    write.csv(data, file=output, row.names=FALSE)
    setwd(Working_directory)
  }
}

# Second function
compiler <- function(
	path_country1, name_country1, path_country2, name_country2, 
	output_name = "compiledData", remove_NA = TRUE, silent_NA = FALSE){
	# Create a list of all the files in each directory
	file_list_country1 <- list.files(path = path_country1)
	file_list_country2 <- list.files(path = path_country2)
	# For loop to compile data for country 1
	for (file_name in file_list_country1){
		# If the compilation file doesn't exist, then create it by reading the first csv.
		if (!exists("compiled_data_country1")){ 
			compiled_data_country1 <- read.csv(file = paste(path_country1, 
				"/", file_name, sep = ""), header = TRUE)
			# And add the day from the file name, formatted "screen_xxx.csv"
			# Begin by creating a substring with just the day
			screen_day <- sub(".csv", "", sub("screen_", "", noquote(file_name)))
			compiled_data_country1$dayofYear <- screen_day
		# If the comp file does exist, then add to it by reading and binding the
		# second .csv, in the "else if" statement below.
		}else if (exists("compiled_data_country1")){
			temp_data <- read.csv(file = paste(path_country1, "/", 
				file_name, sep = ""), header = TRUE) # Create temp data of 2nd file
			# Add the day from the file name
			temp_day <- sub(".csv", "", sub("screen_", "", noquote(file_name)))
			temp_data$dayofYear <- temp_day
			# Bind the 2nd file onto the existing comp file
			compiled_data_country1 <- rbind(compiled_data_country1, temp_data)
			rm(temp_data) # Then remove the temp file to redo process for all files
		}
	}
	# Now let's add the country name to the file
	compiled_data_country1$country <- name_country1
	
	# For loop to compile data for country 2, exact same code as above
	for (file_name in file_list_country2){
		if (!exists("compiled_data_country2")){ 
			compiled_data_country2 <- read.csv(file = paste(path_country2, 
				"/", file_name, sep = ""), header = TRUE)
			screen_day <- sub(".csv", "", sub("screen_", "", noquote(file_name)))
			compiled_data_country2$dayofYear <- screen_day 
		}else if (exists("compiled_data_country2")){
			temp_data <- read.csv(file = paste(path_country2, "/", 
				file_name, sep = ""), header = TRUE) 
			temp_day <- sub(".csv", "", sub("screen_", "", noquote(file_name)))
			temp_data$dayofYear <- temp_day
			compiled_data_country2 <- rbind(compiled_data_country2, temp_data)
			rm(temp_data)
		}
	}
	# Now let's add the country name to the file
	compiled_data_country2$country <- name_country2
	
	# Combine the 2 country's data
	compiled_data <- rbind(compiled_data_country1, compiled_data_country2)
	
	# Remove NA's
	# To start, convert the compiled_data into a workable data frame.
	if (any(is.na(compiled_data)) == TRUE){ # Checks compiled_data for NA's in all rows.
		# If there are NA's, and remove_NA is TRUE/default, then this removes them.
		if (remove_NA == TRUE){ 
			compiled_data <- data.frame(compiled_data)
			compiled_data <- na.omit(compiled_data) # na.omit removes the row w/ NA's.
		# If remove_NA is FALSE, and silent_NA is FALSE/default, then warning given.
		} else if (remove_NA == FALSE & silent_NA == FALSE){
			warning("Warning: row(s) containing NA's present")
		} # If remove_NA is FALSE and silent_NA is TRUE, then no warning given.
	}
	
	# Write and save it as a .csv in thje current working directory
	write.csv(compiled_data, file = paste0("./", output_name, ".csv"), 
		row.names = FALSE)
}


# Gretchen note, Dec. 14 2022 16:01 pm
# This is not quite compatible yet, I'm working on it!

# Third Function
summarizer <- function(path_to_data){
  compiledData <- read.csv(file = path_to_data, header = TRUE)
	Number_Screens = nrow(compiledData)
  Number_Infected = 0
  Number_Female = 0
  Number_Male = 0
  
  for(i in 1:nrow(compiledData)){
    # Counts for infected individuals
  	# If any of the marker rows contain a value, the individual is infected and
  	# counted.
    if(compiledData[i, 3]==1 || compiledData[i, 4]==1 || compiledData[i, 5]==1 
       || compiledData[i, 6]==1 || compiledData[i, 7]==1 || compiledData[i, 8]==1 ||
         compiledData[i, 9]==1 || compiledData[i, 10]==1 || compiledData[i, 11]==1 
       || compiledData[i, 12]==1 ){
      Number_Infected = Number_Infected + 1
    }
    
    # Counts of females and males 
    if(compiledData[i, 1]=="female"){
      Number_Female = Number_Female + 1
    } else{
      Number_Male = Number_Male + 1
    }
  }
  
  # Percentage of infected individuals in country Y
  Percentage_Infected = Number_Infected / Number_Screens * 100
  
  # Calculating distribution
  # Start with the mean
  Age_Mean <- mean(compiledData$age)
  # Then calculate the standard deviation
  Age_SD <- sd(compiledData$age)
  
  # Print data to console
  cat("\nCompiled data summary:")
  cat("\nNumber of screens done: ", Number_Screens, sep = "")
  cat("\nPercent of patients infected: ", Percentage_Infected, sep = "")
  cat("\nNumber of male (", Number_Male, ") and female (", Number_Female, 
  	") patients", sep = "")
  cat("\nPatient age distribution: ", Age_Mean, " ± ", Age_SD, sep = "")
}
