#First Function
Converting_txt_to_csv <- function(){
setwd("~/Desktop/Biocomputing_RProject-main/Rproject2022/countryY/")
dir.create("~/Desktop/Biocomputing_RProject-main/Rproject2022/new_countryY")
filelist = list.files(pattern = ".txt")
for (i in 1:length(filelist)){
  input <- filelist[i]
  output <- paste0(gsub("\\.txt$", "", input), ".csv")
  print(paste("Processing the file: ", input))
  data = read.table(input, header = TRUE)   
  setwd("~/Desktop/Biocomputing_RProject-main/Rproject2022/new_countryY/")
  write.csv(data, file=output, sep="," , col.names = TRUE, row.names=FALSE)
  setwd("~/Desktop/Biocomputing_RProject-main/Rproject2022/countryY/")
}
}

# Gretchen notes:
# I think we should use if statements to make our function applicable to all
# file types.
# Something like...
library(tools) # This loads a library with the function below
?file_ext # This function can pull a file type just from a path
# Now I set it to my working directory (just to test it)
setwd("~/Documents/Courses/Biocomputing_BIOS_60318/Tutorials/Biocomputing_RProject/Rproject2022/countryX")
# And use the function to get the file type of screen_120.csv!
file_ext("~/Documents/Courses/Biocomputing_BIOS_60318/Tutorials/Biocomputing_RProject/Rproject2022/countryX/screen120.csv")
# The function returns "csv"

# So maybe we could do something like..
for (file in filelist){
	if (file_ext(file) = "csv"){
		# blah blah blah
	}else if (file_ext(file) = "txt"){
		# blah blah blah
	}
}


#Third Function
Compiling_data <- function(){
  #Country Y 
  setwd("~/Desktop/Biocomputing_RProject-main/Rproject2022/new_countryY/")
  files_Y <- list.files(path="~/Desktop/Biocomputing_RProject-main/Rproject2022/new_countryY/", pattern="*.csv")
  for(i in 1:length(files_Y)){
   compiled_data_countryY <- read.csv(files_Y[i], header= TRUE)
  }
  Number_Screens_CountryY = nrow(compiled_data_countryY)
  Number_Infected_CountryY = 0
  Number_Female_CountryY = 0
  Number_Male_CountryY = 0
  for(i in 1:nrow(compiled_data_countryY)){
    #Counts for infected individuals
    if(compiled_data_countryY[i, 3]==1 || compiled_data_countryY[i, 4]==1 || compiled_data_countryY[i, 5]==1 
       || compiled_data_countryY[i, 6]==1 || compiled_data_countryY[i, 7]==1 || compiled_data_countryY[i, 8]==1 ||
         compiled_data_countryY[i, 9]==1 || compiled_data_countryY[i, 10]==1 || compiled_data_countryY[i, 11]==1 
       || compiled_data_countryY[i, 12]==1 ){
      Number_Infected_CountryY = Number_Infected_CountryY + 1
      
    }
    
    #Counts females vs males 
    if(compiled_data_countryY[i,1]=="female"){
      Number_Female_CountryY = Number_Female_CountryY + 1
    } else{
      Number_Male_CountryY = Number_Male_CountryY + 1
    }
    
  }
  
  # Percentage of infected individuals in country Y
  Percentage_Infected_CountryY = Number_Infected_CountryY / Number_Screens_CountryY * 100
  
  
  #Country X 
  setwd("~/Desktop/Biocomputing_RProject-main/Rproject2022/countryX/")
  files_X <- list.files(path="~/Desktop/Biocomputing_RProject-main/Rproject2022/countryX/", pattern="*.csv")
  for(i in 1:length(files_X)){
    compiled_data_countryX <- read.csv(files_X[i], header= TRUE)
  }
  Number_Screens_CountryX = nrow(compiled_data_countryX)
  Number_Infected_CountryX = 0
  Number_Female_CountryX = 0
  Number_Male_CountryX = 0
  for(i in 1:nrow(compiled_data_countryX)){
    #Counts for infected individuals
    if(compiled_data_countryX[i, 3]==1 || compiled_data_countryX[i, 4]==1 || compiled_data_countryX[i, 5]==1 
       || compiled_data_countryX[i, 6]==1 || compiled_data_countryX[i, 7]==1 || compiled_data_countryX[i, 8]==1 ||
       compiled_data_countryX[i, 9]==1 || compiled_data_countryX[i, 10]==1 || compiled_data_countryX[i, 11]==1 
       || compiled_data_countryX[i, 12]==1 ){
      Number_Infected_CountryX = Number_Infected_CountryX + 1
      
    }
    
    #Counts females vs males 
    if(compiled_data_countryX[i,1]=="female"){
      Number_Female_CountryX = Number_Female_CountryX + 1
    } else{
      Number_Male_CountryX = Number_Male_CountryX + 1
    }
    
  }
  # Percentage of infected individuals in country X
  Percentage_Infected_CountryX = Number_Infected_CountryX / Number_Screens_CountryX * 100
}

# Carol Notes: for the third function, I was using the dplyr package because I thought it was a cool tool. I also understood we were supposed to use the 
	# allData.csv file instead of doing by country x and country Y. So, for number of screens run, female x male count and age distribution I have the
	# following code:
		# The age distribution is also grouped by gender following Prof. Stuart suggestion by e-mail. 

setwd("~/Desktop/Rproject2022")
read.csv(file='allData.csv')
allData <- read.csv(file='allData.csv')
library(dplyr)

# Number of screens run
allData %>% count

# Male vs. Female patients
allData_gender <- group_by(allData, gender)
summarize(allData_gender, abundance = n())

# Age distribution of patients 
allData_gender_age <- group_by(allData, gender, age)
age_distribution_df <- summarize(allData_gender_age, abundance = n())
