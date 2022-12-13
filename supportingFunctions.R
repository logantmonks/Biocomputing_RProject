## Intro to Biocomputing Final R Project: Supporting Functions
## Clayton Glasgow, Sydney Harris, Jaden Bailey
## 14 December 2022

# supporting function #1
# function to compile data from all csv files in a directory into a single csv file
# note: you must set your working directory to be the same directory used as the function argument
# compile_csv(dir, type, place, start = 8, stop = 10, na_rm): 
# "dir" = path to directory of interest
# "type" = file type to include in compilation ("csv", "txt", etc)
# "place" = place to include in added "country" column ("X" or "Y")
# "start" = start point to extract information from file name to include as a new column (day of year of screening)
# "stop" = end point to "start"; the defaults (start = 8, stop = 10) extract the day of screening from each file name
# "na_rm" = manage NA values within resulting dataframe
#           na_rm = "remove": rows with NA values are removed
#           na_rm = "warn": if NA values are present, a warning message appears
#           na_rm = "include": NA values are included without a warning
compileFiles <- function(dir, type, place, start = 8, stop = 10, na_rm){
  # create object with current working directory to be able to run function without changing directories
  wd <- getwd()
  # set working directory to directory with files of interest
  setwd(dir) 
   # create a list of files from the directory of interest
  file_list <- list.files(dir, type)
  # read in first file in file_list; this will be the dataframe to which all
  # subsequent csv's will be appended
  all <- read.csv(file_list[1], header = TRUE)
  # add country column
  all$country = place
  # add dayofYear column (day of year of screening)
  all$dayofYear = substr(file_list[1], start = start, stop = stop)
  # loop through the rest of file_list, appending each csv to the bottom of "all"
  for(file in 2:length(file_list)){
      # read in next csv file in file_list
      screen <- read.csv(file_list[file], header = TRUE)
      # add country column
      screen$country <- place
      # add dayofYear column (day of year of screening)
      screen$dayofYear <- substr(file_list[file], start = start, stop = stop)
      # append ith csv file to all csv dataframe
      all <- rbind(all, screen)
      # remove the unnecessary objects to save memory
      rm(screen)
  }
  # include optional NA warning/removal
  # if na_rm = "remove", rows with NA values are deleted from dataframe and the message
  # "NA values removed" is printer
  if(na_rm == "remove"){
    all <- na.omit(all)
    print("NA values removed")
  } # if na_rm = "warn", a warning appears if NA values are present in dataframe
    else if(na_rm == "warn"){
    if(any(is.na(all)) == TRUE){
      print("Warning: data contain NA values")
      # if no NA values are present, the message "No NA values" is printed
    } else{print("No NA values")}
  } # if na_rm = "include", NA values are included without a warning message
    else if(na_rm == "include"){
    all <- all
    }
  # reset working directory to original working directory to place output file in the directory you are working in
  setwd(wd)
  # write csv file with all data
  write.csv(all, file = paste(place, "_allData.csv", sep = ""), row.names = FALSE) 
}

# supporting function #2
# convert files from txt to csv
# text_to_csv(dir):
# "dir" = path to directory of interest
txt_to_csv <- function(dir){
  # create a list of txt files to convert to csv in provided directory
  files<-list.files(dir, pattern ="*.txt", full.names = TRUE, recursive = FALSE)
  # loop through file list to convert each txt file within the directory to csv
  for (file in files){
    # read in the ith file
    read<-read.table(file, sep = "", header = FALSE)
    # replace "txt" with "csv"
    filename<-gsub(".txt", ".csv", file)
    # rewrite file as csv
    write.table(read, file = filename, sep=",", row.names=FALSE, col.names=FALSE)
  }
}

#supporting function #3
#Write a function to summarize the compiled data set in terms of
  #number of screens run
  #percent of patients screened that were infected
  #percent of female patients infected and percent of male patients infected
  #the age distribution graphs of all patients screened and of all patients infected
# summarize_data(data):
# "data" = dataframe of interest; must be of the format of the screening data provided by countries X and Y
summarize_data <- function(data){
  #find number of screens run
  total <- nrow(data)
  print("number of screens run:")
  print(total)
  #percent total patients infected
  #create column in data set that will hold whether the patient was infected (value=1) or not infected (value=0)
  data$infected <- numeric(length(1:nrow(data)))
  #create variable that will be the total number of patients infected
  num_infect <-0
  for (i in 1:nrow(data)){ #for each row in the data set, sum the values of all of the marker columns
    if (sum(data[i, 3:12]) >=1){ #if the sum is greater than 1 (ie. the patient has at least one marker)
      data$infected[i] <- 1 #make the value in the column "infected" equal to 1, meaning that patient is infected
    }else if (sum(data[i, 3:12])<1){ #if the sum is less than 1 (ie. the patient has no markers)
      data$infected[i] <- 0 #make the value in the column "infected" equal to 0, meaning that patient is not infected
    }
  }
  #sum values in the "infected" column which will give the total number of infected patients
  num_infect <- sum(data[,"infected"])
  #calculate the percent of patients infected by dividing the number of infected patients by the total number of screens run
  percent_infected <- (num_infect/total)*100
  print("percent of all patients infected:")
  print(percent_infected)
  #number of male vs female infected
  #create variables for:
  #number of female patients infected
  num_female_inf <- 0
  #total number of female patients screened
  total_female <-0
  #number of male patients infected
  num_male_inf <- 0
  #total number of male patients screened
  total_male <- 0
  #for each row in the data set: evaluate whether the patient is female or male and whether they are infected or not
  for (i in 1:nrow(data)) { 
    if (data$gender[i] == "female"){ #if the patient (row i) is female
      total_female <- total_female+1 #add 1 to the number of total female patients
      if (data$infected[i] == 1){ # if the patient is female and infected (has value=1 in "infected" column)
        num_female_inf <- num_female_inf+1 #add 1 to number of infected females
      }else if (data$infected[i] == 0){ # if the patient is female and not infected (has value=0 in "infected" column)
        num_female_inf <- num_female_inf+0 #add 0 to number of infected females
      }
    } else if (data$gender[i] == "male"){ #if the patient (row i) is male
      total_male <- total_male+1 #add 1 to the number of total male patients
      if (data$infected[i] == 1){ # if the patient is male and infected (has value=1 in "infected" column)
        num_male_inf <- num_male_inf+1 #add 1 to number of infected males
      }else if (data$infected[i] == 0){ # if the patient is male and not infected (has value=0 in "infected" column)
        num_male_inf <- num_male_inf+0 #add 0 to number of infected males
      }
    }
  }
  #calculate percentages of each gender that is infected by dividing the number infected patients of that gender by the total number of patients screened of that gender
  female_inf_per <- (num_female_inf/total_female)*100
  male_inf_per <- (num_male_inf/total_male)*100
  print("percent of female patients infected:")
  print (female_inf_per)
  print("percent of male patients infected:")
  print(male_inf_per)
  #age distribution and infection percentage graphs
  #load ggplot
  library(ggplot2)
  #make a histogram to visualize age distribution of all patients screened
  allData_graph <-ggplot (data, aes (x=age))+
    #make bin width equal to 10 years, ranging from 0-10 to 90-100
    #exclude data for patients older than 100, which was likely an error in data collection or entry
    geom_histogram(binwidth=10, breaks = c(10,20,30,40,50,60,70,80,90,100))+ 
    xlab("Age")+
    ylab("Number of Patients Screened")+
    theme_classic()+
    labs(title="Age Distribution of Patients Screened")
  print(allData_graph)
  #create data frame with only infected patients
  infected_data <-  data[data$infected == "1",]
  #make a histogram to visualize age distribution of all infected patients
  infected_graph <-ggplot (infected_data, aes (x=age))+
    #make bin width equal to 10 years, ranging from 0-10 to 90-100
    #exclude data for patients older than 100, which was likely an error in data collection or entry
    geom_histogram(binwidth=10, breaks = c(10,20,30,40,50,60,70,80,90,100))+
    xlab("Age")+
    ylab("Number of Infected Patients")+
    theme_classic()+
    labs(title="Age Distribution of Patients Infected")
  print(infected_graph)
}
  


#create another custom function that creates a graph to show percent of patients infected for each 10 year age range
#this will help show if a particular age range is more susceptible to infection 
# precent_age_infections(data):
# "data" = dataframe of interest; must be of the format of the screening data provided by countries X and Y
percent_age_infections <- function(data){
  #create column in data set that will hold whether the patient was infected (value=1) or not infected (value=0)
  data$infected <- numeric(length(1:nrow(data)))
  for (i in 1:nrow(data)){ #for each row in the data set, sum the values of all of the marker columns
    if (sum(data[i, 3:12]) >=1){ #if the sum is greater than 1 (ie. the patient has at least one marker)
      data$infected[i] <- 1 #make the value in the column "infected" equal to 1, meaning that patient is infected
    }else if (sum(data[i, 3:12])<1){ #if the sum is less than 1 (ie. the patient has no markers)
      data$infected[i] <- 0 #make the value in the column "infected" equal to 0, meaning that patient is not infected
    }
  }
  #create variables to store information for total number of patients screened and number of patients infected within an age group
  #exclude data for patients older than 100, which was likely an error in data collection or entry
  infected_by_age <- numeric (10)
  total_by_age <- numeric (10)
  #for each row in the data set: determine if age is within a 10 year range and whether the patient is infected
  for (i in 1:nrow(data)) { 
    #create variable for the age of the patient
    age <- data[i,"age"]
    #if the patient is 0 to 10 years old:
    if (0<age && age<=10){ 
      #add 1 to the total number of 0-10 year old patients screened
      total_by_age[1] <- total_by_age[1]+1 
      if (data$infected[i] == 1){ 
        #add 1 to the total number of 0-10 year old patients infected
        infected_by_age[1] <- infected_by_age[1]+1 
      }else if (data$infected[i] == 0){ 
        #add 0 to the total number of 0-10 year old patients infected
        infected_by_age[1] <- infected_by_age[1]+0
      }
    #if the patient is 11 to 20 years old:
    } else if (10<age && age<=20){ 
      #add 1 to the total number of 11-20 year old patients screened
      total_by_age[2] <- total_by_age[2]+1 
      if (data$infected[i] == 1){ 
        #add 1 to the total number of 11-20 year old patients infected
        infected_by_age[2] <- infected_by_age[2]+1
      }else if (data$infected[i] == 0){ 
        #add 0 to the total number of 11-20 year old patients infected
        infected_by_age[2] <- infected_by_age[2]+0
      }
    #if the patient is 21 to 30 years old:
    }else if (20<age && age<=30){ 
      #add 1 to the total number of 21-30 year old patients screened
      total_by_age[3] <- total_by_age[3]+1  
      if (data$infected[i] == 1){ 
        #add 1 to the total number of 21-30 year old patients infected
        infected_by_age[3] <- infected_by_age[3]+1 
      }else if (data$infected[i] == 0){ 
        #add 0 to the total number of 21-30 year old patients infected
        infected_by_age[3] <- infected_by_age[3]+0
      }
    #if the patient is 31 to 40 years old:
    }else if (30<age && age<=40){ 
      #add 1 to the total number of 31-40 year old patients screened
      total_by_age[4] <- total_by_age[4]+1  
      if (data$infected[i] == 1){ 
        #add 1 to the total number of 31-40 year old patients screened
        infected_by_age[4] <- infected_by_age[4]+1 
      }else if (data$infected[i] == 0){ 
        #add 0 to the total number of 31-40 year old patients infected
        infected_by_age[4] <- infected_by_age[4]+0
      }
    #if the patient is 41 to 50 years old:
    }else if (40<age && age<=50){ 
     #add 1 to the total number of 41-50 year old patients screened
      total_by_age[5] <- total_by_age[5]+1  
      if (data$infected[i] == 1){
        #add 1 to the total number of 41-50 year old patients infected
        infected_by_age[5] <- infected_by_age[5]+1 
      }else if (data$infected[i] == 0){ 
        #add 0 to the total number of 41-50 year old patients infected
        infected_by_age[5] <- infected_by_age[5]+0
      }
    #if the patient is 51 to 60 years old:
    }else if (50<age && age<=60){ 
      #add 1 to the total number of 51-60 year old patients screened
      total_by_age[6] <- total_by_age[6]+1  
      if (data$infected[i] == 1){ 
        #add 1 to the total number of 51-60 year old patients infected
        infected_by_age[6] <- infected_by_age[6]+01
      }else if (data$infected[i] == 0){ 
        #add 0 to the total number of 51-60 year old patients infected
        infected_by_age[6] <- infected_by_age[6]+0
      }
    #if the patient is 61 to 70 years old:
    }else if (60<age && age<=70){ 
      #add 1 to the total number of 61-70 year old patients screened
      total_by_age[7] <- total_by_age[7]+1  
      if (data$infected[i] == 1){ 
        #add 1 to the total number of 61-70 year old patients infected
        infected_by_age[7] <- infected_by_age[7]+1 
      }else if (data$infected[i] == 0){ 
        #add 0 to the total number of 61-70 year old patients infected
        infected_by_age[7] <- infected_by_age[7]+0
      }
    #if the patient is 71 to 80 years old:
    }else if (70<age && age<=80){ 
      #add 1 to the total number of 71-80 year old patients screened
      total_by_age[8] <- total_by_age[8]+1  
      if (data$infected[i] == 1){ 
        #add 1 to the total number of 71-80 year old patients infected
        infected_by_age[8] <- infected_by_age[8]+1
      }else if (data$infected[i] == 0){ 
        #add 0 to the total number of 71-80 year old patients infected
        infected_by_age[8] <- infected_by_age[8]+0
      }
    #if the patient is 81 to 90 years old:
    }else if (80<age && age<=90){ 
      #add 1 to the total number of 81-90 year old patients screened
      total_by_age[9] <- total_by_age[9]+1  
      if (data$infected[i] == 1){ 
        #add 1 to the total number of 81-90 year old patients infected
        infected_by_age[9] <- infected_by_age[9]+1 
      }else if (data$infected[i] == 0){ 
        #add 0 to the total number of 81-90 year old patients infected
        infected_by_age[9] <- infected_by_age[9]+0
      }
    #if the patient is 91 to 100 years old:
    }else if (90<age && age<=110){ 
      #add 1 to the total number of 81-90 year old patients screened
      total_by_age[10] <- total_by_age[10]+1 
      if (data$infected[i] == 1){ 
        #add 1 to the total number of 91-100 year old patients infected
        infected_by_age[10] <- infected_by_age[10]+1 
      }else if (data$infected[i] == 0){ 
        #add 0 to the total number of 91-100 year old patients infected
        infected_by_age[10] <- infected_by_age[10]+0
      }
    }
  }
  #calculate percentages of infected patients for each age group 
  age_percents <- numeric (10)
  age_percents <- (infected_by_age/total_by_age)*100
  #create vector with all age ranges
  age_ranges <- c("0-10", "11-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90","91-100")
  #create data frame with age ranges and percent infected by age group
  age_infected_data <- data.frame(age_ranges, age_percents)
  #graph data to display percent of patients infected for each each group to see if one age group is more susceptible
  infected_by_age_graph <- ggplot(age_infected_data, aes(x=age_ranges, y=age_percents))+
    geom_col()+
    xlab("Age Ranges")+
    ylab("Percent of Patients Infected")+
    ylim(min=0, max=100)+
    theme_classic()
  print(infected_by_age_graph)
  print(age_infected_data)
}
