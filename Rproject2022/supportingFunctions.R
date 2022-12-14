# supportingFunctions.R
# Alex Brandt, Olivia Maracina, Hugh Smith
# 12/14/2022

# convert .txt files of each country to .csv
txt_to_csv = function(countries) {
  for (country in countries) { # list of countries passed as an argument
    # update this path to be the path to your working directory which contains 
    # the country directories
    path = paste("~/biocomputing/Biocomputing_RProject/Rproject2022/country", country, sep = '')
    setwd(path)
    filelist = list.files(pattern = "*.txt")
    for (file in filelist) {
      data = read.table(file, header=TRUE, sep = "", stringsAsFactors = FALSE)
      output=gsub("txt","csv", file)
      write.table(data, file, sep = ',')
    }
  }
}

# create full dataset- compile all file data into one
compile_files <- function(countries){
  fulldata <- data.frame(matrix(ncol = 15, nrow = 0))
  colnames(fulldata) = c('gender','age','marker01','marker02','marker03','marker04','marker05','marker06','marker07','marker08','marker09','marker10','country','dayofYear','infected')

  # add all country X data to full dataset
  for (country in countries) { # list of countries passed as an argument
    path = paste("~/biocomputing/Biocomputing_RProject/Rproject2022/country", country, sep = '')
    setwd(path)
    filelist = list.files(pattern = "*.csv")
    for (file in filelist) {
      data = read.csv(file)
      day = as.numeric(substr(file, 8, 10))
      data$country = country
      data$dayofYear = day
      data$infected = FALSE
      fulldata = rbind(fulldata, data)
    }
  }

  # Ask user for preference for handling NAs
  print("Remove NAs: (1) Warn for NAs: (2) Include NAs: (3)")
  warnings = readline()   # takes user input
  warnings = as.integer(warnings) # converts to integer
  # Handle NAs
  if (warnings == 1) {
    fulldata = na.omit(fulldata) # remove NAs
  } else if (warnings == 2) {
    print("Number of rows with NA values:")
    print(nrow(fulldata)-nrow(na.omit(fulldata))) 
  } # any other value input, including 3, will trigger no warnings

  # write compiled data to one csv
  data_for_csv = fulldata[,1:ncol(fulldata)-1] # removes the infected column
  write.table(data_for_csv, '~/biocomputing/Biocomputing_RProject/Rproject2022/compiled_data.csv', sep = ',')
  
  return(fulldata)
}

# summarize statistics function
summary <- function(countries, fulldata){
  
  # Number of screenings
  print(paste("Number of screens performed: ", length(unique(fulldata$dayofYear))))
  
  # percent of patients infected:
  num_infected = 0
  total_patients = 0
  for (i in 1:nrow(fulldata)) {
    row <- fulldata[i,]
    for (val in row[3:12]) {
      # if any marker is 1, person was infected
      if (val == 1){
        num_infected = num_infected + 1
        fulldata[i,15] = TRUE
        # break immediately to prevent over-recording cases when multiple 
        # markers present
        break
      }
    }
    total_patients = total_patients + 1
  }
  print(paste("Total patients screened: ", total_patients))
  print(paste("Total patients infected: ", num_infected))
  print(paste("Percent infected: ", num_infected/total_patients*100))


  # male vs female patients
  # calculate num infected males, females, as well as totals
  infected_males = sum(fulldata[which(fulldata$gender=='male'), 15])
  total_males = sum(fulldata$gender=='male')
  infected_females = sum(fulldata[which(fulldata$gender=='male'), 15])
  total_females = sum(fulldata$gender=='female')
  
  # create a dataframe of the gender data found above
  gender_data <- data.frame(
    condition = c("infected", "infected", "uninfected", "uninfected"),
    gender = c("male", "female", "male", "female"),
    count = c(infected_males, infected_females, total_males-infected_males, total_females-infected_females))

  # plot the gender data- male vs female and infected vs uninfected
  gender_plot <- ggplot(gender_data, aes(fill=gender, y=count, x=condition)) + 
    geom_bar(position="dodge", stat="identity") +
    ggtitle("Male vs Female") +
    theme_classic()
  print(gender_plot)
  # Nearly identical amounts of males and females were screened and are infected.
  
  # display the age distribution of the infected vs uninfected
  age_plot <- ggplot(fulldata, aes(x=age, color = infected)) +
    geom_density() +
    xlim(0,100) + # limits age to 100- removes unrealistic ages > 100
    ggtitle("Patient Age Distribution") +
    theme_classic()
  print(age_plot)
  # The peak age of infection appears to be around 5 years old
  
  return(fulldata)
}
  