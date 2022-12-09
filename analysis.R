## Intro to Biocomputing R Final Project: Analysis
## Clayton Glasgow and Sydney Harris and Jaden Bailey
## 14 December 2022

## set working directory
# maybe let's type another brief paragraph describing our file structure like last time?
setwd("~/Desktop/IntroBiocomputing/R/Biocomputing_RProject/Rproject2022")


## Data prep

# load functions from supportingFunctions script
#source(supportingFunctions.R)

# convert country Y txt files to csv files
# Jaden_function_here

# use compileFiles function to merge all csv files from country X and country Y
# country X 
compileFiles("~/Desktop/IntroBiocomputing/R/Biocomputing_RProject/Rproject2022/countryX",
             type = "csv", place = "X", na_rm = "warn")
# country Y 
compileFiles("~/Desktop/IntroBiocomputing/R/Biocomputing_RProject/Rproject2022/countryY",
             type = "csv", place = "Y", na_rm = "warn")

# load country X and country Y full data csv files and merge them to create one large dataframe
allData_x <- read.csv("X_allData.csv", header = TRUE)
allData_y <- read.csv("Y_allData.csv", header = TRUE)
allData <- rbind(allData_x, allData_y)

# use summarize_data to gain preliminary insight into disease trends
summarize_data(allData)

#Question 1: In which country (X or Y) did the disease outbreak likely begin?
#To answer this question, we are going to analyze the trend in number of infections
#over time in both countries. If one country shows a much larger number of infections
#during early screening days than the other, this would indicate that the disease
#originated in that country.

# load data
allData <- read.csv("allData.csv", header = TRUE)

# filter data to reasonable age group
young <- allData[allData$age < 101,]

# add column indicating whether or not an individual is infected (1) or not (0)
for(i in 1:nrow(young)){ #for each row in the data set, sum the values of all of the marker columns
  if (sum(young[i, 3:12]) >=1){ #if the sum is greater than 1 (ie. the patient has at least one marker)
    young$infected[i] <- 1 #make the value in the column "infected" equal to 1, meaning that patient is infected
  }else if (sum(young[i, 3:12])<1){ #if the sum is less than 1 (ie. the patient has no markers)
    young$infected[i] <- 0 #make the value in the column "infected" equal to 0, meaning that patient is not infected
  }
}

# create a dataframe with only infected individuals
infected <- young[young$infected == "1",]

# plot infection over time in each country
ggplot(infected, aes(x = dayofYear)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~country) +
  xlab("Day of year") +
  ylab("Number of reported infections") +
  theme_classic()

#Question 2: If Country Y develops a vaccine for the disease, is it likely to work for citizens of Country X
#To answer this question, we are going to analyze whether there is a difference in which markers infect patients in each country
#if there is a difference in which markers are infecting patients of each country, then the vaccines will likely not work for both countries

#load data
allData <- read.csv ("/users/sydneyharris/desktop/allData.csv", header = T)
#create a data frame that is only data for each marker in each country
country_x <-allData[allData$country == "X",3:12]
country_y <- allData[allData$country == "Y",3:12]
#create vector list of marker names
marker_names <- c("marker01", "marker02","marker03","marker04","marker05","marker06","marker07","marker08","marker09","marker10")
#create vectors that will hold the number of patients infected by each marker for each country
x_data <- numeric(10)
y_data <-numeric(10)

#for loop
for (i in 1:10){ 
  #for each marker (and corresponding column)
  #sum the values within the column (giving the number of patients infected by that marker)
  #assign that value to the correct country vector
    x_data[i] <- sum (country_x[,i])
    y_data[i] <- sum (country_y[,i])
}

#create new data frame that holds the marker and the number of patients infected by that marker in country X and in country Y
marker_infected_data <- data.frame(marker_names, x_data, y_data)
#graph data
library (ggplot2)
#graph of the number of patients infected by each marker in country X
ggplot(marker_infected_data, aes(x=marker_names, y=x_data))+
  geom_col()+
  xlab("Marker")+
  ylab("Number of Patients Infected")+
  labs(title="Country X: Number of Patients Infected by Each Marker")+
  theme_classic()
#graph of the number of patients infected by each marker in country X
ggplot(marker_infected_data, aes(x=marker_names, y=y_data))+
  geom_col()+
  xlab("Marker")+
  ylab("Number of Patients Infected")+
  labs(title="Country Y: Number of Patients Infected by Each Marker")+
  theme_classic()

#The distributions of which markers infect patients from country X and country Y are very different
#country X patients are infected by mostly markers 01-05 while country Y patients are infected mainly by markers 06-10
#it is unlikely that a vaccine developed in country Y will work for patients in country X because the vaccine will be targed at different markers
>>>>>>> e0b335c657b148ced03bd3adef32edf0099d91c3
