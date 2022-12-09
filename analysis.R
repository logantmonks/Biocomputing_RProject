## Intro to Biocomputing R Final Project: Analysis
## Clayton Glasgow and Sydney Harris and Jaden Bailey
## 14 December 2022

#Question 2: If Country Y develops a vaccine for the disease, is it likely to work for citizens of Country X
#To answer this questions, we are going to analyze whether there is a difference in which markers infect patients in each country
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
