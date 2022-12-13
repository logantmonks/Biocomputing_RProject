## Intro to Biocomputing R Final Project: Analysis
## Clayton Glasgow and Sydney Harris and Jaden Bailey
## 14 December 2022

## set working directory
setwd("~/Desktop/IntroBiocomputing/R/Biocomputing_RProject")

# load necessary libraries
library(ggplot2)
library(tidyr)

## Data prep

# load functions from supportingFunctions script
source("~/Desktop/IntroBiocomputing/R/Biocomputing_RProject/supportingFunctions.R")

# convert country Y txt files to csv files
txt_to_csv("/Users/claytonglasgow/Desktop/IntroBiocomputing/R/Biocomputing_RProject/Rproject2022/countryY")

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
# non-graphical output:
# number of screens run: 39888
# percent of all patients infected: 56.55084
# percent of female patients infected: 56.61942
# percent of male patients infected: 56.48259
# These summary statistics indicate that male and female patients are equally susceptible to the novel disease.

# use percent_age_infections function to graph the percentage of infected individuals from each 10-year age group
percent_age_infections(allData)
# non-graphical output:
#    age group    percent of individuals infected from that age group
#        0-10     56.71854
#       11-20     56.10936
#       21-30     57.01200
#       31-40     56.14162
#       41-50     57.72595
#       51-60     53.28467
#       61-70     55.32787
#       71-80     57.85714
#       81-90     51.40187
#       91-100    59.63303
# as can be seen from this output and the accompanying graph, all 10-year age groups are 
# approximately equally susceptible to the novel disease.

#Question 1: In which country (X or Y) did the disease outbreak likely begin?
#To answer this question, we are going to analyze the trend in number of infections
#over time in both countries. If one country shows a much larger number of infections
#during early screening days than the other, this would indicate that the disease
#originated in that country.

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
# and with only country, dayofYear, and infected columns
no_markers <- infected[,13:15]

# plot number of infections over time
ggplot(no_markers, aes(x = dayofYear)) + # x is dayofYear, so y will be the count of infected individuals because it is a histogram
  geom_histogram(binwidth = 1) + # set binwidth as 1 so there is a bar for each day
  facet_wrap(~country) + # facet by country to compare temporal disease trends between Country X and Country Y
  xlab("Day of year") +
  ylab("Number of reported infections") +
  theme_classic()


## As can be seen on the histograms, cases were detected in Country X as early as
## day 120 whereas no cases were reported in Country Y until day 139. Thus, it is highly
## likely that the disease originated in Country X given that cases were reported
## there almost 3 weeks before they were in Country Y.

#Question 2: If Country Y develops a vaccine for the disease, is it likely to work for citizens of Country X
#To answer this question, we are going to analyze whether there is a difference in which markers infected patients display in each country
#If there is a difference in which markers are present in infected patients between countries, then a single vaccine will unlikely be effective in both countries.


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


#Bonus: disease evolution
#Given the short generation time of the disease-causing bacteria, it is suspected that 
#the bacteria is evolving along its transmission path.
#Given this expected evolution, we hypothesize that the markers present in the populations will change over time.

# plot markers present over time

# convert data to long format
long <- gather(young, key = "marker", value = "status", 3:12)

# data with only marker status, marker name, dayofYear
long_markers <- long[,c(4, 6:7)]
# data with only infected markers
long_markers_infected <- long_markers[long_markers$status == 1,]

# plot infection over time, faceted by marker to see number of positive tests per marker over time
ggplot(long_markers_infected, aes(x = dayofYear)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~marker) +
  theme_classic() +
  xlab("Day of year") +
  ylab("Number of positive tests")
# plot, color by marker (harder to interpret but cooler to look at)
ggplot(long_markers_infected, aes(x = dayofYear, fill = marker)) +
  geom_histogram(binwidth = 1) +
  theme_classic() +
  xlab("Day of year") +
  ylab("Number of positive tests")

##As both graphs show (the first, faceted graph more clearly than the colored graph),
##markers 1-5 are consistently present from the onset of the outbreak, and they all
##increase in prevalence over the course the screening trials. Markers 6-10, however,
##do not begin to consistently appear until day 150 (markers 6 and 7) or 160 (markers 8-10),
##indicating that different strains of the infectious bacteria began to evolve at that
##time. These days also coincide with the outbreak of the disease in Country Y, suggesting
##that evolution of the infectious bacteria may have permitted it to spread to a new population 
# with different immune function. Indeed, individuals from Country Y were much more likely
##to have markers 6-10 present than were individuals from Country X, who mostly displayed
##markers 1-5. We thus conclude that the spread of the disease from Country X to Country Y
##was likely made possible by a mutation in the bacteria that allowed it to infect individuals
##with an immune profile characteristic of individuals of Country Y.

