# Biocomputing R Project

# Analysis Script

# Carolina Jimenez and Fabely Moreno

# Set working directory:
#setwd("C:/Users/carolina/Biocomputing_RProject/Rproject2022/")
setwd("C:/Users/fabel/Biocomputing_RProject/Rproject2022/Rproject2022")


# Load supporting functions:
source("supportingFunctions.R")

# Use supporting functions to create and compile the files needed for analysis:
# The input for the convert.csv in based on your current working directory inside RProject2022
# The input for the merge.all.csv in based on your current working directory inside RProject2022 and
# 1 to ignore NA's, 2 to warn of NA presence, 3 to remove any NA's found
# The input for summarise.compiled.data is your current working directory where allData.csv is located


convert.csv("./countryX/")
convert.csv("./countryY/")
merge.all.into.one("./countryX/",3)
merge.all.into.one("./countryY/",3)
summarise.compiled.data("./")

# Load the compiled file:

allData <- read.csv(file = "allData.csv", sep = ",", header = TRUE)

# install and load libraries:

#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("ggpubr")
library(ggplot2)
library(dplyr)
library(ggpubr)

### Question 1: In which country (X or Y) did the disease outbreak likely begin?

# Create column of infected (1) or non infected (0) for all patients:

infected.dis <- length(allData)

for(i in 1:nrow(allData)){
  if(sum(allData$marker01[i], allData$marker02[i], allData$marker03[i], allData$marker04[i], allData$marker05[i],
         allData$marker06[i], allData$marker07[i], allData$marker08[i], allData$marker09[i], allData$marker10[i])==0){
    infected.dis[i] <- "0"
  }else if(sum(allData$marker01[i], allData$marker02[i], allData$marker03[i], allData$marker04[i], allData$marker05[i],
               allData$marker06[i], allData$marker07[i], allData$marker08[i], allData$marker09[i], allData$marker10[i])> 0){
    infected.dis[i] <- "1"
  }
}

infected <- data.frame(infectionstatus = infected.dis)
data.infected.dis <- data.frame(date = allData$dayofYear, infectionstatus = infected.dis, country = allData$country)

infected.cont <- length(allData)

for(i in 1:nrow(allData)){
  if(sum(allData$marker01[i], allData$marker02[i], allData$marker03[i], allData$marker04[i], allData$marker05[i],
         allData$marker06[i], allData$marker07[i], allData$marker08[i], allData$marker09[i], allData$marker10[i])==0){
    infected.cont[i] <- 0
  }else if(sum(allData$marker01[i], allData$marker02[i], allData$marker03[i], allData$marker04[i], allData$marker05[i],
               allData$marker06[i], allData$marker07[i], allData$marker08[i], allData$marker09[i], allData$marker10[i])> 0){
    infected.cont[i] <- 1
  }
}

data.infected.cont <- data.frame(date = allData$dayofYear, infectionstatus = infected.cont, country = allData$country)


## Figure 1. Bar plot of Country and date by infected status:

# Bar plot when infected status continuous
ggplot(data.infected.cont, aes(country, date, color = infectionstatus))+
  geom_bar(stat="identity")+
  ggtitle("Fig. 1")+
  theme_bw()+ scale_fill_brewer(palette="Paired")

# Bar plot when infected status discrete
ggplot(data.infected.dis, aes(country, date, fill = infectionstatus))+
  geom_bar(stat="identity")+
  ggtitle("Fig. 2")+
  theme_bw()+ scale_fill_brewer(palette="Paired")

## Figure 3. Histogram plot of the infected individuals in each county by date:

ggplot(subset(data.infected.dis, infectionstatus==1), aes(date, color= country, fill = country))+
  geom_histogram(bins=55, alpha=0.5)+
  ggtitle("Fig.3 Number of infected individuals in each country by day")+
  xlab("Day of year")+
  ylab("Number of Infected individuals")+
  theme_bw()


## Answer for Question 1: 

# To answer the question of where did the disease outbreak first occur we conducted an analysis
# of the distribution of infected by day of the year in each country. To achieve this goal,
# we calculated the number of infected individuals in each country and created a series of plots
# to evaluate the distribution of infected individuals compared to day of year.
# Figure 1 suggests that country X had more infected individuals in earlier days of the year
# compared to country Y. This can be seen as country X has a greater amount of light blue indicating infected
# in the lower half of the graph. Meanwhile, country Y has a greater amount of dark blue indicating non infected individuals
# in the lower half of the graph. The Figure 3 (histogram) also shows the same result. Country X (red) has more infected
# individuals in earlier days of the year (left portion of the histogram), meanwhile Country Y (blue) has very low numbers
# of infected individuals in the same period (left side of the histogram). The infected individual population of Country Y
# starts to increase around day 140.
# These observations support the conclusion that the disease originated in Country X were the infected individuals first appeared.



### Question 2: If Country Y develops a vaccine for the disease, is it likely to work for citizens of Country X?


# Join allData.csv with infected column:

allData2 <- cbind(allData, infected)


# Subset allData by removing all ages greater than age 120:
# This is done because no human has lived more than 120 years of age, therefore individuals
# with ages like 423, 418,367 are not realistic. A total of 15 individuals are removed from the data.

allData3 <- subset(allData2, age <= 120)
removed.ages <- subset(allData2, age >120)
length(removed.ages) #15


# Assign age level to each individual:
age.levels <- length(allData3$age)

for(i in 1:nrow(allData3)){
  if(allData3$age[i]<= 2){
    age.levels[i] <- "<2"
  }else if(between(allData3$age[i], 3, 12)){
    age.levels[i] <- "3-12"
  }else if(between(allData3$age[i],13,17)){
    age.levels[i]<- "13-17"
  }else if(between(allData3$age[i],18,24)){
    age.levels[i] <- "18-24"
  }else if(between(allData3$age[i],25,49)){
    age.levels[i]<- "25-49"
  }else if(between(allData3$age[i],50,64)){
    age.levels[i]<-"50-64"
  }else if(allData3$age[i]>= 65){
    age.levels[i]<- "65+"
  }
}

# Create and bind column with age levels to allData3

Age.Levels <- data.frame(Agelevels = age.levels)

allData4 <- cbind(allData3, Age.Levels)

## Percent for each of the 7 age levels by Country:

X.data <- subset(allData4, country=="X")
Y.data <- subset(allData4, country=="Y")

X.data %>% 
  group_by(Agelevels) %>% 
  summarise( percent = 100 * n() / nrow(X.data))
Y.data %>%
  group_by(Agelevels) %>%
  summarise(percent=100*n()/nrow(Y.data))

age.percent.X <- c(9.64, 59.1, 11.0, 8.36, 9.22, 1.39, 1.35)
age.percent.Y <- c(9.58, 59.1, 11.4, 8.32, 8.76, 1.48, 1.33)

levels <- c("<2", "3-12", "13-17", "18-24", "25-49", "50-64", "65+")

data.X.age <- data.frame(level = c("<2", "3-12", "13-17", "18-24", "25-49", "50-64", "65+"),
                         percent = age.percent.X)
data.Y.age <- data.frame(level = c("<2", "3-12", "13-17", "18-24", "25-49", "50-64", "65+"),
                         percent = age.percent.Y)

## Figure 4 & 5 Pie chart for Age levels by Country:

par(mfrow=c(1,2))
pie(age.percent.X, levels, main = "Fig. 4 Pie Chart of Age Level Distribution for Country X")
pie(age.percent.Y, levels, main = "Fig. 5 Pie Chart of Age Level Distribution for Country Y")


## Figure 6 & 7 Pie charts by genders for each Country:
X.data %>% 
  group_by(gender) %>% 
  summarise( percent = 100 * n() / nrow(X.data))
Y.data %>%
  group_by(gender) %>%
  summarise(percent=100*n()/nrow(Y.data))

gen.percent.X <- c(49.7, 50.3)
gen.percent.Y <- c(50.0, 50.0)

genders <- c("female", "male")

par(mfrow=c(1,2))

pie(gen.percent.X, genders, main = "Fig. 6 Pie Chart of Gender Distribution for Country X")
pie(gen.percent.Y, genders, main = "Fig. 7 Pie Chart of Gender Distribution for Country Y")

### Calculations and Bar plots (Fig. 8 & 9) for the percent of each marker by country:

markers <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10")

# Calculation of the percents for each marker

# for country X:
X.data %>% 
  group_by(marker01) %>% 
  summarise( percent = 100 * n() / nrow(X.data))
X.data %>% 
  group_by(marker02) %>% 
  summarise( percent = 100 * n() / nrow(X.data))
X.data %>% 
  group_by(marker03) %>% 
  summarise( percent = 100 * n() / nrow(X.data))
X.data %>% 
  group_by(marker04) %>% 
  summarise( percent = 100 * n() / nrow(X.data))
X.data %>% 
  group_by(marker05) %>% 
  summarise( percent = 100 * n() / nrow(X.data))
X.data %>% 
  group_by(marker06) %>% 
  summarise( percent = 100 * n() / nrow(X.data))
X.data %>% 
  group_by(marker07) %>% 
  summarise( percent = 100 * n() / nrow(X.data))
X.data %>% 
  group_by(marker08) %>% 
  summarise( percent = 100 * n() / nrow(X.data))
X.data %>% 
  group_by(marker09) %>% 
  summarise( percent = 100 * n() / nrow(X.data))
X.data %>% 
  group_by(marker10) %>% 
  summarise( percent = 100 * n() / nrow(X.data))

# Vector of percents for country X:
m.percents.conX <- c(32.7, 33.3, 32.8, 33.9, 33.1, 0.275, 0.390, 0.390, 0.330, 0.315)

# For country Y:
Y.data %>%
  group_by(marker01) %>%
  summarise(percent=100*n()/nrow(Y.data))
Y.data %>%
  group_by(marker02) %>%
  summarise(percent=100*n()/nrow(Y.data))
Y.data %>%
  group_by(marker03) %>%
  summarise(percent=100*n()/nrow(Y.data))
Y.data %>% 
  group_by(marker04) %>% 
  summarise( percent = 100 * n() / nrow(Y.data))
Y.data %>%
  group_by(marker05) %>%
  summarise(percent=100*n()/nrow(Y.data))
Y.data %>% 
  group_by(marker06) %>% 
  summarise( percent = 100 * n() / nrow(Y.data))
Y.data %>%
  group_by(marker07) %>%
  summarise(percent=100*n()/nrow(Y.data))
Y.data %>% 
  group_by(marker08) %>% 
  summarise( percent = 100 * n() / nrow(Y.data))
Y.data %>% 
  group_by(marker09) %>% 
  summarise( percent = 100 * n() / nrow(Y.data))
Y.data %>% 
  group_by(marker10) %>% 
  summarise( percent = 100 * n() / nrow(Y.data))

# Vector of percents by marker in country y:

m.percents.conY <- c(1.0, 1.04, 1.06, 4.09, 4.00, 9.50, 9.64, 7.21, 7.42, 7.28)

# Data frames with data about marker percents in each country:

marker.percents.conX <- data.frame(markers= markers, percents = m.percents.conX)

marker.percents.conY <- data.frame(markers= markers, percents = m.percents.conY)


# Figure 8 Plot of markers in country X
a <- ggplot(marker.percents.conX, aes(x=markers,y=percents, fill= markers))+
  geom_bar(stat="identity")+
  ggtitle("Fig. 8 Percent of infected individuals with each marker in Country X")+
  xlab("Markers")+
  ylab("Percent")+
  theme_bw()+
  guides(fill = FALSE)

# Figure 9 Plot of markers in country Y
b <- ggplot(marker.percents.conY, aes(x=markers,y=percents, fill= markers))+
  geom_bar(stat="identity")+
  ggtitle("Fig. 9 Percent of infected individuals with each marker in Country X")+
  xlab("Markers")+
  ylab("Percent")+
  theme_bw()

## Plot of both countries side by side for markers
ggarrange(a,b,labels = c("A", "B"),ncol = 2)


## Answer for Question 2:

# To answer the question regarding the efficiency of a vaccine developed in Country Y
# to combat the disease in Country X, we took into account some factors and conducted
# an analysis. The first factor we considered was the age distribution in each country because
# vaccine effectiveness, testing, and dosage may be different based on age group.
# Based on our analysis, both countries have very similar age distributions as seen in figures 4 and 5.
# This result does not provide sufficient evidence to reject the use of the vaccine in Country X.

# The second factor analyzed was the gender distribution of each country. This was evaluated
# because the physiology of females and males are different and may affect vaccine effectiveness. 
# Based on our analysis, both countries have very similar gender distributions as seen in figures 6 and 7.
# Based on this result, there is no sufficient evidence to reject the use of the vaccine in Country X.

# The third and final factor analyzed was the presence of each disease marker by country.
# We evaluated this because having different markers indicated changes in the protein of the bacteria
# that causes the disease which may suggest different strains of the disease and may cause issues with
# effectiveness of the vaccine. To analyze this factor, we calculated the percent of each marker
# in each country and created two bar plot (figure 8 & 9). Based on Fig. 8 & 9, we can observe that in
# Country X (Fig. 8) there is a greater presence of infected individuals with markers 1-5 and
# very low presence of infected individuals with markers 6-10. On the other hand, we can observe that in
# Country Y (Fig. 9) there is a greater presence of infected individuals with markers 6-10 and a lower presence 
# of individuals with markers 1-5. This results suggests that the disease in Country X may be a different strain
# that the one in Country Y. Therefore, a vaccine developed in Country Y may be less effective
# in combating the disease in Country X.

# In conclusion, despite both countries having very similar populations, the markers for the disease are
# very different between countries which may indicate different strains of the disease.
# Therefore, the vaccine developed in Country Y will not work on the citizens of Country X. 
