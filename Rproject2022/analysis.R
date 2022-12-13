setwd("~/GitHub/Biocomputing_RProject/Rproject2022/")
source("~/Github/Biocomputing_RProject/Rproject2022/supportingFunctions.R")
dirY <- "~/Github/Biocomputing_RProject/RProject2022/countryY"
dirX <- "~/Github/Biocomputing_RProject/RProject2022/countryX"

# Use functions from supporting functions to compile data
convertToCSV(dirY)
allData <- compileXY(dirX, dirY)

output <- screenStats(allData)

infectionStats <- output[[2]]
ageDistGraph <- output[[1]]
allData <- output[[3]]

#### QUESTION 1 ####
infected_individuals <- allData[allData$infected_marker=="Infected",]

infected_individuals_X <- infected_individuals[infected_individuals$country=="X",]
infected_individuals_Y <- infected_individuals[infected_individuals$country=="Y",]

#Figure 1
ggplot(infected_individuals,aes(x = dayofYear, fill = country)) + geom_bar() + xlab("Day of Year") + ylab("Number of Infected Individuals") + theme_classic() +
  scale_fill_manual(values=c('mediumorchid4', 'lightblue'))

#Figure 2
ggplot(infected_individuals_X,aes(x = dayofYear)) + geom_bar(fill='mediumorchid4') + xlab("Day of Year") + ylab("Number of Infected Individuals X") + theme_classic()

#Figure 3
ggplot(infected_individuals_Y,aes(x = dayofYear)) + geom_bar(fill='lightblue') + xlab("Day of Year") + ylab("Number of Infected Individuals Y") + theme_classic()

# Answer to Question 1:
# The country which likely started the outbreak is country X. In figure 1, you can see that
# starting from day 120, country X had a higher number of infected individuals than country Y.
# From figure 2, we can see that on the first day of screening (dayofYear = 120), country X
# already had ~50 infected individuals, and this number begins to increase. However, from figure 3
# we can see that patients in country Y were only infected beginning around day 140, so 20 days later. 

#### QUESTION 2 ####
Y_markers <- c(sum(infected_individuals_Y$marker01 == 1), sum(infected_individuals_Y$marker02 == 1), sum(infected_individuals_Y$marker03 == 1), sum(infected_individuals_Y$marker04 == 1), 
               sum(infected_individuals_Y$marker05 == 1), sum(infected_individuals_Y$marker06 == 1), sum(infected_individuals_Y$marker07 == 1), sum(infected_individuals_Y$marker08 == 1), 
               sum(infected_individuals_Y$marker09 == 1), sum(infected_individuals_Y$marker10 == 1))
X_markers <- c(sum(infected_individuals_X$marker01 == 1), sum(infected_individuals_X$marker02 == 1), sum(infected_individuals_X$marker03 == 1), sum(infected_individuals_X$marker04 == 1), 
               sum(infected_individuals_X$marker05 == 1), sum(infected_individuals_X$marker06 == 1), sum(infected_individuals_X$marker07 == 1), sum(infected_individuals_X$marker08 == 1), 
               sum(infected_individuals_X$marker09 == 1), sum(infected_individuals_X$marker10 == 1))

markers_df <- data.frame("Marker" = c("marker01", "marker02", "marker03", "marker04", "marker05", "marker06", "marker07", "marker08", "marker09", "marker10", "marker01", "marker02", "marker03", "marker04", "marker05", "marker06", "marker07", "marker08", "marker09", "marker10"), "Number" = c(X_markers, Y_markers), "Country" = c("X","X","X","X","X","X","X","X","X","X","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y"))

# Figure 4
ggplot(data=markers_df, aes(x = Marker, y = Number, fill=Country)) + 
  geom_bar(position='dodge', stat='identity') +
  xlab("Markers") + ylab("Number") + theme_classic()

# Answer to question 2:

# If country Y develops a vaccine, it is unlikely to work for country X.
# From figure 4, we can see that the markers 6-10 are the most common in 
# country Y's population. The markers 1-5 are the most common in country X, 
# and because the vaccines will target these markers, it is likely there
# will not be as much success if country Y's vaccine is used in country X.


