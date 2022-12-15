#Bringing in functions 
source("~/Desktop/Biocomputing_RProject/RProject2022/kaceysupportingFunctions.R")

#compilation of data
compile("~/Desktop/Biocomputing_RProject/RProject2022/countryX","~/Desktop/Biocomputing_RProject/RProject2022/countryY","X","Y")

#summary function execution
summarize("~/Desktop/Biocomputing_RProject/RProject2022/allData.csv")

#creation of useful dataframes
alldata <- read.csv("~/Desktop/Biocomputing_RProject/RProject2022/allData.csv")
alldata$markersum = rowSums(alldata[,3:12])

for (i in 1:nrow(alldata)){
  if (alldata$markersum[i]==0)
    alldata$infection[i]="no"
  else
    alldata$infection[i]="yes"
}


infected <- alldata[alldata$infection=="yes",]

#determination of country of origin
countryoforigin()
#This histogram demonstrates clearly that the disease likely began in country X because from day of year 120
#up until close to day of year 140, all of the infections were in country X. Only after day 140
#do the cases begin to pick up in country Y

#determination of usefulness of country Y vaccine
markers()
#histograms for each marker show the prevalence of that marker in a given country's infected population
#no the vaccine developed by country Y, if it is based off of the markers in its own population, will not be effective in country X.
#There are various markers in country X that are not apparent in country Y, but they all produce the disease.
#If country Y targets positive markers in its own population that are not positive in the other country's population, the vaccine will be ineffective.