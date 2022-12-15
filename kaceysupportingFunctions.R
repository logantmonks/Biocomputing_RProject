#this function converts txt files to csv
#it must be used in the directory where the txt files are present
convert <- function() {
textlist <- list.files('.', pattern='txt')
for (i in 1:length(textlist)){
input<-textlist[i]
#substitution of extension name
output<-gsub("\\.txt", "\\.csv", input)
data <-  read.table(input, header = TRUE)   
write.csv(data, file=output, row.names=FALSE)
}}

#this function compiles all csv files into one, also adds two requested rows
#for path1 and path2, the ABSOLUTE PATH of each country's directory must be entered surrounded by quotes
#for country1 and country2, the name of the country must be entered, surrounded in quotes
#example usage: compile("~/Desktop/Biocomputing_RProject/RProject2022/countryX","~/Desktop/Biocomputing_RProject/RProject2022/countryY","X","Y")
compile <- function(path1, path2, country1, country2) {
  #the next 4 lines show the creation of an empty csv that each individual csv will be added into
  columns <- c("gender","age","marker01","marker02","marker03","marker04","marker05","marker06","marker07","marker08","marker09","marker10","countryname","dayofYear")
  d <- data.frame(matrix(nrow=0,ncol=length(columns)))
  colnames(d) <- columns
  write.csv(d, file="~/Desktop/Biocomputing_RProject/RProject2022/whole.csv", row.names=FALSE)
  #lists are created for use in for loop
  country1list <- list.files(path1, pattern = "csv")
  country2list <- list.files(path2, pattern = "csv")
  #working directory is set to the country1's directory
  setwd(path1)
for (datafile in country1list){
input1 <- datafile
initialcsv <- read.csv(datafile)
#addition of country column
initialcsv$country <- country1
#creation of dayofYear value
NNNtxt <-gsub("\\.csv", "", input1)
NNN <- gsub("screen_", "", NNNtxt)
#addition of dayofYear line
initialcsv$dayofYear <- NNN
#each csv is added into the empty csv that was created
write.table(initialcsv, file="~/Desktop/Biocomputing_RProject/RProject2022/whole.csv", sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
}
#symmetrical code for country2 csv files
setwd(path2)
for (datafile in country2list){
input2 <- datafile
initialcsv2 <- read.csv(datafile)
initialcsv2$country <- country2
NNNtxt2 <-gsub("\\.csv", "", input2)
NNN2 <- gsub("screen_", "", NNNtxt2)
initialcsv2$dayofYear <- NNN2
write.table(initialcsv2, file="~/Desktop/Biocomputing_RProject/RProject2022/whole.csv", sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
  } 
}


#this function summarizes the compiled data
#the argument for this function is a data file that has total compiled data (ex. allData.csv that is provided in the folder)
#to use correctly the ABSOLUTE PATHWAY to the file is put in quotes to avoid error (ex. "~/Desktop/Biocomputing_RProject/RProject2022/allData.csv")
summarize <- function(alldatafile){
  alldata <- read.csv(alldatafile)
  #creation of marker sum column that will indicate an infection if there is at least one marker present
  alldata$markersum = rowSums(alldata[,3:12])
  #creation of infection column that evaluates whether marker sum is 0 or not
  for (i in 1:nrow(alldata)){
    if (alldata$markersum[i]==0)
      alldata$infection[i]="no"
    else
      alldata$infection[i]="yes"
  }
  
  #counting rows in complete data frame will show total screens done
  totalscreens <- nrow(alldata)
  print(paste("Total number of screens run:",totalscreens))
  
  #counting rows in complete data frame that have a yes in the infection column gives total infected
  infected <- alldata[alldata$infection=="yes",]
  totalinfected <- nrow(infected)
  #dividing the two numbers we have obtained will give a percentage infected that were screened
  ratio <- (as.numeric(totalinfected)/as.numeric(totalscreens))*100
  print(paste("Percent of patients screened that were infected:",ratio,"%"))
  
  #creation of data frames for males/females infected and total males/females to again count rows
  maleinfected <- infected[infected$gender=="male",]
  femaleinfected <- infected[infected$gender=="female",]
  totalmale <- alldata[alldata$gender=="male",]
  totalfemale <- alldata[alldata$gender=="female",]
  maleinfectedcount <- nrow(maleinfected)
  femaleinfectedcount <- nrow(femaleinfected)
  totalmalecount <- nrow(totalmale)
  totalfemalecount <- nrow(totalfemale)
  print(paste("Total number of men screened:",totalmalecount))
  print(paste("Total number of men screened that were infected:",maleinfectedcount))
  print(paste("Total number of women screened:",totalfemalecount))
  print(paste("Total number of women screened that were infected:",femaleinfectedcount))
  
  #plotting distribution of age of the infected using a histogram
  #it is cut off at 115 because there were ages in the dataset that were extremely high (erroneous?)
  library(ggplot2)
  ggplot(infected, aes(x=infected$age)) + geom_histogram()+ xlim(0, 115)
}

#This function creates a histogram that looks at the infected cases by day of year with different colors for each country
countryoforigin <- function(){
  library(ggplot2)
  ggplot(infected, aes(x = infected$dayofYear, fill = infected$country)) + 
  geom_histogram(alpha = 0.2, bins = 50)
}

#This function creates histograms for each marker to visualize how present it is by country
markers <- function(){
  library(ggplot2)
  ggplot(infected, aes(x = infected$marker01, fill = infected$country)) + 
  geom_histogram(alpha = 0.2, bins = 50)
  
  ggplot(infected, aes(x = infected$marker02, fill = infected$country)) + 
  geom_histogram(alpha = 0.2, bins = 50)
  
  ggplot(infected, aes(x = infected$marker03, fill = infected$country)) + 
  geom_histogram(alpha = 0.2, bins = 50)
  
  ggplot(infected, aes(x = infected$marker04, fill = infected$country)) + 
  geom_histogram(alpha = 0.2, bins = 50)
  
  ggplot(infected, aes(x = infected$marker05, fill = infected$country)) + 
  geom_histogram(alpha = 0.2, bins = 50)
  
  ggplot(infected, aes(x = infected$marker06, fill = infected$country)) + 
  geom_histogram(alpha = 0.2, bins = 50)
  
  ggplot(infected, aes(x = infected$marker07, fill = infected$country)) + 
  geom_histogram(alpha = 0.2, bins = 50)
  
  ggplot(infected, aes(x = infected$marker08, fill = infected$country)) + 
  geom_histogram(alpha = 0.2, bins = 50)
  
  ggplot(infected, aes(x = infected$marker09, fill = infected$country)) + 
  geom_histogram(alpha = 0.2, bins = 50)
  
  ggplot(infected, aes(x = infected$marker10, fill = infected$country)) + 
  geom_histogram(alpha = 0.2, bins = 50)
}

