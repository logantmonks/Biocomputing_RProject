# R Project -- Completed Final Project supportingFunctions.R

# Laurel Lown
# Matthew Hawkins
# Andrew Lupinski

##All paths are specific to personal working directory during development

# First, install necessary packages.
install.packages("dplyr")                           # Install dplyr package
install.packages("plyr")                            # Install plyr package
install.packages("readr")                           # Install readr package
install.packages("stringr")                         # Install stringr package
install.packages("ggplot2")                         # Install ggplot2 package
library(dplyr)
library(plyr)
library(readr)
library(stringr)
library(ggplot2)

# Function 1: Converts all files into .csv
## First, set working directory to where file(s) of interest are located
fileConvert<-function(filename){ # filename acts as placeholder for file of interest
  for(i in 1:length(filename)){
    DATA=read.table(file=filename[i],header=T,sep="\t")
    # setwd to desired location of converted .csv files to be saved
    ## The pathway leads to a previously created foldername "Test" where .csv files will be saves into
    write.table(DATA,file=paste0("/Users/matha/Desktop/RProject/Test",sub(".txt","",filename[i]),".csv"),row.names=F,quote=F,sep=",") 
  }
}
# To Use: fileConvert(countryYfileConvert) 
## Specify here which filename you wish to use with the function
### End of Function 1

# Function 2: Merges all .csv files of interest into a single .csv
## Includes/adds columns for Country name (X or Y) and Day of Year

# For the insertion of all files with path included
merge <- function(filename, na_handling="no warning"){
  # Specify in 'na_handling' either 'no warning', 'remove NA', or 'warn of NA presence' depending on desired outcome
  ## 'No warning' as default in function
(filesx <- fs::dir_ls("C:/Users/matha/Desktop/RProject/countryX", glob="*.csv"))
datax <<- read_csv(filesx, id="path")
datax$country <<- "X"
(filesY <- fs::dir_ls("C:/Users/matha/Desktop/RProject/countryY", glob="*.csv"))
dataY <<- read_csv(filesY, id="path")
dataY$country <<- "Y"
alldata <<- rbind(datax, dataY)
colnames <- c("gender", "age", "marker01", "marker02", "marker03", "marker04", "marker05", "marker06", "marker07", "marker08", "marker09", "marker10", "path", "country")
alldata <<- alldata[,colnames]
bigMatrix <- matrix(data=NA, nrow=0, ncol=14)
if(na_handling == "remove NA"){
  bigMatrix=na.omit(bigMatrix)
  print("Any NA Values in data removed")
}
if(na_handling == "warn of NA presence" ){
  if(any(is.na.data.frame(bigMatrix))){print("Warning : Your Data Contains NA Values")}else{
    print("No NA Values Present")
  }
}
}
# To Use: merge(rproject, na_handling="warn of NA presence") 
## Runs the second function with warning for NA presence
### End function 2

# Function 3: Summary of Data - uses 'allData.csv'
data_summary<-function(file) {
  summary_table<-read.csv(file,header=TRUE,sep=",")
  screenNum<-nrow(summary_table)
  infectedNum<-0
  totMaleNum<-0
  totFemaleNum<-0
  infectMale<-0
  infectFemale<-0
  infectedTot<-c()
  for(row in 1:screenNum){
    col<-3
    while(summary_table[row,col]!=1&col<13){
      col=col+1
    }
    if(summary_table[row,col]==1){
      infectedNum=infectedNum+1;
      if(summary_table[row,1]=="male"){
        infectMale=infectMale+1
      }
      else if(summary_table[row,1]=="female"){
        infectFemale=infectFemale+1
      }
      infectedTot<-append(infectedTot,1)
    }
    else{
      infectedTot<-append(infectedTot,0)
    }
    if (summary_table[row,1]=="male"){
      totMaleNum=totMaleNum+1
    }
    else if(summary_table[row,1]=="female"){
      totFemaleNum=totFemaleNum+1
    }
  }
  percent_infect<-infectedNum/screenNum
  summary<-sprintf("Number of Screens: %d\nPercent of Patients Screened that were Infected: %f%%\nNumber of Males Screened: %d\nNumber of Females Screened: %d\nNumber of Males Infected: %d\nNumber of Females Infected: %d\n",screenNum,percent_infect*100,totMaleNum,totFemaleNum,infectMale,infectFemale)
  writeLines(summary,"data_summary.txt") # create a summary .txt file displays all summary stats of interest
  Countries<-c(summary_table[,13]) # pulls country data from country column
  Days<-c(summary_table[,14]) # pulls Day data from dayofyear column
  firstDisease<-data.frame(Countries,infectedTot,Days)
  summary_table$csum<-ave(infectedTot,summary_table$country,FUN=cumsum)
  Age_distribution_graph<-ggplot(data=summary_table,aes(x=age))+ 
    geom_histogram(binwidth=2)+ 
    theme_classic()+ 
    ggtitle("Age Distribution")+
    xlab("Age")+ 
    ylab("Count")
  show(Age_distribution_graph)
  NumInfected_DoY<-ggplot(data=summary_table,aes(x=dayofYear,y=csum,color=as.factor(country)))+ 
    geom_line()+ 
    xlab("Day of Year")+
    ylab("Number of Infected")+ 
    ggtitle("Number of Infected v Day of Year")+ 
    theme_classic()+ 
    labs(color="Countries")
  show(NumInfected_DoY)
}

# To Use: data_summary("allData.csv")
## Output will include 'data_summary.txt' file that includes information on female v male infections,
## total number of females v males screened, percentage of individuals screened that were infected,
## and total number of screens.
### Output also includes two graphs - one of age distribution of all individuals in the data set, and
### a line graph comparing the number of records in Countries X and Y.
#### End function 3

# Function 4: Completes marker analysis comparing number of markers per country (X or Y)
markers<- function(filename){
  summary_table2 <- read.csv(filename, header=TRUE, stringsAsFactors = FALSE, sep = ",")
  markerID <- c("marker01", "marker02", "marker03", "marker04", "marker05", "marker06", "marker07", "marker08", "marker09", "marker10")
  countryxmrk<- c(0,0,0,0,0,0,0,0,0,0)
  countryymrk<- c(0,0,0,0,0,0,0,0,0,0)
  for(i in 3:12){
    for(j in 2:nrow(summary_table2)){
      if(summary_table2[j,i]=="1"){
        if(summary_table2[j,13]=="X"){
          countryxmrk[i-2]=countryxmrk[i-2]+1
        }
        else if(summary_table2[j,13]=="Y"){
          countryymrk[i-2]=countryymrk[i-2]+1
        }
      }
    }
  }
  markerfinal <- cbind( markerID, countryxmrk, countryymrk)
  markeranalysis <- write.csv(markerfinal, "markerfinal.csv")
}
# To Use: markers("allData.csv")
## Output includes summary file ('markeranalysis.csv') that contains percent prevalence of each marker
## in either Country X or Country Y
### End of function 4
