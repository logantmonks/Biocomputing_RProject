#Introduction to Biocomputing 
#R Script Project
#Written by Carolina A. Jimenez and Fabely Moreno
#12-14-22
#Script for Supporting Functions

#Set Working Directory
#setwd("C:/Users/carolina/Biocomputing_RProject/Rproject2022/")
setwd("C:/Users/fabel/Biocomputing_RProject/RProject2022/RProject2022")

convert.csv <- function(SourceAndDestinationFolder) {
  #load library with str_replace_all function
  library(stringr)
  #get list of paths of txt files in folder
  filenames <- list.files(path=SourceAndDestinationFolder,pattern='*.txt',full.names = TRUE)
  #save the filenames for manipulation
  filenamesWithTXT <- list.files(path=SourceAndDestinationFolder,pattern='*.txt',full.names = FALSE)
  #change filenames from file.txt to file.csv
  filenamesWithCSV <- rapply(as.list(filenamesWithTXT),str_replace_all,pattern=".txt",replacement=".csv",how='replace')
  #read all files into list of data frames
  ldf <- lapply(filenames,read.table)
  #rename each data frame to the filename.csv
  names(ldf) <- filenamesWithCSV
  #save each file as a CSV file without column or row names
  lapply(1:length(ldf), function(i) write.table(ldf[[i]], sep=",", file = paste(SourceAndDestinationFolder,names(ldf[i]),sep=""), row.names = FALSE, col.names = FALSE))
}

merge.all.into.one <- function(SourceAndDestinationFolder,NAOption) {
  #folder name is the country
  #screen_NNN.txt is the file name where NNN is the day of the year
  #load library with str_replace_all function
  library(stringr)
  #clear existing single.csv file before we read all csv files
  if (file.exists(paste(SourceAndDestinationFolder,"single.csv",sep=""))) {
    file.remove(paste(SourceAndDestinationFolder,"single.csv",sep=""))
  }
  #get list of paths of csv files in folder
  filenames <- list.files(path=SourceAndDestinationFolder,pattern='*.csv',full.names = TRUE)
  #save the filenames for manipulation
  filenamesWithCSV <- list.files(path=SourceAndDestinationFolder,pattern='*.csv',full.names = FALSE)
  #get day of year from the filename by removing screen_ and .csv from it
  dayOfYear <- rapply(as.list(filenamesWithCSV),str_replace_all,pattern=".csv",replacement="",how='replace')
  dayOfYear <- rapply(as.list(dayOfYear),str_replace_all,pattern="screen_",replacement="",how='replace')
  #get country from directory name
  country=basename(SourceAndDestinationFolder)
  country <- str_replace_all(country,"country","")
  #read all files into list of data frames
  ldf <- lapply(filenames,read.csv)
  #rename each data frame to the filename.csv
  names(ldf) <- filenamesWithCSV
  
  #include, warn or exclude rows with NA
  if (NAOption==1) { #keep NA
  } else if (NAOption==2) { #warn NA
    print("WARNING: records with NA values exist in the data and will be included!")
  } else if (NAOption==3) { #exclude NA
    lapply(1:length(ldf), function(i) {
      #rewrite the global ldf outside the function with the rows that have no NAs
      ldf[[i]] <<- ldf[[i]][rowSums(is.na(ldf[[i]]))==0,]
    })
  } else {
    print("invalid NA option")
  }

  #add country column to data frames
  #create empty vector of same length as data frame and then assign country to all elements
  lapply(1:length(ldf), function(i) {
    x <- character(nrow(ldf[[i]]));
    x[] <- country;
    #rewrite to the global ldf outside the function
    ldf[[i]]['country'] <<- x
  })

  #add dayOfYear column to data frames
  lapply(1:length(ldf), function(i) {
    #rewrite to the global ldf outside the function
    ldf[[i]]['dayOfYear'] <<- as.numeric(dayOfYear[[i]])
  })

  #fill with first data frame and column names
  write.table(ldf[[1]], sep=",", file = paste(SourceAndDestinationFolder,"single.csv",sep=""), append=FALSE, row.names = FALSE, col.names = TRUE)
  #append each additional csv file into a single CSV file without column or row names
  lapply(2:length(ldf), function(i) write.table(ldf[[i]], sep=",", file = paste(SourceAndDestinationFolder,"single.csv",sep=""), append=TRUE, row.names = FALSE, col.names = FALSE))
}

summarise.compiled.data <- function(SourceFolder) {
  #get list of paths of csv files in folder
  filenames <- list.files(path=SourceFolder,pattern='allData.csv',full.names = TRUE)
  #read all files into list of data frames
  ldf <- lapply(filenames,read.csv)
  print(paste("Number of screens run :",nrow(ldf[[1]])))
  print(paste("Number of female patients :",nrow((ldf[[1]][ldf[[1]]$gender=="female",]))))
  print(paste("Number of male patients :",nrow((ldf[[1]][ldf[[1]]$gender=="male",]))))
  print(paste("Number of infected patients :",nrow(ldf[[1]][rowSums(ldf[[1]][3:12])>0,])))
  z <- 100*as.integer(nrow(ldf[[1]][rowSums(ldf[[1]][3:12])>0,]))/as.integer(nrow(ldf[[1]]))
  print(paste("Percent of patients that were infected:",format(round(z,2), nsmall = 2)))
  age=ldf[[1]]$age
  h <- hist(age,xlim=c(0,250),ylim=c(0,30000),breaks = 40,main="Age distribution of patients",xlab="age in years",ylab="number of patients")
  text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))
}
