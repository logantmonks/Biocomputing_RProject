#Biocomputing R Project Supporting Functions - Sara Bennett & Jonathan Gilman

#load in ggplot library
library(ggplot2)

#Convert TXT to CSV (only need to do for Country Y)
makeCSV <- function(dir) {
  #separate out txt files and create for loop to loop through each file and change to csv
  justTXT <- list.files(dir,pattern=".txt")
  for (i in justTXT) {
    location <- file.path(dir,i) # location of txt files
    new <- sub(".txt",".csv",i)
    newLoc <- file.path(dir,new) #where to save CSVs
    data <- read.table(file=location, header=TRUE, sep="")
    write.csv(data,file=newLoc,row.names=FALSE,col.names=FALSE)
  }
}

#Make one master CSV with all country data
masterData <- function(dir1,dir2) {
  path_init <- file.path("countryX","screen_120.csv")
  data_init <- read.csv(path_init,header=TRUE)
  data_init$country = "X"
  data_init$dayofYear = "120"
  write.csv(head(data_init,n=0), file = "allData.csv", row.names=FALSE)

  #loop through Country X Files
  files_1 <- list.files(dir1,pattern=".csv")
  for (i in files_1) {
    file_path <- file.path(dir1,i)
    day_number <- substr(i,8,10)
    #country_letter <- substr(dir1,8,8)
    data <- read.csv(file=file_path, header=TRUE)
    data$country = "X"
    data$dayofYear = day_number
    write.table(data, file = "allData.csv", append=TRUE, sep=",", col.names=FALSE, row.names=FALSE)
    
  }
  
  #loop through Country Y files
  files_2 <- list.files(dir2,pattern=".csv")
  for (i in files_2) {
    file_path <- file.path(dir2,i)
    day_number <- substr(i,8,10)
    #country_letter <- substr(dir2,8,8)
    data <- read.csv(file=file_path, header=TRUE)
    data$country = "Y"
    data$dayofYear = day_number
    write.table(data, file = "allData.csv", append=TRUE, sep=",", col.names=FALSE, row.names=FALSE)
  }
}
  
#Summarize Data from Complete CSV According to Requested Breakdowns (See Below)
sumData <- function(file){
  allData <- read.table(file("allData.csv"), header = TRUE, sep = ",")

  #Number of Screens Run 
  print("# of Screens Run:")
  print(nrow(allData))

  #Percent Patient Screens Infected
  infected = 0
  for(i in 1:nrow(allData)){
    if (sum(allData[i,3:12]) > 0) {
      infected = infected + 1}
  }
  percentInfected <- (infected_count/nrow(allData))*100
  
  print("Percent Patients Infected:")
  print(percentInfected)


  #Male vs. Female Patients
  females <- allData[allData$gender=="female",]
  print("Total # of Female patients:")
  print(nrow(females))
  
  males <- allData[allData$gender=="male",]
  print("Total # of Male Patients:")
  print(nrow(males))
  
    #Infected
  infectMale = 0
  for(i in 1:nrow(males)){
    if (sum(males[i,3:12]) > 0) {
      infectMale = infectMale + 1}
  }
  print("# Male Patients Infected:")
  print(infectMale)
  
  infectFemale = 0
  for(i in 1:nrow(females)){
    if (sum(females[i,3:12]) > 0) {
      infectFemale = infectFemale + 1}
  }
  print("# Female Patients Infected:")
  print(infectFemale)
  
  
  #Age Distribution
    #Total
  print("Age Distribution of All Patients:")
  print(summary(allData$age))

    #Infected
  allData$markerSum=allData$marker01+allData$marker02+allData$marker03+allData$marker04+allData$marker05+allData$marker06+allData$marker07+allData$marker08+allData$marker09+allData$marker10
  allInfected <- allData[allData$markerSum>0,]
  print("Age Distribution of Infected Patients:")
  print(summary(allInfected$age))
  
}

#Graph Evidence 1
outbreak <- function(file){
  #Initialize new data frames to count total infections over time
  xInfect <- data.frame(matrix(NA,0,3))
  yInfect <- data.frame(matrix(NA,0,3))
  colnames(xInfect) <- c("country","dayofYear", "numberofInfections")
  colnames(yInfect) <- c("country","dayofYear", "numberofInfections")
  
  #Index all unique day of year values
  days <- unique(allData$dayofYear)
  
  #Gather Country X Infected Numbers by Day
  for(i in 1:length(days)){
    xInfect[i,1] <- "X"
    xInfect[i,2] <- days[i]
    XDay <- allData[allData$dayofYear==days[i],]
    XPatients <- XDay[XDay$country=="X",]
    XPat.num <- XPatients[(rowSums(XPatients[,3:12])>0),]
    xInfect[i,3] <- nrow(XPat.num)
  }
  
  #Gather Country Y Infected Numbers by Day
  for(i in 1:length(days)){
    yInfect[i,1] <- "Y"
    yInfect[i,2] <- days[i]
    YDay <- allData[allData$dayofYear==days[i],]
    YPatients <- YDay[YDay$country=="Y",]
    YPat.num <- YPatients[(rowSums(YPatients[,3:12])>0),]
    yInfect[i,3] <- nrow(YPat.num)
  }
  
  #Combine X and Y infections into one dataset
  allInfections <- rbind(xInfect, yInfect)
  
  #Create line plot of infections over time indexed by day of year
  #ggplot lib should have been loaded in at top of script
  ggplot(data = allInfections, aes(x = dayofYear, y = numberofInfections, group = country, color = country))+
    geom_line() +
    geom_point() +
    theme_bw() +
    xlab("Day of the Year") +
    ylab("Number of Infections") +
    #scale_fill_manual('Country', values = c("purple", "turquoise3")) +
    ggtitle("Infection Spread in Countries over Time") 
 
}

#Graph Evidence 2
markerPrevalence <- function(file){
  allData<-read.csv("allData.csv")
  #subset X and Y data
  XData <- allData[allData$country=="X",]
  YData <- allData[allData$country=="Y",]
  
  #Initialize Biomarker Counts
  X01<-0
  X02<-0
  X03<-0
  X04<-0
  X05<-0
  X06<-0
  X07<-0
  X08<-0
  X09<-0
  X10<-0
  
  Y01<-0
  Y02<-0
  Y03<-0
  Y04<-0
  Y05<-0
  Y06<-0
  Y07<-0
  Y08<-0
  Y09<-0
  Y10<-0
  
  #Count Country X Biomarkers 
  for (row in 1:nrow(XData)){
    if (XData$marker01[row] == 1){
      X01<-X01 + 1 
    }
    if (XData$marker02[row] == 1){
      X02<-X02 + 1 
    }
    if (XData$marker03[row] == 1){
      X03<-X03 + 1 
    }
    if (XData$marker04[row] == 1){
      X04<-X04 + 1 
    }
    if (XData$marker05[row] == 1){
      X05<-X05 + 1 
    }
    if (XData$marker06[row] == 1){
      X06<-X06 + 1 
    }
    if (XData$marker07[row] == 1){
      X07<-X07 + 1
    }
    if (XData$marker08[row] == 1){
      X08<-X08 + 1 
    }
    if (XData$marker09[row] == 1){
      X09<-X09 + 1 
    }
    if (XData$marker10[row] == 1){
      X10<-X10 + 1 
    }}
  
  #Count Country Y Biomarkers
  for (row in 1:nrow(YData)){
    if (YData$marker01[row] == 1){
      Y01<-Y01 + 1 
    }
    if (YData$marker02[row] == 1){
      Y02<-Y02 + 1 
    }
    if (YData$marker03[row] == 1){
      Y03<-Y03 + 1 
    }
    if (YData$marker04[row] == 1){
      Y04<-Y04 + 1 
    }
    if (YData$marker05[row] == 1){
      Y05<-Y05 + 1 
    }
    if (YData$marker06[row] == 1){
      Y06<-Y06 + 1 
    }
    if (YData$marker07[row] == 1){
      Y07<-Y07 + 1 
    }
    if (YData$marker08[row] == 1){
      Y08<-Y08 + 1 
    }
    if (YData$marker09[row] == 1){
      Y09<-Y09 + 1
    }
    if (YData$marker10[row] == 1){
      Y10<-Y10 + 1 
    }}
  
  #Combine all Country X Biomarker Counts in dataframe
  xVals <- data.frame(matrix(NA,10,3))
  colnames(xVals) <- c("country", "marker","occurrence")
  xVals[,1] <- "X"
  xVals[,2] <- c("marker01","marker02","marker03","marker04","marker05","marker06","marker07","marker08","marker09","marker10")
  xVals[,3] <- c(X01,X02,X03,X04,X05,X06,X07,X08,X09,X10)
  
  #Combine all Country Y Biomarker Counts in dataframe
  yVals <- data.frame(matrix(NA,10,3))
  colnames(yVals) <- c("country", "marker","occurrence")
  yVals[,1] <- "Y"
  yVals[,2] <- c("marker01","marker02","marker03","marker04","marker05","marker06","marker07","marker08","marker09","marker10")
  yVals[,3] <- c(Y01,Y02,Y03,Y04,Y05,Y06,Y07,Y08,Y09,Y10)
  
  #Combine X and Y Biomarker Data into one dataset
  allMarkers <- rbind(xVals, yVals)
  
  #Create Bar Graph with Direct Comparison of Country X and Y by biomarker
  ggplot(allMarkers, aes(x = marker, y = occurrence, fill = country)) +
    geom_bar(stat="identity", position = position_dodge(), alpha = 0.75) +
    theme_classic() +
    xlab("Biomarker ") +
    ylab("Presence in Screenings") +
    ggtitle("Prevalence of Biomarkers in each Country") +
    scale_fill_manual('Country', values = c("purple", "turquoise3")) +
    theme(axis.text.x = element_text(angle=65, vjust=0.6))
}

