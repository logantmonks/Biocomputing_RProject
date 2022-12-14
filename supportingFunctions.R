# Supporting Functions

# Function 1: convert tab-delimited files to comma-separated files
# usage: csvConvert(dirPath=directory)
    # directory="/Users/annamccartan/Library/CloudStorage/GoogleDrive-annamac2021@gmail.com/My Drive/School/Fall 2022/Biocomputing/R/Biocomputing_RProject/Rproject2022/countryY"
# dirPath: string, path to directory of .txt files to convert
csvConvert<-function(){
  # identify .txt files in directory
  v1<-list.files(pattern=".txt")
  for (i in v1){
    # convert each file to dataframe
    v2<-read.table(file=i,sep=" ",header=TRUE)
    # rename each dataframe
    newname<-sub("txt","csv",i)
    # convert each dataframe to .csv
    write.csv(x=v2, file=newname, row.names=FALSE)   
  }
}

# Function 2: compile .csv files in a directory
# usage: compileData(dirPath=directory, NAmode="remove", country="X", dataName="XData")
      # directory="/Users/annamccartan/Library/CloudStorage/GoogleDrive-annamac2021@gmail.com/My Drive/School/Fall 2022/Biocomputing/R/Biocomputing_RProject/Rproject2022/countryX"
# NAmode: string, accepts "remove" "warn" or "include" commands, otherwise returns an error
# country: string, country name
# dataName: string, desired filename for compiled .csv file
compileData<-function(dirPath, NAmode="remove", country, dataName){
  # set working directory
  setwd(dirPath)
  # set variables
  # fileNames: a list of all .csv files in the directory
  fileNames=list.files(path=dirPath, pattern=".csv")
  # country: transfer user input to countryVar
  countryVar=country
  # doy: extract day of year from list of file names
  doy=sub("screen_","",fileNames)
  doy=sub(".csv","",doy)
  # create empty dataframe
  initialData=data.frame()
  
  for(i in 1:length(fileNames)){
    # import .csv file as new dataframe
    newData=read.csv(fileNames[i],header=TRUE)
    # initial dataframe contains previously compiled data
    df1=initialData
    # new dataframe contains new data
    df2=newData
    # add country column to new data
    df2$country=(countryVar)
    # add dayOfYear column to new data
    df2$dayOfYear=(doy[i])
    # bind new dataframe with previous
    allData=rbind(df1, df2)
    # update initial dataframe to reflect most recent addition
    initialData=allData
  }
  
  # respond to user input
  if (NAmode=="remove"){
    for (r in 1:nrow(allData)){
      # search each row for presence of NA values
      if (anyNA(allData[r,])){
        # remove rows containing NAs
        allData=allData[(-r),]
      }
    }
  }
  else if (NAmode=="warn"){
    # print warning message
    print("Warning: NAs are included in compiled data.")
  }
  else if (NAmode=="include"){
    # no change
  }
  else {
    # return error
    print("Invalid input for NAMode")
  }
  # make allData dataframe a .csv file
  write.csv(x=allData, file=dataName)
}

# Function 3: summarize compiled data
# usage: summData(compData=dataPath)
# dataPath: path to compiled .csv data file
summData <- function(compData){
  # create dataframe from dataset
  DataSet<-read.table(compData, header=TRUE, sep=",", stringsAsFactors=FALSE)
  
  # number of screens run is equal to the number of rows (patients) in the DataSet
  NumberOfScreens <- nrow(DataSet)
  print("Number of Screens: ")
  print(NumberOfScreens)
  
  # percent of patients screened that were infected (have at least one marker)
  # variables for infected and noninfected patients
  Infected<-0
  NotInfected<-0
  # count total number of infected and noninfected patients
  for(i in 1:nrow(DataSet)){
    SumRow<-sum(DataSet$marker01[i], DataSet$marker02[i],DataSet$marker03[i],DataSet$marker04[i], DataSet$marker05[i],DataSet$marker06[i],DataSet$marker07[i], DataSet$marker08[i],DataSet$marker09[i],DataSet$marker10[i])
    if(SumRow>0){
      Infected<-Infected+1
    }else{
      if(SumRow==0)
        NotInfected<-NotInfected+1
    }
  } 
  
  # percent of population infected = infected patients/total patients screened
  PercentInfected<-Infected/NumberOfScreens
  print("Proportion of infected patients:")
  print(PercentInfected)
  
  # Мale vs. Female infected patient percentages
  # create variables for infected/noninfected male patients
  MaleInfected<-0
  MaleNotInfected<-0
  # add up total number of male infected/noninfected patients
  for(i in 1:nrow(DataSet)){
    if(DataSet$gender[i]=="male"){
      MSumRow<-sum(DataSet$marker01[i], DataSet$marker02[i],DataSet$marker03[i],DataSet$marker04[i], DataSet$marker05[i],DataSet$marker06[i],DataSet$marker07[i], DataSet$marker08[i],DataSet$marker09[i],DataSet$marker10[i])
      if(MSumRow>0){
        MaleInfected<-MaleInfected+1
      }else{
        if(MSumRow==0)
          MaleNotInfected<-MaleNotInfected+1
      }
    }
  }
  MaleTotal=(MaleInfected+MaleNotInfected)
  # Percent of Male infected = Infected Patients/Total number of male patients screened
  PercentMaleInfected<-MaleInfected/MaleTotal
  print("Percentage of infected male patients:")
  print(PercentMaleInfected)
  
  # create variables for infected/noninfected female Patients
  FemaleInfected<-0
  FemaleNotInfected<-0
  # add up total number of female infected and notinfected patients
  for(i in 1:nrow(DataSet)){
    if(DataSet$gender[i]=="female"){
      FSumRow<-sum(DataSet$marker01[i], DataSet$marker02[i],DataSet$marker03[i],DataSet$marker04[i], DataSet$marker05[i],DataSet$marker06[i],DataSet$marker07[i], DataSet$marker08[i],DataSet$marker09[i],DataSet$marker10[i])
      if(FSumRow>0){
        FemaleInfected<-FemaleInfected+1
      }else{
        if(FSumRow==0)
          FemaleNotInfected<-FemaleNotInfected+1
      }
    }
  }
  FemaleTotal=(FemaleInfected+FemaleNotInfected)
  # percent of females infected can be determined by Infected female patients/Total number of female patients screened
  PercentFemaleInfected<-FemaleInfected/FemaleTotal
  print("Percentage of infected female patients:")
  print(PercentFemaleInfected)
  
  # age distribution of all patients plotted
  ggplot(data=DataSet,aes(x=age))+
    geom_freqpoly()+
    theme_classic()+
    xlim(0,100)
  
  # create new data frame with infected patient data
  InfectedPlot=data.frame()
  for(i in 1:nrow(DataSet)){
    SumRow<-sum(DataSet$marker01[i], DataSet$marker02[i],DataSet$marker03[i],DataSet$marker04[i], DataSet$marker05[i],DataSet$marker06[i],DataSet$marker07[i], DataSet$marker08[i],DataSet$marker09[i],DataSet$marker10[i])
    if(SumRow>0){
      InfectedPlot= rbind(InfectedPlot, DataSet[i,])
    }
  }
  
  # age distribution of all infected patients plotted
  print("Plotting age distribution of infected patients:")
  ggplot(data=InfectedPlot,aes(x=age))+
    geom_freqpoly()+
    theme_classic()+
    xlim(0,100)
}

# Function 4: Plot number of infected patients over time
QuestionOne=function(DataSet){
  # create dataframe of infected patients
  InfectedPlot=data.frame()
  for(i in 1:nrow(DataSet)){
    SumRow<-sum(DataSet$marker01[i], DataSet$marker02[i],DataSet$marker03[i],DataSet$marker04[i], DataSet$marker05[i],DataSet$marker06[i],DataSet$marker07[i], DataSet$marker08[i],DataSet$marker09[i],DataSet$marker10[i])
    if(SumRow>0){
      InfectedPlot= rbind(InfectedPlot, DataSet[i,])
    }
  }
  # density of infected patients plotted over time
  ggplot(data=InfectedPlot,aes(x=dayOfYear))+
    geom_density()+
    theme_classic()+
    xlim(100,185)
}

# Function 5: Create a vector of marker value column sums for a particular day
QuestionTwo=function(DataSet, day){
  # create vector of markers
  allMarkers=c("marker 01",
               "marker 02", 
               "marker 03",
               "marker 04",
               "marker 05",
               "marker 06",
               "marker 07",
               "marker 08",
               "marker 09",
               "marker 10")
  # isolate data from desired day of collection
  ThisDay = data.frame()
  for(i in 1:nrow(DataSet)){
    if(DataSet$dayOfYear[i]==day){
      ThisDay=rbind(ThisDay, DataSet[i,])
    }
  }
  # count number of markers on this day, add to vector 
  sumVec=c(
    sum(ThisDay$marker01),
    sum(ThisDay$marker02),
    sum(ThisDay$marker03),
    sum(ThisDay$marker04),
    sum(ThisDay$marker05),
    sum(ThisDay$marker06),
    sum(ThisDay$marker07),
    sum(ThisDay$marker08),
    sum(ThisDay$marker09),
    sum(ThisDay$marker010))
  # combine markers vector and marker sum vector into dataframe
  summaryData=data.frame(allMarkers, sumVec)
  # for each marker, plot number of positives
  ggplot(summaryData, aes(x=allMarkers, y=sumVec)) +
    geom_point() + 
    xlab("Marker") + 
    ylab("Number of Positive Markers")
}




