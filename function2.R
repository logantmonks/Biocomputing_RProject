# compilation function
# usage: compileData(dirPath, NAmode, country)
  # dirPath: string, path to desired directory for function application
  # NAmode: string, accepts "remove" "warn" or "include" commands, otherwise returns an error
  # country: string, country name

compileData=function(dirPath, NAmode="remove", country){
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
  
  return (allData)
}

directory="/Users/annamccartan/Library/CloudStorage/GoogleDrive-annamac2021@gmail.com/My Drive/School/Fall 2022/Biocomputing/unedited_Rproj/countryX"
allData=compileData(dirPath=directory, NAmode="remove", country="X")
# make allData dataframe a .csv file
write.csv(x=allData, file="/Users/annamccartan/Library/CloudStorage/GoogleDrive-annamac2021@gmail.com/My Drive/School/Fall 2022/Biocomputing/Biocomputing_RProject-anna\\allData.csv")
                