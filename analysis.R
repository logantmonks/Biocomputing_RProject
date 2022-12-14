# read in functions defined in supportingFunctions.R
functionPath="/Users/annamccartan/Library/CloudStorage/GoogleDrive-annamac2021@gmail.com/My Drive/School/Fall 2022/Biocomputing/R/Biocomputing_RProject/Rproject2022/supportingFunctions.R"
source(functionPath)

# use csvConvert to convert country Y files to .csv
Ypath="/Users/annamccartan/Library/CloudStorage/GoogleDrive-annamac2021@gmail.com/My Drive/School/Fall 2022/Biocomputing/R/Biocomputing_RProject/Rproject2022/countryY"
setwd(Ypath)
csvConvert()

# use compileData to combine all country X files into one, all country Y files into one
Xpath="/Users/annamccartan/Library/CloudStorage/GoogleDrive-annamac2021@gmail.com/My Drive/School/Fall 2022/Biocomputing/R/Biocomputing_RProject/Rproject2022/countryX"
compileData(dirPath=Xpath, NAmode="remove", country="X", dataName="XData.csv")
Ypath="/Users/annamccartan/Library/CloudStorage/GoogleDrive-annamac2021@gmail.com/My Drive/School/Fall 2022/Biocomputing/R/Biocomputing_RProject/Rproject2022/countryY"
compileData(dirPath=Ypath, NAmode="warn", country="Y", dataName="YData.csv")
# combine country X and country Y files
setwd(Xpath)
Xdf=read.csv("XData.csv")
setwd(Ypath)
Ydf=read.csv("YData.csv")
compiledData=rbind(Xdf, Ydf)
dataPath="/Users/annamccartan/Library/CloudStorage/GoogleDrive-annamac2021@gmail.com/My Drive/School/Fall 2022/Biocomputing/R/Biocomputing_RProject/Rproject2022/"
setwd(dataPath)
write.csv(compiledData, file="compData.csv")

# use summData to process the data included in the entire data set
library(ggplot2)
summData("compData.csv")

# in which country did the disease outbreak likely begin?
    # for each country, use function 4 to plot infected patients over time
    QuestionOne(DataSet=Xdf)
    # country X's uptick in infection rate begins around day 119
    QuestionOne(DataSet=Ydf)
    # country Y's begins around day 139
    # thus, the disease outbreak likely began in Country X

# if country Y develops a vaccine, is it likely to work for country X's citizens?
    # for each country, use function 5 to plot number of each marker on last day of collection
    QuestionTwo(DataSet=Xdf, day=175)
    # country X shows high values for markers 1-5
    QuestionTwo(DataSet=Ydf, day=175)
    # country Y shows high values for markers 6-9
    # patients in each country are positive for very different markers
    # a vaccine developed in country Y is unlikely to work for country X's citizens
    