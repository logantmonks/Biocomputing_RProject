# PeterMartin_analysis.R

setwd('~/Desktop/Rfiles/Biocomputing/Biocomputing_RProject/Rproject2022')
library(ggplot2)
source("PeterMartin_supportingFunctions.R")

countryX_path<- "~/Desktop/Rfiles/Biocomputing/Biocomputing_RProject/Rproject2022/countryX"
countryY_path<- "~/Desktop/Rfiles/Biocomputing/Biocomputing_RProject/Rproject2022/countryY"

txt.to.csv(file=list.files(path = countryY_path, pattern = "txt"),path_to_file = countryY_path)

compile.csv(file_name = list.files(path = countryY_path, pattern = "csv"),
            path_to_file = c(countryX_path,countryY_path),nPaths = 2,na.rm = "warn")
## Test
#compile.csv(file_name = "screen_120.csv",
            #path_to_file = getwd(),nPaths = 1,na.rm = "warn")

screenStats(file_name = "allData.csv")

allData<-read.csv("allData.csv",header = TRUE,stringsAsFactors = TRUE)
