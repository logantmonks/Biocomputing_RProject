# PeterMartin_analysis.R

setwd('~/Desktop/Rfiles/Biocomputing/Biocomputing_RProject/Rproject2022')
library(ggplot2)
library(viridis)
library(wesanderson)
library(multcompView)
library(plotrix)
source("PeterMartin_supportingFunctions.R")

countryX_path<- "~/Desktop/Rfiles/Biocomputing/Biocomputing_RProject/Rproject2022/countryX"
countryY_path<- "~/Desktop/Rfiles/Biocomputing/Biocomputing_RProject/Rproject2022/countryY"

txt.to.csv(file=list.files(path = countryY_path, pattern = "txt"),path_to_file = countryY_path)

compile.csv(file_name = list.files(path = countryY_path, pattern = "csv"),
            path_to_file = c(countryX_path,countryY_path),nPaths = 2,na.rm = "warn")
## Test of na.rm argument
#compile.csv(file_name = "screen_120.csv",
            #path_to_file = getwd(),nPaths = 1,na.rm = "yes")

screenStats(file_name = "allData.csv")

allData<-read.csv("compileData.csv",header = TRUE,stringsAsFactors = TRUE)
positive_allData<-allData[allData$microsat_num>0,]

res.aov<-aov(microsat_num ~ country,data = positive_allData)
summary(res.aov)



