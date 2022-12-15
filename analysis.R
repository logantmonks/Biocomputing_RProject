# Troy McFarland
# Intro to Biocomputing Final
# analysis.R

# initial wd
# setwd("~/Documents/gitrepos/Biocomputing_RProject")

# source custom functions into working environment
source("supportingFunctions.R")

# convert txt to csv in countryY folder
txt2csv(wd = "~/Documents/gitrepos/Biocomputing_RProject/Rproject2022/countryY")
# compile csv from respective folders into one file
compile.csv("Y", wd = "~/Documents/gitrepos/Biocomputing_RProject/Rproject2022/countryY")
compile.csv("X", wd = "~/Documents/gitrepos/Biocomputing_RProject/Rproject2022/countryX")
# combine resulting files into new .csv "allData2.csv"
allData2 <- rbind(read.csv("Y.csv"),read.csv("X.csv"))
write.csv(allData2, file = "allData2.csv")

# generate summary
overview("allData2.csv")

# pasted output from this function:
# [1] "Total infected: 22557"
# [1] "Total screened: 39888"
# [1] "Females infected vs. screened: 11265 / 19896"
# [1] "or 56.6194209891435%"
# [1] "Males infected vs. screened: 11292 / 19992"
# [1] "or 56.4825930372149%"
# [1] "Check your working space for plot outputs."

# plots are also generated and saved in the current working directory.

# 1. The disease began in Country X. Plot "IncidenceOverDoY.png" clearly shows that
# incidences of the disease began in Country X nearly 20 days before incidences were first recorded
# in Country Y.
# 
# 2. Plot "MarkerIncidence.png" shows that markers 1-5 are overwhelmingly present in Country X
# incidences, while markers 6-10 are more commonly present in Country Y incidences.
# Because of this difference, and our knowledge that the presence of absence
# of these markers might indicate different immunal responses from screened persons,
# it may be likely that a vaccine developed by Country Y to treat its own people would
# be less effective for citizens of Country X.