#source("~/biocomputing/Biocomputing_RProject/Rproject2022/supportingFunctions.R")
#txt_to_csv()
#compile_files()
#summary()


data = read.csv("~/biocomputing/Biocomputing_RProject/Rproject2022/allData.csv")
print("test")
ggplot() +
  geom_bar(data = data, aes(x = age)) +
  xlim(0,100)