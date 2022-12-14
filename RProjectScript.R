#Function:Convert all files in a directory with space or tab-deliminated files into comma-separated files

  csvconvert<-function(){
  v1<-list.files(pattern=".txt")
  for (i in v1){
  v2<-read.table(file=i,sep=" ",header=TRUE)
     newname<-sub("txt","csv",i) 
  write.csv(x=v2, file=newname, row.names=FALSE)   
  }
  }
csvconvert()

#does this actually go through all the file in the directory?
#how do I retain the file name while only changing the file type 
use if-else statement do not need else jsut operate on the csv files 
list.files will help idnetify a pattern and then it will give you a vector of file names that you have to operate on 
screen120.txt.csv 

gsub(x=1, )