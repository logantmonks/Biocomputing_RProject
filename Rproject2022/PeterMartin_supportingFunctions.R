# PeterMartin_supportingFunctions.R

setwd('~/Desktop/Rfiles/Biocomputing/Biocomputing_RProject/Rproject2022')

txt.to.csv<-function(file_name,path_to_file=getwd()){
  path_to_file<-paste(path_to_file,"/",sep = "")
  for(i in file_name){
    table<-read.table(file = paste(path_to_file,i,sep = ""),header = TRUE,sep = " ")
    i<-gsub(".txt",".csv",i)
    write.table(table,file=i,sep = ",",row.names = FALSE)
  }
}

compile.csv<-function(file_name,path_to_file=getwd(),nPaths=1,na.rm="warn"){
  for(i in 1:nPaths){
    path_to_file[i]<-paste(path_to_file[i],"/",sep = "")
    for(j in 1:length(file_name)){
      table<-read.csv(paste(path_to_file[i],file_name[j],sep = ""),header = TRUE,stringsAsFactors = TRUE)
      table$country<-substr(path_to_file[i],nchar(path_to_file[i])-1,nchar(path_to_file[i])-1)
      table$dayofYear<-as.numeric(substr(file_name[j],nchar(file_name[j])-6,nchar(file_name[j])-4))
      for(l in 1:nrow(table)){
        if(anyNA(table[l,])==TRUE){
          if(na.rm == "warn"){
            print(paste0("Warning: there is one or more missing values in row: ",l," of ",file_name[j]))
          }else if(na.rm == "yes"){
            table<-table[-l,]
          }
        }
      }
      if(j==1){
        compileDF<-table
      }else{
        compileDF<-rbind(compileDF,table)
      }
    }
    if(i==1){
      compileData<-compileDF
    }else{
      compileData<-rbind(compileData,compileDF)
    }
  }
  write.table(compileData,file="compileData.csv",sep = ",",row.names = FALSE)
}