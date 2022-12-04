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
  for(m in 1:nrow(table)){
    compileData$microsat_num[m]<-sum(compileData[m,3:12])
  }
  write.table(compileData,file="compileData.csv",sep = ",",row.names = FALSE)
}

screenStats<-function(file_name,path_to_file=getwd()){
  compileData<-read.csv(file = paste0(path_to_file,"/",file_name),header = TRUE,stringsAsFactors = TRUE)
  n_country<-unique(compileData$country)
  run_sum<-0
  for(j in 1:nrow(compileData)){
    compileData$microsat_num[j]<-sum(compileData[j,3:12])
    if(sum(compileData[j,3:12]>0)){
      run_sum<-run_sum+1
      if(run_sum==1){
        positive_test<-paste0("Patient ",j," tested on day ",compileData$dayofYear[j]," in country ",
                              compileData$country[j]," has tested positive with ",sum(compileData[j,3:12]),
                              " microsatellite(s) detected")
      }else{
        positive_test<-rbind(positive_test,paste0("Patient ",j," tested on day ",compileData$dayofYear[j],
                                                  " in country ",compileData$country[j]," has tested positive with ",
                                                  sum(compileData[j,3:12])," microsatellite(s) detected"))
      }
    }
  }
  write.table(positive_test,file = "positive_test_info.txt",sep = " ",col.names = FALSE,row.names = FALSE)
  for(i in 1:length(n_country)){
    n_screens<-nrow(compileData[compileData$country==n_country[i],])
    country<-compileData[compileData$country==n_country[i],]
    positive_country<-country[country$microsat_num>0,]
    country_count<-nrow(country[country$microsat_num>0,])
    decimal<-(country_count/n_screens)*100
    percent<-round(decimal,digits=2)
    male<-nrow(country[country$gender=="male",])
    female<-nrow(country[country$gender=="female",])
    positive_male<-nrow(positive_country[positive_country$gender=="male",])
    positive_female<-nrow(positive_country[positive_country$gender=="female",])
    
    #dist<-ggplot(country,aes(age)) +
      #geom_histogram(binwidth = 5)
    #return(dist)
    dist<-summary(country$age)
    dist<-cbind(dist,sd(country$age))
    
    age_dist<-ggplot(country,aes(age)) +
      geom_histogram(binwidth = 5) +
      theme_bw()
    print(age_dist)
    
    print(paste0("There have been a total of ",n_screens," screenings in country ",n_country[i]))
    print(paste0(country_count," people have tested positive"))
    print(paste0(female," females have been screened, of which ",positive_female," have tested positive"))
    print(paste0(male," males have been screened, of which ",positive_male," have tested positive"))
    print(paste0("The average age screened was ",round(dist[4],digits = 2),", the minimum age was ",dist[1],
                 ", and the maximum was ",dist[6]))
    print(paste0("The standard deviation in age screened was ",dist[7]))
    print("—")
    print(paste0(percent,"% of people screened in country ",n_country[i]," have tested positive"))
    print("—")
  }
  print(paste0("Across all countries included in this study, ",nrow(positive_test)," people have tested positive"))
}