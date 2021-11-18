rm(list=ls(all=TRUE))

library(geosphere)

#set path here
setwd("C:/Users/HP/OneDrive/SFEA/Student Program/Internship Program(Tasks and Activities)/Outputs(from students)/Mapping Codes/Mappingtest/")

farmers<-read.csv("Farmers.csv",header=TRUE,skip=0,row.names=1)
stations<-read.csv("RG GPS.csv",header=TRUE,skip=0,row.names=1)

station.allocation<-matrix(NA,nrow=dim(farmers)[1],ncol=dim(stations)[1]+1)
colnames(station.allocation)<-c(rownames(stations),"Closest")
rownames(station.allocation)<-rownames(farmers)

for(station in 1:nrow(stations)){
  station.allocation[,station]<-distHaversine(farmers[,c(2,1)],stations[station,],r=6378137)
  print(station)
}

for(farmer in 1:nrow(station.allocation)){
  station.allocation[farmer,"Closest"]<-rownames(stations)[as.vector(which.min(station.allocation[farmer,]))]
}

write.csv(station.allocation,"StationAllocated.csv")
