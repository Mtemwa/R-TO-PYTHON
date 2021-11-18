######      Eidted By:    Junior Mtemwa, 0708482519
######      Modified On:   17th September 2020
######      Purpose:      To Download The Entire NOAA ARC2 dataset in ".gz" format
######                    To Extract the ".gz" to the binary file
######      Requires:     Preload the packages
######                    Internet Connection

rm(list=ls(all=TRUE)) #clear the environment
system.time({
  
library(RCurl)      #load package to assist in ftp download
library(R.utils)    #load package to assist unzip ".gz" files
library(zoo)        #load package to assist with dates

options(download.file.method = "auto") #adjust options for "ftp" download
 #adjust options for "ftp" download


setwd("C:/Users/emman/Desktop/ARC2gzfiles/csv")


gzfiles <- "C:/Users/emman/Desktop/ARC2gzfiles/csv"

date.range <- seq(as.Date("2017/01/01"), as.Date("2017/01/18"), by="day")#setting the range of dates to be downloaded.
bin.dates <- paste0("daily_clim.bin.",format(date.range,"%Y%m%d"),".gz")
prebinaddress<-"ftp://ftp.cpc.ncep.noaa.gov/fews/fewsdata/africa/arc2/bin/"

#Download the ".gz" file from the noaa ARC2 ftp ------

for (bin.date in bin.dates){
  postbinaddress<-paste0(prebinaddress,bin.date)
  download.file(postbinaddress,paste0(gzfiles,bin.date),quiet = TRUE)
  print(postbinaddress)
}


#Extract the ".gz" file ----
for (bin.date in bin.dates){
  gunzip(paste0(gzfiles,bin.date),overwrite=TRUE,remove=TRUE)
  print(bin.date)
}

#Check For Files Smaller Than 2.4 MB & Redownload------
setwd(gzfiles)

bin.dates <- paste0(list.files()[which(file.size(list.files()) < 2000000)])

if(length(which(file.size(list.files(gzfiles)) < 2000000)) > 0){
  for (bin.date in bin.dates){
    prebinaddress<-"ftp://ftp.cpc.ncep.noaa.gov/fews/fewsdata/africa/arc2/bin/"
    postbinaddress<-paste0(prebinaddress,bin.date)
    download.file(postbinaddress,paste0(gzfiles,bin.date),quiet = TRUE)
    print(postbinaddress)
  }
  
  for (bin.date in bin.dates){
    gunzip(paste0(gzfiles,bin.date),overwrite=TRUE,remove=TRUE)
    print(bin.date)
  }
}

})
