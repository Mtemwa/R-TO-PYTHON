######      Created By:   David Muigai, David.Muigai@syngenta.com, +254735631164/+254725631164
######      Created On:   20th August 2016
######      Modified On:  22nd August 2016
######      Purpose:      A unique naming protocol for ARC2 10km Pixels
######                    ARC2 10km(0.1 degrees) Latitude steps are 801 (-40,40)
######                    ARC2 10km(0.1 degrees) Longitude steps are 751 (-20,55)
######      Requirements  R Base 

rm(list=ls(all=TRUE))   #Clear R Global Environment

setwd("C:/Users/emman/Desktop/ARC210km/")

#Determine The Steps Sequence from the Geographical Coverage of the ARC2 dataset
start.lat <- -40.1      #Define the least latitude range minus one step
start.lon <-  -20.1     #Define the west most longitude range minus one step
end.lat <- 40.0           #Define the maximum latitude range
end.lon <- 55.0           #Define the east most longitude
pixel.size.lat <- 0.1   #Define the latitude step
pixel.size.lon <- 0.1   #Define the longitude step

round.size<-3           #Define rounding digits
#round.size<-1

steps.lat <- (end.lat-start.lat)/pixel.size.lat     #Determine the number of latitude steps
steps.lon <- (end.lon-start.lon)/pixel.size.lon     #Determine the number of latitude steps

#Define an empty matrix to store the details of each pixel centre point
the.grid <- matrix(data=NA,nrow = steps.lat*steps.lon,ncol=3)   #define matrix to store all details
colnames(the.grid)<-c("PixelID","Latitude","Longitude")  #rename the column titles

#Loop over longitude steps then latitude steps
count<-1 #Give the count of the steps a default value of zero
for (lat.step in 1 : steps.lat){ #begin loop over latitude
  for(lon.step in 1:steps.lon){  #begin loop over latitude
    lat <- round(start.lat + (lat.step * pixel.size.lat), round.size) #determine current step centre point latitude
    lon <- round(start.lon + (lon.step * pixel.size.lon), round.size) #determine current step centre point longitude
    pixelid <- paste(lat,lon,sep=",") #create the pixelid which is a combination of centre lat and centre lon
    the.grid[count,] <- c(pixelid,lat,lon) #fill in the the.grid with the above info
    print(c(pixelid,lat,lon)) #print out just to check the progress
    count<-count+1 #count the step up in progress to 576000
  } #end lon step
} # end lat step

rownames(the.grid) <- the.grid[,"PixelID"] #rename the rownames to the unique 


#Come up with a unique naming criteria eg Pixel001001 represents the first latitude step and first longitude step
#                                         Pixel009700 represents the 9th latitude step and 700th longitude step

#Determine The Latitude Steps
latitude <- seq(1,801,1)
latitude[which(nchar(latitude)==1)] <- paste0("00",latitude[which(nchar(latitude)==1)])
latitude[which(nchar(latitude)==2)] <- paste0("0",latitude[which(nchar(latitude)==2)])

#Determine The Longitude Steps
longitude <- seq(1,751,1)
longitude[which(nchar(longitude)==1)] <- paste0("00",longitude[which(nchar(longitude)==1)])
longitude[which(nchar(longitude)==2)] <- paste0("0",longitude[which(nchar(longitude)==2)])

#Combine To Obtain The Unique Names
pixelnames <- paste0(latitude,sort(rep(longitude,length(latitude))))
pixelnames <- sort(pixelnames)
pixelnames <- paste0("Pixel",pixelnames)

#Combine The Names To The Unique Center Points
the.grid <- data.frame(the.grid,pixelnames)
colnames(the.grid) <- c("PixelID","Latitude","Longitude","PixelName")
save(the.grid, file = "ARC210kmPixels.RData")   #save the file

#Save The Dolyleap File
dolyleap <- setNames(as.data.frame(rep(gsub(pattern = " ",replacement = "",x = format(seq(as.Date("20160101","%Y%m%d"),as.Date("20161231","%Y%m%d"),1),"%e-%b")),2)),"date")
save(dolyleap, file = "dolyleap.RData")
