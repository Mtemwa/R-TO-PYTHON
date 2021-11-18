##Kenya Administrative boundaries
rm(list=ls(all=TRUE))  # clear environment

library(sp)            #package to assist with polygon data
library(raster)
library(ggplot2)       #package to assist with plotting
library(openxlsx)          #package to assist with MS Excel

setwd("C:/Users/HP/OneDrive/SFEA/Student Program/Internship Program(Tasks and Activities)/Outputs(from students)/Mapping Codes/Mappingtest/")

mapsfolder <- "C:/Users/HP/OneDrive/SFEA/Student Program/Internship Program(Tasks and Activities)/Outputs(from students)/Mapping Codes/Mappingtest/mapsfolder/"  #Define where the map ".rds" file is stored

getData(name = "GADM",download = TRUE,path = mapsfolder,country = "KEN", level = 0)
getData(name = "GADM",download = TRUE,path = mapsfolder,country = "KEN", level = 1)
getData(name = "GADM",download = TRUE,path = mapsfolder,country = "KEN", level = 2)
getData(name = "GADM",download = TRUE,path = mapsfolder,country = "KEN", level = 3)

#Country Data ------
country.boundaries <- readRDS(paste0(mapsfolder,"/gadm36_KEN_0_sp.rds")) #read in the zero administrative boundaries (country)
country.data <- slot(country.boundaries,"data")
country <- country.data$NAME_0

#counties Data ------
county.boundaries <- readRDS(paste0(mapsfolder,"/gadm36_KEN_1_sp.rds")) #read in the first administrative boundaries (counties)
county.data <- slot(county.boundaries,"data")
counties <- county.boundaries$NAME_1

#Constituency Data ------
constituency.boundaries <- readRDS(paste0(mapsfolder,"/gadm36_KEN_2_sp.rds")) #read in the first administrative boundaries (counties)
constituency.data <- slot(constituency.boundaries,"data")
constituencies <- constituency.data$NAME_2

#ward Data------
ward.boundaries <- readRDS(paste0(mapsfolder,"/gadm36_KEN_3_sp.rds")) #read in the second administrative boundaries (ward)
ward.data <- slot(ward.boundaries,"data")
wards <- ward.data$NAME_3


#Plot Country ----
png(paste0(mapsfolder,"KenyaCountry.png"), width = 12, height = 12, units = 'in',res=600)
plot(country.boundaries, col = "green", border = 1)
legend("bottomright", legend = country,
       fill = c("green"),title = "Kenya Map",cex=.75)
dev.off()

#Plot Counties ----
png(paste0(mapsfolder,"KenyaCounties.png"), width = 12, height = 12, units = 'in',res=600)
myColours.Counties <- sample(colors(),length(counties))
plot(county.boundaries, col = myColours.Counties, border = 1)
legend("bottomright", legend = counties,
       fill = myColours.Counties,title = "Kenya counties",cex=.75)
dev.off()

#Plot constituencies ----
png(paste0(mapsfolder,"KenyaConstituencies.png"), width = 12, height = 12, units = 'in',res=600)
myColours.constituencies <- rep(x = myColours.Counties,rle(constituency.data$NAME_1)$lengths)
plot(constituency.boundaries, col = myColours.constituencies, border = 1)
legend("bottomright", legend = counties,
       fill = myColours.Counties,title = "Kenya counties",cex=.75)
dev.off()

png(paste0(mapsfolder,"KenyaWards.png"),width = 12,height = 12, units = "in", res = 600)
myColours.wards <- rep(x = myColours.Counties, rle(ward.data$NAME_1)$lengths)
plot(ward.boundaries,col= myColours.wards, border= 1)
legend("bottomright", legend = counties,
       fill = myColours.Counties, title =  "Kenya wards", cex = .75)
dev.off()

#Determine Country Outside Polygon GPS Coordinates ------
country.GPS <- data.frame()
for(i in 1:length(country.boundaries@polygons)){
  polygons.GPS <- data.frame()
  for(j in 1:length(country.boundaries@polygons[[i]]@Polygons)){
    polygonGPS <- country.boundaries@polygons[[i]]@Polygons[[j]]@coords
    colnames(polygonGPS) <- c("Longitude","Latitude")
    Country <- rep("Kenya",dim(polygonGPS)[1])
    CountryPolygon <- rep(paste0("Kenya",i,"Polygon",j),dim(polygonGPS)[1])
    polygon.GPS <- data.frame(polygonGPS,Country,CountryPolygon)
    polygons.GPS <- rbind(polygons.GPS,polygon.GPS)
    print(c("i",i,"j",j))
  }
  country.GPS <- rbind(country.GPS,polygons.GPS)
}
save(country.GPS, file = paste0(mapsfolder,"/country.GPS.RData"))

#Determine 10km Pixels In Each Country Pixel ------
load(file = paste0(mapsfolder,"ARC210kmPixels.RData"))
the.grid$PixelID <- as.character(the.grid$PixelID)
the.grid$Latitude <- as.numeric(as.character(the.grid$Latitude))
the.grid$Longitude <- as.numeric(as.character(the.grid$Longitude))
the.grid$PixelName <- as.character(the.grid$PixelName)

Country <- rep(NA,dim(the.grid)[1])
countrypolygons <- as.character(unique(country.GPS$CountryPolygon))
for(countrypolygon in countrypolygons){
  countrypolygonGPS <- country.GPS[which(country.GPS$CountryPolygon == countrypolygon),]
  countrypoints <- point.in.polygon(the.grid$Longitude,the.grid$Latitude,countrypolygonGPS$Longitude,countrypolygonGPS$Latitude)
  if(length(which(!countrypoints==0)) > 0){
    Country[which(!countrypoints==0)] <- countrypolygon
    print(paste0(countrypolygon," ",length(which(!countrypoints==0))))
  }
}

Country[grep("Kenya",Country)] <- "Kenya"

the.grid <- data.frame(the.grid,Country)

the.grid.Kenya <- the.grid[which(the.grid$Country == "Kenya"),]

#------ Plot The Country 10km Pixels ------
the.grid.Kenya[,"LongitudeMin"] <- the.grid.Kenya[,"Longitude"] - 0.05
the.grid.Kenya[,"LatitudeMin"] <- the.grid.Kenya[,"Latitude"] - 0.05
the.grid.Kenya[,"LongitudeMin1"] <- the.grid.Kenya[,"Longitude"] - 0.05
the.grid.Kenya[,"LatitudeMin1"] <- the.grid.Kenya[,"Latitude"] + 0.05
the.grid.Kenya[,"LongitudeMin2"] <- the.grid.Kenya[,"Longitude"] + 0.05
the.grid.Kenya[,"LatitudeMin2"] <- the.grid.Kenya[,"Latitude"] + 0.05
the.grid.Kenya[,"LongitudeMin3"] <- the.grid.Kenya[,"Longitude"] + 0.05
the.grid.Kenya[,"LatitudeMin3"] <- the.grid.Kenya[,"Latitude"] - 0.05
the.grid.Kenya[,"LongitudeMin0"] <- the.grid.Kenya[,"Longitude"] - 0.05
the.grid.Kenya[,"LatitudeMin0"] <- the.grid.Kenya[,"Latitude"] - 0.05

ID <- rownames(the.grid.Kenya)
square <- the.grid.Kenya[,c("LongitudeMin","LatitudeMin","LongitudeMin1","LatitudeMin1","LongitudeMin2","LatitudeMin2","LongitudeMin3","LatitudeMin3","LongitudeMin0","LatitudeMin0")]
square <- as.matrix(square)

# Create SP
polys <- SpatialPolygons(mapply(function(poly, id) {
  xy <- matrix(poly, ncol=2, byrow=TRUE)
  Polygons(list(Polygon(xy)), ID=id)
}, split(square, row(square)), ID))

# Create SPDF
polys.df <- SpatialPolygonsDataFrame(polys, data.frame(id=ID, row.names=ID))

png(paste0(mapsfolder,"KenyaCountryARC210kmGrid.png"), width = 12, height = 12, units = 'in',res=600)
plot(polys.df, col="green3",border = 0, lty = 1,lwd=0.1)
dev.off()

#Determine The polygons per county ------
county.GPS <- data.frame()
for(i in 1:length(county.boundaries@polygons)){
  polygons.GPS <- data.frame()
  for(j in 1:length(county.boundaries@polygons[[i]]@Polygons)){
    polygonGPS <- county.boundaries@polygons[[i]]@Polygons[[j]]@coords
    colnames(polygonGPS) <- c("Longitude","Latitude")
    county <- rep(counties[i],dim(polygonGPS)[1])
    countyPolygon <- rep(paste0(counties[i],i,"Polygon",j),dim(polygonGPS)[1])
    polygon.GPS <- data.frame(polygonGPS,county,countyPolygon)
    polygons.GPS <- rbind(polygons.GPS,polygon.GPS)
    print(c("i",i,"j",j))
  }
  county.GPS <- rbind(county.GPS,polygons.GPS)
}
save(county.GPS, file = paste0(mapsfolder,"/county.GPS.RData"))

county <- rep(NA,dim(the.grid.Kenya)[1])
countypolygons <- as.character(unique(county.GPS$countyPolygon))
for(countypolygon in countypolygons){
  countypolygonGPS <- county.GPS[which(county.GPS$countyPolygon == countypolygon),]
  countypoints <- point.in.polygon(the.grid.Kenya$Longitude,the.grid.Kenya$Latitude,countypolygonGPS$Longitude,countypolygonGPS$Latitude)
  if(length(which(!countypoints==0)) > 0){
    county[which(!countypoints==0)] <- countypolygon
    print(paste0(countypolygon," ",length(which(!countypoints==0))))
  }
}


county <- as.vector(county.GPS$county[match(county,county.GPS$countyPolygon)])

the.grid.Kenya <- data.frame(the.grid.Kenya,county)
save(the.grid.Kenya,file = paste0(mapsfolder,"ARC210kmPixels(Kenya).RData"))

tocolor <- rep(x = NA,dim(the.grid.Kenya)[1])

for(county in counties){
  if(length(which(the.grid.Kenya$county == county)) > 0){
    tocolor[which(the.grid.Kenya$county == county)] <- myColours.Counties[which(counties == county)]
  }
  
}

ID <- rownames(the.grid.Kenya)
square <- the.grid.Kenya[,c("LongitudeMin","LatitudeMin","LongitudeMin1","LatitudeMin1","LongitudeMin2","LatitudeMin2","LongitudeMin3","LatitudeMin3","LongitudeMin0","LatitudeMin0")]
square <- as.matrix(square)

# Create SP
polys <- SpatialPolygons(mapply(function(poly, id) {
  xy <- matrix(poly, ncol=2, byrow=TRUE)
  Polygons(list(Polygon(xy)), ID=id)
}, split(square, row(square)), ID))

# Create SPDF
polys.df <- SpatialPolygonsDataFrame(polys, data.frame(id=ID, row.names=ID))

png(paste0(mapsfolder,"KenyacountyARC210kmGrid.png"), width = 12, height = 12, units = 'in',res=600)
plot(polys.df, col=tocolor,border = 0, lty = 1,lwd=0.1)
dev.off()


#Determine The Pixels Per constituency ------
constituency.GPS <- data.frame()
for(i in 1:length(constituency.boundaries@polygons)){
  polygons.GPS <- data.frame()
  for(j in 1:length(constituency.boundaries@polygons[[i]]@Polygons)){
    polygonGPS <- constituency.boundaries@polygons[[i]]@Polygons[[j]]@coords
    colnames(polygonGPS) <- c("Longitude","Latitude")
    constituency <- rep(constituencies[i],dim(polygonGPS)[1])
    constituencyPolygon <- rep(paste0(constituencies[i],i,"Polygon",j),dim(polygonGPS)[1])
    polygon.GPS <- data.frame(polygonGPS,constituency,constituencyPolygon)
    polygons.GPS <- rbind(polygons.GPS,polygon.GPS)
    print(c("i",i,"j",j))
  }
  constituency.GPS <- rbind(constituency.GPS,polygons.GPS)
}
save(constituency.GPS, file = paste0(mapsfolder,"/constituency.GPS.RData"))

constituency <- rep(NA,dim(the.grid.Kenya)[1])
constituencypolygons <- as.character(unique(constituency.GPS$constituencyPolygon))
for(constituencypolygon in constituencypolygons){
  constituencypolygonGPS <- constituency.GPS[which(constituency.GPS$constituencyPolygon == constituencypolygon),]
  constituencypoints <- point.in.polygon(the.grid.Kenya$Longitude,the.grid.Kenya$Latitude,constituencypolygonGPS$Longitude,constituencypolygonGPS$Latitude)
  if(length(which(!constituencypoints==0)) > 0){
    constituency[which(!constituencypoints==0)] <- constituencypolygon
    print(paste0(constituencypolygon," ",length(which(!constituencypoints==0))))
  }
}


constituency <- as.vector(constituency.GPS$constituency[match(constituency,constituency.GPS$constituencyPolygon)])

the.grid.Kenya <- data.frame(the.grid.Kenya,constituency)
save(the.grid.Kenya,file = paste0(mapsfolder,"ARC210kmPixels(Kenya).RData"))

tocolor <- rep(x = NA,dim(the.grid.Kenya)[1])
tocolor.constituencies <- sample(colors(),length(constituencies))
for(constituency in constituencies){
  if(length(which(the.grid.Kenya$constituency == constituency)) > 0){
    tocolor[which(the.grid.Kenya$constituency == constituency)] <- tocolor.constituencies[which(constituencies == constituency)]
  }
  
}

ID <- rownames(the.grid.Kenya)
square <- the.grid.Kenya[,c("LongitudeMin","LatitudeMin","LongitudeMin1","LatitudeMin1","LongitudeMin2","LatitudeMin2","LongitudeMin3","LatitudeMin3","LongitudeMin0","LatitudeMin0")]
square <- as.matrix(square)

# Create SP
polys <- SpatialPolygons(mapply(function(poly, id) {
  xy <- matrix(poly, ncol=2, byrow=TRUE)
  Polygons(list(Polygon(xy)), ID=id)
}, split(square, row(square)), ID))

# Create SPDF
polys.df <- SpatialPolygonsDataFrame(polys, data.frame(id=ID, row.names=ID))

png(paste0(mapsfolder,"KenyaconstituencyARC210kmGrid.png"), width = 12, height = 12, units = 'in',res=600)
plot(polys.df, col=tocolor,border = 0, lty = 1,lwd=0.1)
dev.off()


##Determine the pixels per ward----
ward.GPS <- data.frame()
for(i in 1:length(ward.boundaries@polygons)){
  polygons.GPS <- data.frame()
  for(j in 1:length(ward.boundaries@polygons[[i]]@Polygons)){
    polygonGPS <- ward.boundaries@polygons[[i]]@Polygons[[j]]@coords
    colnames(polygonGPS) <- c("Longitude","Latitude")
    ward <- rep(wards[i],dim(polygonGPS)[1])
    wardPolygon <- rep(paste0(wards[i],i,"Polygon",j),dim(polygonGPS)[1])
    polygon.GPS <- data.frame(polygonGPS,ward,wardPolygon)
    polygons.GPS <- rbind(polygons.GPS,polygon.GPS)
    print(c("i",i,"j",j))
  }
  ward.GPS <- rbind(ward.GPS,polygons.GPS)
}
save(ward.GPS, file = paste0(mapsfolder,"/ward.GPS.RData"))

ward <- rep(NA,dim(the.grid.Kenya)[1])
wardpolygons <- as.character(unique(ward.GPS$wardPolygon))
for(wardpolygon in wardpolygons){
  wardpolygonGPS <- ward.GPS[which(ward.GPS$wardPolygon == wardpolygon),]
  wardpoints <- point.in.polygon(the.grid.Kenya$Longitude,the.grid.Kenya$Latitude,wardpolygonGPS$Longitude,wardpolygonGPS$Latitude)
  if(length(which(!wardpoints==0)) > 0){
    ward[which(!wardpoints==0)] <- wardpolygon
    print(paste0(wardpolygon," ",length(which(!wardpoints==0))))
  }
}


ward <- as.vector(ward.GPS$ward[match(ward,ward.GPS$wardPolygon)])

the.grid.Kenya <- data.frame(the.grid.Kenya,ward)
save(the.grid.Kenya,file = paste0(mapsfolder,"ARC210kmPixels(Kenya).RData"))

tocolor <- rep(x = NA,dim(the.grid.Kenya)[1])
tocolor.wards <- sample(colors(),length(wards), replace = TRUE)
for(ward in wards){
  if(length(which(the.grid.Kenya$ward == ward)) > 0){
    tocolor[which(the.grid.Kenya$ward == ward)] <- tocolor.wards[which(wards == ward)]
  }
  
}

ID <- rownames(the.grid.Kenya)
square <- the.grid.Kenya[,c("LongitudeMin","LatitudeMin","LongitudeMin1","LatitudeMin1","LongitudeMin2","LatitudeMin2","LongitudeMin3","LatitudeMin3","LongitudeMin0","LatitudeMin0")]
square <- as.matrix(square)

# Create SP
polys <- SpatialPolygons(mapply(function(poly, id) {
  xy <- matrix(poly, ncol=2, byrow=TRUE)
  Polygons(list(Polygon(xy)), ID=id)
}, split(square, row(square)), ID))

# Create SPDF
polys.df <- SpatialPolygonsDataFrame(polys, data.frame(id=ID, row.names=ID))

png(paste0(mapsfolder,"KenyawardARC210kmGrid.png"), width = 12, height = 12, units = 'in',res=600)
plot(polys.df, col=tocolor,border = 0, lty = 1,lwd=0.1)
dev.off()
