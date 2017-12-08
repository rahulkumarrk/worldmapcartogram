# worldmapcartogram
world map cartogram
setwd()

#install.packages("maptools")
#install.packages("rgdal")
library(maptools)
library(rgdal)

world1<-rgdal::readOGR("world-base-map.shp")
world<-rgdal::readOGR("world-base-map.shp")

library(rworldmap)
#install.packages("shapefiles")
#----------------------------------------------------------------
path <- ###
setwd(path)

gdpdata <- read.csv("gdp.csv", stringsAsFactors = F, sep = ",", na.strings = "n/a")

colnames(gdpdata)
gdp <- gdpdata

gdp[, -c(1:8)]  <- apply(gdp[, -c(1:8)], 2, as.numeric)
gdp <- data.frame(gdp)
gdp$name <- gdp$Country

gdp <- gdp[order(gdp$name),]
world@data$psl <- 1:nrow(world@data)
world@data <- world@data[order(world@data$name),]
world1@data <- world1@data[order(world1@data$name),]

gdpfnl <- merge(world@data, gdp, all = TRUE, by = "name")
#gdpfnl <- gdpfnl[, unique(names(gdpfnl))]
world@data <- gdpfnl
world@data <- world@data[, unique(names(world@data))]
world@data <- world@data[which(world@data$name%in%world1@data$name),]
names(world@data) <- strtrim(names(world@data), 10) 
world@data <- world@data[order(world@data$psl),]
#world1@data$name
#-------------------
td <- file.path(tempdir(), "rgdal_examples"); dir.create(td)
# BDR 2016-12-15 (MapInfo driver fails writing to directory with ".")
if(nchar(Sys.getenv("OSGEO4W_ROOT")) > 0) {
  OLDPWD <- getwd()
  setwd(td)
  td <- "."
}

#-------------------
writeOGR(world, td, "world", driver="ESRI Shapefile")
#-------------------------------------------------------------------
worldgdp1 <- readOGR(dsn = "C:/Users/Rahul Kumar/Desktop/scapetoad", "world1")
worldgdp2 <- readOGR(dsn = "C:/Users/Rahul Kumar/Desktop/scapetoad", "world2")

plot(worldgdp2)

mapCountryData( worldgdp2
                , nameColumnToPlot="X2016"
                , catMethod="categoricall "
                , mapTitle="gdpdata"
                  ,colourPalette="palette"
                  ,oceanCol= "lightblue"
                  ,missingCountryCol="white")


######################################################################################################
# Packages Required - 
library(sp)
library(rgeos)
library(rworldmap)
library(cartogram)
library(maptools)
library(cartogram)
library(rgdal)
data(wrld_simpl)
library(maps)
library(tmap)
library(shapefiles)
#####################################################################################################

# Note - Gross domestic product, current prices (U.S. dollars)
#Values are based upon GDP in national currency converted to U.S. dollars using market exchange rates (yearly average). 
#Exchange rate projections are provided by country economists for the group of other emerging market and developing #countries. Exchanges rates for advanced economies are established in the WEO assumptions for each WEO exercise. #Expenditure-based GDP is total final expenditures at purchasers’ prices (including the f.o.b. value of exports of goods and #services), less the f.o.b. value of imports of goods and services. [SNA 1993]

# Source of Data - https://www.imf.org/external/pubs/ft/weo/2017/01/weodata/index.aspx

path <- ###
setwd(path)

gdpdata <- read.csv("gdpd.csv", stringsAsFactors = F, sep = ",", na.strings = "n/a")

colnames(gdpdata)
gdp <- gdpdata

gdp[, -c(1:8)]  <- apply(gdp[, -c(1:8)], 2, as.numeric)
gdp <- data.frame(gdp)
########################################################################################################
##### merging with existing data
sPDF <- joinCountryData2Map(gdp,
                            joinCode = "ISO3",
                            nameJoinColumn = "ISO")

afr <- spTransform(sPDF, CRS("+init=epsg:4269"))

write.shp(shp = afr, out.name = "gdpshp")

afrc <- cartogram(afr, "X2016", itermax=30)
plot(afrc)
#plot(nc_cartogram(afr, "product1"), add = TRUE, col = 'red')

#tm_shape(afrc) + tm_fill("gdp", style="jenks") + tm_borders() + tm_layout(frame=T)

tm_shape(afrc) + tm_fill("gdp", style="jenks") + tm_borders(col = "NA", lwd = 1, lty = "solid", alpha = 0.2) + tm_layout(frame=T)
#########################################################################################################
path1 <- ##
setwd()
library(maptools)
x <- rgdal::readOGR("world-base-map.shp")
afrx <- rgdal::readOGR("afr.shp")

library(GISTools) 
library(rgdal)
data(tornados)

writeOGR(obj=torn, dsn="tempdir", layer="torn", driver="ESRI Shapefile") # this is in geographical projection

writeOGR(obj=torn2, dsn="tempdir", layer="torn2", driver="ESRI Shapefile") # this is in equal area projection





################################
################################
# NOT RUN {
cities <- readOGR(system.file("vectors", package = "rgdal")[1], "cities")
is.na(cities$POPULATION) <- cities$POPULATION == -99
summary(cities$POPULATION)
td <- file.path(tempdir(), "rgdal_examples"); dir.create(td)
# BDR 2016-12-15 (MapInfo driver fails writing to directory with ".")
if(nchar(Sys.getenv("OSGEO4W_ROOT")) > 0) {
  OLDPWD <- getwd()
  setwd(td)
  td <- "."
}
writeOGR(cities, td, "cities", driver="ESRI Shapefile")
writeOGR(world, td, "world", driver="ESRI Shapefile")


try(writeOGR(cities, td, "cities", driver="ESRI Shapefile"))
writeOGR(cities, td, "cities", driver="ESRI Shapefile", overwrite_layer=TRUE)
cities2 <- readOGR(td, "cities")
summary(cities2$POPULATION)
if ("SQLite" %in% ogrDrivers()$name) {
  tf <- tempfile()
  try(writeOGR(cities, tf, "cities", driver="SQLite", layer_options="LAUNDER=NO"))
}
if ("GeoJSON" %in% ogrDrivers()$name) {
  js <- '{
  "type": "MultiPolygon",
  "coordinates": [[[[102.0, 2.0], [103.0, 2.0], [103.0, 3.0], [102.0, 3.0],
  [102.0, 2.0]]], [[[100.0, 0.0], [101.0, 0.0], [101.0, 1.0], [100.0, 1.0],
  [100.0, 0.0]]]]
}'
  spdf <- readOGR(js, layer='OGRGeoJSON')
  in1_comms <- sapply(slot(spdf, "polygons"), comment)
  print(in1_comms)
  tf <- tempfile()
  writeOGR(spdf, tf, "GeoJSON", driver="GeoJSON")
  #spdf1 <- readOGR(tf, "GeoJSON")
  spdf1 <- readOGR(tf)
  in2_comms <- sapply(slot(spdf1, "polygons"), comment)
  print(in2_comms)
  print(isTRUE(all.equal(in1_comms, in2_comms)))
  }
# }
# NOT RUN {
if ("GML" %in% ogrDrivers()$name) {
  airports <- try(readOGR(system.file("vectors/airports.gml",
                                      package = "rgdal")[1], "airports"))
  if (class(airports) != "try-error") {
    writeOGR(cities, paste(td, "cities.gml", sep="/"), "cities", driver="GML")
    cities3 <- readOGR(paste(td, "cities.gml", sep="/"), "cities")
  }
}
# }
# NOT RUN {
# The GML driver does not support coordinate reference systems
if ("KML" %in% ogrDrivers()$name) {
  data(meuse)
  coordinates(meuse) <- c("x", "y")
  proj4string(meuse) <- CRS("+init=epsg:28992")
  meuse_ll <- spTransform(meuse, CRS("+proj=longlat +datum=WGS84"))
  writeOGR(meuse_ll["zinc"], paste(td, "meuse.kml", sep="/"), "zinc", "KML")
}
list.files(td)
roads <- readOGR(system.file("vectors", package = "rgdal")[1],
                 "kiritimati_primary_roads")
summary(roads)
if (strsplit(getGDALVersionInfo(), " ")[[1]][2] < "2") {
  # For GDAL >= 2, the TAB driver may need a BOUNDS layer option
  writeOGR(roads, td, "roads", driver="MapInfo File")
  roads2 <- readOGR(paste(td, "roads.tab", sep="/"), "roads")
  summary(roads2)
}
scot_BNG <- readOGR(system.file("vectors", package = "rgdal")[1], "scot_BNG")
summary(scot_BNG)
if (strsplit(getGDALVersionInfo(), " ")[[1]][2] < "2") {
  # For GDAL >= 2, the TAB driver may need a BOUNDS layer option
  writeOGR(scot_BNG, td, "scot_BNG", driver="MapInfo File")
  list.files(td)
  scot_BNG2 <- readOGR(paste(td, "scot_BNG.tab", sep="/"), "scot_BNG",
                       addCommentsToPolygons=FALSE)
  summary(scot_BNG2)
}
writeOGR(scot_BNG, td, "scot_BNG", driver="MapInfo File",
         dataset_options="FORMAT=MIF")
list.files(td)
scot_BNG3 <- readOGR(paste(td, "scot_BNG.mif", sep="/"), "scot_BNG")
summary(scot_BNG3)
if(nchar(Sys.getenv("OSGEO4W_ROOT")) > 0) {
  setwd(OLDPWD)
}
# }
########################################################
########################################################
