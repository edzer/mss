# script to reproduce Fig 1 in:
#
# Stasch, C., S. Scheider, E. Pebesma, W. Kuhn, 2014.
# Meaningful Spatial Prediction and Aggregation.
# Environmental Modelling and Software, 51, 149--165 (open access)

options(warn=2)            # promote warnings to errors
library(classInt)          # used for creating class intervals for plotting the maps
library(sp)
library(spacetime)
library(gstat)
library(cshapes)           # used for loading the country border of Germany

par(mfrow=c(2,3), mar=c(0.1,.2, 2,.2))

# CO2 emissions

# retrieving germany for background and aggregation
countries.data <- cshp(date=as.Date("2008-06-30")) #retrieve latest borders from 30/6/2008
germany <- countries.data[countries.data$CNTRY_NAME=="Germany",]

# read the co2 Emissions
file = system.file("external/co2_emission_powerplants.csv", package="mss")[1]
co2all <- read.csv(file, header = TRUE)

# filter power plants without locations (=zero latitudes) and with carbon dioxide emissions in 2008
co2cleaned <- co2all[co2all$latitude != 0 & co2all$carbon_2007 != 0,
		c("plant_id", "name", "latitude", "longitude", "carbon_2007")]

# convert to spatial points dataframe
coords <- cbind(co2cleaned$longitude,co2cleaned$latitude)
co2sp <- SpatialPointsDataFrame(coords,co2cleaned, proj4string = CRS(proj4string(germany)))

# prepare plot
pal <- grey.colors(4, 0.95, 0.55, 2.2)
q5 <- classIntervals(co2sp$carbon_2007, n = 5, style = "quantile")
q5Colours <- findColours(q5, pal)

# plot of locations with emissions
plot(germany)
plot(co2sp,col = q5Colours,pch=19,add=TRUE)
title(expression("CO"[2]*" emissions of power plants"))
box(col='grey')

# compute and plot aggregate (sum) of CO2 emissions
plot(germany)
co2agg <- aggregate(co2sp["carbon_2007"], germany, sum)
plot(co2agg,add=TRUE)
invisible(text(coordinates(co2agg), labels=as.character(co2agg$carbon_2007), cex=1.3))
title(expression("Sum of CO"[2]*" emissions"))
box(col='grey')

grd = spsample(germany, 10000, "regular", offset = c(0,0))
gridded(grd) = TRUE

co2_interpolated <- krige(carbon_2007~1, co2sp, grd)
pal_int <- grey.colors(40, 0.95, 0.05, 2.2)
image(co2_interpolated,col=pal_int)
plot(germany, add = TRUE)
title(expression("Interpolated CO"[2]*" emissions"))
box(col='grey')

# PM10 
# read Airbase PM10 data
file = system.file("external/EU_meas_2005_june.dat", package="mss")[1]
# since R 3.1, coordinates will be read as factor, because they otherwise loose precision;
# hence, we need to specify colClasses
cc = c("factor", "Date", "Date", "factor", "factor", "numeric", "numeric", "numeric")
pm10.tab <- read.table(file, header = TRUE, colClasses = cc)
pm10all = STIDF(SpatialPoints(pm10.tab[c("x", "y")]), as.POSIXct(pm10.tab$time), pm10.tab)
pm10all = as(pm10all, "STSDF")
pm10sel <- pm10all[,"2005-06-01"] #select only one day
proj4string(pm10sel) <- proj4string(germany) #set crs of pm10 values
pm10germanyIndex <- over(pm10sel,as(germany,"SpatialPolygons")) #select only german stations
pm10germany <- pm10sel[!is.na(pm10germanyIndex),]
pm10germany <- pm10germany[!is.na(pm10germany$PM10),] #remove NA values 
pm10germany <- subset(pm10germany,select=c("PM10"))

# plot PM10 stations
q5 <- classIntervals(pm10germany$PM10, n = 5, style = "quantile")
q5Colours <- findColours(q5, pal)
plot(germany)
plot(pm10germany,col = q5Colours,pch=19,add=TRUE)
title(expression("PM"[10]*" measurements"))
box(col='grey')

# compute and plot sum of pm10 emissions
plot(germany)
proj4string(pm10germany)<-proj4string(germany)
pm10agg <- aggregate(pm10germany,germany,sum)
plot(pm10agg,add=TRUE)
invisible(text(coordinates(pm10agg), labels=as.character(pm10agg$PM10), cex=1.3))
title(expression("Sum of PM"[10]*" measurements"))
box(col='grey')

proj4string(pm10germany) <- proj4string(grd)
pm10_interpolated <- krige(PM10~1, pm10germany, grd)
image(pm10_interpolated,col=pal_int)
plot(germany, add=TRUE)
title(expression("Interpolated PM"[10]*" measurements"))
box(col='grey')
