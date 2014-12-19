# script to reproduce Fig 1 in:
#
# Stasch, C., S. Scheider, E. Pebesma, W. Kuhn, 2014.
# Meaningful Spatial Prediction and Aggregation.
# Environmental Modelling and Software, 51, 149--165 (open access)

pdf("intro.pdf")
options(warn=2)            # promote warnings to errors
library(classInt)          # used for creating class intervals for plotting the maps
library(sp)
library(spacetime)
library(gstat)
library(cshapes)           # used for loading the country border of Germany
# install the mss package from github:

par(mfrow=c(2,3), mar=c(0.1,.2, 2,.2))

# CO2 emissions

# retrieving germany for background and aggregation
countries.data <- cshp(date=as.Date("2008-06-30")) #retrieve latest borders from 30/6/2008
germany <- countries.data[countries.data$CNTRY_NAME=="Germany",]

# read the co2 Emissions
co2all <- read.csv("co2_emission_powerplants.csv", header = TRUE)

# filter power plants without locations (=zero latitudes) and with carbon dioxide emissions in 2008
co2cleaned <- co2all[co2all$latitude != 0 & co2all$carbon_2007 != 0,
		c("plant_id", "name", "latitude", "longitude", "carbon_2007")]

# convert to spatial points dataframe
coords <- cbind(co2cleaned$longitude,co2cleaned$latitude)
co2sp <- SpatialPointsDataFrame(coords,co2cleaned, proj4string = CRS(proj4string(germany)))

# prepare plot
#pal <- grey.colors(4, 0.95, 0.55, 2.2)
pal = bpy.colors()
q5 <- classIntervals(log(co2sp$carbon_2007), n = 5, style = "quantile")
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

co2_interpolated <- krige(log(carbon_2007)~1, co2sp, grd)
#pal_int <- grey.colors(40, 0.95, 0.05, 2.2)
pal_int = bpy.colors()
image(co2_interpolated, col = pal_int)
plot(germany, add = TRUE)
title(expression("Interpolated CO"[2]*" emissions"))
box(col='grey')

# PM10 
# read Airbase PM10 data
# since R 3.1, coordinates will be read as factor, because they otherwise loose precision;
# hence, we need to specify colClasses
cc = c("factor", "Date", "Date", "factor", "factor", "numeric", "numeric", "numeric")
pm10.tab <- read.table("EU_meas_2005_june.dat", header = TRUE, colClasses = cc)
pm10all = STIDF(SpatialPoints(pm10.tab[c("x", "y")]), as.POSIXct(pm10.tab$time), pm10.tab)
pm10all = as(pm10all, "STSDF")
pm10sel <- pm10all[,"2005-06-01"] #select only one day
proj4string(pm10sel) <- proj4string(germany) #set crs of pm10 values
pm10germany <- pm10sel[germany, "PM10"]
pm10germany <- pm10germany[!is.na(pm10germany$PM10),] #remove NA values 

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

options(warn=1) # print warnings where they occur

#library(spatstat) # point patterns -- overwrites idw!
#library(maptools) # convert sp <--> spatstat classes


##############################
### SECTION: demonstration scripts
##############################
co2sp = co2sp["carbon_2007"]

library(mss)
co2 = as(co2sp, "PointPatternDataFrame")
co2_int <- krige(log(carbon_2007)~1, co2, grd)

# Aggregation of a marked point pattern:
co2.mean = aggregate(co2, germany, mean)
co2.sum  = aggregate(co2, germany, sum)

# Aggregation of a geostatistical variable:
pm10 = as(pm10germany, "GeostatisticalDataFrame")
pm10.mean = aggregate(pm10, germany, mean)
pm10.sum  = aggregate(pm10, germany, sum)
