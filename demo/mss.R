options(warn=1) # print warnings where they occur

library(spatstat) # point patterns -- overwrites idw!
library(maptools) # convert sp <--> spatstat classes
##############################
### SECTION: demonstration scripts
##############################

### 1. interpolate a pp; email EP->CS of 3/22/12
data(longleaf)
# loads long-leaved pinus data set: diameter at breast height
plot(idw(longleaf),main="") # done by spatstat
# inverse distance interpolation
plot(longleaf, add = TRUE)
ll = as(as(longleaf, "SpatialPointsDataFrame"), "PointPatternDataFrame")
grd0 = SpatialPoints(makegrid(ll, 1000))
gridded(grd0) = TRUE

out <- krige(marks~1, ll, grd0, vgm(1, "Exp", 50))
# if we would coerce to GeostatisticalDataFrame, then no problem there:
ll = as(as(longleaf, "SpatialPointsDataFrame"), "GeostatisticalDataFrame")
out <- krige(marks~1, ll, grd0, vgm(1, "Exp", 50))
# GeostatisticalDataFrame derives from Spatial, so a krige method is inherited:
showMethods("krige")
spplot(out[1])


### 2. Model, as point-pattern, a geostat variable (zinc?)
data(meuse)
coordinates(meuse) = ~x+y
# convert meuse data set into a ppp, marked point pattern:
meuse.pp = as.ppp(meuse[1:2])
meuse.pp
# convert the grid of the study area into a point pattern window:
data(meuse.grid)
gridded(meuse.grid) = ~x+y
meuse.win = as.owin(meuse.grid[1])
meuse.win
# plot a smoothed version of the marks:

plot(idw(meuse.pp))
# plot the mark correlation function:
plot(markcorr(meuse.pp))
# plot E(r) function, a diagnostics plot for dependence between the points and the marks.
plot(Emark(spruces))


### 3. aggregation examples

### 3.1 longleaf pines
ll = as(longleaf, "SpatialPointsDataFrame")
llgrd = SpatialPoints(makegrid(ll, 100))
gridded(llgrd)=TRUE
ll.agg = aggregate(ll, llgrd, sum)
names(ll.agg) = "sum"
ll.agg$mean = aggregate(ll, llgrd, mean)[[1]] * 5
spl =list("sp.points", ll,cex=ll[[1]]/30,pch=1, col='grey')
na = c("grid cell mean (x 5)", "grid cell sum")
spplot(ll.agg[c("mean", "sum")], sp.layout=spl, col.regions=bpy.colors(),
    names.attr = na, main = "tree diameters")
ll = as(as(longleaf, "SpatialPointsDataFrame"), "PointPatternDataFrame")


# with warnings:
ll.agg.mean <- aggregate(ll, llgrd, mean)
# without warnings:
ll.agg.sum = aggregate(ll, llgrd, sum)

### 3.2 aggregation of meuse data
meuse.points = as(meuse[1:2], "GeostatisticalDataFrame")
#str(meuse.points)
meuse.target = SpatialPoints(makegrid(meuse.points, 10))
gridded(meuse.target)=TRUE
#with warnings
ms.agg = aggregate(meuse.points,meuse.target, sum)
#without warnings
ms.agg = aggregate(meuse.points, meuse.target, mean)
