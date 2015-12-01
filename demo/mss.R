options(warn=1) # print warnings where they occur

library(spatstat) # point patterns -- overwrites idw!
library(maptools) # convert sp <--> spatstat classes
library(gstat) # vgm
library(mss)
##############################
### SECTION: demonstration scripts
##############################

### 1. interpolate a pp; email EP->CS of 3/22/12
data(longleaf)
# loads long-leaved pinus data set: diameter at breast height
plot(spatstat::idw(longleaf),main="") # done by spatstat
# inverse distance interpolation
plot(longleaf, add = TRUE)
ll = as(longleaf, "SpatialPointsDataFrame")
grd0 = SpatialPoints(makegrid(ll, 1000))
gridded(grd0) = TRUE
ll = SObjects(ll, grd0)
out <- interpolate(marks~1, ll, SField(grd0, cellsArePoints = TRUE), vgm(1, "Exp", 50))
# if we would coerce to GeostatisticalDataFrame, then no problem there:
ll = as(longleaf, "SpatialPointsDataFrame")
ll = SField(ll, grd0)
out <- interpolate(marks~1, ll, SField(grd0, cellsArePoints = TRUE), vgm(1, "Exp", 50))

spplot(out@observations[1])


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

plot(spatstat::idw(meuse.pp))
# plot the mark correlation function:
plot(markcorr(meuse.pp))
# plot E(r) function, a diagnostics plot for dependence between the points and the marks.
plot(Emark(spruces))


### 3. aggregation examples

### 3.1 longleaf pines
ll = as(longleaf, "SpatialPointsDataFrame")
llgrd = SpatialPoints(makegrid(ll, 100))
gridded(llgrd) = TRUE
ll.agg = aggregate(ll, llgrd, sum)
names(ll.agg) = "sum"
ll.agg$mean = aggregate(ll, llgrd, mean)[[1]] * 5
spl =list("sp.points", ll,cex=ll[[1]]/30,pch=1, col='grey')
na = c("grid cell mean (x 5)", "grid cell sum")
spplot(ll.agg[c("mean", "sum")], sp.layout=spl, col.regions=bpy.colors(),
    names.attr = na, main = "tree diameters")
ll = SObjects(ll, as(llgrd, "SpatialPolygons"))

# with warnings:
ll.agg.mean <- aggregate(ll, SLattice(llgrd), mean)
# without warnings:
ll.agg.sum = aggregate(ll, SLattice(llgrd), sum)

### 3.2 aggregation of meuse data
#str(meuse.points)
meuse.grd = SpatialPoints(makegrid(meuse, 10))
gridded(meuse.grd) = TRUE
meuse.target = SLattice(meuse.grd)
meuse.points = SField(meuse[1:2], as(meuse.grd, "SpatialPolygons"))
#with warnings
ms.agg = aggregate(meuse.points, meuse.target, sum)
#without warnings
ms.agg = aggregate(meuse.points, meuse.target, mean)
