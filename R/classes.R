####
# GeostatisticalDataFrame represents a geostatistical variable
#
setClass("GeostatisticalDataFrame", contains = "SpatialPointsDataFrame",
	representation(window = "SpatialPolygons"))
	# should the window really be Polygons?

####
# Point pattern represents a (marked) point pattern variable
#
setClass("PointPatternDataFrame", contains = "SpatialPointsDataFrame",
	representation(window = "SpatialPolygons"))

###
# LatticeData represents a lattice variable
#
setClass("LatticeDataFrame", contains = "SpatialPolygonsDataFrame",
	representation(window = "SpatialPolygons"))
