is.sum = function(x) {
	paste(deparse(x), collapse="") == paste(deparse(sum),collapse="")
}

#
# augmented generic aggregate function for GeostatisticalDataFrame; 
# a warning is thrown, if the function is not a sum function
#
aggregate.GeostatisticalDataFrame = function(x, by, FUN = mean, ...) {
	if (is.sum(FUN))
		warning("aggregation using a sum function is not considered meaningful for Geostatistical data")
	aggregate(as(x, "SpatialPointsDataFrame"), by, FUN, ...)
}


#
# augmented generic aggregate function for PointPattern; a warning is 
# thrown if the function is not a sum function
#
aggregate.PointPatternDataFrame = function(x, by, FUN = mean, ...) {
	if (!is.sum(FUN))
		warning("aggregation using a non-sum function is not considered meaningful for Point Pattern data")
	aggregate(as(x, "SpatialPointsDataFrame"), by, FUN, ...)
}
