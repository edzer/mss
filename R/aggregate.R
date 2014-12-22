#' check whether x equals function sum
#'
#' this function checks whether its argument equals function sum
#' @param x the function to check
#' @export
#' @examples
#' is.sum(sum)
#' is.sum(sum(1:3))
#' is.sum(mean)
#' is.sum(function(x) sum(x))
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
		warning("aggregation using a non-sum function may not be meaningful for Point Pattern data")
	aggregate(as(x, "SpatialPointsDataFrame"), by, FUN, ...)
}
