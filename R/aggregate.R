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

#' aggregate SpatialField or SpatialEntities objects
#'
#' aggregation methods for objects of class SpatialField or SpatialEntities
#' 
#' @aliases aggregate aggregate.SpatialField
#' @usage aggregate(x, ...)
#' @param x object of class \code{SpatialField}
#' @param by aggregation predicate
#' @param FUN aggregation function
#' @param ... arguments passed on to \code{FUN}
#' @rdname aggregate
#' @export aggregate.SpatialField
#' @export aggregate
aggregate.SpatialField = function(x, by, FUN = mean, ...) {
	if (!is(by, "SpatialAggregation"))
		stop("argument `by' needs to be of class `SpatialAggregation'")
	if (is.sum(FUN))
		not_meaningful("for SpatialField objects, aggregation using a sum function")
	SpatialAggregation(aggregate(x@observations, by@observations, FUN = FUN, ...))
}
#' @rdname aggregate
#' @export aggregate.SpatialEntities
aggregate.SpatialEntities = function(x, by, FUN = mean, ...) {
	if (!is(by, "SpatialAggregation"))
		stop("argument `by' needs to be of class `SpatialAggregation'")
	if (!requireNamespace("rgeos", quietly = TRUE))
		mss("for checking meaningfull aggregation areas, package rgeos required")
	else if (is(by@observations, "SpatialPolygons") && 
					is(x@window@area, "SpatialPolygons")) {
		diff = rgeos::gDifference(by@observations, x@window@area)
		if (!is.null(diff) && getArea(diff) > 0.0)
			not_meaningful("aggregation over an area larger than the observation window")
	}
	if (!is.sum(FUN))
		maybe_meaningful("aggregation using a non-sum function")
	SpatialAggregation(aggregate(x@observations, by@observations, FUN = FUN, ...))
}
