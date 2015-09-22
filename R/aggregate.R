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
	paste(deparse(x), collapse="") == paste(deparse(sum), collapse="")
}

#' aggregate SpatialField, SpatialEntities or SpatialAggregation objects
#'
#' aggregation methods for objects of class SpatialField, SpatialEntities or SpatialAggregation
#' 
#' @aliases aggregate aggregate.SpatialField
#' @usage aggregate(x, ...)
#' @param x object of class \link{SpatialField}, \link{SpatialEntities}, or \link{SpatialAggregation}
#' @param by aggregation predicate
#' @param FUN aggregation function
#' @param ... arguments passed on to \code{FUN}
#' @rdname aggregate
#' @export aggregate
#' @export
aggregate.SpatialField = function(x, by, FUN = mean, ...) {
	if (!is(by, "SpatialAggregation"))
		stop("argument `by' needs to be of class `SpatialAggregation'")
	if (isTRUE(obs_extends_window(by@observations, x@domain)))
		not_meaningful("aggregation over an area larger than the domain")
	if (is.sum(FUN))
		not_meaningful("for SpatialField objects, aggregation using a sum function")
	SpatialAggregation(aggregate(x@observations, by@observations, FUN = FUN, ...))
}
#' @rdname aggregate
#' @export
aggregate.SpatialEntities = function(x, by, FUN = mean, ...) {
	if (!is(by, "SpatialAggregation"))
		stop("argument `by' needs to be of class `SpatialAggregation'")
	if (isTRUE(obs_extends_window(by@observations, x@window)))
		not_meaningful("aggregation over an area larger than the observation window")
	if (!is.sum(FUN))
		maybe_meaningful("aggregation using a non-sum function")
	SpatialAggregation(aggregate(x@observations, by@observations, FUN = FUN, ...))
}
#' @rdname aggregate
#' @export
aggregate.SpatialAggregation = function(x, by, FUN = mean, ...) {
	if (!is(by, "numeric"))
		stop("argument `by' needs to be numeric")
	# SpatialAggregation(aggregate(x@observations, by@observations, FUN = FUN, ...))
	stop("not yet implemented")
}
