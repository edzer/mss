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
#' @aliases aggregate
#' @usage aggregate(x, ...)
#' @param x object of class \code{SpatialField}
#' @param by aggregation predicate
#' @param FUN aggregation function
#' @param ... arguments passed on to \code{FUN}
#' @rdname aggregate
#' @export aggregate SpatialField
aggregate.SpatialField = function(x, by, FUN = mean, ...) {
	if (is.sum(FUN))
		warning("aggregation using a sum function is not considered meaningful for SpatialField objects")
	stop("not yet implemented")
	aggregate(x@sp, by, FUN, ...)
}

#' @rdname aggregate
#' @export aggregate SpatialField
aggregate.SpatialEntities = function(x, by, FUN = mean, ...) {
	if (!is.sum(FUN))
		warning("aggregation using a non-sum function may not be meaningful for Point Pattern data")
	stop("not yet implemented")
	aggregate(as(x, "SpatialPointsDataFrame"), by, FUN, ...)
}
