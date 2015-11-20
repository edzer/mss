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

#' aggregate SField, SObjects or SLattice objects
#'
#' aggregation methods for objects of class SField, SObjects or SLattice
#' 
#' @aliases aggregate aggregate.SField
#' @usage aggregate(x, ...)
#' @param x object of class \link{SField}, \link{SObjects}, or \link{SLattice}
#' @param by aggregation predicate
#' @param FUN aggregation function
#' @param ... arguments passed on to \code{FUN}
#' @rdname aggregate
#' @export aggregate
#' @export
aggregate.SField = function(x, by, FUN = mean, ...) {
	if (!is(by, "SLattice"))
		stop("argument `by' needs to be of class `SLattice'")
	if (isTRUE(obs_extends_window(by@observations, x@domain)))
		not_meaningful("aggregation over an area larger than the domain")
	if (is.sum(FUN))
		not_meaningful("for SField objects, aggregation using a sum function")
	SLattice(aggregate(x@observations, by@observations, FUN = FUN, ...))
}
#' @rdname aggregate
#' @export
aggregate.SObjects = function(x, by, FUN = mean, ...) {
	if (!is(by, "SLattice"))
		stop("argument `by' needs to be of class `SLattice'")
	if (isTRUE(obs_extends_window(by@observations, x@window)))
		not_meaningful("aggregation over an area larger than the observation window")
	if (!is.sum(FUN))
		maybe_meaningful("aggregation using a non-sum function")
	SLattice(aggregate(x@observations, by@observations, FUN = FUN, ...))
}
#' @rdname aggregate
#' @export
aggregate.SLattice = function(x, by, FUN = mean, ...) {
	if (!is(by, "numeric"))
		stop("argument `by' needs to be numeric")
	# SLattice(aggregate(x@observations, by@observations, FUN = FUN, ...))
	stop("not yet implemented")
}
