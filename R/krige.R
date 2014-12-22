##############################################################
#' krige method for PointPatternDataFrame objects
#'
#' augmented krige method for point pattern; shows warning message, 
#' if krige is applied to point pattern
#'
#' @param formula see \link[gstat]{krige}
#' @param locations see \link[gstat]{krige}
#' @param ... see \link[gstat]{krige}
#'
#' @return see \link[gstat]{krige}
#' @aliases krige
#' 
#' @seealso \link[gstat]{krige}
#' 
#' @export
#' @docType methods
#' @rdname krige-pp-methods
#'
#' @examples
#' library(sp)
#' demo(meuse, ask = FALSE)
#' pp = as(meuse, "PointPatternDataFrame")
#' kr = krige(log(zinc)~1, pp, meuse.grid)
setMethod("krige", c("formula", "PointPatternDataFrame"), 
	function(formula, locations, ...){
		not_meaningful("interpolating point patterns")
		krige(formula, as(locations, "SpatialPointsDataFrame"), ...)
	}
)
