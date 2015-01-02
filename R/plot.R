#' plot method for SpatialAggregation objects
#' 
#' plot method for SpatialAggregation objects
#' 
#' @param x object of class \link{SpatialAggregation}, \link{SpatialField} or \link{SpatialEntities}
#' @param ... passed on to \link[sp]{plot,Spatial,missing-method}
#' @param y should be missing
#' @param bg background colour for domain or observation window (default grey(.7))
#' @aliases plot,SpatialAggregation,missing-method plot,SpatialField,missing-method plot,SpatialEntities,missing-method
#' @examples
#' library(sp)
#' demo(meuse, ask = FALSE, echo = FALSE)
#' plot(SpatialAggregation(meuse.grid))
#' plot(SpatialAggregation(meuse.area))
#' plot(SpatialField(meuse, meuse.grid))
#' plot(SpatialField(meuse, meuse.area))
#' plot(SpatialEntities(meuse, meuse.grid))
#' plot(SpatialEntities(meuse, meuse.area))
#' @rdname plot
#' @export
setMethod("plot", signature(x = "SpatialAggregation", y = "missing"),
    function(x, y,...) plot(x@observations, ...))
#' @rdname plot
#' @export
setMethod("plot", signature(x = "SpatialField", y = "missing"),
    function(x,y,..., bg = grey(0.7)) {
		if (gridded(x@domain@area))
			image(x@domain@area, col = bg)
		else
			plot(x@domain@area, col = bg)
		plot(x@observations, add = TRUE, ...)
	}
)
#' @rdname plot
#' @export
setMethod("plot", signature(x = "SpatialEntities", y = "missing"),
	function(x,..., bg = grey(0.7)) {
		if (gridded(x@window@area))
			image(x@window@area, col = bg)
		else
			plot(x@window@area, col = bg)
		plot(x@observations, add = TRUE, ...)
	}
)
