#' plot method for SLattice objects
#' 
#' plot method for SLattice objects
#' 
#' @param x object of class \link{SLattice}, \link{SField} or \link{SObjects}
#' @param ... passed on to \link[sp]{plot,Spatial,missing-method}
#' @param y should be missing
#' @param bg background colour for domain or observation window (default grey(.7))
#' @aliases plot,SLattice,missing-method plot,SField,missing-method plot,SObjects,missing-method
#' @examples
#' library(sp)
#' demo(meuse, ask = FALSE, echo = FALSE)
#' plot(SLattice(meuse.grid))
#' plot(SLattice(meuse.area))
#' plot(SField(meuse, meuse.grid))
#' plot(SField(meuse, meuse.area))
#' plot(SObjects(meuse, meuse.grid))
#' plot(SObjects(meuse, meuse.area))
#' @rdname plot
#' @export
setMethod("plot", signature(x = "SLattice", y = "missing"),
    function(x, y,...) plot(x@observations, ...))
#' @rdname plot
#' @export
setMethod("plot", signature(x = "SField", y = "missing"),
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
setMethod("plot", signature(x = "SObjects", y = "missing"),
	function(x,..., bg = grey(0.7)) {
		if (gridded(x@window@area))
			image(x@window@area, col = bg)
		else
			plot(x@window@area, col = bg)
		plot(x@observations, add = TRUE, ...)
	}
)
