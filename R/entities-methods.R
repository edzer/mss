#' SpatialEntities density method
#'
#' density estimate for SpatialEntities data
#'
#' @aliases density density.SpatialEntities
#' @usage density(x, ...)
#' @usage density.SpatialEntities(x, bandwidth, newdata, ncells = 5000, ...)
#' @param x object of class \link{SpatialEntities-class}
#' @param bandwidth bandwidth parameter (see \link[MASS]{kde2d})
#' @param newdata target grid; if omitted, a grid over the window is created
#' @param ncells in case no newdata is provided and window is a polygon, the approximate number of grid cells for the grid created
#' @param ... ignored
#' @rdname density
#'
#' @return object of class \link{SpatialField-class}
#' 
#' @export density.SpatialEntities
#' @export density
density.SpatialEntities = function(x, bandwidth, newdata, ncells = 5000, ...) {
	if (missing(newdata)) {
		newdata = x@window@sp
		if (!gridded(newdata)) {
			newdata = spsample(newdata, ncells, "regular")
			gridded(newdata) = TRUE
		}
	} else if (is(newdata, "SpatialField"))
		newdata = newdata@sp
	if (!requireNamespace("MASS", quietly = TRUE))
		stop("package MASS required")

	# kde2d(x, y, h, n = 25, lims = c(range(x), range(y)))
	cc = coordinates(x@observations)
	gp = gridparameters(newdata)
	k = MASS::kde2d(cc[,1], cc[,2], h = bandwidth, n = gp$cells.dim, lims = 
		as.vector(t(bbox(newdata))))
	n = newdata
	fullgrid(n) = TRUE
	a = addAttrToGeom(n, data.frame(density = as.vector(k$z[,ncol(k$z):1,drop=FALSE])))
	a = a[newdata,]
	fullgrid(a) = fullgrid(newdata) # if wanted, coerce to SpatialPixelsDataFrame
	SpatialField(a, newdata)
}
#' plot method for SpatialEntities objects
#' 
#' plot method for SpatialEntities objects
#' 
#' @usage plot(x, ..., bg = grey(0.7))
#' @param x object of class \link{SpatialEntities}
#' @param bg background colour for observation window
#' @export
#' @examples
#' library(sp)
#' demo(meuse, ask = FALSE, echo = FALSE)
#' plot(SpatialField(meuse, meuse.grid))
#' plot(SpatialField(meuse, meuse.area))
plot.SpatialEntities = function(x,..., bg = grey(0.7)) {
	if (gridded(x@window@sp))
		image(x@window@sp, col = bg)
	else
		plot(x@window@sp, col = bg)
	plot(x@observations, add = TRUE, ...)
}
