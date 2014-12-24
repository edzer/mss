#' SpatialEntities density method
#'
#' density estimate for SpatialEntities data
#' @usage density(x,...)
#'
#' @param x object of class \link{SpatialEntities-class}
#' @param support target support
#' @param newdata target locations; if missing, these are derived from  the domain of \code{data}
#' @param ncells in case no newdata is provided and point support interpolations are computed over a polygon area, the approximate number of grid cells (default 5000)
#' @param ... passed on to \link[gstat]{krige}
#'
#' @return object of class \link{SpatialField-class}
#' 
#' @rdname density
#' @export density
#' @export density.SpatialEntities
density.SpatialEntities = function(x, support = "point", bandwidth, newdata, ncells = 5000, ...) {
	if (missing(newdata)) {
		newdata = x@window@sp
		if (!gridded(newdata)) {
			newdata = spsample(newdata, ncells, "regular")
			gridded(newdata) = TRUE
		}
	} else {
		if (support == "feature") {
		}
	}
	library(MASS)
	# kde2d(x, y, h, n = 25, lims = c(range(x), range(y)))
	cc = coordinates(x@sp)
	gp = gridparameters(newdata)
	k = kde2d(cc[,1], cc[,2], h = bandwidth, n = gp$cells.dim, lims = 
		as.vector(t(bbox(newdata))))
	n = newdata
	fullgrid(n) = TRUE
	a = addAttrToGeom(n, data.frame(density = as.vector(k$z[,ncol(k$z):1,drop=FALSE])))
	a = a[newdata,]
	SpatialField(a, "point", newdata)
}
