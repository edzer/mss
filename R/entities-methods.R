#' SpatialEntities density method
#'
#' density estimate for SpatialEntities data
#'
#' @aliases density density.SpatialEntities
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
#' @export
density.SpatialEntities = function(x, bandwidth, newdata, ncells = 5000, ...) {
	if (missing(newdata)) {
		newdata = x@window@area
		if (!gridded(newdata)) {
			newdata = spsample(newdata, ncells, "regular")
			gridded(newdata) = TRUE
		}
	} else if (is(newdata, "SpatialField"))
		newdata = newdata@observations
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
	#SpatialField(a, newdata, cellsArePoints = TRUE)
	SpatialAggregation(a)
}
#' @rdname interpolate
#' @export
setMethod("interpolate", c("formula", "SpatialEntities", "ANY"),
	function(formula, data, newdata, ...) {
		not_meaningful("interpolation of SpatialEntities")
		data
	}
)
