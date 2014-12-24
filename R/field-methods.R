
#' Method interpolate
#' @name interpolate
#' @rdname interpolate-methods
#' @exportMethod interpolate
if (!isGeneric("interpolate"))
	setGeneric("interpolate", function(formula, data, ...)
		standardGeneric("interpolate"))
#' SpatialField interpolation method
#'
#' interpolate SpatialField data
#'
#' @param formula specifing which variable should be interpolated, see also \link[gstat]{krige}
#' @param data object of class \link{SpatialField-class}
#' @param support target support
#' @param newdata target locations; if missing, these are derived from  the domain of \code{data}
#' @param ncells in case no newdata is provided and point support interpolations are computed over a polygon area, the approximate number of grid cells (default 5000)
#' @param ... passed on to \link[gstat]{krige}
#'
#' @return object of class \link{SpatialField-class}
#' 
#' @seealso \link[gstat]{krige}
#' 
#' @export
#' @docType methods
#' @rdname interpolate-methods
setMethod("interpolate", c("formula", "SpatialField"),
  function(formula, data, support, ..., newdata, ncells = 5000) {
	if (missing(support))
		support = Support("point")
	else if (is.character(support))
		support = Support(support)
	if (missing(newdata)) {
		newdata = data@domain@sp
		if (!gridded(newdata) && support@what == "point") {
			newdata = spsample(newdata, ncells, "regular")
			gridded(newdata) = TRUE
		}
	}
	if (is.complete(data) && is(newdata, "SpatialPoints")) {
		stopifnot(support@what == "point")
		SpatialField(addAttrToGeom(newdata, over(newdata, data@sp), TRUE),
			support, data@domain)
	} else {
		args = append(list(formula, data@sp, newdata), list(...))
		if (!requireNamespace("gstat", quietly = TRUE))
			stop("package gstat required for interpolate()")
		if (gridded(newdata) && support@what == "feature")
			args$block = gridparameters(newdata)$cellsize
		else if (support@what == "function") {
			n = length(as.list(args(support@fn))) - 1
		if (n == 2)
				args$block = support@fn(0,0) # only for 2-D!
			else if (n == 3)
				args$block = support@fn(0,0,0) # only for 2-D!
			else stop("nr of arguments not understood")
		}
		SpatialField(do.call("krige", args), support, data@domain)
	}
})
setMethod("spplot", "SpatialField", function(obj,...) spplot(obj@sp, ...))
setMethod("$", "SpatialField", function(x, name) x@sp[[name]])
setMethod("[", "SpatialField", 
	function(x, i, j, ..., drop = TRUE) x@sp[i, j, ..., drop = drop])
