#' Method interpolate
#' @name interpolate
#' @exportMethod interpolate
if (!isGeneric("interpolate"))
	setGeneric("interpolate", function(formula, data, newdata, ...)
		standardGeneric("interpolate"))
#' SpatialField interpolation method
#'
#' interpolate SpatialField data
#'
#' @param formula specifing which variable should be interpolated, see also \link[gstat]{krige}
#' @param data object of class \link{SpatialField-class}
#' @param newdata target locations; if missing, points are chosen from the domain of \code{data}
#' @param ncells in case no newdata is provided and point support interpolations are computed over a polygon area, the approximate number of grid cells (default 5000)
#' @param ... passed on to \link[gstat]{krige}
#'
#' @return object of class \link{SpatialField-class}
#' 
#' @seealso \link[gstat]{krige}
#' 
#' @rdname interpolate
#' @export
#' @docType methods
#' @aliases interpolate,formula,SpatialField,SpatialField-method interpolate,formula,SpatialField,missing-method interpolate,formula,SpatialField,SpatialAggregation-method interpolate,formula,SpatialAggregation,SpatialField-method interpolate,formula,SpatialAggregation,SpatialAggregation-method
setMethod("interpolate", c("formula", "SpatialField", "SpatialField"),
	function(formula, data, newdata, ...) {
		if (any(is.na(over(newdata@observations, data@domain@area))))
			not_meaningful("interpolation outside the observation domain")
		if (full.coverage(data)) {
			stopifnot(is(newdata@observations, "SpatialPoints"))
			SpatialField(addAttrToGeom(newdata@observations, 
					over(newdata@observations, data@observations), TRUE), 
				data@domain, cellsArePoints = TRUE)
		} else { # point kriging
			if (!requireNamespace("gstat", quietly = TRUE))
				stop("package gstat required")
			SpatialField(gstat::krige(formula, data@observations, 
				newdata@observations, ...), data@domain, cellsArePoints = TRUE)
		}
	}
)
#' @rdname interpolate
#' @export
setMethod("interpolate", c("formula", "SpatialField", "missing"),
	function(formula, data, newdata, ..., ncells = 5000) {
		newdata = data@domain@area
		if (!gridded(newdata)) {
			newdata = spsample(newdata, ncells, "regular")
			gridded(newdata) = TRUE
		}
		interpolate(formula, data, SpatialField(newdata, newdata, cellsArePoints = TRUE), ...)
})
#' @rdname interpolate
#' @export
setMethod("interpolate", c("formula", "SpatialField", "SpatialAggregation"),
  	function(formula, data, newdata, ..., ncells = 5000) {
		if (any(is.na(over(newdata@observations, data@domain@area))))
			not_meaningful("interpolation outside the data domain")
		if (!requireNamespace("gstat", quietly = TRUE))
			stop("package gstat required")
		if (isTRUE(obs_extends_window(newdata@observations, data@domain)))
			not_meaningful("interpolating over an area larger than the domain")
		if (gridded(newdata@observations)) {
			block = gridparameters(newdata@observations)$cellsize
			SpatialAggregation(gstat::krige(formula, data@observations, 
				newdata@observations, block = block, ...))
		} else
			SpatialAggregation(gstat::krige(formula, data@observations, 
				newdata@observations, ...))
})
#' @rdname interpolate
#' @export
setMethod("interpolate", c("formula", "SpatialAggregation", "SpatialField"),
	function(formula, data, newdata, model, ...) {
		if (!requireNamespace("gstat", quietly = TRUE))
			stop("package gstat required")
		if (isTRUE(obs_extends_window(newdata@observations, 
				Window(data@observations))))
			not_meaningful("interpolating over an area larger than the domain")
		var1.pred = gstat::krige0(formula, data@observations, newdata@observations,
			gstat::vgmArea, vgm = model, ...)
		nd = addAttrToGeom(newdata@observations, data.frame(var1.pred), FALSE)
		SpatialField(nd, domain = newdata@domain)
	}
)
#' @rdname interpolate
#' @export
setMethod("interpolate", c("formula", "SpatialAggregation", "SpatialAggregation"),
	function(formula, data, newdata, model, ...) {
		if (!requireNamespace("gstat", quietly = TRUE))
			stop("package gstat required")
		if (isTRUE(obs_extends_window(newdata@observations, 
				Window(data@observations))))
			not_meaningful("interpolating over an area larger than the domain")
		var1.pred = gstat::krige0(formula, data@observations, newdata@observations,
			gstat::vgmArea, vgm = model, ...)
		newdata = addAttrToGeom(newdata@observations, data.frame(var1.pred), FALSE)
		SpatialAggregation(newdata)
	}
)
setMethod("spplot", "SpatialField", 
	function(obj,..., cellsAsPoints = 3000, colorkey = TRUE) {
		obs = obj@observations
		if (gridded(obs) && prod(gridparameters(obs)$cells.dim[1:2]) < cellsAsPoints)
			gridded(obs) = FALSE
		spplot(obs, ..., colorkey = colorkey)
	}
)
setMethod("over", c("SpatialField", "SpatialField"), 
	function(x, y, returnList = FALSE, fn = NULL, ...) {
		if (gridded(y@observations) && y@cellsArePoints)
			gridded(y@observations) = FALSE  # converts to Points
		over(x@observations, y@observations, returnList, fn, ...)
	}
)
setMethod("over", c("SpatialField", "SpatialAggregation"), 
	function(x, y, returnList = FALSE, fn = NULL, ...) {
		not_meaningful("deriving field values from aggregations")
		NULL
	}
)

#' assess whether function has complete coverage over the domain
#' 
#' fields with point support, defined for all areas in the domain are completely known; this function verifies that this is the case
#' 
#' @param x object of class \link{SpatialField-class}
#' @return logical; TRUE if support is "point" and area size of the observations is identical to that of the domain; this implies we have a coverage with knowledge of all points in the domain.
#' @note what the function does is not sufficient: identical area size does not guarantee that areas are identical
#' @export
full.coverage = function(x) {
	stopifnot(is(x, "SpatialField"))
	if (x@observationsEqualDomain)
		return(TRUE)
	((gridded(x@domain@area) && ! x@cellsArePoints) || is(x@domain@area, "SpatialPolygons")) &&
		isTRUE(all.equal(getArea(x@observations), 
			getArea(x@domain@area))) # allows for numerical fuzz
}
#' compute area of a Spatial* object
#'
#' compute the area of an object of one of the subclasses of \link[sp]{Spatial}
#'
#' area of gridded or polygon objects deriving from \link[sp]{Spatial}, zero
#' for other classes (points, lines)
#' @usage getArea(x)
#'
#' @param x object of a subclass of \link[sp]{Spatial}
#' @return the area of the features (grid cells or polygons) in the object
#' @export
#' @examples
#' library(sp)
#' demo(meuse, ask = FALSE, echo = FALSE)
#' getArea(meuse)
#' getArea(meuse.area)
getArea = function(x) {
	stopifnot(is(x, "Spatial"))
	if (gridded(x))
		return(areaSpatialGrid(x))
	getAreaSP = function(x) { # copied from sp/R/spsample.R:
    		getAreaPolygons = function(x) {
        		holes = unlist(lapply(x@Polygons, function(x) x@hole))
        		areas = unlist(lapply(x@Polygons, function(x) x@area))
        		area = ifelse(holes, -1, 1) * areas
        		area
    		}
    		sum(unlist(lapply(x@polygons, getAreaPolygons)))
	}
	if (is(x, "SpatialPolygons"))
		return(getAreaSP(x))
	# lines, points:
	return(0.0)
}
