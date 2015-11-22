#' Method interpolate
#' @name interpolate
#' @exportMethod interpolate
if (!isGeneric("interpolate"))
	setGeneric("interpolate", function(formula, data, newdata, ...)
		standardGeneric("interpolate"))
#' SField interpolation method
#'
#' interpolate SField data
#'
#' @param formula specifing which variable should be interpolated, see also \link[gstat]{krige}
#' @param data object of class \link{SField-class}
#' @param newdata target locations; if missing, points are chosen from the domain of \code{data}
#' @param ncells in case no newdata is provided and point support interpolations are computed over a polygon area, the approximate number of grid cells (default 5000)
#' @param model covariance model, see \link[gstat]{krige} and \link[gstat]{vgm}
#' @param ... passed on to \link[gstat]{krige}
#'
#' @return object of class \link{SField-class}
#' 
#' @seealso \link[gstat]{krige}
#' 
#' @rdname interpolate
#' @export
#' @docType methods
#' @aliases interpolate,formula,SField,SField-method interpolate,formula,SField,missing-method interpolate,formula,SField,SLattice-method interpolate,formula,SLattice,SField-method interpolate,formula,SLattice,SLattice-method
#' @examples
#' library(sp)
#' demo(meuse, ask = FALSE, echo = FALSE)
#' sf = SField(meuse, SExtent(meuse.grid))
#' p = interpolate(zinc~1, sf)
#' spplot(p)
#' sf = SField(meuse, SExtent(meuse.area))
#' p = interpolate(zinc~1, sf)
#' spplot(p)
setMethod("interpolate", c("formula", "SField", "SField"),
	function(formula, data, newdata, ...) {
		if (any(is.na(over(newdata@observations, data@domain@area))))
			not_meaningful("interpolation outside the observation domain")
		if (full.coverage(data)) {
			stopifnot(is(newdata@observations, "SpatialPoints"))
			SField(addAttrToGeom(newdata@observations, 
					over(newdata@observations, data@observations), TRUE), 
				data@domain, cellsArePoints = TRUE)
		} else { # point kriging
			if (!requireNamespace("gstat", quietly = TRUE))
				stop("package gstat required")
			SField(gstat::krige(formula, data@observations, 
				newdata@observations, ...), data@domain, cellsArePoints = TRUE)
		}
	}
)
#' @rdname interpolate
#' @export
setMethod("interpolate", c("formula", "SField", "missing"),
	function(formula, data, ..., ncells = 5000) {
		newdata = data@domain@area
		if (!gridded(newdata)) {
			where = spsample(newdata, ncells, "regular")
			gridded(where) = TRUE
		} else
			where = newdata
		interpolate(formula, data, SField(where, SExtent(newdata), cellsArePoints = TRUE), ...)
})
#' @rdname interpolate
#' @export
setMethod("interpolate", c("formula", "SField", "SLattice"),
  	function(formula, data, newdata, ..., ncells = 5000) {
		if (any(is.na(over(newdata@observations, data@domain@area))))
			not_meaningful("interpolation outside the data domain")
		if (!requireNamespace("gstat", quietly = TRUE))
			stop("package gstat required")
		if (isTRUE(obs_extends_window(newdata@observations, data@domain)))
			not_meaningful("interpolating over an area larger than the domain")
		if (gridded(newdata@observations)) {
			block = gridparameters(newdata@observations)$cellsize
			SLattice(gstat::krige(formula, data@observations, 
				newdata@observations, block = block, ...))
		} else
			SLattice(gstat::krige(formula, data@observations, 
				newdata@observations, ...))
})
#' @rdname interpolate
#' @export
setMethod("interpolate", c("formula", "SLattice", "SField"),
	function(formula, data, newdata, model, ...) {
		if (!requireNamespace("gstat", quietly = TRUE))
			stop("package gstat required")
		if (isTRUE(obs_extends_window(newdata@observations, 
				SExtent(data@observations))))
			not_meaningful("interpolating over an area larger than the domain")
		var1.pred = gstat::krige0(formula, data@observations, newdata@observations,
			gstat::vgmArea, vgm = model, ...)
		nd = addAttrToGeom(newdata@observations, data.frame(var1.pred), FALSE)
		SField(nd, domain = newdata@domain)
	}
)
#' @rdname interpolate
#' @export
setMethod("interpolate", c("formula", "SLattice", "SLattice"),
	function(formula, data, newdata, model, ...) {
		if (!requireNamespace("gstat", quietly = TRUE))
			stop("package gstat required")
		if (isTRUE(obs_extends_window(newdata@observations, 
				SExtent(data@observations))))
			not_meaningful("interpolating over an area larger than the domain")
		var1.pred = gstat::krige0(formula, data@observations, newdata@observations,
			gstat::vgmArea, vgm = model, ...)
		newdata = addAttrToGeom(newdata@observations, data.frame(var1.pred), FALSE)
		SLattice(newdata)
	}
)
setMethod("spplot", "SField", 
	function(obj,..., cellsAsPoints = 3000, colorkey = TRUE) {
		obs = obj@observations
		if (gridded(obs) && prod(gridparameters(obs)$cells.dim[1:2]) < cellsAsPoints)
			gridded(obs) = FALSE
		spplot(obs, ..., colorkey = colorkey)
	}
)
setMethod("over", c("SField", "SField"), 
	function(x, y, returnList = FALSE, fn = NULL, ...) {
		if (gridded(y@observations) && y@cellsArePoints)
			gridded(y@observations) = FALSE  # converts to Points
		over(x@observations, y@observations, returnList, fn, ...)
	}
)
setMethod("over", c("SField", "SLattice"), 
	function(x, y, returnList = FALSE, fn = NULL, ...) {
		not_meaningful("deriving field values from aggregations")
		NULL
	}
)

#' assess whether function has complete coverage over the domain
#' 
#' fields with point support, defined for all areas in the domain are completely known; this function verifies that this is the case
#' 
#' @param x object of class \link{SField-class}
#' @return logical; TRUE if support is "point" and area size of the observations is identical to that of the domain; this implies we have a coverage with knowledge of all points in the domain.
#' @note what the function does is not sufficient: identical area size does not guarantee that areas are identical
#' @export
full.coverage = function(x) {
	stopifnot(is(x, "SField"))
	if (is.null(x@domain))
		TRUE
	else ((gridded(x@domain@area) && ! x@cellsArePoints) || is(x@domain@area, "SpatialPolygons")) &&
		isTRUE(all.equal(getArea(x@observations), getArea(x@domain@area))) # allows for numerical fuzz
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
	getAreaSP = function(x) { # copied from sp/R/spsample.R:
    		getAreaPolygons = function(x) {
        		holes = unlist(lapply(x@Polygons, function(x) x@hole))
        		areas = unlist(lapply(x@Polygons, function(x) x@area))
        		area = ifelse(holes, -1, 1) * areas
        		area
    		}
    		sum(sapply(x@polygons, getAreaPolygons))
	}
	if (gridded(x))
		areaSpatialGrid(x)
	else if (is(x, "SpatialPolygons"))
		getAreaSP(x)
	else 0.0 # lines, points:
}
