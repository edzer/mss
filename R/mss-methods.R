# Helper:
double_bracket = function(x, i, j, ...) {
	if (!("data" %in% slotNames(x@observations)))
		stop("no [[ method for object without attributes")
	x@observations@data[[i]]
}
double_bracket_repl = function(x, i, j, value) {
	if (!("data" %in% slotNames(x@observations)))
		stop("no [[ method for object without attributes")
	if (is.character(i) && 
			any(!is.na(match(i, dimnames(coordinates(x@observations))[[2]]))))
		stop(paste(i, "is already present as a coordinate name!"))
	x@observations@data[[i]] <- value
	x
}
# SpatialField:
setMethod("[[", c("SpatialField", "ANY", "missing"), double_bracket)
setReplaceMethod("[[", c("SpatialField", "ANY", "missing", "ANY"), double_bracket_repl)
setMethod("$", "SpatialField", function(x, name) x@observations[[name]])
setMethod("[", "SpatialField", 
	function(x, i, j, ..., drop = TRUE) SpatialField(
		x@observations[i, j, ..., drop = drop], x@domain, 
		cellsArePoints = x@cellsArePoints))
setAs("SpatialField", "data.frame", function(from) as(from@observations, "data.frame"))

# SpatialAggregation
setMethod("[[", c("SpatialAggregation", "ANY", "missing"), double_bracket)
setReplaceMethod("[[", c("SpatialAggregation", "ANY", "missing", "ANY"), double_bracket_repl)
setMethod("$", "SpatialAggregation", function(x, name) x@observations[[name]])
setMethod("spplot", "SpatialAggregation", function(obj,...) spplot(obj@observations, ...))
setMethod("[", "SpatialAggregation", function(x, i, j, ..., drop = TRUE) 
	SpatialAggregation(x@observations[i, j, ..., drop = drop]))
setAs("SpatialAggregation", "data.frame", function(from) as(from@observations, "data.frame"))

# SpatialEntities
setMethod("[[", c("SpatialEntities", "ANY", "missing"), double_bracket)
setReplaceMethod("[[", c("SpatialEntities", "ANY", "missing", "ANY"), double_bracket_repl)
setMethod("$", "SpatialEntities", function(x, name) x@observations[[name]])
setMethod("spplot", "SpatialEntities", function(obj,...) spplot(obj@observations, ...))
setMethod("[", "SpatialEntities", function(x, i, j, ..., drop = TRUE) 
	SpatialAggregation(x@observations[i, j, ..., drop = drop]))
setAs("SpatialEntities", "data.frame", function(from) as(from@observations, "data.frame"))
