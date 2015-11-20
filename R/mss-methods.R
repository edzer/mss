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
# SField:
setMethod("[[", c("SField", "ANY", "missing"), double_bracket)
setReplaceMethod("[[", c("SField", "ANY", "missing", "ANY"), double_bracket_repl)
setMethod("$", "SField", function(x, name) x@observations[[name]])
setMethod("[", "SField", 
	function(x, i, j, ..., drop = TRUE) SField(
		x@observations[i, j, ..., drop = drop], x@domain, 
		cellsArePoints = x@cellsArePoints))
setAs("SField", "data.frame", function(from) as(from@observations, "data.frame"))

# SLattice
setMethod("[[", c("SLattice", "ANY", "missing"), double_bracket)
setReplaceMethod("[[", c("SLattice", "ANY", "missing", "ANY"), double_bracket_repl)
setMethod("$", "SLattice", function(x, name) x@observations[[name]])
setMethod("spplot", "SLattice", function(obj,...) spplot(obj@observations, ...))
setMethod("[", "SLattice", function(x, i, j, ..., drop = TRUE) 
	SLattice(x@observations[i, j, ..., drop = drop]))
setAs("SLattice", "data.frame", function(from) as(from@observations, "data.frame"))

# SObjects
setMethod("[[", c("SObjects", "ANY", "missing"), double_bracket)
setReplaceMethod("[[", c("SObjects", "ANY", "missing", "ANY"), double_bracket_repl)
setMethod("$", "SObjects", function(x, name) x@observations[[name]])
setMethod("spplot", "SObjects", function(obj,...) spplot(obj@observations, ...))
setMethod("[", "SObjects", function(x, i, j, ..., drop = TRUE) 
	SLattice(x@observations[i, j, ..., drop = drop]))
setAs("SObjects", "data.frame", function(from) as(from@observations, "data.frame"))
