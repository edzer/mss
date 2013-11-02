
# augmented krige method for point pattern; shows warning message, if krige is applied to point pattern
setMethod("krige", c("formula", "PointPatternDataFrame"), 
	function(formula, locations, ...){
		warning("interpolating point patterns is not considered meaningful")
		krige(formula, as(locations, "SpatialPointsDataFrame"), ...)
	}
)
