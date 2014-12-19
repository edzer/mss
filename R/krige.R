INCM = function(x) warning(paste(x, "is not considered meaningful"))

# augmented krige method for point pattern; shows warning message, 
# if krige is applied to point pattern
setMethod("krige", c("formula", "PointPatternDataFrame"), 
	function(formula, locations, ...){
		INCM("interpolating point patterns")
		krige(formula, as(locations, "SpatialPointsDataFrame"), ...)
	}
)
