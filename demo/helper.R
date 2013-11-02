##############################
### SECTION: helper functions
##############################

#
# Function for checking disjointness of polygons in SpatialPolygons
#
checkDisjointness <- function(spPolygons) { 
	if (class(spPolygons)!="SpatialPolygons")
		stop("Argument for function checkDisjointness needs to be of type SpatialPolygon!")
	cat("Checking disjointness of polygons.\n")
	#iterate over all polygons (except the last one) and check whether next neighbour is disjoint
	i <- 1
	length <- length(spPolygons@polygons)
	while (i<length) {
		polygon1 <- spPolygons[i,]
		tmpPolys <- spPolygons[-(1:i),]
		j <- 1
		tmpLength <- length(tmpPolys@polygons)
		while (j <= tmpLength){
			polygon2 <-tmpPolys[j]
			#allow borders to overlap; only interiors have to be disjoint
			if (!(gDisjoint(polygon1,polygon2) || gTouches(polygon1,polygon2))){
				cat("Polygons are not disjoint.")
				return(FALSE)
			}
			j <- j+1
		}
		i <- i+1
	} 
	cat("Polygons are disjoint.")
	return(TRUE)
}

#
# function for checking containment of points in SpatialPoints in polygons in SpatialPolygons
#
checkContainment <- function(spPolygons,spPoints) {
	cat("Checking containment of points in polygons.\n")
	if (class(spPolygons) != "SpatialPolygons")
		stop("Argument for function checkDisjointness needs to be of type SpatialPolygon!")
	if (class(spPoints)!="SpatialPoints")
		stop("Argument for function checkDisjointness needs to be of type SpatialPoints!")
	if (gContains(spPolygons,spPoints)) {
		cat("All points are contained in polygons.\n")
		return(TRUE)
	} else {
		cat("There are points that are not contained in polygons.\n")
		return(FALSE)
	}
}
