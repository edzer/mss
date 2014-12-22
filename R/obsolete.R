# the following classes are proof-of-concept, and are obsolete

#' The GeostatisticalDataFrame class
#'
#' This class is obsolete.
#'
#' This line and the next ones go into the details.
#' This line thus appears in the details as well.
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{bbox}}{see \link[sp]{SpatialPointsDataFrame}}
#'    \item{\code{coords}}{see \link[sp]{SpatialPointsDataFrame}}
#'    \item{\code{coords.nrs}}{see \link[sp]{SpatialPointsDataFrame}}
#'    \item{\code{data}}{see \link[sp]{SpatialPointsDataFrame}}
#'    \item{\code{proj4string}}{see \link[sp]{SpatialPointsDataFrame}}
#'    \item{\code{window}}{object of class \link[sp]{SpatialPolygons}}
#'  }
#'
#' @note You can still add notes
#' @name GeostatisticalDataFrame 
#' @rdname GeostatisticalDataFrame
#' @aliases GeostatisticalDataFrame-class
#' @exportClass GeostatisticalDataFrame
#' @author Christoph Stasch
setClass("GeostatisticalDataFrame", contains = "SpatialPointsDataFrame",
	representation(window = "SpatialPolygons"))
	# should the window really be Polygons?

####
# Point pattern represents a (marked) point pattern variable
#
setClass("PointPatternDataFrame", contains = "SpatialPointsDataFrame",
	representation(window = "SpatialPolygons"))

###
# LatticeData represents a lattice variable
#
setClass("LatticeDataFrame", contains = "SpatialPolygonsDataFrame",
	representation(window = "SpatialPolygons"))
