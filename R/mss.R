# general declarations

#.onAttach = function(libname, pkgname) {
#	packageStartupMessage(
#	"mss functions assume grid data reflect raster cell area, not point geometries")
#}

.mssOptions <- new.env(FALSE, globalenv())
assign("mss_error", FALSE, envir = .mssOptions)

#' @importFrom stats aggregate density
#' @import methods
#' @import sp 
#' @import spacetime
#' @import trajectories
NULL

#' assign whether non-meaningful actions should result in an error
#
#' assign whether non-meaningful actions should result in an error
# 
#' @param error logical; should a non-meaningful activity result in an error?
#' @return the logical value assigned
#' @export
mss_error = function(error = TRUE) {
	stopifnot(is.logical(error))
	assign("mss_error", error, envir = .mssOptions)
}

mss = function(x, what = "") {
	stopifnot(is.character(x))
	stopifnot(length(x) == 1)
	msg = paste(x, what)
    if (get("mss_error", envir = .mssOptions))
		stop(msg, call. = FALSE)
	else
		warning(msg, call. = FALSE)
}

not_meaningful = function(x) mss(x, "is not considered meaningful")

maybe_meaningful = function(x) mss(x, "may not be meaningful")

obs_extends_window = function(o, w, convert = FALSE) {
	if (convert && gridded(w@area))
		w@area = as(w@area, "SpatialPolygons")
	if (is(w@area, "SpatialPolygons")) {
		if (convert && gridded(o))
			o = as(o, "SpatialPolygons")
		if (is(o, "SpatialPolygons")) {
			if (!requireNamespace("rgeos", quietly = TRUE)) {
				mss("for comparing aggregation areas/domain/window, package rgeos required")
				return(NA) # can't tell!
			}
			o = rgeos::gUnaryUnion(o)
			w = rgeos::gUnaryUnion(w@area)
			return(! rgeos::gWithin(o, w))
		}
		# if o is SpatialLines, do here something better;
		# for SpatialPoints:
		return(any(is.na(over(o, w@area))))
	}
	NA
}
