# general declarations

.mssOptions <- new.env(FALSE, globalenv())
assign("mss_error", FALSE, envir = .mssOptions)

#' @import methods
#' @importFrom stats aggregate
#' @import sp 
#' @import gstat
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

mss = function(x, what) {
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
