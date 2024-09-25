#' The Square Function
#'
#' @param v vector of data
#'
#' @return The mode of the data.
#' @export
#'
#' @examples
#' getmode(1:10)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
