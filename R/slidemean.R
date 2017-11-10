#' left-aligned sliding mean
#' 
#' @param x numeric; data vector
#' @param w integer; window width
#' @param by integer; step length
#' 
#' @export
#' @examples
#' x <- c(0, 0, 0, 0, 0, 0, 3, 0, 2, 0, 1, 0, 0, 0)
#' plot(x, type="b")
#' lines(slidemean(x, 5))

slidemean <- function(x, w=5, by=1) {
    idx1 <- seq(1, length(x), by=by)
    idx2 <- idx1 + w
    idx2[idx2 > (length(x) + 1)] <- length(x) + 1
    cx <- c(0, cumsum(x))
    (cx[idx2] - cx[idx1]) / w
}

