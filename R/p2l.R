#' Projection to Line
#'
#' In 2D space, calculate the projection coordinates of points to a line.
#'
#' @param x (numeric) X-coordinates.
#'
#' @param y (numeric) Y-coordinates.
#'
#' @param k (numeric) The k for the target y=kx+h.
#'
#' @param h (numeric) The h for the target y=kx+h.
#' 
#' @return (numeric) Projection coordinates on the target line.
#'
#' @examples
#' 
#' x <- c(1, 2)
#' y <- c(2, 3)
#' k <- 1
#' h <- 0
#' pj <- p2l(x, y, k, h)
#' plot(x, y, xlim = c(0, 5), ylim = c(0, 5))
#' abline(h, k, NULL, NULL)
#' arrows(x[1], y[1], pj[1, 1], pj[1, 2])
#' arrows(x[2], y[2], pj[2, 1], pj[2, 2])
#' 
#' @author DING, HONGXU (dinghongxu1130@gmail.com)
#'
#' @export

p2l <- function(x, y, k, h){
  xx <- (y + x/k - h)/(k + 1/k)
  yy <- k*xx + h
  point <- cbind("x"=xx, "y"=yy)
  return(point)}
