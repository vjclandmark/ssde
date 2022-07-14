#' Distance to Line
#'
#' In 2D space, calculate the distance from points to a target line.
#'
#' @param x (numeric) X-coordinates.
#'
#' @param y (numeric) Y-coordinates.
#'
#' @param k (numeric) The k for the target line y=kx+h.
#'
#' @param h (numeric) The h for the target line y=kx+h.
#' 
#' @return (numeric) Distance values.
#'
#' @examples
#' 
#' x <- c(1, 2)
#' y <- c(1, 3)
#' d2l(x, y, 1, 0)
#' 
#' @author DING, HONGXU (dinghongxu1130@gmail.com)
#'
#' @export

d2l <- function(x, y, k=1, h=0){
  d <- abs((x - (y - h)/k))/sqrt(1 + k^2) * k
  return(d)}
