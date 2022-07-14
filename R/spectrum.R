#' Color Spectrum
#'
#' Create a spectrum of colors corresponded to a numeric vector.
#'
#' @param x (numeric) The numeric vector to be represented as a color spectrum.
#'
#' @param pcol (character) Colors for positive values.
#'
#' @param ncol (character) Colors for negative values.
#'
#' @param range (numeric) Xs with freater absolute value will be set as the "maximum" pcol/ncol.
#' 
#' @param resolution (numeric) The number of interpolations for pcol/ncol.
#'
#' @return (character) The color spectrum vector.
#'
#' @examples
#' 
#' x <- 1:21
#' y <- 1:21
#' color <- spectrum(-10:10)
#' plot(x, y, col = color, pch = 16)
#' 
#' @author DING, HONGXU (dinghongxu1130@gmail.com)
#'
#' @importFrom grDevices colorRampPalette
#'
#' @export

spectrum <- function(x, pcol=c("grey", "red"), ncol=c("grey", "green"), range=5, resolution=100){
  x[abs(x) > range] <- range * sign(x)[abs(x) > range]
  x <- sign(x)*(abs(x)/range)*resolution
  color <- rep(NA, length(x))
  color[x >= 0] <- colorRampPalette(pcol)(resolution+2)[floor(abs(x[x >= 0]))+1]
  color[x <= 0] <- colorRampPalette(ncol)(resolution+2)[floor(abs(x[x <= 0]))+1]
  return(color)}

