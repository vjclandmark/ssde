#' Fisher's P-Value Integration
#'
#' Perform Fisher's method for p-value integration.
#'
#' @param p (numeric) P-vales.
#'
#' @return (numeric) The integrated p-value using Fisher's method.
#'
#' @examples
#' 
#' p <- c(0.001, 0.01, 0.1)
#' fisher(p)
#' 
#' @author DING, HONGXU (dinghongxu1130@gmail.com)
#'
#' @importFrom stats pchisq
#'
#' @export

fisher <- function(p){
  if (length(p) < 2) stop("must have at least 2 p values")
  if (sum(p > 1 | p <= 0) > 0) stop("p should be in (0, 1]")
  p <- pchisq(-2*sum(log(p)), 2*length(p), lower.tail=FALSE)
  return(p)}

