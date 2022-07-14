#' @importFrom stats lm
#' 
#' @keywords internal

ssfit <- function(x, y){
  model <- lm(y ~ x)
  model <- list(k=structure(model$coefficients[2], names=NULL),
                h=structure(model$coefficients[1], names=NULL))
  return(model)}

