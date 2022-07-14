#' Single Sample Differential Expression
#'
#' The main function for performing single sample differential expression analysis.
#'
#' @param x (numeric) X-coordinates.
#'
#' @param y (numeric) Y-coordinates.
#'
#' @param k (numeric) The k for the target line y=kx+h.
#'
#' @param h (numeric) The h for the target line y=kx+h.
#' 
#' @param neighbor (numeric) The number of neighboring genes for calculating empirical p-values.
#' 
#' @param epoch (numeric) The number of permutations for building null models for calculating empirical p-values.
#' 
#' @return (list) A list containing the k, h, d and p values, as the ssde result.
#'
#' @examples
#' 
#' library(bcellViper)
#' data(bcellViper)
#' x <- dset@assayData$exprs[, "GSM44113"]
#' y <- dset@assayData$exprs[, "GSM44246"]
#' res <- ssde(x, y, k=1, h=0, epoch=100)
#' 
#' @author DING, HONGXU (dinghongxu1130@gmail.com)
#'
#' @importFrom utils txtProgressBar
#' @importFrom stats ecdf
#' 
#' @export

ssde <- function(x, y, k=NULL, h=NULL, neighbor=200, epoch=1000, seed=1){
  if (is.null(names(x)) | is.null(names(y))) stop("x and/or y gene names missing")
  if (sum(names(x) != names(y)) > 0) stop("require matching x and y")
  # check expression profiles
  
  if (is.null(k) | is.null(h)){
    model <- ssfit(x, y)
    k <- model$k
    h <- model$h}
  # y=kx+h baseline for dispersing gene estimation
  
  dvalue <- d2l(x, y, k, h)
  # distance towards the baseline
  
  pb <- txtProgressBar(0, length(dvalue), style=3)
  pvalue <- numeric()
  for (i in 1:length(dvalue)){
    setTxtProgressBar(pb, i)
    null <- ssnull(x, y, names(dvalue)[i], neighbor, epoch, seed)
    px <- d2l(null$xnull[, "x"], null$xnull[, "y"], k, h)
    px <- 1-ecdf(px)(dvalue[i])
    py <- d2l(null$ynull[, "x"], null$ynull[, "y"], k, h)
    py <- 1-ecdf(py)(dvalue[i])
    if (px == 0 | py == 0){
      pvalue[names(dvalue)[i]] <- 0
    }else{
      pvalue[names(dvalue)[i]] <- fisher(c(px, py))}}
  # calculate pvalue
  
  ssde <- list(k=k, h=h, dvalue=dvalue, pvalue=pvalue)
  return(ssde)}
