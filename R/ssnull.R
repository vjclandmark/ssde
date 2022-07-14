#' @keywords internal

ssnull <- function(x, y, gene, neighbor=200, epoch=1000, seed=1){
  dist <- abs(x - x[gene])
  genes <- names(sort(dist[dist > 0], decreasing=F)[1:neighbor])
  set.seed(seed)
  xnull <- structure(cbind(sample(x[genes], epoch, replace=TRUE),
                           sample(y[genes], epoch, replace=TRUE)),
                     dimnames=list(NULL, c("x", "y")))
  # null model from x expression
  
  dist <- abs(y - y[gene])
  genes <- names(sort(dist[dist > 0], decreasing=F)[1:neighbor])
  set.seed(seed)
  ynull <- structure(cbind(sample(x[genes], epoch, replace=TRUE),
                           sample(y[genes], epoch, replace=TRUE)),
                     dimnames=list(NULL, c("x", "y")))
  # null model from y expression
  
  null <- list(xnull=xnull, ynull=ynull)
  return(null)}
