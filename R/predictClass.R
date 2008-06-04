`predictClass` <-
function(scores) {
  cll <- factor(colnames(scores))
  f1 <- function(x) {
    return(cll[which(x == max(x))])
  }
  f2 <- function(x) {
    return(x[which(x == max(x))])
  }
  cl.pred <- apply(scores, 1, f1)
  cl.pred <- factor(cl.pred, levels=levels(cll))
  cl.sc <- apply(scores, 1, f2)
  list(class=cl.pred, score=cl.sc)
}

