geometric.plot <- function(y,...){
  nk <- table(y)
  k <- as.numeric(names(nk))
  freq <- as.numeric(nk)
  d <- data.frame(k=k, log.nk=log(freq))
  require(ggplot2)
  p <- ggplot(d, aes(k, log.nk)) + geom_point(size=5) +
    stat_smooth(method=lm, se=FALSE, size=2) +
    stat_smooth(method=loess, se=FALSE, size=2, color="red", ...) +
    theme(axis.text = element_text(size = rel(2))) +
    theme(axis.title = element_text(size = rel(4))) 
  print(p)
}