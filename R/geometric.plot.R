geometric.plot <- function(y,...){
  nk <- table(y)
  k <- as.numeric(names(nk))
  freq <- as.numeric(nk)
  d <- data.frame(k=k, log.nk=log(freq))
  p <- ggplot(d, aes(k, log.nk)) + geom_point(size=3) +
    stat_smooth(method=lm, se=FALSE, 
                size=1, color="black") +
    stat_smooth(method=loess, se=FALSE, size=2, ...) +
    theme_minimal() +
    theme(axis.text = element_text(size = rel(1.5))) +
    theme(axis.title = element_text(size = rel(1.5))) +
    xlab("Spacing") + ylab("Log Frequency")
  p
}