plot_streak_data <- function(y){
  N <- length(y)
  ab <- 1:N
  event <- ab[as.logical(y)]
  D <- data.frame(Index=event, y=1)
  p <- ggplot(D, aes(Index, y)) + 
     geom_point(shape="|", size=7) +
     ylim(0, 2) + 
     geom_path(data=data.frame(x=c(0, N + 1, N + 1, 0, 0),
                          y=c(1.2, 1.2, .8, .8, 1.2)),
               aes(x, y), color="blue") +
     theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
    theme(aspect.ratio=3/7)
  p
}
  