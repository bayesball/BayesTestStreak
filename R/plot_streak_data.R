plot_streak_data <- function(y){
  ab <- 1:length(y)
  event <- ab[as.logical(y)]
  D <- data.frame(Index=event, y=1)
  p <- ggplot(D, aes(Index, y)) + 
     geom_point(shape="|", size=7) +
     ylim(0, 2) +
     theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
    theme(aspect.ratio=3/7)
  p
}
  