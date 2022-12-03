plot_streak_data <- function(streak_data){
  # assume input is a data frame
  Nrow <- dim(streak_data)[1]
  ggplot(filter(streak_data, Outcome == 1),
         aes(N, Outcome)) + 
     geom_point(shape="|", size=7) +
     ylim(0, 2) + 
     geom_path(data=data.frame(x=c(0, Nrow + 1, 
                               Nrow + 1, 0, 0),
                          y=c(1.2, 1.2, .8, .8, 1.2)),
               aes(x, y), color="blue") +
     theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
    theme(aspect.ratio=3/7)
}
  