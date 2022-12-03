moving_average_plot <- function(mavg_data){
  ggplot(mavg_data,
        aes(x=Index, ymax=Average, ymin=AVG)) +
    geom_ribbon(fill="blue") +
    theme_minimal()
}
