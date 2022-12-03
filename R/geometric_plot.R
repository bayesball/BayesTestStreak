geometric_plot <- function(spacings_df){
  
  spacings_df %>% 
    group_by(Spacing) %>% 
    summarize(N = n()) -> spacings_table
  
 ggplot(spacings_table, 
            aes(Spacing, log(N))) + 
            geom_point(size=3) +
    stat_smooth(method=lm, 
                se=FALSE, 
                formula = "y ~ x",
                linewidth=1, 
                color="black") +
    stat_smooth(method=loess, 
                se=FALSE, 
                linewidth=2) +
    theme_minimal() +
    theme(axis.text = element_text(size = rel(1.5))) +
    theme(axis.title = element_text(size = rel(1.5))) +
    xlab("Spacing") + 
    ylab("Log Frequency")
}