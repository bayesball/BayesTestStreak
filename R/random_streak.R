random_streak <- function(n = 50, p = 0.5, iter = 1000,
                          stat = max){
  simulating <- function(){
    coinflips <- rbinom(n, size = 1, prob = p)
    out <- rle(1 - coinflips)
    stat(out$lengths[out$values == 1])
  }
  many_stats <- replicate(1000, simulating())
  
  ggplot(data.frame(Statistic=many_stats), 
              aes(Statistic)) +
    geom_histogram(color = "white", fill="brown",
                   bins = 12) +
    ylab("Frequency") +
    xlab(paste(deparse(substitute(stat)), "ofer")) +
    ggtitle(paste("Random Coin: (n = ", n, ", p = ", p,")")) +
    theme(
      plot.title = element_text(
        colour = "blue",
        size = 18,
        hjust = 0.5,
        vjust = 0.8,
        angle = 0
      ),
      text=element_text(size=18)
    )
}
