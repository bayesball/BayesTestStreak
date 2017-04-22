mavg_plot <- function(y, width=20){
  moving.average <- function(H, AB, width){
    N <- length(H)
    mavg <- function(j){
      indices <- j : (j + width - 1)
      c(mean(indices), sum(H[indices]) / 
          sum(AB[indices]))
    }
    P <- data.frame(t(sapply(1: (N - width + 1), mavg)))
    names(P) <- c("Index", "Average")
    P
  }
  y_ma <- moving.average(y,
                         rep(1, length(y)), width)
  
  y_ma$AVG <- mean(y)
  
  p <- ggplot(y_ma,
        aes(x=Index, ymax=Average, ymin=AVG)) +
    geom_ribbon(fill="blue") +
    theme_minimal()
  
  p
}
