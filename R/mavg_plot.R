mavg_plot <- function(y, width=20){
  moving.average <- function(H, AB, width){
    N <- length(H)
    mavg <- function(j){
      indices <- j : (j + width - 1)
      c(mean(indices), sum(H[indices]) / 
          sum(AB[indices]))
    }
    P <- data.frame(t(sapply(1: (N - width + 1), mavg)))
    names(P) <- c("AB", "Average")
    P
  }
  y_ma <- moving.average(y,
                         rep(1, length(y)), width)
  
  AVG <- mean(y)
  y_ma$Deviation <- y_ma$Average - AVG
  p <- ggplot(y_ma, aes(x=AB, y=Deviation)) +
    geom_area(fill="blue") + 
    geom_hline(yintercept=0) +
    theme_minimal()
  p
}
