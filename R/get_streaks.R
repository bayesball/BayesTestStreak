get_streaks <- function(y){
  out <- rle(y)
  out$lengths[out$values == 1]
}
