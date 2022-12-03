permutation_test = function(streak_data, 
                            ITER = 1000){
  
  measure <- function(zero_one_vector){
    out <- rle(zero_one_vector)
    spacings <- out$lengths[out$values == 0]
    sum(spacings ^ 2)
  }
  
  random_measures <- replicate(ITER, 
            measure(sample(streak_data$Outcome)))
  
  observed <- measure(streak_data$Outcome)  
  mean(random_measures >= observed)  
}
