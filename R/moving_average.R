moving_average <- function(streak_data, width=20){
  library(zoo)
  streak_data %>% 
    mutate(N = row_number()) -> streak_data
  
  streak_data %>% 
      mutate(Index = rollmean(N, width, fill = NA),
             Average = rollmean(Outcome, width, fill = NA),
             AVG = mean(Outcome)) 
}
