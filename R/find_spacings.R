find_spacings = function(streak_data, value = 0) {
  # input is a data frame
  runs <- rle(streak_data$Outcome)
  data.frame(Spacing = 
          runs$lengths[runs$values == value]) %>% 
    mutate(N = row_number(),
           BAT_ID = streak_data$BAT_ID[1]) %>% 
    select(BAT_ID, N, Spacing)
}
