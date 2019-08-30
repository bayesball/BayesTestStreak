streak_game_data <- function(pbpdata, pid){
  pbpdata %>%
    filter(BAT_EVENT_FL == TRUE,
         BAT_ID == pid) %>%
    group_by(GAME_ID) %>%
    summarize(AB = sum(AB_FL),
            H = sum(H_FL > 0),
            Hit = ifelse(H > 0, 1, 0))  %>%
    mutate(Date = substr(GAME_ID, 4, 12)) %>%
    arrange(Date) %>% 
    select(Date, AB, H, Hit)
}






