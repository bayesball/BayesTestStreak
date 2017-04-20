streak_data <- function(pbpdata, pid, eventcode, AB=FALSE){
  pbpdata <- mutate(pbpdata,
                      Date=ymd(str_sub(GAME_ID, 4, 11)),
                      Game=str_sub(GAME_ID, 12, 12))
  filter(pbpdata, 
           BAT_ID == pid, BAT_EVENT_FL == TRUE) %>%
      arrange(Date, Game) -> d
  if(AB==TRUE)
    d <- filter(d, AB_FL==TRUE)
    
  ifelse(d$EVENT_CD %in% eventcode, 1, 0)
}
