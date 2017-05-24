streak_data <- function(pid, pbpdata, eventcode, AB=FALSE){
  Flag <- 0
  F1 <- is.numeric(eventcode)
  if(F1 == FALSE){
    F2 <- eventcode %in% c("H", "SO", "HR", "OB")
    if(F2 == FALSE){
    print("Invalid event code")
    Flag <- 1}
  }
  if(F1 == TRUE) ecode <- eventcode
  if(eventcode[1] == "H") ecode <- 20:23
  if(eventcode[1] == "SO") ecode <-  3
  if(eventcode[1] == "HR") ecode <- 23
  if(eventcode[1] == "OB") ecode <- c(14:16, 20:23)

  if(Flag == 0){
     pbpdata <- dplyr::mutate(pbpdata,
                      Date=ymd(str_sub(GAME_ID, 4, 11)),
                      Game=str_sub(GAME_ID, 12, 12))
     dplyr::filter(pbpdata, 
           BAT_ID == pid, BAT_EVENT_FL == TRUE,
           EVENT_CD != 17) %>%
      dplyr::arrange(Date, Game) -> d
     if(AB==TRUE)
       d <- dplyr::filter(d, AB_FL==TRUE)
     ifelse(d$EVENT_CD %in% ecode, 1, 0)
  }
}
