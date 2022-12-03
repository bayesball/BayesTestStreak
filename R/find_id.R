find_id <- function(player){
  Names <- unlist(str_split(player, " "))
  filter(People, nameFirst == Names[1],
                 nameLast == Names[2])$retroID
}
