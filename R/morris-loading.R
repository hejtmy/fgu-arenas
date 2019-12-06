load_experiment <- function(){
  
}

load_header <- function(){
  
}

load_data <- function(filepath){
  i_last_header <- get_last_header_row(filepath)
  out <- read.table(filepath, skip = i_last_header)
  colnames(out) <- c("FrameCount", "1msTimeStamp", "RoomX", "RoomY", "Sectors",
                     "State", "Flags", "FrameInfo")
  return(out)
}

get_last_header_row <- function(filepath){
  lines <- grep("%", readLines(filepath))
  return(max(lines))
}
