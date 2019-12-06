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


# HELPERS -----

## HEADER -----

SECTION_BEGINNING <- "\\s+%%BEGIN\\s+"
SECTION_END <- "\\s+%%END\\s+"

read_header <- function(filepath){
  txt <- readLines(filepath, get_last_header_row(filepath))
  i_sections_start <- get_section_beginnings(txt)
  out <- list()
  for(i_start in i_section_starts){
    read_section(txt, i_start)
  }
  return(out)
}

read_section <- function(txt, i_start){
  section_name <- get_section_name(txt[i_start])
  i_section <- get_section_indices(txt, section_name)
}

get_section_beginnings <- function(txt){
  return(grep(SECTION_BEGINNING, txt))
}

get_section_name <- function(section_row){
  ptr <- paste0(SECTION_BEGINNING, "(.*)")
  return(gsub(ptr, "\\1", section_row))
}

get_section_indices <- function(txt, section_name){
  ptr_start <- paste0(SECTION_BEGINNING, section_name)
  i_start <- grep(ptr_start, txt)
  ptr_end <- paste0(SECTION_END, section_name)
  i_end <- grep(ptr_end, txt)
  return(c(i_start, i_end))
  
}

get_last_header_row <- function(filepath){
  lines <- grep("%", readLines(filepath))
  return(max(lines))
}

get_header_indices <- function(filepath){
  return(c(1, get_last_header_row(filepath)))
}