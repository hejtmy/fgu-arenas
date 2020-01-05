#' Loads all data from a single folder into a session object
#'
#' @param folder folder with multiple *.dat files
#'
#' @return session object list
#' @export
#'
#' @examples
load_session <- function(folder){
  files <- list.files(folder, pattern = "*.dat", full.names = TRUE)
  out <- list()
  for(i in 1:length(files)){
     out[[i]] <- load_trial(files[i])
  }
  class(out) <- append(class(out), "session")
  return(out)
}

#' Loads a single *.dat file into a single navr object
#'
#' @param filepath path to a valid *.dat file to load
#'
#' @return navr object
#' @export
#'
#' @examples
load_trial <- function(filepath){
  out <- list()
  out$header <- read_header(filepath)
  out$data <- read_data(filepath)
  out <- as.navr(out)
  return(out)
}


#' Reads data into a unprocessed data.frame
#' 
#' @description unlike the load_trial, the loaded data is without the settings portion and not converted to the navr object
#' @param filepath path to a valid *.dat file
#'
#' @return data.frame
#' @export
#'
#' @examples
read_data <- function(filepath){
  i_last_header <- get_last_header_row(filepath)
  df <- read.table(filepath, skip = i_last_header)
  colnames(df) <- c("frame", "timestamp", "position_x", "position_y", "sectors",
                     "state", "flags", "frameinfo")
  return(df)
}

# HELPERS -----

## HEADER -----

SECTION_BEGINNING <- "\\s+%%BEGIN\\s+"
SECTION_END <- "\\s+%%END\\s+"

read_header <- function(filepath){
  txt <- readLines(filepath, get_last_header_row(filepath))
  section_names <- get_section_names(txt)
  out <- list()
  for(section_name in section_names){
    sec <- read_section(txt, section_name)
    out[[section_name]] <- sec
  }
  return(out)
}

read_section <- function(txt, section_name){
  i_section <- get_section_indices(txt, section_name)
  txt_section <- txt[(i_section[1] + 1):(i_section[2] - 1)]
  out <- list()
  for(line in txt_section){
    if(!grepl("\\s+%(.*)\\s+\\((.*)\\)", line)) next
    if(grepl("\\s+/", line)) next #skipping \\parameters
    name <- gsub("\\s+%(.*)\\s+\\((.*)\\)", "\\1", line)
    name <- gsub("\\.[0-9]$", "", name) #removing the .0
    value <- gsub("\\s+%(.*)\\s+\\((.*)\\)", "\\2", line)
    value <- gsub("^\\s|\\s$", "", value) #striping whitestpace fomr the beginning or end
    out[[name]] <- value 
  }
  return(out)
}

get_section_beginnings <- function(txt){
  return(grep(SECTION_BEGINNING, txt))
}

get_section_names <- function(txt){
  ptr <- paste0(SECTION_BEGINNING, "(.*)")
  section_rows <- txt[grep(ptr, txt)]
  section_names <- sapply(section_rows, function(x){gsub(ptr, "\\1", x)}, 
                          simplify = TRUE, USE.NAMES = FALSE)
  return(section_names)
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