#' Converts the morris object to a navr object for easy plotting of paths and heatmaps
#'
#' @param obj_morris 
#'
#' @return
#' @export
#'
#' @examples
as.navr <- function(obj_morris){
  obj <- NavrObject()
  obj <- load_position_data(obj, obj_morris$data)
  obj$settings <- obj_morris$header
  obj <- prepare_navr(obj)
  return(obj)
}

#' Combines data from multiple trials into a single navr object
#'
#' @param session session object as loaded by load_session
#'
#' @return single navr object with all position data together. 
#' Adds an iTrial column into the data for each separate trial. Strips away the settings
#' @export
#'
#' @examples
combine_data_from_session <- function(session){
  df <- data.frame()
  for(i in 1:length(session)){
    trial <- as.navr(session[[i]])
    df_trial <- trial$data
    df_trial$iTrial <- i
    df <- rbind(df, df_trial)
  }
  obj <- NavrObject()
  obj$data <- df
  return(obj)
}