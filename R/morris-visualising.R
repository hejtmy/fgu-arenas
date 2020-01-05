#' Plots a single trial with animal's path
#'
#' @param obj navr object
#'
#' @return ggplot object
#' @export
#'
#' @examples
plot_trial <- function(obj){
  plt <- ggplot() + 
    theme_void() + 
    navr::geom_navr_circle(ARENA_CENTER, ARENA_RADIUS, size = 2, color = "blue")
  plt <- plt + geom_navr_path(obj)
  return(plt)
}

#' Plots combined heatmap from multiple trials
#'
#' @param session session object as loaded by load_session
#'
#' @return
#' @export
#'
#' @examples
plot_heatmap.session <- function(session){
  # Combining objects
  obj <- combine_data_from_session(session)
  plt <- plot_heatmap.trial(obj)
  return(plt)
}

#' Title
#'
#' @param obj 
#'
#' @return
#' @export
#'
#' @examples
plot_heatmap.trial <- function(obj){
  plt <- ggplot(obj$data, aes(position_x, position_y)) + 
    stat_density2d(aes(fill=..level..), bins = 100, geom="polygon") + 
    scale_fill_gradientn(colours=rev(rainbow(100, start=0, end=0.75))) +
    lims(x = c(-50,300),y = c(-50,300)) +
    navr::geom_navr_circle(ARENA_CENTER, ARENA_RADIUS, size = 2, color = "white") +
    theme_void() +
    guides(fill=FALSE)
  return(plt)
}