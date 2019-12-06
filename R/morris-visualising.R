#' Title
#'
#' @param obj 
#'
#' @return
#' @export
#'
#' @examples
plot_trial <- function(obj){
  plt <- ggplot() + 
    theme_void() + 
    navr::geom_navr_circle(ARENA_CENTER/2, ARENA_RADIUS, size = 2, color = "blue")
  plt <- plt + geom_navr_path(obj)
  return(plt)
}