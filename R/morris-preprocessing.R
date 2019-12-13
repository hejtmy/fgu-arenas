as.navr <- function(obj_morris){
  obj <- NavrObject()
  obj <- load_position_data(obj, obj_morris$data)
  obj$settings <- obj_morris$header
  obj <- prepare_navr(obj)
  return(obj)
}

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