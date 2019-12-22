library(fgu.morris)
filepath <- "example-data/LY/LY_SWIM1/Day04ex.fin.sk1R05-S1_Room.dat"

exp <- load_trial(filepath)
obj <- as.navr(exp)

session <- load_session("example-data/MK-LY/MK-LY_SWIM1/")

plot_trial(session[[5]])

all <- combine_data_from_session(session)

ggplot(all$data, aes(position_x, position_y)) + 
  stat_density2d(aes(fill=..level..), bins = 100, geom="polygon") + 
  scale_fill_gradientn(colours=rev(rainbow(100, start=0, end=0.75))) +
  lims(x = c(-50,300),y = c(-50,300)) + theme_void() + guides(fill=FALSE)
