filepath <- "example-data/LY/LY_SWIM1/Day04ex.fin.sk1R05-S1_Room.dat"

exp <- load_trial(filepath)
obj <- as.navr(exp)

session <- load_session("example-data/LY/LY_SWIM1/")

plot_trial(session[[1]])
