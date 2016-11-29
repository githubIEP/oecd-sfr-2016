# # Example preprocessing script.
filename <- "./data/sfr model data/state forces.csv"
state.forces <- read.csv(filename)
raw.data$state.forces <- state.forces
rmExcept("raw.data") 
