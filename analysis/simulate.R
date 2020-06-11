source("utilities.r")

# load data
alldata <- get_all_data(agg = T, delete = F)

# run simulations
simulate_treatment("partial", "Partial Monitoring", 2)
simulate_treatment("full", "Full Monitoring", 3)
