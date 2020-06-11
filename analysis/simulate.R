rm(list = ls())
source("~/Desktop/notebook/research/dissertation/third_paper/enforcement-networks/analysis/helper_functions.r")
source("~/Desktop/notebook/research/dissertation/third_paper/enforcement-networks/analysis/simulate_networks/simulate_helpers.R")
source("~/Desktop/notebook/research/dissertation/third_paper/enforcement-networks/analysis/simulate_networks/erdos_renyi.R")

# load data
alldata <- get_all_data(agg = T, delete = F)

# run simulations
simulate_treatment("zero", "Zero Monitoring", 1)
simulate_treatment("partial", "Partial Monitoring", 2)
simulate_treatment("full", "Full Monitoring", 3)


# sanity check: graph value for observed and reshuffled graph should be equal
g <- alldata$full$g_2$agg_graph
g <- delete.edges(g, which(E(g)$weight==0))
in_degree = graph.strength(g, mode = "in")
out_degree = graph.strength(g, mode = "out")
sim_g <- sample_degseq(out_degree, in_degree, method = "simple")
sum(get.adjacency(g, attr="weight")) == sum(get.adjacency(sim_g))
graph.strength(sim_g, mode="out") == out_degree
graph.strength(sim_g, mode="in") == in_degree
