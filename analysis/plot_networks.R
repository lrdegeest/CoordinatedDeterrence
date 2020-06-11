source("helper_functions.r")
data <- get_all_data(agg=T)
pdf("pm_graph.pdf", height = 5, width=6); plot_treatment(data$partial); dev.off()
pdf("fm_graph.pdf", height = 5, width=6); plot_treatment(data$full); dev.off()