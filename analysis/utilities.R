# load and shaping network data -------------------------------------------
get_sanctions <- function(dat){
  dat <- read.csv(dat)
  names(dat) <- tolower(names(dat))
  ## recode subject numbers
  dat$sender <- with(dat, 
                     ifelse(
                       sender > 8, 
                       sender - 8,
                       sender
                     )
  )
  dat$receiver <- with(dat, 
                       ifelse(
                         receiver > 8, 
                         receiver - 8,
                         receiver
                       )
  )
  ## rename "Points" to "Weight"
  names(dat)[names(dat)=="points"] <-  "weight"
  # return
  return(dat)
}

get_atts <- function(dat,treatment, treatment_n){
  require(foreign)
  dat <- read.dta(dat)
  # subset data
  keep_cols <- c("treatment", "period","subject", "group", "type", "h", "ip", "fp", "typep")
  dat <- dat[dat$treatment == treatment, colnames(dat) %in% keep_cols]
  ## return subject numbers to 1-8
  dat$subject <- dat$subject - (dat$group*100)
  dat$subject <- with(dat, 
                    ifelse(
                      subject > 8, 
                      subject - 8,
                      subject
                    )
  )
  ## return group numbers 1 - 4
  dat$group <- dat$group - 10*treatment_n
  ## recode type to 1 (insider) or 2 (outsider)
  dat$type <- with(dat, 
                 ifelse(
                   type == "Insider",
                   1,
                   2
                 )
  )
  ## return 
  dat <- droplevels(dat)
  return(dat)
}

get_treatment <- function(sanctions, attributes, treatment, treatment_n){
  dat_list <- list()
  # sanctions data
  dat_list[["dat.s"]] <- get_sanctions(sanctions)
  # attributes
  dat_list[["dat.atts"]] <- get_atts(attributes, treatment, treatment_n)
  return(dat_list)
}

make_graphs <- function(dat_list, grp_n, treatment_abbrv, agg, delete, delete_type){
  dat.s <- dat_list[[1]]
  dat.s <- dat.s[dat.s$group == grp_n,]
  dat.atts <- dat_list[[2]]
  dat.atts <- dat.atts[dat.atts$group == grp_n,]
  graph_list <- list()
  require(igraph)
  if(agg==T){
    ## aggregate graph
    agg_graph <- simplify(graph.data.frame(dat.s[c("sender", "receiver", "weight")]))
    ### add attributes type, h, ip
    require(data.table)
    summary.atts <- data.table(dat.atts)[,list(h = sum(h), ip=sum(ip), fp=sum(fp) ), by = .(treatment, subject, type)]
    V(agg_graph)$type = summary.atts$type[match(V(agg_graph)$name, summary.atts$subject)]
    V(agg_graph)$h = summary.atts$h[match(V(agg_graph)$name, summary.atts$subject)]
    V(agg_graph)$ip = summary.atts$ip[match(V(agg_graph)$name, summary.atts$subject)]
    V(agg_graph)$fp = summary.atts$ip[match(V(agg_graph)$name, summary.atts$subject)]
    #E(agg_graph)$type <- V(agg_graph)$type[ends(agg_graph, es=E(agg_graph), names=F)[,2]]
    if(delete==T){
      agg_graph <- delete.edges(agg_graph, which(E(agg_graph)$type == delete_type))
    }
    agg_graph <- delete.edges(agg_graph, which(E(agg_graph)$weight == 0)) 
    ### add to list
    graph_list$agg_graph <- agg_graph
  }
  ## create graph for each time step
  niter <- length(unique(dat.s$period))
  for(i in 1:niter){
    # get the data ready
    group.p <- dat.s[dat.s$period == i,]
    group.p.edgelist <- group.p[c("sender", "receiver", "weight")]
    # make the graph
    graph <- graph.data.frame(group.p.edgelist)
    graph <- delete.edges(graph, which(E(graph)$weight == 0)) 
    # ADD ATTRIBUTES
    group.atts <- dat.atts[dat.atts$period == i,]
    ## add subject type
    V(graph)$type <- group.atts$type[match(V(graph)$name, group.atts$subject)]
    ## add harvest 
    V(graph)$h <- group.atts$h[match(V(graph)$name, group.atts$subject)]
    ## add initial payoff
    V(graph)$ip <- group.atts$ip[match(V(graph)$name, group.atts$subject)]
    ## add final payoff
    V(graph)$fp <- group.atts$fp[match(V(graph)$name, group.atts$subject)]
    ## add monitoring 
    V(graph)$mon <- as.numeric(group.atts$typep[match(V(graph)$name, group.atts$subject)])
    ## add type as edge attribute via ends() (note: 2nd column of matrix is edge target, first column is origin)
    E(graph)$type <- V(graph)$type[ends(graph, es=E(graph), names=F)[,2]]
    if(delete==T){
      graph <- delete.edges(graph, which(E(graph)$type == delete_type))
    }
    # store the graph and add to list
    new_entry <- paste(paste(treatment_abbrv, ".g", sum(dat.s$group)/nrow(dat.s), sep=""), i, sep = ".")
    graph_list[[new_entry]] <- graph
  }
  # return list of graphs
  return(graph_list)
}

load_data <- function(sanctions, attributes, treatment, treatment_abbrv, treatment_n, agg, delete, delete_type){
  dat <- get_treatment(sanctions, attributes, treatment, treatment_n)
  graphs_all_groups <- list()
  suppressWarnings(
    for (i in 1:4){
      group = paste0("g_",i)
      graphs_all_groups[[group]] <- make_graphs(dat,i,treatment_abbrv,agg, delete, delete_type)
    }
  )
  return(graphs_all_groups)
}

get_all_data <- function(agg, delete, delete_type) {
  # PARTIAL MONITORING
  partial_dat <- load_data("../data/punishment_pm.csv", 
                           "../data/dp_data_R.dta", 
                           "PARTIAL", "pm", 2, agg, delete, delete_type)
  # FULL MONITORING
  full_dat <- load_data("../data/punishment_fm.csv", 
                        "../data/dp_data_R.dta", 
                        "FULL", "fm", 3, agg, delete, delete_type)
  # combine into level three list
  alldata <- list(zero = zero_dat, partial = partial_dat, full = full_dat)
  return(alldata)
}


# summarization -----------------------------------------------------------
# function to get various summary measures, set up to make ggplotting easier
get_measure <- function(combined_data, measure, options=F, option){
  return_list <- list()
  for(i in 1:3){
    treatment <- paste0("treatment",i)
    treatment <- list()
    for(j in 1:4){
      if(options==F){
        treatment[[j]] <- sapply(combined_data[[i]][[paste0("g_",j)]],measure)
      } else if(options == T) {
        treatment[[j]] <- sapply(combined_data[[i]][[paste0("g_",j)]], function(x) measure(x, mode=option))
      }
    }
    return_list[[i]] <- treatment
  }
  return_list <- lapply(return_list, unlist, use.names = FALSE)
  return_df <- data.frame(
    "value" = unlist(return_list),
    "treatment" = rep(c('ZERO', 'PARTIAL', 'FULL'), each=60),
    "group" = rep(1:4, times=3, each=15),
    "period" = rep(1:15, 4*3)
    )
  return_df$treatment <- ordered(return_df$treatment, levels = c('ZERO', 'PARTIAL', 'FULL'))
  return(return_df)
}

get_stats_dataframe <- function(treatment){
  fulldata <- data.frame()
  for(i in 1:4){
    group <- paste0("g_",i,sep="")
    # start by getting the out-degree (weight) and use this to set up data frame
    dat <- sapply(treatment[[group]], function(x) graph.strength(x, mode="out"))
    dat <- reshape2::melt(dat)
    dat$Var2 <- NULL
    colnames(dat) <- c("subject", "out_degree_weight")
    # now add other stats
    ## INDIVIDUAL
    ## out-degree (edges)
    dat$out_degree_edges <- reshape2::melt(sapply(treatment[[group]], function(x) degree(x, mode="out")))[,3]
    ## in-degree (weight)
    dat$in_degree_weight <- reshape2::melt(sapply(treatment[[group]], function(x) graph.strength(x, mode="in")))[,3]
    ## in-degree (edges)
    dat$in_degree_edges <- reshape2::melt(sapply(treatment[[group]], function(x) degree(x, mode="in")))[,3]
    ## GROUP
    dat$reciprocity <- rep(reshape2::melt(sapply(treatment[[group]], reciprocity))[,1],each=8)
    dat$transitivity <- rep(reshape2::melt(sapply(treatment[[group]], transitivity))[,1],each=8)
    dat$density <- rep(reshape2::melt(sapply(treatment[[group]], edge_density))[,1],each=8)
    # lastly, add period, group etc and export
    ## add period
    dat$period <- rep(1:15,each=8)
    ## add group
    dat$group <- i
    ## add unique subject_id
    dat$subject_id <- dat$subject + (dat$group * 100)
    ## bind it to full data frame
    fulldata <- rbind(fulldata, dat)
  }
  # drop subject
  fulldata$subject <- NULL
  # replace NaN's with zeros
  fulldata <- replace(fulldata, is.na(fulldata), 0)
  # order the data
  refcols <- c("subject_id", "period", "group")
  fulldata <- fulldata[, c(refcols, setdiff(names(fulldata), refcols))]
  # sort the data
  fulldata <- fulldata[with(fulldata, order(subject_id, period, group)), ]
  return(fulldata)
}


# visualization -----------------------------------------------------------
# custom graph plot function
plot_graph <- function(g, color_edges=F){
  if (!is.igraph(g)) {
    stop("Not a graph object")
  }
  ## vertices
  V(g)$rankh <- rank(V(g)$h / max(V(g)$h))
  nodeSize <- V(g)$rankh*5
  V(g)$in_degree <- order(-graph.strength(g, mode = "in"))
  V(g)$out_degree <- order(-graph.strength(g, mode = "out"))
  V(g)$ev <- order(-page.rank(g, directed = TRUE)$vector)
  vertexColor <- ifelse(
    V(g)$name == V(g)$in_degree[1], "lightblue",
    ifelse(
      V(g)$name == V(g)$out_degree[1], "lightgreen",
      "gray")
  )
  frameColor_initial <- ifelse(
    V(g)$name == V(g)$in_degree[1], "lightblue",
    ifelse(
      V(g)$name == V(g)$out_degree[1], "lightgreen",
      "gray"
    )
  )
  frameColor_updated <- ifelse(
    V(g)$name == V(g)$ev[1], "red", frameColor_initial
  )
  ## edges
  g <- delete.edges(g, which(E(g)$weight ==0))
  E(g)$normalizedEdges <- E(g)$weight / max(E(g)$weight)
  nodeShape = ifelse(V(g)$type == 1, "circle", "square")
  edgeSize = E(g)$normalizedEdges * 5
  E(g)$curved = TRUE
  ## layout & plot
  set.seed(15)
  #layout=layout.fruchterman.reingold(g, niter=500)
  layout=layout_as_star(g)
  plot.igraph(g,
       #layout=layout,
       edge.width=edgeSize,
       edge.arrow.size=0.25,
       edge.color = ifelse(color_edges == F, "black", E(g)$type),
       edge.color = E(g)$type,
       vertex.color = vertexColor,
       vertex.size = nodeSize,
       vertex.frame.color = frameColor_updated,
       vertex.label.color = "white",
       vertex.label = NA,
       vertex.shape = nodeShape
  )
}

plot_treatment <- function(treatment){
  op <- par(mfrow = c(2, 2), mar=rep(1,4), oma=c(0,0,2,0), pty = "s")
  for(i in 1:4){
    group <- paste0("g_",i,sep="")
    plot_graph(treatment[[group]][["agg_graph"]]); title(paste0("Group", " ", i,sep=""))
  }
  par(op)
}


# simulations -------------------------------------------------------------

constrained.erdos.renyi <- function(nodes, prob, reference_graph = NULL, constrained = FALSE, constraints = NULL){
  "
  Grow a directed, weighted network with constraints (nodes that can receive but not send edges) a la Erdos-Renyi
  arguments: 
    - nodes: number of nodes
    - prob: threshold probability for edge formation
    - constrained: include constraints if TRUE
    - constraints: number of nodes
  "
  require(igraph)
  if(constrained) {
    if(constraints>nodes) stop("Constraints cannot exceed nodes")
    constrained_nodes = sample(1:nodes, constraints, replace = F)
  }
  # instantiate an empty adjacency matrix size nodes x nodes
  A <- matrix(data = 0, nrow = nodes, ncol = nodes)
  # create the weights pool from the reference graph if provided
  if(!is.null(reference_graph) & class(reference_graph) != "igraph") stop("Reference graph must be an igraph object")
  if(!is.null(reference_graph)){
    reference_graph <- delete.edges(reference_graph, which(E(reference_graph)$weight==0))
    weights <- E(reference_graph)$weight
  }
  # grow the network
  for(i in 1:nodes){
    for(j in 1:nodes){
      if(!constrained) { # unconstrained network
        if(i!=j){
          p <- runif(1)
          w <- ifelse(!is.null(reference_graph), sample(weights, 1, replace = F), 1)
          ifelse(p > (1-prob), A[i,j] <- w, 0)
          if(!is.null(reference_graph)) weights <- ifelse(p>prob, weights[-match(s, weights)], weights)
        }
      } else { # constrained network
        if(i!=j & !(i%in%constrained_nodes)){
          p <- runif(1)
          w <- ifelse(!is.null(reference_graph), sample(weights, 1, replace = F), 1)
          ifelse(p > prob, A[i,j] <- w, 0)
          if(!is.null(reference_graph)) weights <- ifelse(p>prob, weights[-match(s, weights)], weights)
        }
      }
    }
  }
  # create an igraph object from the adjacency matrix and return it
  g <- graph_from_adjacency_matrix(A, mode = "directed")
  return(g)
}


simulate_graphs <- function(g, group_n, treatment_n){
  require(ggplot2)
  g <- delete.edges(g, which(E(g)$weight==0))
  in_degree = graph.strength(g, mode = "in")
  out_degree = graph.strength(g, mode = "out")
  trans_nr <- replicate(transitivity(sample_degseq(out_degree, in_degree, method = "simple")), n = 1000)
  recip_nr <- replicate(reciprocity(sample_degseq(out_degree, in_degree, method = "simple")), n = 1000)
  if (treatment_n == 2){
    trans_r <- replicate(transitivity(constrained.erdos.renyi(6, prob = 0.5, constrained = TRUE, constraints = 1)),n=1000)
    recip_r <- replicate(reciprocity(constrained.erdos.renyi(6, prob = 0.5, constrained = TRUE, constraints = 1)), n=1000)
  } else if (treatment_n == 3){
    trans_r <- replicate(transitivity(constrained.erdos.renyi(8, prob = 0.5, constrained = TRUE, constraints = 3)),n=1000)
    recip_r <- replicate(reciprocity(constrained.erdos.renyi(8, prob = 0.5, constrained = TRUE, constraints = 3)), n=1000)
  }
  sim_df <- data.frame(trans_nr,trans_r, recip_nr, recip_r)
  p1 <- ggplot() + 
    geom_density(aes(trans_nr), fill="blue", alpha=0.5, data=sim_df) +
    geom_density(aes(trans_r), fill="green",alpha=0.5, data=sim_df) +
    geom_vline(xintercept=transitivity(g), color = "black") + 
    geom_vline(xintercept=mean(sim_df$trans_nr), color = "blue") + 
    geom_vline(xintercept=mean(sim_df$trans_r), color = "green") + 
    theme_bw() + xlab("Transitivity") + ylab("Density") +
    scale_y_continuous(limits = c(0, 25)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  p2 <- ggplot() + 
    geom_density(aes(recip_nr), fill="blue", alpha=0.5, data=sim_df) +
    geom_density(aes(recip_r), fill="green",alpha=0.5, data=sim_df) +
    geom_vline(xintercept=reciprocity(g), color = "black") + 
    geom_vline(xintercept=mean(sim_df$recip_nr), color = "blue") + 
    geom_vline(xintercept=mean(sim_df$recip_r), color = "green") + 
    theme_bw() + xlab("Reciprocity") + ylab("Density") +
    scale_y_continuous(limits = c(0, 25)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  group_plot <- arrangeGrob(p1,p2,ncol=2, top=paste0("Group ", group_n, sep=""))
  return(group_plot)
}

simulate_treatment <- function(treatment, title, treatment_n){
  require(grid)
  require(gridExtra)
  g1 <- simulate_graphs(alldata[[treatment]]$g_1$agg_graph, 1, treatment_n)
  g2 <- simulate_graphs(alldata[[treatment]]$g_2$agg_graph, 2, treatment_n)
  g3 <- simulate_graphs(alldata[[treatment]]$g_3$agg_graph, 3, treatment_n)
  g4 <- simulate_graphs(alldata[[treatment]]$g_4$agg_graph, 4, treatment_n)
  plot_title <- textGrob(title, gp=gpar(fontsize=15, fontface="bold"))
  grid.arrange(g1, g2, g3, g4, ncol=2, top = plot_title)
}