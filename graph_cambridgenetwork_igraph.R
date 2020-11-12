setwd(dirname(rstudioapi::getSourceEditorContext()$path))
rm(list = ls())

library(igraph)

adj_mat = read.csv("in_files/adjacency_matrix.csv", check.names = FALSE)
colours = read.table("in_files/colours.txt", stringsAsFactors = FALSE, sep = "\t")

rownames(adj_mat) = adj_mat[,1]
adj_mat = adj_mat[,-1]

graph = igraph::graph_from_adjacency_matrix(as(adj_mat, 'matrix'))
colours_sorted = colours$V2[match(igraph::get.vertex.attribute(graph)$name, colours$V1)]
igraph::V(graph)$color = c('blue', 'red', 'yellow', 'orange', 'green', 'black', 'purple', 'cyan', 'pink', 'white')[colours_sorted]

pdf("figures/Cambridge_initiatives_climate_and_sustainability_igraph.pdf")
igraph::plot.igraph(graph)
dev.off()
