# rstudioapi() is not allowed in running shiny apps
# setwd(dirname(rstudioapi::getSourceEditorContext()$path)) ## set working directory
rm(list = ls()) ## clear objects

library(visNetwork) ## interactive network visualisation package
library(viridisLite) ## colour package

set.seed(234) # 'random' number generator

version = '' # default
# version = '_20201222'
#' Which version of the code do you want to use? if left '', I will use the default files
#' metadata.txt (in), dataframe_edges.txt (in), html_files.html (out)

metadata_file = paste0("in_files/metadata", version, ".txt")
edges_file = paste0("in_files/dataframe_edges", version, ".txt")
out_file = paste0("html_files", version, ".html")

## Reading in data files
# note: source files (metadata.txt, dataframe_edges.txt) need identical name entries (ordering doesn't matter)
metadata = read.table(metadata_file, stringsAsFactors = FALSE, sep = "\t", comment.char = "#")

## Read in the edges
adj_mat_df = read.table(edges_file, sep = "\t", header = TRUE, stringsAsFactors = FALSE,
                        comment.char = "#")

## Subsetting if necessary
# metadata = metadata[metadata$V1 %in% unique(unlist(adj_mat_df[apply(adj_mat_df, 1, function(i){ ('Research' %in% i[1:2]) | ('Departmental' %in% i[1:2])   }),1:2])),]

## Removing some central links
adj_mat_df = adj_mat_df[!apply(adj_mat_df, 1, function(i) ('Research' %in% i[1:2])),]

## Colour formatting
colours_nodes = metadata$V2 + 1 ## from 0-indexed to 1-indexed
urls = metadata$V3
fontsizes = metadata$V4
labels = metadata$V1

colour_mapping = data.frame(idx=1:(1+length(unique(metadata$V2))), colour=viridisLite::magma((1+length(unique(metadata$V2)))))
labels_groups = c('Outreach', 'Carbon', 'Food', 'Nature/Biodiversity', 'Development', 'Recycling', NA, 'Travel', 'Research',
                  'Infrastructure', 'Policy', 'Cambridge city')
colours_nodes = colour_mapping[colours_nodes,'colour']

all(metadata$V1 %in% unlist(adj_mat_df))
all(unlist(adj_mat_df[,1:2]) %in% metadata$V1)

metadata$V1[!(metadata$V1 %in% unlist(adj_mat_df[,1:2]))]
unlist(adj_mat_df[,1:2])[!(unlist(adj_mat_df[,1:2]) %in%  metadata$V1)]

## Read in url figures for nodes
url_figures = read.table("in_files/url_figures.txt", sep = "\t", stringsAsFactors = FALSE, comment.char = "#")

## Formatting objects "node" and "edges"
nodes_df <- data.frame(id = labels,
                    label=labels,
                    color=colours_nodes)
nodes_df$url <- urls
edges_df <- adj_mat_df
nodes_df$font.size = log(fontsizes+2)*13
nodes_df$image = url_figures$V2[match(labels, url_figures$V1)]
nodes_df$shape = "dot"
nodes_df[nodes_df$label == "Development","shape"] = "text"
nodes_df[nodes_df$label %in% url_figures$V1,"shape"] = "image"

## Producing the graph
graph = visNetwork(nodes_df, edges_df, size=1, width = "100%", height=700,
                   title = 'Graph of sustainability-related initiatives in Cambridge, UK',
                   main='Graph of sustainability-related initiatives in Cambridge, UK',
                   submain=paste0('Lena Morrill 2020.\nLast updated: ', Sys.time(), ' GMT')) %>%
  # visEvents(selectNode =  "function(params) {
  #   var nodeID = params.nodes[0];
  #   var url = this.body.nodes[nodeID].options.url;
  #   window.open(url, '_blank');
  #  }") %>%
  visEvents(selectNode =  "function(params) {
    var nodeID = params.nodes[0];
          var url = this.body.nodes[nodeID].options.url;
          window.open(url);
          }") %>%
  visNodes(shapeProperties = list(useBorderWithImage = TRUE), size=5) %>%
  visOptions(highlightNearest = list(enabled = TRUE, degree = 100)) %>% 
  visPhysics( repulsion=list(nodeDistance=600)
  # hoverNode = "function(e){
  #   this.body.data.nodes.update({id: e.node, font: {size : 14}});
  # }",
  # blurNode = "function(e){
  #   this.body.data.nodes.update({id: e.node, font: {size : 0}});
  # }"
  )%>%
  # visOptions(highlightNearest = list(enabled = TRUE, degree = 2)) %>%
  visInteraction(hover = T)

## Resize to browser (attempt)
graph$sizingPolicy$browser$fill <- TRUE

if(!(readLines("in_files/run_from_app_bool") == 'TRUE')){
  setwd("../")
  graph = graph %>% visPhysics(stabilization = FALSE, hierarchicalRepulsion=list(nodeDistance=400))
  visSave(graph, out_file, selfcontained = TRUE, background = "white")
  graph # plot
}
