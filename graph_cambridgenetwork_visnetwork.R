setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # directory
rm(list = ls()) # clear objects

library(visNetwork) # interactive network visualisation package
library(viridisLite) # colour package

set.seed(234) # 'random' position generator

# # # Compiling data files
# # note: source files need identical name entries (ordering doesn't matter)
adj_mat = read.csv("in_files/adjacency_matrix.csv", check.names = FALSE)
rownames(adj_mat) = adj_mat[,1]
adj_mat = adj_mat[,-1]
adj_mat = adj_mat[match(colnames(adj_mat),rownames(adj_mat)),] # matching the row and column names
colours = read.table("in_files/colours.txt", stringsAsFactors = FALSE, sep = "\t")

# # # Colour formatting
# matched_colours = colours$V2[match(colours$V1, colnames(adj_mat))] 
colours = colours[match(colnames(adj_mat), colours$V1),]
matched_colours = colours$V2
matched_colours = matched_colours + 1 ## from 0-indexed to 1-indexed
matched_urls = colours$V3
matched_fontsizes = colours$V4
#                                               'blue', 'red',        carbon', 'orange', 'outreach', 'greyishgreen', 'purple', 'cyan', '#afeeee', center
# colour_mapping = data.frame(idx=1:10, colour=c('#55CBD3', '#FE8E7B', '#fff1a0', '#FFB68C', '#FF6787', '#C7DAC7', '#7b68ee', 'cyan', '#2DA6AE', 'white'))
colour_mapping = data.frame(idx=1:10, colour=viridisLite::magma(10))
matched_colours = colour_mapping[matched_colours,'colour']

# # # Producing to-from node structure
adj_mat_df = data.frame(t(do.call('cbind', sapply(1:ncol(adj_mat), FUN =  function(i){
  sapply(which(adj_mat[i,] == 1), function(j) c(colnames(adj_mat)[i],colnames(adj_mat)[j]))
  }))))
colnames(adj_mat_df) = c('to', 'from')

# # # Display names and renaming center node
labels = colnames(adj_mat)
labels[labels == "center"] = "Cambridge network"

# # # Formatting objects "node" and "edges"
nodes <- data.frame(id = colnames(adj_mat), label=labels,
                    color=matched_colours)
nodes$url <- matched_urls
edges <- adj_mat_df
nodes$font.size = log(matched_fontsizes+2)*16
nodes[nodes$label == "Cambridge network","image"] = "https://static.wixstatic.com/media/992c2f_23552d8e4acf44ec8cf55e91c86fefad~mv2.png"
nodes$shape = "dot"
nodes[nodes$label == "Cambridge network","shape"] = "image"

# # # Producing the graphic
graph = visNetwork(nodes, edges, size=1, width = "100%", height=700,
                   main='Map of sustainability-related initiatives in Cambridge, UK',
                   submain=paste0('Lena Morrill 2020\tLast updated: ', Sys.time())) %>%
  visEvents(selectNode =  "function(params) {
    var nodeID = params.nodes[0];
    var url = this.body.nodes[nodeID].options.url;
    window.open(url, '_blank');
   }") %>% visNodes(shapeProperties = list(useBorderWithImage = TRUE), size=18) # L 42-49 Producing the graphic

# # # Resize to browser
graph$sizingPolicy$browser$fill <- TRUE # Resizing to browser window

graph # plot
  
# # # Uncomment to update the active html
# visSave(graph, "html_files.html", selfcontained = TRUE, background = "white")

