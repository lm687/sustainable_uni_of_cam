setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # directory
rm(list = ls()) # clear objects

library(visNetwork) # interactive network visualisation package
library(viridisLite) # colour package

set.seed(234) # 'random' number generator

## Reading in data files
# note: source files (metadata.txt, dataframe_edges.txt) need identical name entries (ordering doesn't matter)
metadata = read.table("in_files/metadata.txt", stringsAsFactors = FALSE, sep = "\t")

## Colour formatting
colours_nodes = metadata$V2 + 1 ## from 0-indexed to 1-indexed
urls = metadata$V3
fontsizes = metadata$V4
labels = metadata$V1

#                                               'blue', 'red',        carbon', 'orange', 'outreach', 'greyishgreen', 'purple', 'cyan', '#afeeee', center
# colour_mapping = data.frame(idx=1:10, colour=c('#55CBD3', '#FE8E7B', '#fff1a0', '#FFB68C', '#FF6787', '#C7DAC7', '#7b68ee', 'cyan', '#2DA6AE', 'white'))
colour_mapping = data.frame(idx=1:10, colour=viridisLite::magma(10))
colours_nodes = colour_mapping[colours_nodes,'colour']

## Read in the edges
adj_mat_df = read.table("in_files/dataframe_edges.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE,
                        comment.char = "#")

all(metadata$V1 %in% unlist(adj_mat_df))
all(unlist(adj_mat_df) %in% metadata$V1)

## Read in url figures for nodes
url_figures = read.table("in_files/url_figures.txt", sep = "\t", stringsAsFactors = FALSE)

## Formatting objects "node" and "edges"
nodes <- data.frame(id = labels,
                    label=labels,
                    color=colours_nodes)
nodes$url <- urls
edges <- adj_mat_df
nodes$font.size = log(fontsizes+2)*16
# nodes[nodes$label == "Cambridge network","image"] = "https://static.wixstatic.com/media/992c2f_23552d8e4acf44ec8cf55e91c86fefad~mv2.png"
nodes$image = url_figures$V2[match(labels, url_figures$V1)]
nodes$shape = "dot"
nodes[nodes$label == "Development","shape"] = "text"
nodes[nodes$label %in% url_figures$V1,"shape"] = "image"

## Producing the graph
graph = visNetwork(nodes, edges, size=1, width = "100%", height=700,
                   main='Map of sustainability-related initiatives in Cambridge, UK',
                   submain=paste0('Lena Morrill 2020.\nLast updated: ', Sys.time(), ' GMT')) %>%
  visEvents(selectNode =  "function(params) {
    var nodeID = params.nodes[0];
    var url = this.body.nodes[nodeID].options.url;
    window.open(url, '_blank');
   }") %>% visNodes(shapeProperties = list(useBorderWithImage = TRUE), size=18)

## Resize to browser (attempt)
graph$sizingPolicy$browser$fill <- TRUE

visSave(graph, "html_files.html", selfcontained = TRUE, background = "white")
graph # plot

##-----

# visNetwork(data.frame(label=nodes$label, id=nodes$id, color=nodes$color, url=nodes$url, font.size=nodes$font.size), edges)
