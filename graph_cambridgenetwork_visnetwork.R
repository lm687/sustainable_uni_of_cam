setwd(dirname(rstudioapi::getSourceEditorContext()$path))
rm(list = ls())

set.seed(234)

library(visNetwork)
library(viridisLite)

adj_mat = read.csv("in_files/adjacency_matrix.csv", check.names = FALSE)
rownames(adj_mat) = adj_mat[,1]
adj_mat = adj_mat[,-1]
adj_mat = adj_mat[match(colnames(adj_mat),rownames(adj_mat)),]
colours = read.table("in_files/colours.txt", stringsAsFactors = FALSE, sep = "\t")

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

adj_mat_df = data.frame(t(do.call('cbind', sapply(1:ncol(adj_mat), FUN =  function(i){
  sapply(which(adj_mat[i,] == 1), function(j) c(colnames(adj_mat)[i],colnames(adj_mat)[j]))
  }))))
colnames(adj_mat_df) = c('to', 'from')

labels = colnames(adj_mat)
labels[labels == "center"] = "Cambridge network"

nodes <- data.frame(id = colnames(adj_mat), label=labels,
                    color=matched_colours)
nodes$url <- matched_urls
edges <- adj_mat_df
nodes$font.size = log(matched_fontsizes+1)*25
nodes[nodes$label == "Cambridge network","image"] = "https://static.wixstatic.com/media/992c2f_23552d8e4acf44ec8cf55e91c86fefad~mv2.png"
nodes$shape = "dot"
nodes[nodes$label == "Cambridge network","shape"] = "image" 
graph = visNetwork(nodes, edges, size=1, width = "100%", height=700) %>% visEvents(selectNode = 
                                                    "function(params) {
    var nodeID = params.nodes[0];
    var url = this.body.nodes[nodeID].options.url;
    window.open(url, '_blank');
   }") %>% visNodes(shapeProperties = list(useBorderWithImage = TRUE))
graph$sizingPolicy$browser$fill <- TRUE
graph



visSave(graph, "html_files.html", selfcontained = TRUE, background = "white")







path_to_images <- "https://raw.githubusercontent.com/datastorm-open/datastorm-open.github.io/master/visNetwork/data/img/indonesia/"

nodes2 <- data.frame(id = 1:4,
                    shape = c("image", "circularImage"),
                    image = paste0(path_to_images, 1:4, ".png"),
                    label = "I'm an image")
nodes2[1,'image'] = "https://static.wixstatic.com/media/992c2f_23552d8e4acf44ec8cf55e91c86fefad~mv2.png"

edges2 <- data.frame(from = c(2,4,3,3), to = c(1,2,4,2))

visNetwork(nodes2, edges2, width = "100%") %>%
  visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
  visLayout(randomSeed = 2)

nodes$shape = "image"
visNetwork(nodes, edges, width = "100%") %>%
  visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
  visLayout(randomSeed = 2)
