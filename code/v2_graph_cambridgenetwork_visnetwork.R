rm(list = ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) ## set working directory

library(visNetwork) ## interactive network visualisation package
library(viridisLite) ## colour package
# library(googlesheets4)
library(gsheet)
local_bool <- as.logical(readLines("local_bool"))

set.seed(234) # 'random' number generator

version = '' # default
# version = '_20201222'
#' Which version of the code do you want to use? if left '', I will use the default files
#' metadata.txt (in), dataframe_edges.txt (in), html_files.html (out)

# metadata_file = paste0("in_files/metadata", version, ".txt")
# edges_file = paste0("in_files/dataframe_edges", version, ".txt")
# out_file = paste0("html_files", version, ".html")

## Reading in data files
# note: source files (metadata.txt, dataframe_edges.txt) need identical name entries (ordering doesn't matter)
# metadata = read.table(metadata_file, stringsAsFactors = FALSE, sep = "\t", comment.char = "#", quote = '"')

## Read in the edges
# adj_mat_df = read.table(edges_file, sep = "\t", header = TRUE, stringsAsFactors = FALSE,
#                         comment.char = "#", quote='"')

# metadata <- gsheet2tbl('docs.google.com/spreadsheets/d/1S_GiaQaZ3yBW07OQQJTIN7NuG03KJZNu4gJyIUEcGlE/edit?usp=sharing', sheetid = "metadata")
# adj_mat_df <- gsheet2tbl('docs.google.com/spreadsheets/d/1szeOH5yfxlPGOdacTZqQJlPInLzDxrh_Fu9GP6fef84/edit?usp=sharing')
local_bool=T
if(local_bool){
  metadata <- gsheet2tbl('docs.google.com/spreadsheets/d/1S_GiaQaZ3yBW07OQQJTIN7NuG03KJZNu4gJyIUEcGlE', sheetid = "metadata")
  # write.table(metadata, "in_files/metadata_downloaded.csv", sep = "\t", quote = T, row.names = F)
  # adj_mat_df <- gsheet2tbl('docs.google.com/spreadsheets/d/1szeOH5yfxlPGOdacTZqQJlPInLzDxrh_Fu9GP6fef84')
  # write.table(adj_mat_df, "in_files/adj_mat_df_downloaded.csv", sep = "\t", quote = T, row.names = F)
}else{
  metadata <- read.table("in_files/metadata_downloaded.csv", sep = "\t", header = T, fill = T)
  # adj_mat_df <- read.table("in_files/adj_mat_df_downloaded.csv", sep = "\t", header = T, fill = T)
}

# metadata <- read_sheet('docs.google.com/spreadsheets/d/1S_GiaQaZ3yBW07OQQJTIN7NuG03KJZNu4gJyIUEcGlE')
# adj_mat_df <- read_sheet('docs.google.com/spreadsheets/d/1szeOH5yfxlPGOdacTZqQJlPInLzDxrh_Fu9GP6fef84/edit?usp=sharing')

metadata <- metadata[metadata$`Keep bool`,]
metadata <- metadata[!is.na(metadata$Node),]
metadata[is.na(metadata$`Keep bool`),]
metadata <- metadata[order(metadata$Node),] # sort alphabetically

table(metadata$Category)

metadata[metadata$Category == 'Department',]
metadata[metadata$Category == 'Outreach',]


rename_cols <- function(m, cl){
  colnames(m) <- cl
  m
}
metadata <- rbind(metadata,
                  rename_cols(t(sapply(unique(metadata$Category), function(i) c(rep(NA, 2), i, rep(NA, 5)))),
                              colnames(metadata)))
adj_mat_df <- data.frame(t(sapply(unlist(metadata[which(is.na(metadata$Category)),'Node']),
                                function(i){
                                  c(metadata[which(metadata$Category == 'MAIN'),'Node'], i)
                                })))
adj_mat_df <- rbind(adj_mat_df, rename_cols(do.call('rbind', lapply(unlist(metadata[which(is.na(metadata$Category)),'Node']),
       function(i){
         data.frame(t(sapply(unlist(metadata[which(metadata$Category == i),'Node']),
                function(j){
                  c(i,j)
                })))})), colnames(adj_mat_df)))
adj_mat_df <- data.frame(apply(adj_mat_df, 2, unlist))
colnames(adj_mat_df) <- c('from', 'to')


# ## Subsetting if necessary
# # metadata = metadata[metadata$V1 %in% unique(unlist(adj_mat_df[apply(adj_mat_df, 1, function(i){ ('Research' %in% i[1:2]) | ('Departmental' %in% i[1:2])   }),1:2])),]
# 
# ## Removing some central links
# adj_mat_df = adj_mat_df[!apply(adj_mat_df, 1, function(i) ('Research' %in% i[1:2])),]

## Colour formatting
metadata$Category[is.na(metadata$Category)] <- 'Other'
colours_nodes = metadata$Category ## 0-indexed
urls = metadata$URL
fontsizes = metadata$`Importance/size`
fontsizes[is.na(fontsizes)] <- 1
labels = metadata$Node


# colour_mapping = data.frame(idx=1:(1+length(unique(metadata$V2))), colour=viridisLite::magma((1+length(unique(metadata$V2)))))
labels_groups =  unique(metadata$Category)#c('Outreach', 'Carbon', 'Food', 'Nature/Biodiversity', 'Development', 'Recycling','Finance',  'Travel', 'Research',
                  #'Infrastructure', 'Policy', 'Cambridge city')
colour_mapping = data.frame(idx=rev(unique(colours_nodes)),
                            colour=viridisLite::magma(length(unique(colours_nodes))))
# labels_groups = c('Outreach', 'Carbon', 'Food', 'Nature/Biodiversity', 'Development', 'Recycling', NA, 'Travel', 'Research',
#                   'Infrastructure', 'Policy', 'Cambridge city')
colours_nodes = colour_mapping[match(colours_nodes,colour_mapping$idx), 'colour']

all(metadata$Node %in% unlist(adj_mat_df))
metadata$Node[!(metadata$Node %in% unlist(adj_mat_df))]
all(unlist(adj_mat_df[,1:2]) %in% metadata$Node)

legend_colours <- data.frame(id=unique(colour_mapping$idx),#labels_groups[colour_mapping$idx],
                             label=unique(colour_mapping$idx),#labels_groups[colour_mapping$idx],
                             color=colour_mapping$colour)

## Read in url figures for nodes
url_figures = read.table("in_files/url_figures.txt", sep = "\t", stringsAsFactors = FALSE, comment.char = "#", quote = '"')

## Formatting objects "node" and "edges"
nodes_df <- data.frame(id = labels,
                       label=labels,
                       color=colours_nodes)
nodes_df$url <- urls
edges_df <- adj_mat_df
nodes_df$font.size = log(as.numeric(fontsizes)+2)*20
nodes_df$image = url_figures$V2[match(labels, url_figures$V1)]
nodes_df$shape = "dot"
nodes_df[nodes_df$label == "Development","shape"] = "text"
nodes_df[nodes_df$label %in% url_figures$V1,"shape"] = "image"
nodes_df$theme = legend_colours$label[match(nodes_df$color, legend_colours$color)]

rownames(edges_df) <- NULL
edges_df$X1 <- 1:nrow(edges_df)
edges_df$Notes <- NA
## Producing the graph

graph = visNetwork(nodes = nodes_df, edges = edges_df,
                   #size=10, width = "100%", height=700
                   #title = 'Graph of sustainability-related initiatives in Cambridge, UK',
                   #main='Graph of sustainability-related initiatives in Cambridge, UK',
                   #submain=paste0('Lena Morrill 2020.\nLast updated: ', Sys.time(), ' GMT')
                   ) %>%
  visEvents(selectNode =  "function(params) {
            var nodeID = params.nodes[0];
            var url = this.body.nodes[nodeID].options.url;
            window.open(url);
            }") %>%
  visNodes(shapeProperties = list(useBorderWithImage = TRUE), size=5) %>%
  visOptions(selectedBy = "color", 
             highlightNearest = TRUE, 
             nodesIdSelection = TRUE) %>% 
  visOptions(highlightNearest = list(enabled = TRUE, degree = 100)) %>% 
  visPhysics( repulsion=list(nodeDistance=1800)
              # hoverNode = "function(e){
              #   this.body.data.nodes.update({id: e.node, font: {size : 14}});
              # }",
              # blurNode = "function(e){
              #   this.body.data.nodes.update({id: e.node, font: {size : 0}});
              # }"
  )%>%
  # visOptions(highlightNearest = list(enabled = TRUE, degree = 2)) %>%
  visInteraction(hover = T)%>%
  visPhysics(hierarchicalRepulsion=list(nodeDistance=400))#solver = "forceAtlas2Based",
             #forceAtlas2Based = list(gravitationalConstant = -100))

## Resize to browser (attempt)
graph$sizingPolicy$browser$fill <- TRUE
graph
# if(!(readLines("in_files/run_from_app_bool") == 'TRUE')){
#   setwd("../")
#   graph = graph %>% visPhysics(stabilization = FALSE, hierarchicalRepulsion=list(nodeDistance=400))
#   visSave(graph, out_file, selfcontained = TRUE, background = "white")
#   graph # plot
# }

