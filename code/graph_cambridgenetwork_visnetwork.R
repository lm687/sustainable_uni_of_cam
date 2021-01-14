setwd(dirname(rstudioapi::getSourceEditorContext()$path)) ## set working directory
setwd("../") ## main folder
rm(list = ls()) ## clear objects

library(visNetwork) ## interactive network visualisation package
library(viridisLite) ## colour package

set.seed(234) # 'random' number generator

## Reading in data files
# note: source files (metadata.txt, dataframe_edges.txt) need identical name entries (ordering doesn't matter)
metadata = read.table("in_files/metadata.txt", stringsAsFactors = FALSE, sep = "\t")

## Colour formatting
colours_nodes = metadata$V2 + 1 ## from 0-indexed to 1-indexed
urls = metadata$V3
fontsizes = metadata$V4
labels = metadata$V1

colour_mapping = data.frame(idx=1:(1+length(unique(metadata$V2))), colour=viridisLite::magma((1+length(unique(metadata$V2)))))
colours_nodes = colour_mapping[colours_nodes,'colour']

## Read in the edges
adj_mat_df = read.table("in_files/dataframe_edges.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE,
                        comment.char = "#")

all(metadata$V1 %in% unlist(adj_mat_df))
all(unlist(adj_mat_df[,1:2]) %in% metadata$V1)

metadata$V1[!(metadata$V1 %in% unlist(adj_mat_df[,1:2]))]
unlist(adj_mat_df[,1:2])[!(unlist(adj_mat_df[,1:2]) %in%  metadata$V1)]

## Read in url figures for nodes
url_figures = read.table("in_files/url_figures.txt", sep = "\t", stringsAsFactors = FALSE)

## Formatting objects "node" and "edges"
nodes <- data.frame(id = labels,
                    label=labels,
                    color=colours_nodes)
nodes$url <- urls
edges <- adj_mat_df
nodes$font.size = log(fontsizes+2)*16
nodes$image = url_figures$V2[match(labels, url_figures$V1)]
nodes$shape = "dot"
nodes[nodes$label == "Development","shape"] = "text"
nodes[nodes$label %in% url_figures$V1,"shape"] = "image"

## Producing the graph
graph = visNetwork(nodes, edges, size=1, width = "100%", height=700,
                   title = 'Graph of sustainability-related initiatives in Cambridge, UK',
                   main='Graph of sustainability-related initiatives in Cambridge, UK',
                   submain=paste0('Lena Morrill 2020.\nLast updated: ', Sys.time(), ' GMT')) %>%
  visEvents(selectNode =  "function(params) {
    var nodeID = params.nodes[0];
    var url = this.body.nodes[nodeID].options.url;
    window.open(url, '_blank');
   }") %>% visNodes(shapeProperties = list(useBorderWithImage = TRUE), size=18) %>%
  visOptions(highlightNearest = list(enabled = TRUE, degree = 100)) %>% 
  visPhysics(    repulsion = 6)
  # visHierarchicalLayout(levelSeparation = 200)

## Resize to browser (attempt)
graph$sizingPolicy$browser$fill <- TRUE

visSave(graph, "html_files.html", selfcontained = TRUE, background = "white")
graph # plot


#----------------------------------------------#

# require(visNetwork)
# require(shiny)
# require(shinydashboard)
# 
# ui <- dashboardPage(skin = "black",
#                     dashboardHeader(),
#                     dashboardSidebar(
#                       sidebarMenu(
#                         menuItem("Network", tabName = "network", icon = icon("dashboard")),
#                         sidebarSearchForm(textId = "searchText", buttonId = "searchButton", label = "Search...")
#                       )
#                     ),
#                     dashboardBody(
#                       box(
#                         title = "Network",  status = "warning", solidHeader = TRUE, collapsible = TRUE,
#                         visNetworkOutput("network_proxy", height = 700, width=700)
#                       )
#                     )
# )
# 
# 
# server <- function(input, output, session) {
#   output$network_proxy <- renderVisNetwork({
#     visNetwork(nodes, edges, height = "100%", width="200%")
#   })
#   
#   observe({
#     if(input$searchButton > 0){
#       isolate({
#         print(input$searchText)
#         current_node <- nodes[grep(input$searchText, nodes$label), "id"]
#         print(current_node)
#         visNetworkProxy("network_proxy") %>% visSelectNodes(id  = current_node)
#       })
#     }
#   })
#   
# } #end server
# 
# shiny::shinyApp(ui, server)
