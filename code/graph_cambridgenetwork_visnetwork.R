setwd(dirname(rstudioapi::getSourceEditorContext()$path)) ## set working directory
setwd("../") ## main folder
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
colours_nodes = colour_mapping[colours_nodes,'colour']

all(metadata$V1 %in% unlist(adj_mat_df))
all(unlist(adj_mat_df[,1:2]) %in% metadata$V1)

metadata$V1[!(metadata$V1 %in% unlist(adj_mat_df[,1:2]))]
unlist(adj_mat_df[,1:2])[!(unlist(adj_mat_df[,1:2]) %in%  metadata$V1)]

## Read in url figures for nodes
url_figures = read.table("in_files/url_figures.txt", sep = "\t", stringsAsFactors = FALSE, comment.char = "#")

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
   }",
  hoverNode = "function(e){
    this.body.data.nodes.update({id: e.node, font: {size : 14}});
  }",
  blurNode = "function(e){
    this.body.data.nodes.update({id: e.node, font: {size : 0}});
  }") %>% visNodes(shapeProperties = list(useBorderWithImage = TRUE), size=18) %>%
  # visOptions(highlightNearest = list(enabled = TRUE, degree = 2)) %>%
  visInteraction(hover = T)

## Resize to browser (attempt)
graph$sizingPolicy$browser$fill <- TRUE

visSave(graph, out_file, selfcontained = TRUE, background = "white")
graph # plot


#----------------------------------------------#


# require(visNetwork)
# require(shiny)
# require(shinydashboard)
# 
# ui <- dashboardPage(
#   dashboardHeader(title = "Hello"),
#   dashboardSidebar(
#     sidebarMenu(
#       menuItem("Network", tabName = "network", icon = icon("dashboard")),
#       sidebarSearchForm(textId = "searchText", buttonId = "searchButton", label = "Search...")
#     )
#   ),
#   dashboardBody(
#     tags$head(tags$style(HTML('
#                         /* logo */
#                         .skin-blue .main-header .logo {
#                         background-color: #f4b943;
#                         }
# 
#                         /* logo when hovered */
#                         .skin-blue .main-header .logo:hover {
#                         background-color: #FF0000;
#                         }
# 
#                         /* navbar (rest of the header) */
#                         .skin-blue .main-header .navbar {
#                         background-color: #2d8ec9;
#                         }        
# 
#                         /* main sidebar */
#                         .skin-blue .main-sidebar {
#                         background-color: #a3dbcb;
#                         }
# 
#                         /* active selected tab in the sidebarmenu */
#                         .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
#                         background-color: #d49b9b;
#                         }
# 
#                         /* other links in the sidebarmenu */
#                         .skin-blue .main-sidebar .sidebar .sidebar-menu a{
#                         background-color: #00ff00;
#                         color: #000000;
#                         }
# 
#                         /* other links in the sidebarmenu when hovered */
#                         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
#                         background-color: #69ffb7;
#                         }
#                         /* toggle button when hovered  */                    
#                         .skin-blue .main-header .navbar .sidebar-toggle:hover{
#                         background-color: #ff69b4;
#                         }
#                         .main-header .logo {
#                           font-family: "Georgia", Times, "Times New Roman", serif;
#                           font-weight: bold;
#                           font-size: 24px;
#                                 '))),
#     tags$style(HTML('
#                       .box.box-solid.box-primary>.box-header {
#                       color:#000000;
#                       background:#f6e7e7;
#                       }
#                       .box-header h3.box-title {
#                         font-family: "Georgia", Times, "Times New Roman", serif;
#                       }
#                       ')),
#     box(title = "Map of sustainability-related initiatives in Cambridge, UK",  status = "primary",
#         solidHeader = TRUE, collapsible = TRUE,
#         visNetworkOutput("network_proxy", height = 700, width="100%"), width = 600)))
# 
# 
# sserver <- function(input, output, session) {
#   output$network_proxy <- renderVisNetwork({
#     visNetwork(nodes, edges, height = "100%", width="100%")
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
# shiny::shinyApp(ui, sserver)
