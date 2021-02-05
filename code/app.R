# rstudioapi() is not allowed in running shiny apps. Comment out!
# setwd(dirname(rstudioapi::getSourceEditorContext()$path)) ## set working directory
run_from_app = TRUE
write(run_from_app, "in_files/run_from_app_bool")
source("graph_cambridgenetwork_visnetwork.R")

require(visNetwork)
require(shiny)
require(shinydashboard)
# library(extrafont)

give_linebreaks = function(strng, max_line){
    splt = strsplit(strng, " ")[[1]]
    if(length(splt)>1){
      which_good = which(sapply(1:length(splt), function(i) nchar(paste0(splt[1:i], collapse=' ')) ) < max_line)
      which_good = which_good[length(which_good)]
      if(length(which_good)==0){
        return(paste0(paste0(splt[1], collapse=" "), "\n",
                      give_linebreaks(paste0(splt[2:length(splt)], collapse=" "), max_line)))
      }else{
        if(which_good == length(splt)){
          return(paste0(splt[1:which_good], collapse=" "))
        }else{
          ## continue
          return(paste0(paste0(splt[1:which_good], collapse=" "), "\n",
                      give_linebreaks(paste0(splt[(which_good+1):length(splt)], collapse=" "), max_line)))
        }
      }
    }else{
      return(strng)
    }
}

## for nodes with long names, create linebreaks
nodes_df$label = sapply(nodes_df$label, give_linebreaks, max_line = 14)

ui <- dashboardPage(
  dashboardHeader(title = "Map of sustainability-related initiatives in Cambridge, UK", titleWidth = "100%"),
  dashboardSidebar(
    sidebarMenu(
      # menuItem("Network", tabName = "network", icon = icon("dashboard")),
      br(),
      br(),
      br(),
      fluidPage(p(paste0('Updated: ', Sys.time(), ' GMT'))),
      sidebarSearchForm(textId = "searchText", buttonId = "searchButton", label = "Search..."),
      # div(style="width:220px;height:120px;", verbatimTextOutput('list_matches')),
      fluidPage(verbatimTextOutput('list_matches')),
      br(),
      br(),
      br(),
      selectInput(
        inputId = "subgraph",
        label = NULL,
        choices = c("Simplified map", 'All', 'Sustainability 101', 'Academic', 'Carbon', "Charlie Map"),
        selected = "DL",
        selectize = FALSE
      ),
      actionButton("gennet","Generate"),
      # h4("Note: the graph might take a while to load"),
      br(),
      br(),
      br(),
      # fluidPage(
      #   h1('Example')
      #   ,textInput('txt','','Text')
      #   ,actionButton('add','add')
      #   ,verbatimTextOutput('list')
      # ),
      actionButton("go", "FAQs", onclick="window.open('https://github.com/lm687/sustainable_uni_of_cam/blob/gh-pages/README.md#faqs', '_blank')"),
      actionButton("go2", "Give us feedback! 😃", onclick ="window.open('https://ndk986cpkyq.typeform.com/to/FD7dHDLR', '_blank')"),
      br(),
      br(),
      br(),
      actionButton("btnId", "Click here for the Resilience and regeneration map of the city of Cambridge",
                   style="white-space: normal;
                   text-align:left;
                   height:80px;
                   width:200px;",
                   onclick ="window.open('https://cambridgeresilienceweb.vercel.app/', '_blank')")
    )),
  dashboardBody(
    tags$head(tags$style(HTML('
    #list_matches{color:black; font-size:12px; font-style:italic; padding:10px;
overflow-y:scroll; max-height: 150px; background: white; width=22px;}
                  .skin-blue .main-header .logo {
                          color: #000000;
                              background-color: #FFFFFF;
                              font-family: "Georgia", Times, "Times New Roman", serif;
                              font-weight: bold;
                              font-size: 24px;
                  }
                  .skin-blue .main-header .logo:hover {
                          color: #000000;
                            background-color: #FFFFFF;
                              }
                        .box {
                  .form-group, .selectize-control {
                      padding-top: 0px;
                    margin-bottom: 0px;
                    margin-top: 0px;
                    margin-left: 0px,
                  }
                  .box-body {
                      padding-top: -1px;
                      padding-bottom: 0px;
                      margin-top: 0px;
                    margin-left: 0px,
                  }
                  .hr{
                      padding-top: 0px;
                      margin-top: 0px;
                  }
                }
                        .skin-blue .sidebar-form input[type="text"],
                        .skin-blue .sidebar-form .btn {
                             background-color: #ffffff; //#f7e98e;
                        }

                        /* navbar (rest of the header) */
                        .skin-blue .main-header .navbar {
                        background-color: #ffffff; //#f7e98e;
                        }

                        /* main sidebar */
                        .skin-blue .main-sidebar {
                        color: #000000; //#a3dbcb;
                        background-color: #ffffff; //#a3dbcb;
                        }

                        /* activeelected tab in the sidebarmenu */
                        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                        background-color: #ffffff; //#d49b9b;
                        }

                        /* other links in the sidebarmenu */
                        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                        background-color: #00ff00;
                        color: #000000;
                        }

                        /* other links in the sidebarmenu when hovered */
                        .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                        background-color: #ffffff; //#69ffb7;
                        }
                      .skin-blue .main-sidebar .sidebar {
                        color: #000000;
                      }
                        '))),
    tags$style(HTML('
                      .box.box-solid.box-primary>.box-header {
                      color:#000000;
                      background:#ffffff; //#f6e7e7;
                      }
                      .box-header h3.box-title {
                        font-family: "Georgia", Times, "Times New Roman", serif;
                      }
                      ')),
    box(solidHeader = TRUE, collapsible = TRUE,
        visNetworkOutput("network_proxy",height = 555, width="100%"),
        width = 600),
    box(solidHeader = TRUE, collapsible = F,
        visNetworkOutput("legend_network",height = 90), width = 500, height=120)))
# box(title = "Legend",  status = "primary", plotOutput("legend", height =  200),
#     solidHeader = TRUE, collapsible = TRUE, width = "100%", height=260)))


sserver <- function(input, output, session) {
  net <- reactiveValues(nodes=nodes_df,edges=edges_df)
  prev_input_searchButton="0000" # initialise
  observeEvent(input$gennet,{
    print("regenerating network")
    subgraph <- input$subgraph
    if(subgraph == 'Carbon'){
      list_nodes = readLines("nodes_for_subgraphs/carbon_energy.txt")
      nodes <- nodes_df[nodes_df$id %in% list_nodes,]
    } else if(subgraph=="Sustainability 101"){
      list_nodes = readLines("nodes_for_subgraphs/sustainability101.txt")
      nodes <- nodes_df[nodes_df$id %in% list_nodes,]
    }else if(subgraph=='Academic'){
      list_nodes = readLines("nodes_for_subgraphs/academic.txt")
      nodes <- nodes_df[nodes_df$id %in% list_nodes,]
    }else if(subgraph== "Charlie Map"){
      list_nodes = readLines("nodes_for_subgraphs/charlie.txt")
      nodes <- nodes_df[nodes_df$id %in% list_nodes,]
    }else if(subgraph== "Simplified map"){
      list_nodes = readLines("nodes_for_subgraphs/exclusion_simplification.txt")
      nodes <- nodes_df[! (nodes_df$id %in% list_nodes),]
    }else if(subgraph == "All"){
      nodes = nodes_df
    }
    net$nodes <- nodes
    net$edges <- edges_df
  })
  output$network_proxy <- renderVisNetwork({ 
    req(net$edges)
    exclusion_nodes = readLines("nodes_for_subgraphs/exclusion_simplification.txt")
    net$nodes <- nodes_df[! (nodes_df$id %in% exclusion_nodes),]
    netout <- visNetwork(net$nodes,net$edges) %>% visPhysics(stabilization = FALSE) %>%
      visEvents(selectNode =  "function(params) {
    var nodeID = params.nodes[0];
                var url = this.body.nodes[nodeID].options.url;
                window.open(url, '_blank');
  }") %>% 
      visInteraction(hover = T)
    # %>%   visIgraphLayout()
    netout
  })
  output$legend_network <- renderVisNetwork({ 
    coords <- matrix(ncol=2, byrow=T, data=c(
      1,0,
      0,1,
      -1,0,
      0,-1,
      0,0))
    legendnet=list()
    legendnet$nodes = data.frame(id=labels_groups, label=labels_groups, color=unique(colours_nodes))
    legendnet$edges = data.frame(to=character(),from=character())
    # legendnet$nodes$x <- c(0, 1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 5)*120
    # legendnet$nodes$y <- c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1)*100
    legendnet$nodes$x <- c(0:(length(labels_groups)-1))*120
    legendnet$nodes$y <- rep(-1, length(labels_groups))
    visNetwork(nodes = legendnet$nodes, edges = legendnet$edges)%>%
      visNodes(fixed = TRUE)
    
    # visIgraphLayout(layout = "layout.norm")
  })
  # output$network_proxy <- renderVisNetwork({
  #   visNetwork(nodes, edges, height = "100%", width="100%")
  # })
  output$legend <- renderPlot({
    plot.new()
    par(mar=c(0,0,0,0))
    # rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")
    legend(x=0, y=.9, legend=labels_groups, col=unique(colours_nodes), cex=1.2, lwd=3, ncol=4, bty="n",
           pt.cex = 2, y.intersp=2)
  })
  myValues <- reactiveValues()
  observe({
    if(input$searchButton > 0){
      if(input$searchText != prev_input_searchButton){
        # isolate({
        myValues$dList <- net$nodes$id[grep(tolower(input$searchText), tolower(net$nodes$label))]
        if(is.null(myValues$dList)){
          myValues$dList = "No results"
        }
        c# urrent_node <-  net$nodes[grep(tolower(input$searchText), tolower(net$nodes$label)),]
        v# isNetworkProxy("network_proxy") %>% visSelectNodes(iurrent_node) %>% visGOption()highlightNearest = TRUE
        # # print(current_node)
        # })
      }else{
        myValues$dList = ""
      }
      prev_input_searchButton = input$searchText
    }
  })
  # output$list_matches<-renderText({
  output$list_matches<-renderPrint({
    myValues$dList
  })
  
} #end server

shiny::shinyApp(ui, sserver)

