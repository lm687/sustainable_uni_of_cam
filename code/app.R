# rstudioapi() is not allowed in running shiny apps. Comment out!
# setwd(dirname(rstudioapi::getSourceEditorContext()$path)) ## set working directory
run_from_app = TRUE
source("graph_cambridgenetwork_visnetwork.R")

require(visNetwork)
require(shiny)
require(shinydashboard)
# library(extrafont)

ui <- dashboardPage(
  dashboardHeader(title = "Map of sustainability-related initiatives in Cambridge, UK", titleWidth = "100%"),
  dashboardSidebar(
  sidebarMenu(
    # menuItem("Network", tabName = "network", icon = icon("dashboard")),
    sidebarSearchForm(textId = "searchText", buttonId = "searchButton", label = "Search..."),
    selectInput(
      inputId = "subgraph",
      label = "Subgraph:",
      choices = c('Sustainability 101', 'Academic', 'Carbon', 'All'),
      selected = "DL",
      selectize = FALSE
    ),
    actionButton("gennet","Generate"),
    # h4("Note: the graph might take a while to load"),
    br(),
    br(),
    br(),
    actionButton("go", "FAQs", onclick="window.open('https://github.com/lm687/sustainable_uni_of_cam/blob/gh-pages/README.md#faqs', '_blank')"),
    actionButton("go2", "Give us feedback! 😃", onclick ="window.open('https://ndk986cpkyq.typeform.com/to/FD7dHDLR', '_blank')")
  )),
  dashboardBody(
    tags$head(tags$style(HTML('
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
                        background-color: #ffffff; //#a3dbcb;
                        }

                        /* active selected tab in the sidebarmenu */
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
                        .h3 .main-sidebar {
                          font-family: "Georgia", Times, "Times New Roman", serif;
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
  
  observeEvent(input$gennet,{
    print("regenerating network")
    subgraph <- input$subgraph
    if(subgraph == 'Carbon'){
      list_nodes = c('CU Energy Network', 'Cambridge Zero')
      nodes <- nodes_df[nodes_df$label %in% list_nodes,]
    } else if(subgraph=="Sustainability 101"){
      list_nodes = readLines("code/nodes_for_subgraphs/sustainability101.txt")
      nodes <- nodes_df[nodes_df$label %in% list_nodes,]
    }else if(subgraph=='Academic'){
      list_nodes = c('CU Energy Network')
      nodes <- nodes_df[nodes_df$label %in% list_nodes,]
    }else if(subgraph == "sAll"){
      nodes = nodes_df
    }
    net$nodes <- nodes
    net$edges <- edges_df
  })
  output$network_proxy <- renderVisNetwork({ 
    req(net$edges)
    netout <- visNetwork(net$nodes,net$edges) %>% visPhysics(stabilization = FALSE)
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

  observe({
    if(input$searchButton > 0){
      isolate({
        print(input$searchText)
        current_node <- net$nodes[grep(input$searchText, net$nodes$label), "id"]
        # print(current_node)
        visNetworkProxy("network_proxy") %>% visSelectNodes(id  = current_node) %>% visGetSelectedNodes()
      })
    }
  })

} #end server

shiny::shinyApp(ui, sserver)

