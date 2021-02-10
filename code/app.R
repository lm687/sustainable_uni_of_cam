# setwd(dirname(rstudioapi::getSourceEditorContext()$path)) ## set working directory
source("graph_cambridgenetwork_visnetwork.R")

require(visNetwork)
require(shiny)
require(shinydashboard)
# library(extrafont)

ui <- dashboardPage(
  dashboardHeader(title = "Hello"),
  dashboardSidebar(
  sidebarMenu(
    menuItem("Network", tabName = "network", icon = icon("dashboard")),
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
    # actionButton("go", "FAQs"),
    # fluidRow(
    #   column(1, offset = 0.4, align="center",
    #          actionButton("go", "FAQs")      )),
    # br(),
    actionButton("go", "FAQs"),
    # br(),
    actionButton("go2", "Give us feedback! 😃")
  )),
  # sidebarLayout(position = "left",
  #               sidebarPanel( h2("Parameters"),
  #                             selectInput("subgraph","Subgraph:",c("All","Carbon","Academic", "Sustainability 101"),"Random"),
  #                             actionButton("gennet","Generate"),
  #                             textOutput("networkstat")
  #               )
  dashboardBody(
    tags$head(tags$style(HTML('
                        /* logo */
                        .skin-blue .main-header .logo {
                        background-color: #ffffff; //#ffa07a;
                        }
                        .box {
                  .form-group, .selectize-control {
                      padding-top: 0px;
                    margin-bottom: 0px;
                    margin-top: 0px;
                    margin-left: 0px,
                  }
                  .box-body {
                      padding-top: 0px;
                      padding-bottom: 0px;
                      margin-top: 0px;
                    margin-left: 0px,
                  }
                }

                        /* logo when hovered */
                        .skin-blue .main-header .logo:hover {
                        background-color: #ffffff; //#FF0000;
                        }
                        .skin-blue .sidebar-form .input {
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
                        /* toggle button when hovered  */
                        .skin-blue .main-header .navbar .sidebar-toggle:hover{
                        background-color: #ffffff; //#ff69b4;
                        }
                        .main-header .logo {
                          font-family: "Georgia", Times, "Times New Roman", serif;
                          font-weight: bold;
                          font-size: 24px;
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
    box(title = "Map of sustainability-related initiatives in Cambridge, UK",  status = "primary",
        solidHeader = TRUE, collapsible = TRUE,
        visNetworkOutput("network_proxy",height = 600, width="100%"),
        width = 600),
  box(title = "Legend",  status = "primary", plotOutput("sankey_table", height =  200),
      solidHeader = TRUE, collapsible = TRUE, width = "100%", height=260)))


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
    }else if(subgraph == "All"){
      nodes = nodes_df
    }
    net$nodes <- nodes
    net$edges <- edges_df
  })
  output$network_proxy <- renderVisNetwork({ 
    req(net$edges)
    netout <- visNetwork(net$nodes,net$edges) 
    netout
  })
  
  # output$network_proxy <- renderVisNetwork({
  #   visNetwork(nodes, edges, height = "100%", width="100%")
  # })
  
  output$sankey_table <- renderPlot({
    plot.new()
    par(mar=c(0,0,0,0))
    # rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")
    legend(x=0, y=.9, legend=labels_groups, col=levels(colours_nodes), cex=1.3, lwd = 3, ncol=4, bty = "n",
           pt.cex = 2, y.intersp=2)
  },
  )

  observe({
    if(input$searchButton > 0){
      isolate({
        print(input$searchText)
        current_node <- nodes[grep(input$searchText, nodes$label), "id"]
        print(current_node)
        visNetworkProxy("network_proxy") %>% visSelectNodes(id  = current_node)
      })
    }
  })

} #end server

shiny::shinyApp(ui, sserver)

