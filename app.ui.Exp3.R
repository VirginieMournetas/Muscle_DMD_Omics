fluidRow(column(12 ,
  br(),
  p(icon("info-circle"), "Do not forget to click on the 'display' button to get data.", style = "color:#367FA9"),
  p(icon("hourglass-end"), "When selecting a mRNA / protein, generating graĥs will involve computing time and might take a while, please be patient.", style = "color:#fa6e0a"),
  br(),
  fluidRow(column(2, div(h5(tags$b("Select your gene")),
                         fluidRow(column(3, p(tags$b("Type") , style = "text-align:center")),
                                  column(9, selectInput("Product.type",
                                                        label = NULL,
                                                        choices =  list("miR", "mRNA / Protein"),
                                                        selected = "miR", 
                                                        multiple = FALSE , 
                                                        width = 150))),
                         fluidRow(column(3, p(tags$b("ID Type") , style = "text-align:center")),column(9,  uiOutput("Product.select"))),
                         fluidRow(column(3, p(tags$b("ID") , style = "text-align:center")), column(9, uiOutput("ID.select"))),
                         fluidRow(column(3), column(9, actionButton("display", label = tags$b("Display"), icon("hand-pointer"), width = '150px'))),
                         br(),
                         fluidRow(column(12, verbatimTextOutput("exist"))),
                        br())),
           column(10, p(boxPlus(closable = FALSE , 
                               collapsible = TRUE , 
                               collapsed = TRUE ,
                               title = p(icon("address-card") , tags$b("ID Card")) , 
                               status = "primary" , 
                               solidHeader = TRUE , 
                               width = 12 ,
                               uiOutput("db_links")), 
                               boxPlus(
                                 closable = FALSE , 
                                 collapsible = TRUE , 
                                 collapsed = TRUE ,
                                 title = p(icon("file-invoice") , tags$b("Publications"), "(the last 25 on Europe PMC)") , 
                                 status = "primary" , 
                                 solidHeader = TRUE ,
                                 width = 12 ,
                                 DT::dataTableOutput("publi.table"))))),
  boxPlus(title = p(icon("images") , tags$b("Visualise data")),
          closable = FALSE , 
          collapsible = TRUE , 
          collapsed = FALSE ,
          status = "primary" , 
          solidHeader = TRUE ,
          width = 12 ,
          bs_accordion_sidebar(id = "acc_Exp3_homemade",
                               spec_side = c(width = 1, offset = 0),
                               spec_main = c(width = 11, offset = 0),
                               position = "right") %>%
            bs_append(title_side = p(icon("table") , "Tables") , content_side = NULL , content_main = uiOutput("exp.homemade.tables")) %>% 
            bs_append(title_side = p(icon("chart-bar") , "Graphs"), content_side = NULL , content_main = uiOutput("exp.homemade.graphs")) ,  
          use_bs_accordion_sidebar())))








