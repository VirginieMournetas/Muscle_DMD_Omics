#setwd("/home/virginie/shared/Muscle_DMD_Omics")
setwd("./")

Last_Update <- "15/02/21"


#### FOLDERS #### 
source("app.prior.folders.R", local = TRUE, encoding = "UTF-8")

####LIBRARIES####
source(file.path(dir["base"], "app.prior.libraries.R"), local = TRUE, encoding = "UTF-8")

#### DATA ####
source(file.path(dir["base"], "app.prior.data.R"), local = TRUE, encoding = "UTF-8")

#### FUNCTIONS ####
source(file.path(dir["base"], "app.prior.functions.R"), local = TRUE, encoding = "UTF-8")

####server####
server <- function(input , output, session) {
  
  source(file.path(dir["base"], "app.server.Data_homemade.R"), local = TRUE, encoding = "UTF-8")
  source(file.path(dir["base"], "app.server.Exp.R"), local = TRUE, encoding = "UTF-8")
  source(file.path(dir["base"], "app.server.Exp_homemade.R"), local = TRUE, encoding = "UTF-8")  
  
}

####ui####
ui <- dashboardPagePlus(skin = "black" ,
                        
                        # Header ####
                        header <- dashboardHeaderPlus(
                          title =tagList(span(class = "logo-lg", "Muscle-DMD-Omics"), tags$img(src = Logo , style="height:40%; width:160%"))) ,
                        
                        # Sidebar ####
                        sidebar <- dashboardSidebar(
                          #width = 250 , 
                          sidebarMenu(
                            menuItem("Home" , tabName = "Home" , icon = icon("home")) , 
                            menuItem("Data presentation" , tabName = "Data" , icon = icon("microscope")),
                            menuItem("Expression data" , tabName = "Exp" , icon = icon("chart-bar")),
                            menuItem("Download the app" , tabName = "Download" , icon = icon("download")),
                            menuItem("References" , tabName = "Ref" , icon = icon("atlas")), 
                            menuItem("Contact" , tabName = "Contact" , icon = icon("marker"))
                          )
                        ),
                        
                        # Body ####
                        body <- dashboardBody(
                          
                          tags$head(tags$style(HTML(".main-sidebar { font-size: 17px; }")), #change the font size to 20
                                    includeScript("matomo.js")) , #visit metrix

                          tabItems(
                            
                            # Tab.Home ####
                            tabItem(tabName = "Home", 
                                    source(file.path(dir["base"], "app.ui.Home.R"), local = TRUE)$value),
                            
                            # Tab.Data ####
                            tabItem(tabName = "Data", 
                                    boxPlus( title = p(icon("flask") , tags$b("A brief overview of the differentiation protocol")),
                                             closable = FALSE , 
                                             collapsible = TRUE , 
                                             collapsed = FALSE ,
                                             status = "primary" , solidHeader = TRUE , width = 12 ,
                                             p(tags$img(src = img_exp , style = "height:70% ; width:70% ; text-align:center") , style = "text-align:center")),
                                    
                                    boxPlus( title = p(icon("table") , tags$b("The list of available samples")),
                                             closable = FALSE , 
                                             collapsible = TRUE , 
                                             collapsed = FALSE ,
                                             status = "primary" , solidHeader = TRUE , width = 12 ,
                                             DT::dataTableOutput("Exp_mRNA")
                                    )),
                            
                            # Tab.Exp ####
                            tabItem(tabName = "Exp", 
                                    tabBox(
                                      title = "" , width = 12 , 
                                      id = "tabset_Exp" , # The id lets us use input$tabset1 on the server to find the current tab
                                      side = "left" , 
                                      tabPanel(p(icon("cloud-download-alt") , tags$b("Raw data"), style = "color:#367FA9"), source(file.path(dir["base"], "app.ui.Exp1.R"), local = TRUE)$value),
                                      tabPanel(p(icon("table") , tags$b("Entire datasets") , style = "color:#367FA9"), source(file.path(dir["base"], "app.ui.Exp2.R"), local = TRUE)$value),      
                                      tabPanel(p(icon("chart-bar") , tags$b("Single-gene expression data") , style = "color:#367FA9"), source(file.path(dir["base"], "app.ui.Exp3.R"), local = TRUE)$value))),
                            
                            
                            # Tab.Download ####
                            tabItem(tabName = "Download", 
                                    h3(tags$b("How to run this app from your own computer?"), style = "color:#367FA9"),
                                    withMathJax(includeMarkdown(sapply(rmdfiles, knit, quiet = TRUE)))#,
                                    #includeHTML('app.sharing.html')
                                    ),
                            
                            # Tab.Ref ####
                            tabItem(tabName = "Ref",
                                      p(h4(tags$b("You use data or code from this app in your research (licensed under GNU General Public License v3.0)"), "- Please cite the following paper:", style = "color:#367FA9"),
                                      br(),
                                      h5(tags$a("Myogenesis modelled by human pluripotent stem cells: a multi‐omic study of Duchenne myopathy early onset, Mournetas et al., JCSM, 2021", href = "https://onlinelibrary.wiley.com/doi/10.1002/jcsm.12665")),
                                      br(),
                                      h4(tags$b("Related publications:",style = "color:#367FA9")),
                                      br(),
                                      source(file.path(dir["base"],"app.ui.Ref.01.R"), local = TRUE)$value,
                                      source(file.path(dir["base"],"app.ui.Ref.02.R"), local = TRUE)$value)), 
                            
                            # Tab.Contact ####
                            tabItem(tabName = "Contact",
                                    source(file.path(dir["base"], "app.ui.Contact.R"), local = TRUE)$value,
                                    )
                            
                            
                            #### ####

                          ),
                          
                          #Bottom logo####
                          div(
                            p(tags$img(src = logo_bottom , style="height:70%; width:70%"), style = "text-align:center"),
                            p("Last update: ",tags$b(Last_Update), tags$i(" by Virginie Mournetas") , style = "text-align:center", br(), "Licensed under GNU General Public License v3.0")
                          )
                        )
)

####APP####
shinyApp(ui, server)

