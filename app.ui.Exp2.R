fluidRow(column(12 ,
        bs_accordion_sidebar(id = "acc_Exp2_homemade",
                             spec_side = c(width = 1, offset = 0),
                             spec_main = c(width = 11, offset = 0),
                             position = "right") %>%
          bs_append(title_side = p(icon("table") , "Tables") , 
                    content_side = NULL , 
                    content_main = fluidRow(column(12 ,
                                                   boxPlus(
                                                     closable = FALSE , collapsible = TRUE , collapsed = TRUE ,
                                                     title = "mRNA data table" , status = "primary" , solidHeader = TRUE , width = 12 ,
                                                     DT::dataTableOutput("Homemade_mRNA_table")
                                                   ),
                                                   boxPlus(
                                                     closable = FALSE , collapsible = TRUE , collapsed = TRUE ,
                                                     title = "miR data table" , status = "primary" , solidHeader = TRUE , width = 12 ,
                                                     DT::dataTableOutput("Homemade_miR_table")
                                                   ),
                                                   boxPlus(
                                                     closable = FALSE , collapsible = TRUE , collapsed = TRUE ,
                                                     title = "Protein data table" , status = "primary" , solidHeader = TRUE , width = 12 ,
                                                     DT::dataTableOutput("Homemade_protein_table")
                                                   )))) %>% 
          bs_append(title_side = p(icon("chart-bar") , "Graphs"), 
                    content_side = NULL , 
                    content_main = fluidRow(column(4, 
                                                   boxPlus(
                                                     closable = FALSE , collapsible = TRUE , collapsed = FALSE ,
                                                     title = "Bulk mRNA PCA" , status = "primary" , solidHeader = TRUE , width = 12 ,
                                                     selectInput("mRNA.PCA.legend", label = tags$b("Select the legend"), 
                                                                 choices = list("Cell stage" = 1, "Phenotype" = 2, "Cell line" = 3), 
                                                                 selected = 1),
                                                     fluidRow(column (6, numericInput("mRNA.PCA.component1", label = tags$b("Select component x"), value = 1)),
                                                              column (6, numericInput("mRNA.PCA.component2", label = tags$b("Select component y"), value = 2))),
                                                     plotlyOutput(outputId = "Exp_homemade_PCA.mRNA")
                                                   )),
                                            column(4, 
                                                   boxPlus(
                                                     closable = FALSE , collapsible = TRUE , collapsed = FALSE ,
                                                     title = "Bulk miR PCA" , status = "primary" , solidHeader = TRUE , width = 12 ,
                                                     selectInput("miR.PCA.legend", label = tags$b("Select the legend"), 
                                                                 choices = list("Cell stage" = 1, "Phenotype" = 2, "Cell line" = 3), 
                                                                 selected = 1),
                                                     fluidRow(column (6, numericInput("miR.PCA.component1", label = tags$b("Select component x"), value = 1)),
                                                              column (6, numericInput("miR.PCA.component2", label = tags$b("Select component y"), value = 2))),
                                                     plotlyOutput(outputId = "Exp_homemade_PCA.miR")
                                                   )),
                                            column(4, 
                                                   boxPlus(
                                                     closable = FALSE , collapsible = TRUE , collapsed = FALSE ,
                                                     title = "Single-cell mRNA reduction" , status = "primary" , solidHeader = TRUE , width = 12 ,
                                                     selectInput("SingleCell.reduction", label = tags$b("Select the reduction"), 
                                                                   choices = list("tSNE" = "tsne" , 
                                                                                  "UMAP" = "umap",
                                                                                  "PCA" = "pca") , 
                                                                   selected = "tsne"),
                                                       fluidRow(column (6, numericInput("SingleCell.component1", label = tags$b("Select component x"), value = 1)),
                                                                column (6, numericInput("SingleCell.component2", label = tags$b("Select component y"), value = 2))),
                                                       plotlyOutput(outputId = "Exp_homemade_SingleCell.reduc")
                                                   )))) ,  
        use_bs_accordion_sidebar(),
        p("*Data are normalised, centered and reduced.")))