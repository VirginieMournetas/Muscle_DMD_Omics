#### plots window #### 
output$exp.homemade.graphs <- renderUI({
  exp1.Status()
  if(input$Product.type == "mRNA / Protein"){
    fluidRow(column(12 , fluidRow(column(12 ,  boxPlus(title = "Bulk transcriptomic data (All data points)",
                                                       closable = FALSE , collapsible = TRUE , collapsed = FALSE ,
                                                       solidHeader = TRUE , width = 12 , status = "primary" , 
                                                       p(selectInput("Exp_homemade_Graph" , #Choose the type of wanted graph on the screen
                                                                   "" , 
                                                                   choices = list("Boxplot - All cell lines" = 1 , 
                                                                                  "Boxplot - Healthy vs DMD cell lines" = 2 , 
                                                                                  "Line plot - Healthy vs DMD cell lines" = 3 , 
                                                                                  "Dotplot - Day (x+y)/Day (x) ratios" = 4 , 
                                                                                  "Dotplot - Day (x+y)/Day (x) ratios - with stats" = 5 , 
                                                                                  "Dotplot - DMD/Healthy ratios" = 6 , 
                                                                                  "Dotplot - DMD/Healthy ratios - with stats" = 7) , 
                                                                   selected = 1),
                                                         plotlyOutput(outputId = "Exp_homemade_Graph_plot1"))))),
                          fluidRow(column(3 , boxPlus(title = "Proteomic data (Day 17)",
                                                      closable = FALSE , collapsible = TRUE , collapsed = FALSE ,
                                                      solidHeader = TRUE , width = 12 , status = "primary" , 
                                                      plotlyOutput(outputId = "Exp_homemade_Graph_plot2"))),
                                   column(9 , boxPlus(title = "Single-cell transcriptomic data (Day 17)",
                                                      closable = FALSE , collapsible = TRUE , collapsed = FALSE ,
                                                      solidHeader = TRUE , width = 12 , status = "primary" , 
                                                      fluidRow(column(4 , br(), br(), h5(tags$b("Reads / cell")), br(),
                                                                          plotlyOutput(outputId = "Exp_homemade_Graph_plot3"), style = "text-align:center"),
                                                               column(8 , selectInput("SingleCell.reduction.2", label = tags$b("Select the reduction"), 
                                                                                       choices = list("tSNE" = "tsne" , 
                                                                                                      "UMAP" = "umap",
                                                                                                      "PCA" = "pca") , 
                                                                                       selected = "tsne"),
                                                                          fluidRow(column (6, numericInput("SingleCell.component1.2", label = tags$b("Select component x"), value = 1)),
                                                                                   column (6, numericInput("SingleCell.component2.2", label = tags$b("Select component y"), value = 2))),
                                                                            plotlyOutput(outputId = "Exp_homemade_Graph_plot4"))))))))
  }else if (input$Product.type == "miR"){
    fluidRow(column(12 , selectInput("Exp_homemade_Graph" , #Choose the type of wanted graph on the screen
                                      "" , 
                                      choices = list("Boxplot - All cell lines" = 1 , 
                                                     "Boxplot - Healthy vs DMD cell lines" = 2 , 
                                                     "Line plot - Healthy vs DMD cell lines" = 3 , 
                                                     "Dotplot - Day (x+y)/Day (x) ratios" = 4 , 
                                                     "Dotplot - Day (x+y)/Day (x) ratios - with stats" = 5 , 
                                                     "Dotplot - DMD/Healthy ratios" = 6 , 
                                                     "Dotplot - DMD/Healthy ratios - with stats" = 7) , 
                                      selected = 1) , 
                          plotlyOutput(outputId = "Exp_homemade_Graph_plot1"))) #Chosen plot
  }
})

#### tables window #### 
output$exp.homemade.tables <- renderUI({
  exp1.Status()
  if(input$Product.type == "mRNA / Protein"){
    fluidRow(column(8 ,boxPlus(title = "Bulk transcriptomic data (All data points)",
                               closable = FALSE , collapsible = TRUE , collapsed = FALSE ,
                               solidHeader = TRUE , width = 12 , status = "primary" , 
                               p(selectInput("Unique_table_type" , 
                                             label =  NULL ,
                                             choices =  c("Expression data" = 1, "Myogenesis data" = 2, "Phenotype data" = 3),
                                             selected = 1, 
                                             multiple = FALSE , 
                                             width = 200 ),
                                 DT::dataTableOutput("Exp_homemade_table_unique")))),
              column(4 , boxPlus(title = "Proteomic data (Day 17)",
                                 closable = FALSE , collapsible = TRUE , collapsed = FALSE ,
                                 solidHeader = TRUE , width = 12 , status = "primary" , 
                                 DT::dataTableOutput('Exp_homemade_table_uniqueProt'))))
  }else if (input$Product.type == "miR"){
    fluidRow(column(12 , selectInput("Unique_table_type" , 
                                      label =  NULL ,
                                      choices =  c("Expression data" = 1, "Myogenesis data" = 2, "Phenotype data" = 3),
                                      selected = 1, 
                                      multiple = FALSE , 
                                      width = 200 ),
                          DT::dataTableOutput("Exp_homemade_table_unique")))
  }
})

#### mRNA / miR data table #### 
output$Exp_homemade_table_unique <- DT::renderDataTable({
  
  exp1.Status()
  
  if(input$Product.type == "miR"){
    
    if (input$Unique_table_type == 1){
      UniquemiRData <- Graph1_2_table.b(miRseqData.short, UniquemiRData, "miR", id.query$Id.data, input$ID.type)
      Render_Table(UniquemiRData[c(1:4,6)])
    }
    else if (input$Unique_table_type == 2){
      miR_RatioStat_Data_Myogenesis <- Graph4_to_7_table(miR_RatioStat_Data_Myogenesis, miR_Ratios_Stats.short, "miR", id.query$Id.data, "MYOGENESIS")
      Render_Table(miR_RatioStat_Data_Myogenesis)
    }
    else if (input$Unique_table_type == 3){
      miR_RatioStat_Data_Phenotype <- Graph4_to_7_table(miR_RatioStat_Data_Phenotype, miR_Ratios_Stats.short, "miR", id.query$Id.data, "PHENOTYPE")
      Render_Table(miR_RatioStat_Data_Phenotype)
    }
    
  }else if(input$Product.type == "mRNA / Protein"){
    
    if (input$Unique_table_type == 1){
      UniquemRNAData <- Graph1_2_table.b(mRNAseqData.short, UniquemRNAData, "mRNA", id.query$Id.data, input$ID.type)
      Render_Table(UniquemRNAData[c(1:4,6)])
    }else if (input$Unique_table_type == 2){
      mRNA_RatioStat_Data_Myogenesis <- Graph4_to_7_table(mRNA_RatioStat_Data_Myogenesis, mRNA_Ratios_Stats.short, "mRNA", id.query$Id.data, "MYOGENESIS")
      Render_Table(mRNA_RatioStat_Data_Myogenesis)
    }else if (input$Unique_table_type == 3){
      mRNA_RatioStat_Data_Phenotype <- Graph4_to_7_table(mRNA_RatioStat_Data_Phenotype, mRNA_Ratios_Stats.short, "mRNA", id.query$Id.data, "PHENOTYPE")
      Render_Table(mRNA_RatioStat_Data_Phenotype)
    }
  }
  
})

#### Protein Data table #### 
output$Exp_homemade_table_uniqueProt <- DT::renderDataTable({
  
  exp1.Status()
  
  colnames(ProteinData_Table) <- c("" , id.query$ID.selected)   
  ProteinData.unique <- unique(ProteinData.full[ , c(4,6:22)])
  ProteinData_Table[ , 2] <- t(ProteinData.unique[ProteinData.unique$Uniprot %in% id.query$Id.data$uniprot , ]) #id.query$Id.data$ensembl
  Render_Table(ProteinData_Table)
})

#### mRNA / miR plots #### 
output$Exp_homemade_Graph_plot1 <- renderPlotly({{
  
  exp1.Status()
  
  if(input$Product.type == "miR"){
    
    UniquemiRData <- Graph1_2_table.b(miRseqData.short, UniquemiRData, "miR", id.query$Id.data, input$ID.type)
    UniquemiRData_sorted <- UniquemiRData[order(UniquemiRData$Cell_line) , ]
    UniquemiRData_sorted <- UniquemiRData_sorted[order(UniquemiRData_sorted$Phenotype) , ]
    UniquemiRData_sorted <- UniquemiRData_sorted[order(UniquemiRData_sorted$Cell_stage) , ]
    
    UniquemiRmeanData <- Graph3_table.b(UniquemiRData, UniquemiRmeanData, "miR")
    
    miR_RatioStat_Data_Myogenesis <- Graph4_to_7_table(miR_RatioStat_Data_Myogenesis, miR_Ratios_Stats.short, "miR", id.query$Id.data, "MYOGENESIS")
    miR_RatioStat_Data_Phenotype <- Graph4_to_7_table(miR_RatioStat_Data_Phenotype, miR_Ratios_Stats.short, "miR", id.query$Id.data, "PHENOTYPE")
    
    Graph_view(input$Exp_homemade_Graph, UniquemiRData_sorted, UniquemiRmeanData, id.query$ID.selected, miR_RatioStat_Data_Myogenesis, miR_RatioStat_Data_Phenotype)
    
  }else if(input$Product.type == "mRNA / Protein"){
    
    UniquemRNAData <- Graph1_2_table.b(mRNAseqData.short, UniquemRNAData, "mRNA", id.query$Id.data, input$ID.type)
    UniquemRNAData_sorted <- UniquemRNAData[order(UniquemRNAData$Cell_line), ]
    UniquemRNAData_sorted <- UniquemRNAData_sorted[order(UniquemRNAData_sorted$Phenotype) , ]
    UniquemRNAData_sorted <- UniquemRNAData_sorted[order(UniquemRNAData_sorted$Cell_stage) , ]
    
    UniquemRNAmeanData <- Graph3_table.b(UniquemRNAData, UniquemRNAmeanData, "mRNA")
    
    mRNA_RatioStat_Data_Myogenesis <- Graph4_to_7_table(mRNA_RatioStat_Data_Myogenesis, mRNA_Ratios_Stats.short, "mRNA", id.query$Id.data, "MYOGENESIS")
    mRNA_RatioStat_Data_Phenotype <- Graph4_to_7_table(mRNA_RatioStat_Data_Phenotype, mRNA_Ratios_Stats.short, "mRNA", id.query$Id.data, "PHENOTYPE")
    
    Graph_view(input$Exp_homemade_Graph, UniquemRNAData_sorted, UniquemRNAmeanData, id.query$ID.selected, mRNA_RatioStat_Data_Myogenesis, mRNA_RatioStat_Data_Phenotype)
    
  }
  
 
}})

output$Exp_homemade_PCA.mRNA <- renderPlotly({
  PCA.graph("mRNA", mRNA.expr.PCs$ind$coord, input$mRNA.PCA.component1, input$mRNA.PCA.component2, SampleDescription[,as.integer(input$mRNA.PCA.legend)])
})

output$Exp_homemade_PCA.miR <- renderPlotly({
  PCA.graph("miR", miR.expr.PCs$ind$coord, input$miR.PCA.component1, input$miR.PCA.component2, SampleDescription[c(1:12,seq(13,102,3)),as.integer(input$miR.PCA.legend)])
})

#### Protein plot #### 
output$Exp_homemade_Graph_plot2 <- renderPlotly({
  
  exp1.Status()
  
  {
    ProteinData.unique <- unique(ProteinData.full[, c(4, 9:17)])
    ProteinData_Boxplot[ , 3] <- t(ProteinData.unique[ProteinData.unique$Uniprot %in% id.query$Id.data$uniprot , -1])
  }
  {
    ggplot(ProteinData_Boxplot
           , aes(x = Data , y = Protein , text = paste(Ratio , ": " , Protein)))+            
      geom_hline(yintercept = 1 , linetype = "dashed")+
      geom_boxplot(width = 0.7 , color = "BLACK" , alpha = 0.5 , fill = 'grey') +
      geom_point(color = "black" , size = 3)+ 
      geom_hline(yintercept = 1)+
      geom_hline(yintercept = 2 , linetype = "dashed")+
      geom_hline(yintercept = 1.32 , linetype = "dashed" , color = "grey")+
      geom_hline(yintercept = 0.76 , linetype = "dashed" , color = "grey")+
      geom_hline(yintercept = 0.5 , linetype = "dashed")+
      scale_y_continuous(trans = log2_trans() , breaks = c(0.5 , 0.76 , 1 , 1.32 , 2))+  
      xlab("") +
      ylab("DMD/Healthy ratios") +
      theme_bw() +
      scale_x_discrete(labels = function(x) str_wrap(x , width = 9))+
      theme(
        axis.title.y = element_text(color = "black" , face = "bold" , size = 9) , 
        axis.text.y = element_text(color = "black" , face = "bold" , size = 9) , 
        plot.title = element_text(lineheight = 8 , face = "bold" , size = 9)
      )#+
    #ggtitle(paste(id.query$ID.selected))
    Graph <- ggplotly(p = ggplot2::last_plot() , 
                      width = NULL , 
                      height = NULL , 
                      tooltip = "text" , 
                      dynamicTicks = FALSE , 
                      layerData = 1 , 
                      originalData = TRUE , 
                      source = "A" , 
                      text = "")
  }
})

#### Single-cell mRNA plots ####
output$Exp_homemade_Graph_plot3 <- renderPlotly({
  
  table <- SC_datacount(singlecell.matrix, id.query$Id.data$symbol)
  fig <- plot_ly(
    x = rownames(table),
    y = table[,1],
    type = "bar", 
    marker = list(color = 'rgb(158,217,59)')
  ) %>% layout(xaxis = list(title = "Number of reads"), yaxis = list(title = "Number of cells"))
  fig
})

output$Exp_homemade_Graph_plot4 <- renderPlotly({
  FeaturePlot(
    singlecell,
    id.query$Id.data$symbol,
    reduction = input$SingleCell.reduction.2,
    dims = c(input$SingleCell.component1.2, input$SingleCell.component2.2),
    cells = NULL,
    c("lightgrey", '#9ED93B', '#3F5617', '#000000'))
  ggplotly(p = ggplot2::last_plot() , 
           width = NULL , 
           height = NULL , 
           tooltip = "all" , 
           dynamicTicks = FALSE , 
           layerData = 1 , 
           originalData = TRUE , 
           source = "A")
})

output$Exp_homemade_SingleCell.reduc <- renderPlotly({
  DimPlot(object = singlecell, 
          reduction = input$SingleCell.reduction,
          dims = c(input$SingleCell.component1, input$SingleCell.component2),
          cols =  '#9ED93B'
          )
  ggplotly(p = ggplot2::last_plot() , 
           width = NULL , 
           height = NULL , 
           tooltip = "all" , 
           dynamicTicks = FALSE , 
           layerData = 1 , 
           originalData = TRUE , 
           source = "A")})
