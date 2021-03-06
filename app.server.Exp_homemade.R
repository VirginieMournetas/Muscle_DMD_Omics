#### WINDOWS #### 

#### plots window
output$exp.homemade.graphs <- renderUI({
  
  exp1.Status()
  
  if (id.query$Id.data != "No data"){
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
                                                      verbatimTextOutput("proteomic.exist"),
                                                      plotlyOutput(outputId = "Exp_homemade_Graph_plot2"))),
                                   column(9 , boxPlus(title = "Single-cell transcriptomic data (Day 17)",
                                                      closable = FALSE , collapsible = TRUE , collapsed = FALSE ,
                                                      solidHeader = TRUE , width = 12 , status = "primary" , 
                                                      p(br(), actionButton("display.singlecell", label = tags$b("Display"), icon("hand-pointer"), width = '150px'), br()),
                                                      uiOutput("Single.cell.graphs"))))))
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
  }

})

observeEvent(input$display.singlecell, {
  
  output$Single.cell.graphs <- renderUI({
    
    fluidRow(column(4 , br(), br(), h5(tags$b("Reads / cell")), br(),
                        plotlyOutput(outputId = "Exp_homemade_Graph_plot3"), style = "text-align:center"),
             column(8 , selectInput("SingleCell.reduction.2", label = tags$b("Select the reduction"), 
                                    choices = list("tSNE" = "tsne" , 
                                                   "UMAP" = "umap",
                                                   "PCA" = "pca") , 
                                    selected = "tsne"),
                    fluidRow(column (6, numericInput("SingleCell.component1.2", label = tags$b("Select component x"), value = 1)),
                             column (6, numericInput("SingleCell.component2.2", label = tags$b("Select component y"), value = 2))),
                    plotlyOutput(outputId = "Exp_homemade_Graph_plot4")))
    
  })
  
})

#### tables window
output$exp.homemade.tables <- renderUI({
  
  exp1.Status()
  
  if (id.query$Id.data != "No data"){
  
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
  }
  
})



#### TABLES #### 

####ALL GENE

#mRNA datatable
output$Homemade_mRNA_table <- DT::renderDataTable({
  
  if (all(input$mRNAseqData.selected.columns  == colnames(mRNAseqData.full))){
    Ready.table <- Render_Table(mRNAseqData.full)}else{
    withProgress(message = 'Generating table data', detail = "part 0", value = 0, {
      for (i in 1:2) {
        # Each time through the loop, add another row of data. This a stand-in
        # for a long-running computation.
        
          table <- mRNAseqData.full %>% select(one_of(input$mRNAseqData.selected.columns))
          Ready.table <- Render_Table(unique(table))
       
          # Increment the progress bar, and update the detail text.
          incProgress(0.5, detail = paste("part", i))
          
          # Pause for 0.1 seconds to simulate a long computation.
          #Sys.sleep(0.1)
      }})}
  
  Ready.table
  #Render_Table(SQL_Table("mRNAseqData"))
})

#miR datatable
output$Homemade_miR_table <- DT::renderDataTable({
  table <- miRseqData.full %>% select(one_of(input$miRseqData.selected.columns))
  Render_Table(unique(table))
  #Render_Table(SQL_Table("miRseqData"))
})

#ALL Protein datatable
output$Homemade_protein_table <- DT::renderDataTable({
  table <- ProteinData.full %>% select(one_of(input$ProteinData.selected.columns))
  Render_Table(unique(table))
  #Render_Table(SQL_Table("ProteinData"))
})

####SINGLE GENE

# mRNA / miR data TABLE 
output$Exp_homemade_table_unique <- DT::renderDataTable({
  
  exp1.Status()
  
  if (id.query$Id.data != "No data"){
  
  if(input$Product.type == "miR"){
    
    if (input$Unique_table_type == 1){
      Render_Table(data.query$UniquemiRData[c(1:4,6)])
    }
    else if (input$Unique_table_type == 2){
      Render_Table(data.query$miR_RatioStat_Data_Myogenesis)
    }
    else if (input$Unique_table_type == 3){
      Render_Table(data.query$miR_RatioStat_Data_Phenotype)
    }
    
  }else if(input$Product.type == "mRNA / Protein"){
    
    if (input$Unique_table_type == 1){
      Render_Table(data.query$UniquemRNAData[c(1:4,6)])
    }else if (input$Unique_table_type == 2){
      Render_Table(data.query$mRNA_RatioStat_Data_Myogenesis)
    }else if (input$Unique_table_type == 3){
      Render_Table(data.query$mRNA_RatioStat_Data_Phenotype)
    }
  }
  
  }
})

# Protein Data TABLE 
output$Exp_homemade_table_uniqueProt <- DT::renderDataTable({
  
  exp1.Status()
  
  if (id.query$Id.data != "No data"){
    colnames(ProteinData_Table) <- c("" , id.query$ID.selected)   
    ProteinData.unique <- unique(ProteinData.full[ , c(4,6:22)])
    ProteinData_Table[ , 2] <- t(ProteinData.unique[ProteinData.unique$Uniprot %in% id.query$Id.data$uniprot , ]) #id.query$Id.data$ensembl
    Render_Table(ProteinData_Table)
  }
  
})


#### PLOTS #### 

#### PCA - ALL GENE #### 

output$Exp_homemade_PCA.mRNA <- renderPlotly({
  
  if(input$mRNA.PCA.threshold == 10){
    if(input$mRNA.PCA.samples == "All"){
      PCA.graph("mRNA",input$mRNA.PCA.samples, mRNA.PCs, input$mRNA.PCA.component1, input$mRNA.PCA.component2, input$mRNA.PCA.legend)
    }else if(input$mRNA.PCA.samples == "Healthy"){
      PCA.graph("mRNA",input$mRNA.PCA.samples, mRNA.healthy.PCs, input$mRNA.PCA.component1, input$mRNA.PCA.component2, input$mRNA.PCA.legend)
    }else if(input$mRNA.PCA.samples == "DMD"){
      PCA.graph("mRNA",input$mRNA.PCA.samples, mRNA.DMD.PCs, input$mRNA.PCA.component1, input$mRNA.PCA.component2, input$mRNA.PCA.legend)
    }
    
  }else{
  
  withProgress(message = 'Generating PCA data', detail = "part 0", value = 0, {
    for (i in 1:3) {
      # Each time through the loop, add another row of data. This a stand-in
      # for a long-running computation.
      
      data.query$mRNA.PCs <- PCA.data("mRNA", mRNAseqData.short, input$mRNA.PCA.samples, input$mRNA.PCA.threshold)
      graph2 <- PCA.graph("mRNA",input$mRNA.PCA.samples, data.query$mRNA.PCs, input$mRNA.PCA.component1, input$mRNA.PCA.component2, input$mRNA.PCA.legend)

      # Increment the progress bar, and update the detail text.
      incProgress(0.333, detail = paste("part", i))
      
      # Pause for 0.1 seconds to simulate a long computation.
      #Sys.sleep(0.1)
    }})
  
  graph2  
  }
})

output$Exp_homemade_PCA.mRNA.Download <- downloadHandler(
  filename = paste0("PCA.mRNA.", input$mRNA.PCA.samples, "(",input$mRNA.PCA.threshold ,")data.csv"),
  content = function(file) {
    if(input$mRNA.PCA.samples == "All" && input$mRNA.PCA.threshold == 10){
      write.csv(mRNA.PCs$ind$coord, file)}else{
        write.csv(data.query$mRNA.PCs$ind$coord, file)}
  }
)

output$Exp_homemade_PCA.miR <- renderPlotly({
  data.query$miR.PCs <- PCA.data("miR", miRseqData.short, input$miR.PCA.samples, input$miR.PCA.threshold)
  PCA.graph("miR",input$miR.PCA.samples, data.query$miR.PCs, input$miR.PCA.component1, input$miR.PCA.component2, input$miR.PCA.legend)
})

output$Exp_homemade_PCA.miR.Download <- downloadHandler(
  filename = paste0("PCA.miR.", input$miR.PCA.samples, "(", input$miR.PCA.threshold,")data.csv"),
  content = function(file) {
    write.csv(data.query$miR.PCs$ind$coord, file)
  }
)


#### Clustering - ALL GENE #### 
output$Exp_homemade_clust.mRNA <- renderPlot({
  data.query$mRNA.Clust <- Clust.data(mRNAseqData.short, "mRNA", input$mRNA.clust.samples, input$mRNA.clust.threshold, input$mRNA.clust.dist, input$mRNA.clust.clust)
  Clust.graph(data.query$mRNA.Clust)
})

output$Exp_homemade_clust.mRNA.Download <- downloadHandler(
  filename = paste0("Clustering.mRNA.", input$mRNA.clust.samples, "(",input$mRNA.clust.threshold , "_", input$mRNA.clust.dist,"_", input$mRNA.clust.clust,")data.rds"),
  content = function(file) {
    saveRDS(Clust.data, file)
  }
)

output$Exp_homemade_clust.miR <- renderPlot({
  data.query$miR.Clust <- Clust.data(miRseqData.short, "miR", input$miR.clust.samples, input$miR.clust.threshold, input$miR.clust.dist, input$miR.clust.clust)
  Clust.graph(data.query$miR.Clust)
})

output$Exp_homemade_clust.miR.Download <- downloadHandler(
  filename = paste0("Clustering.miR.", input$miR.clust.samples, "(",input$miR.clust.threshold , "_", input$miR.clust.dist,"_", input$miR.clust.clust,")data.rds"),
  content = function(file) {
    saveRDS(Clust.data, file)
  }
)

#### Correlations - ALL GENE #### 

output$Exp_homemade_corr.mRNA <- renderPlotly({
  
  if (input$mRNA.corr.threshold == 10 && input$mRNA.corr.method == "spearman"){
      if(input$mRNA.corr.samples == "All"){
        Corr.graph(mRNA.Corrs)
      }else if(input$mRNA.corr.samples == "Healthy"){
        Corr.graph(mRNA.healthy.Corrs)
      }else if(input$mRNA.corr.samples == "DMD"){
        Corr.graph(mRNA.DMD.Corrs)
      }
    
  }else{
  
    withProgress(message = 'Generating correlation data', detail = "part 0", value = 0, {
      for (i in 1:3) {
        # Each time through the loop, add another row of data. This a stand-in
        # for a long-running computation.
        
        data.query$mRNA.Corrs <- Corr.data(mRNAseqData.short, "mRNA", input$mRNA.corr.samples, input$mRNA.corr.threshold, input$mRNA.corr.method)
        graph <- Corr.graph(data.query$mRNA.Corrs)
 
        # Increment the progress bar, and update the detail text.
        incProgress(0.333, detail = paste("part", i))
        
        # Pause for 0.1 seconds to simulate a long computation.
        #Sys.sleep(0.1)
      }})
    
    graph 
  
  }
})

output$Exp_homemade_corr.mRNA.Download <- downloadHandler(
  filename = paste0("Correlation.mRNA.", input$mRNA.corr.samples, "(",input$mRNA.corr.threshold , "_", input$mRNA.corr.method,")data.csv"),
  content = function(file) {
    if (input$mRNA.corr.samples == "All" && input$mRNA.corr.threshold == 10 && input$mRNA.corr.method == "spearman"){
      write.csv(mRNA.Corrs, file)}else{write.csv(data.query$mRNA.corrs, file)}
  }
)

output$Exp_homemade_corr.miR <- renderPlotly({
  data.query$miR.Corrs <- Corr.data(miRseqData.short, "miR", input$miR.corr.samples, input$miR.corr.threshold, input$miR.corr.method)
  Corr.graph(data.query$miR.Corrs)
})

output$Exp_homemade_corr.miR.Download <- downloadHandler(
  filename = paste0("Correlation.miR.", input$miR.corr.samples, "(",input$miR.corr.threshold , "_", input$miR.corr.method,")data.csv"),
  content = function(file) {
    Correlation.data <- data.query$miR.Corrs
    write.csv(Correlation.data, file)
  }
)

#### Single-cell mRNA - ALL GENE #### 
output$Exp_homemade_SingleCell.reduc <- renderPlotly({

    DimPlot(object = singlecell, 
            reduction = input$SingleCell.reduction,
            dims = c(input$SingleCell.component1, input$SingleCell.component2),
            label = FALSE,
            cols =  '#9ED93B'
    ) + NoLegend()
    ggplotly(p = ggplot2::last_plot() , 
             width = NULL , 
             height = NULL , 
             tooltip = "all" , 
             dynamicTicks = FALSE , 
             layerData = 1 , 
             originalData = TRUE , 
             source = "A") %>% layout(margin = m)
  
})

output$Exp_homemade_SingleCell.reduc.Download <- downloadHandler(
  filename = "SingleCell.rds",
  content = function(file) {
    saveRDS(singlecell, file)
  }
)

#### mRNA / miR plots - SINGLE GENE #### 
output$Exp_homemade_Graph_plot1 <- renderPlotly({
  
  exp1.Status()
  
  if (id.query$Id.data != "No data"){
    if(input$Product.type == "miR"){
      
      UniquemiRData_sorted <- data.query$UniquemiRData[order(data.query$UniquemiRData$Cell_line) , ]
      UniquemiRData_sorted <- UniquemiRData_sorted[order(UniquemiRData_sorted$Phenotype, decreasing = TRUE) , ]
      UniquemiRData_sorted <- UniquemiRData_sorted[order(UniquemiRData_sorted$Cell_stage) , ]
      
      UniquemiRmeanData <- Graph3_table.b(data.query$UniquemiRData, UniquemiRmeanData, "miR")
      
      Graph_view(input$Exp_homemade_Graph, UniquemiRData_sorted, UniquemiRmeanData, id.query$ID.selected, data.query$miR_RatioStat_Data_Myogenesis, data.query$miR_RatioStat_Data_Phenotype)
      
    }else if(input$Product.type == "mRNA / Protein"){

      UniquemRNAData_sorted <- data.query$UniquemRNAData[order(data.query$UniquemRNAData$Cell_line), ]
      UniquemRNAData_sorted <- UniquemRNAData_sorted[order(UniquemRNAData_sorted$Phenotype, decreasing = TRUE) , ]
      UniquemRNAData_sorted <- UniquemRNAData_sorted[order(UniquemRNAData_sorted$Cell_stage) , ]
      
      UniquemRNAmeanData <- Graph3_table.b(data.query$UniquemRNAData, UniquemRNAmeanData, "mRNA")
      
      Graph_view(input$Exp_homemade_Graph, UniquemRNAData_sorted, UniquemRNAmeanData, id.query$ID.selected, data.query$mRNA_RatioStat_Data_Myogenesis, data.query$mRNA_RatioStat_Data_Phenotype)
    }   
  }
})

#### Protein plot - SINGLE GENE #### 

output$Exp_homemade_Graph_plot2 <- renderPlotly({
    
    exp1.Status()
  
    if (id.query$Id.data != "No data"){
      {
        ProteinData.unique <- unique(ProteinData.full[, c(4, 9:17)])
        ProteinData_Boxplot[ , 3] <- t(ProteinData.unique[ProteinData.unique$Uniprot %in% id.query$Id.data$uniprot , -1])
        if (!is.na(ProteinData_Boxplot[1,3])){
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
                              text = "") %>% layout(title = id.query$ID.selected, margin = m)
          }}else{message("No data")}
      }}
})

output$proteomic.exist <- renderPrint({ 
  
  if (id.query$Id.data != "No data"){
      ProteinData.unique <- unique(ProteinData.full[, c(4, 9:17)])
      ProteinData_Boxplot[ , 3] <- t(ProteinData.unique[ProteinData.unique$Uniprot %in% id.query$Id.data$uniprot , -1])
  }
  
  if (!is.null(id.query$Id.data) && is.na(ProteinData_Boxplot[1,3])){print("No proteomic data")}
  
})

#### Single-cell mRNA - SINGLE GENE #### 


singleCell.Status <- eventReactive(input$display.singlecell, {
  
  withProgress(message = 'Generating single-cell data', detail = "part 0", value = 0, {
    for (i in 1:3) {
      # Each time through the loop, add another row of data. This a stand-in
      # for a long-running computation.
  
    data.query$sc.reads.table <- SC_datacount(singlecell.matrix, id.query$Id.data$symbol)
    
    FeaturePlot(
      singlecell,
      id.query$Id.data$symbol,
      reduction = input$SingleCell.reduction.2,
      dims = c(input$SingleCell.component1.2, input$SingleCell.component2.2),
      cells = NULL,
      c("lightgrey", '#9ED93B', '#3F5617', '#000000'))
    
    data.query$sc.reads.plot <- ggplotly(p = ggplot2::last_plot() , 
                                         width = NULL , 
                                         height = NULL , 
                                         tooltip = "all" , 
                                         dynamicTicks = FALSE , 
                                         layerData = 1 , 
                                         originalData = TRUE , 
                                         source = "A") %>% layout(margin = m)
  
  
  
    # Increment the progress bar, and update the detail text.
    incProgress(0.333, detail = paste("part", i))
    
    # Pause for 0.1 seconds to simulate a long computation.
    #Sys.sleep(0.1)
    }})

}) 
  
output$Exp_homemade_Graph_plot3 <- renderPlotly({
  
  singleCell.Status()
  
  plot_ly(
    x = rownames(data.query$sc.reads.table),
    y = data.query$sc.reads.table[,1],
    type = "bar", 
    marker = list(color = 'rgb(158,217,59)')
  ) %>% layout(title = id.query$ID.selected, xaxis = list(title = "Number of reads"), yaxis = list(title = "Number of cells"), margin = m)
})

output$Exp_homemade_Graph_plot4 <- renderPlotly({
  singleCell.Status()
  data.query$sc.reads.plot
})

