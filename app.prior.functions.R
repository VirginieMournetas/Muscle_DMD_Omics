#### GENERAL -  Table view ####
Render_Table <- function(DATA){
  DT::datatable(
    DATA , 
    escape = FALSE, #for HTML input
    filter = 'top', 
    style = "default",
    class = 'cell-border stripe' , 
    extensions = c('Scroller','Buttons'), 
    options = list(
      dom = 'Bfrtip', #for buttons
      buttons = 
        list('copy', 'print', list(
          extend = 'collection',
          buttons = c('csv', 'excel', 'pdf'),
          text = 'Download'
        )),
      deferRender = TRUE, #with 'Scroller'
      scrollY = 250,#with 'Scroller'
      scroller = TRUE ,#with 'Scroller'
      autoWidth = FALSE,
      scrollX = TRUE ,
      lengthMenu = list(c(10 , 50 , -1) , c('10' , '50' , 'All')) ,
      columnDefs = list(
        list(className = 'dt-center', targets = c(0:length(DATA[1, ])) #Center columns
        ) 
      )
    )
  )%>%
    DT::formatStyle(columns = c(0:length(DATA[1, ])), fontSize = '80%') #change text size 
}


#### EXP - Gene ID CARD #### 
Infos <- function (Product.type, ID.selected, ID.type, con){
  
  if (Product.type == "miR"){
    IDs.db <- "human_full_miR_ids_db"
  }else if(Product.type == "mRNA / Protein"){
    IDs.db <- "human_genes_ids_db_simplified"
  }
  
  if(ID.type == "EntrezGene ID"){
    ID.type <- "entrezgeneid"
  }else if(ID.type == "Ensembl ID"){
    ID.type <- "ensembl"
  }else if(ID.type == "Official Symbol"){
    ID.type <- "symbol"
  }else if(ID.type == "Official Name"){
    ID.type <- "name"
  }else if(ID.type == "Uniprot ID"){
    ID.type <- "uniprot"
  }else if(ID.type == "mirBase ID"){
    ID.type <- "human_mirbase_id"
  }else if(ID.type == "mirBase Name"){
    ID.type <- "human_mirbase_name"
  }
  
  query <- paste("SELECT * FROM", IDs.db, "WHERE", ID.type, "=", paste0("'", ID.selected,"'"))
  connect <- dbConnect(RMySQL::MySQL(), user = con[4] , password = con[5] , host=con[2] , port = as.integer(con[3]) , dbname = con[1])
  res1 <- dbGetQuery(connect, query)
  dbDisconnect(connect) 
  
  res2 <- NULL
  for (i in c(1:length(names(res1)))){
    res2 <- c(res2, list(unique(res1[,i])))
  }
  names(res2) <- names(res1)
  res2
  
  return(res2)
}

Infos2 <- function (Product.type, ID.selected, ID.type){
  
  if (Product.type == "miR"){
    IDs.db <- human.mir.IDs.final
  }else if(Product.type == "mRNA / Protein"){
    IDs.db <- human.genes.IDs.final
  }
  
  if(ID.type == "EntrezGene ID"){
    ID.type <- "entrezgeneid"
  }else if(ID.type == "Ensembl ID"){
    ID.type <- "ensembl"
  }else if(ID.type == "Official Symbol"){
    ID.type <- "symbol"
  }else if(ID.type == "Official Name"){
    ID.type <- "name"
  }else if(ID.type == "Uniprot ID"){
    ID.type <- "uniprot"
  }else if(ID.type == "mirBase ID"){
    ID.type <- "human_mirbase_id"
  }else if(ID.type == "mirBase Name"){
    ID.type <- "human_mirbase_name"
  }
  
  query <- unique(IDs.db[IDs.db[[ID.type]] == ID.selected,])
  query <- query[rowSums(is.na(query)) != ncol(query), ]
  
  for (i in c(1:length(names(query)))){
    query[,i] <- as.character(query[,i])
  }
  query$entrezgeneid <- as.integer(query$entrezgeneid)

  query.data <- NULL
  for (i in c(1:length(names(query)))){
    query.data <- c(query.data , list(unique(query[,i])))
  }
  names(query.data) <- names(query)
  
  return(query.data)
  
}

db_Links <- function (Product.type, Infos){ #need to deal with multiple 
  links_list <- data.frame(matrix(NA, ncol =3, nrow = 25))
  links_list[1,3] <- paste0("https://www.genecards.org/cgi-bin/carddisp.pl?gene=" , Infos$symbol[1]) #genecards
  links_list[2,3] <- paste0("https://www.ncbi.nlm.nih.gov/gene/" , Infos$entrezgeneid[1]) #ncbi
  links_list[3,3] <- paste0("http://www.ensembl.org/Homo_sapiens/Gene/Summary?db=core;g=" , Infos$ensembl[1]) #ensembl
  links_list[4,3] <- paste0("https://pax-db.org/search?q=",Infos$symbol[1] , "%0A&speciesId=9606") #pax-db
  links_list[5,3] <- paste0("https://www.uniprot.org/uniprot/" , Infos$uniprot[1]) #uniprot
  links_list[6,3] <- paste0("https://www.proteinatlas.org/" , Infos$ensembl[1], "-" , Infos$symbol[1]) #proteinatlas
  links_list[7,3] <- paste0("https://www.disgenet.org/browser/1/1/0/" , Infos$entrezgeneid[1] , "/") #disgenet
  links_list[8,3] <- paste0("https://ghr.nlm.nih.gov/gene/", Infos$symbol[1]) #
  links_list[9,3] <- paste0("https://omim.org/search/?index=entry&start=1&limit=10&sort=score+desc%2C+prefix_sort+desc&search=", Infos$symbol[1]) #omim
  links_list[10,3] <- paste0("https://targetmine.mizuguchilab.org/targetmine/keywordSearchResults.do;jsessionid=F57CD39E0E1A771527B06B2894858C29?searchTerm=", Infos$symbol[1], "&searchSubmit=GO") #targetmine
  links_list[11,3] <- paste0("https://www.humanmine.org/humanmine/keywordSearchResults.do?searchTerm=", Infos$symbol[1], "&searchSubmit=GO") #humanmine
  links_list[12,3] <- paste0("https://www.grnpedia.org/trrust/result_tonly.php?gene=", Infos$symbol[1], "&species=human&confirm=0") #trrust
  links_list[13,3] <- paste0("https://www.genome.jp/dbget-bin/www_bget?hsa:", Infos$entrezgeneid[1]) #kegg
  links_list[14,3] <- paste0("https://thebiogrid.org/108096/summary/homo-sapiens/", Infos$symbol[1], ".html") #biogrid
  links_list[15,3] <- paste0("http://carolina.imis.athena-innovation.gr/diana_tools/web/index.php?r=tarbasev8%2Findex&miRNAs%5B%5D=&genes%5B%5D=", Infos$symbol[1], "&sources%5B%5D=1&sources%5B%5D=7&sources%5B%5D=9&publication_year=&prediction_score=&sort_field=&sort_type=&query=") #diana_tools - gene
  links_list[22,3] <- paste0("http://carolina.imis.athena-innovation.gr/diana_tools/web/index.php?r=tarbasev8%2Findex&miRNAs%5B%5D=&miRNAs%5B%5D=", Infos$human_mirbase_name[1], "&genes%5B%5D=&sources%5B%5D=1&sources%5B%5D=7&sources%5B%5D=9&publication_year=&prediction_score=&sort_field=&sort_type=&query=1") #diana_tools - miR
  links_list[16,3] <- paste0("http://mirtarbase.mbc.nctu.edu.tw/php/search.php?org=hsa&kw=", Infos$symbol[1], "&opt=target") #mirtarbase
  links_list[23,3] <- paste0("http://mirtarbase.mbc.nctu.edu.tw/php/search.php?org=hsa&opt=mirna_id&kw=", Infos$human_mirbase_name[1]) #mirtarbase
  links_list[17,3] <- paste0("http://www.targetscan.org/cgi-bin/targetscan/vert_72/targetscan.cgi?species=Human&gid=", Infos$symbol[1], "&mir_sc=&mir_c=&mir_nc=&mir_vnc=&mirg=") #TargetScanHuman
  links_list[24,3] <- paste0("http://www.targetscan.org/cgi-bin/targetscan/vert_72/targetscan.cgi?species=Human&gid=&mir_sc=&mir_c=&mir_nc=&mir_vnc=&mirg=", Infos$human_mirbase_name[1]) #TargetScanHuman
  links_list[25,3] <- paste0("http://www.mirbase.org/cgi-bin/mirna_entry.pl?acc=", Infos$human_mirbase_id[1])
  links_list[18,3] <- paste0("https://www.ncbi.nlm.nih.gov/search/all/?term=", Infos$symbol[1]) #ALL_NCBI
  links_list[19,3] <- paste0("https://www.ncbi.nlm.nih.gov/bioproject/?term=", Infos$symbol[1]) #BioProject
  links_list[20,3] <- paste0("https://clinicaltrials.gov/ct2/results?cond=&term=", Infos$symbol[1]) #ClinicalTrials
  links_list[21,3] <- paste0("https://www.ncbi.nlm.nih.gov/clinvar/?term=", Infos$symbol[1], "%5Bgene%5D") #ClinVar
  
  return(links_list)
}


#### EXP - HOMEMADE GRAPHS #### 

#Table For graph1 & 2
Graph1_2_table.b <- function (RNAseqData, UniqueRNAData, product.type, Id.data, ID.input){
  
  if (product.type == "mRNA"){
    list <- c(2:103)
    input_RNA <- Id.data$ensembl
  }else if (product.type == "miR"){
    list <- c(2:43)
    input_RNA <- Id.data$symbol
  }
  p <- 1
  
  RNAdata <- t(RNAseqData[RNAseqData[ , p] == input_RNA , list])
  colnames(RNAdata) <- ID.input
  UniqueRNAData[ , 6] <- RNAdata
  
  return(UniqueRNAData)
}

#Table For graph3
Graph3_table.b <- function (UniqueRNAData, UniqueRNAmeanData, product.type){
  if (product.type == "mRNA"){
    a <- 1
    for(i in c(1:4)){
      expression <- c(UniqueRNAData[a , 6] , UniqueRNAData[a+1 , 6] , UniqueRNAData[a+2 , 6])
      UniqueRNAmeanData[i , 4] <- mean(expression)
      UniqueRNAmeanData[i , 5] <- sd(expression)
      UniqueRNAmeanData[i , 6] <- UniqueRNAmeanData[i , 5]/sqrt(3)
      a <- a+3
    }
    for(i in c(5:14)){
      expression <- c(UniqueRNAData[a , 6] , UniqueRNAData[a+1 , 6] , UniqueRNAData[a+2 , 6] , UniqueRNAData[a+3 , 6] , UniqueRNAData[a+4 , 6] , UniqueRNAData[a+5 , 6] , UniqueRNAData[a+6 , 6] , UniqueRNAData[a+7 , 6] , UniqueRNAData[a+8 , 6])
      UniqueRNAmeanData[i , 4] <- mean(expression)
      UniqueRNAmeanData[i , 5] <- sd(expression)
      UniqueRNAmeanData[i , 6] <- UniqueRNAmeanData[i , 5]/sqrt(9)
      a <- a+9
    }
  }else if (product.type == "miR"){
    a <- 1
    for(i in c(1:14)){
      expression <- c(UniqueRNAData[a , 6] , UniqueRNAData[a+1 , 6] , UniqueRNAData[a+2 , 6])
      UniqueRNAmeanData[i , 4] <- mean(expression)
      UniqueRNAmeanData[i , 5] <- sd(expression)
      UniqueRNAmeanData[i , 6] <- UniqueRNAmeanData[i , 5]/sqrt(3)
      a <- a+3
    }
  }
  return(UniqueRNAmeanData)
}

#Table preparation for a unique mRNA/miR - Graph/table 4-5 MYOGENESIS with stats & Graph/table 6-7 PHENOTYPE with stats
Graph4_to_7_table <- function (empty_table, RNA_RatioStats_Data_all, product.type, Id.data, comparison){
  
  list <- c(2, 8, 14, 20, 26, 32, 38)
  p <- 1
  
  if (comparison == "MYOGENESIS"){
    a <- 0
  }else if(comparison == "PHENOTYPE"){
    a <- 42
  }
  
  if (product.type == "miR"){
    input_RNA <- Id.data$symbol
  }else if(product.type == "mRNA"){
    input_RNA <- Id.data$ensembl
  }
  
  {
    empty_table[ , 2] <- t(RNA_RatioStats_Data_all[RNA_RatioStats_Data_all[ , p] == input_RNA , list + a])
    empty_table[ , 3] <- t(RNA_RatioStats_Data_all[RNA_RatioStats_Data_all[ , p] == input_RNA , list + a + 1])
  }#For graph4
  {
    empty_table[ , 4] <- t(RNA_RatioStats_Data_all[RNA_RatioStats_Data_all[ , p] == input_RNA , list + a + 4])
    empty_table[ , 5] <- t(RNA_RatioStats_Data_all[RNA_RatioStats_Data_all[ , p] == input_RNA , list + a + 5])
    for (n in c(1:7))
    {
      if(empty_table[n , 5]>0.05 | is.na(empty_table[n , 5])) {empty_table[n , 6] = "NO"}else{empty_table[n , 6] = "YES"}
    }
  }#For graph5
  
  return(empty_table)
} 

#mRNA / miR graphs
Graph_view <- function(input_RNA_Graph, UniqueRNAData_sorted, UniqueRNAmeanData, input_RNA, RNA_RatioStats_Data_Myogenesis, RNA_RatioStats_Data_Phenotype){
  if (input_RNA_Graph == 1){
    fig <- plot_ly() 
    fig <- fig %>%
      add_trace( 
        y = UniqueRNAData_sorted[ , 6] , 
        x = UniqueRNAData_sorted[ , 7] , 
        color = UniqueRNAData_sorted[ , 2] , 
        colors = c('#340042' , '#340042' , '#340042','#9ED93B' , '#9ED93B' , '#9ED93B') , 
        type = "box" , 
        boxpoints = "all" , jitter = 0.2 , pointpos = -1.8
      )
    fig <- fig %>% layout(title = input_RNA, yaxis = list(title = "Normalised counts") , barmode = 'group')
    Graph <- fig
  }else if (input_RNA_Graph == 2) {
    fig <- plot_ly() 
    fig <- fig %>%
      add_trace(
        type = 'box' , 
        y = UniqueRNAData_sorted[ , 6] , 
        x = UniqueRNAData_sorted[ , 8] , 
        color = UniqueRNAData_sorted[ , 2] , 
        colors = c('#340042', '#9ED93B') , 
        boxpoints = "all" , jitter = 0.2 , pointpos = -1.5 , 
        text = UniqueRNAData_sorted[ , 7] , 
        showlegend = T
      ) 
    fig <- fig %>% layout(title = input_RNA, yaxis = list(title = "Normalised counts") , barmode = 'group')
    Graph <- fig
  }else if (input_RNA_Graph == 3) {
    ggplot(UniqueRNAmeanData
           , aes(x = Cell_stage , y = Expression.mean , colour = Phenotype , group = interaction(Phenotype , Group)))+          
      geom_errorbar(aes(ymin = Expression.mean-SEM , ymax = Expression.mean+SEM) , colour = "black" , width = .2 , size = 1) +
      geom_line(size = 1.2) +
      geom_point(size = 4)+
      xlab("Cell stage") +
      ylab("Normalised counts") +
      theme_bw() +
      scale_colour_manual(values = c('#340042', '#9ED93B'))+
      theme(axis.title.x = element_text(color = "black" , face = "bold" , size = 9) , 
            axis.title.y = element_text(color = "black" , face = "bold" , size = 9) , 
            legend.text = element_text(color = "black" , face = "bold" , size = 9) , 
            axis.text.x = element_text(color = "black" , angle = 90 ,  size = 9) , 
            axis.text.y = element_text(color = "black" , face = "bold" , size = 9) , 
            plot.title = element_text(lineheight = 8 , face = "bold" , size = 9)) +
      ggtitle(input_RNA)
    Graph <- ggplotly(p = ggplot2::last_plot() , 
                      width = NULL , 
                      height = NULL , 
                      tooltip = "all" , 
                      dynamicTicks = FALSE , 
                      layerData = 1 , 
                      originalData = TRUE , 
                      source = "A") %>% layout(margin = m)
  }else if (input_RNA_Graph == 4) {
    ggplot(RNA_RatioStats_Data_Myogenesis
           , aes(x = Days , y = Mean)) +          
      geom_errorbar(aes(ymin = Mean-SEM , ymax = Mean+SEM) , colour = "black" , width = .2) +
      geom_point(color = "black" , size = 3) + 
      geom_hline(yintercept = 1) +
      geom_hline(yintercept = 2 , linetype = "dashed") +
      geom_hline(yintercept = 1.32 , linetype = "dashed" , color = "grey") +
      geom_hline(yintercept = 0.76 , linetype = "dashed" , color = "grey") +
      geom_hline(yintercept = 0.5 , linetype = "dashed") +
      scale_y_continuous(trans = log2_trans() , breaks = c(0.5 , 0.76 , 1 , 1.32 , 2)) +  
      ylab("Expression ratios") +    
      theme_bw() +
      ggtitle(input_RNA)+
      theme(axis.title.x = element_text(color = "black" , face = "bold" , size = 9) , 
            axis.title.y = element_text(color = "black" , face = "bold" , size = 9) , 
            legend.text = element_text(color = "black" , face = "bold" , size = 9) , 
            axis.text.x = element_text(color = "black" , angle = 90 ,  size = 9) , 
            axis.text.y = element_text(color = "black" , face = "bold" , size = 9) , 
            plot.title = element_text(lineheight = 8 , face = "bold" , size = 9))
    Graph <- ggplotly(p = ggplot2::last_plot() , 
                      width = NULL , 
                      height = NULL , 
                      tooltip = "all" , 
                      dynamicTicks = FALSE , 
                      layerData = 1 , 
                      originalData = TRUE , 
                      source = "A") %>% layout(margin = m)
  }else if (input_RNA_Graph == 5) {
    ggplot(RNA_RatioStats_Data_Myogenesis
           , aes(x = Days , y = Mean , colour = Significant)) +        
      geom_errorbar(aes(ymin = Mean-SEM , ymax = Mean+SEM) ,  width = .2 , color = "black") +
      scale_color_manual(values = c("black","orange")) +
      geom_point(size = 3) + 
      geom_hline(yintercept = 1) +
      geom_hline(yintercept = 2 , linetype = "dashed") +
      geom_hline(yintercept = 1.32 , linetype = "dashed" , color = "grey") +
      geom_hline(yintercept = 0.76 , linetype = "dashed" , color = "grey") +
      geom_hline(yintercept = 0.5 , linetype = "dashed") +
      scale_y_continuous(trans = log2_trans() , breaks = c(0.5 , 0.76 , 1 , 1.32 , 2)) +  
      ylab("Expression ratios") +    
      theme_bw() +
      ggtitle(input_RNA)+
      theme(axis.title.x = element_text(color = "black" , face = "bold" , size = 9) , 
            axis.title.y = element_text(color = "black" , face = "bold" , size = 9) , 
            legend.text = element_text(color = "black" , face = "bold" , size = 9) , 
            axis.text.x = element_text(color = "black" , angle = 90 ,  size = 9) , 
            axis.text.y = element_text(color = "black" , face = "bold" , size = 9) , 
            plot.title = element_text(lineheight = 8 , face = "bold" , size = 9))
    Graph <- ggplotly(p = ggplot2::last_plot() , 
                      width = NULL , 
                      height = NULL , 
                      tooltip = "all" , 
                      dynamicTicks = FALSE , 
                      layerData = 1 , 
                      originalData = TRUE , 
                      source = "A") %>% layout(margin = m)
  }else if (input_RNA_Graph == 6) {
    ggplot(RNA_RatioStats_Data_Phenotype
           , aes(x = Days , y = Mean))+          
      geom_errorbar(aes(ymin = Mean-SEM , ymax = Mean+SEM) , colour = "black" , width = .2)+
      geom_point(color = "black" , size = 3)+ 
      geom_hline(yintercept = 1)+
      geom_hline(yintercept = 2 , linetype = "dashed")+
      geom_hline(yintercept = 1.32 , linetype = "dashed" , color = "grey")+
      geom_hline(yintercept = 0.76 , linetype = "dashed" , color = "grey")+
      geom_hline(yintercept = 0.5 , linetype = "dashed")+
      scale_y_continuous(trans = log2_trans() , breaks = c(0.5 , 0.76 , 1 , 1.32 , 2))+  
      ylab("DMD/Healthy expression ratios") +    
      theme_bw() +
      ggtitle(input_RNA)+
      theme(axis.title.x = element_text(color = "black" , face = "bold" , size = 9) , 
            axis.title.y = element_text(color = "black" , face = "bold" , size = 9) , 
            legend.text = element_text(color = "black" , face = "bold" , size = 9) , 
            axis.text.x = element_text(color = "black" , angle = 90 ,  size = 9) , 
            axis.text.y = element_text(color = "black" , face = "bold" , size = 9) , 
            plot.title = element_text(lineheight = 8 , face = "bold" , size = 9))
    Graph <- ggplotly(p = ggplot2::last_plot() , 
                      width = NULL , 
                      height = NULL , 
                      tooltip = "all" , 
                      dynamicTicks = FALSE , 
                      layerData = 1 , 
                      originalData = TRUE , 
                      source = "A") %>% layout(margin = m)
  }else if (input_RNA_Graph == 7) {
    ggplot(RNA_RatioStats_Data_Phenotype
           , aes(x = Days , y = Mean , colour = Significant))+        
      geom_errorbar(aes(ymin = Mean-SEM , ymax = Mean+SEM) ,  width = .2 , color = "black")+
      scale_color_manual(values = c("black","orange"))+
      geom_point(size = 3)+ 
      geom_hline(yintercept = 1)+
      geom_hline(yintercept = 2 , linetype = "dashed")+
      geom_hline(yintercept = 1.32 , linetype = "dashed" , color = "grey")+
      geom_hline(yintercept = 0.76 , linetype = "dashed" , color = "grey")+
      geom_hline(yintercept = 0.5 , linetype = "dashed")+
      scale_y_continuous(trans = log2_trans() , breaks = c(0.5 , 0.76 , 1 , 1.32 , 2))+  
      ylab("Expression ratios") +    
      theme_bw() +
      ggtitle(input_RNA)+
      theme(axis.title.x = element_text(color = "black" , face = "bold" , size = 9) , 
            axis.title.y = element_text(color = "black" , face = "bold" , size = 9) , 
            legend.text = element_text(color = "black" , face = "bold" , size = 9) , 
            axis.text.x = element_text(color = "black" , angle = 90 ,  size = 9) , 
            axis.text.y = element_text(color = "black" , face = "bold" , size = 9) , 
            plot.title = element_text(lineheight = 8 , face = "bold" , size = 9))
    Graph <- ggplotly(p = ggplot2::last_plot() , 
                      width = NULL , 
                      height = NULL , 
                      tooltip = "all" , 
                      dynamicTicks = FALSE , 
                      layerData = 1 , 
                      originalData = TRUE , 
                      source = "A") %>% layout(margin = m)
  }  
  return(Graph)
}

#Single-cell graph
SC_datacount <- function(matrix, gene){
  
  gene.DATA <- matrix[gene, ] 
  
  data <- c(sum(gene.DATA == 0),
            sum(gene.DATA != 0),
            sum(gene.DATA == 1),
            sum(gene.DATA >= 2),
            sum(gene.DATA >= 5))
  
  table <- data.frame(gene = data)
  colnames(table) <- gene
  rownames(table) <- c("== 0","!= 0","== 1",">= 2",">= 5")
  
  return(table)
}

#PCA data
PCA.data <- function(Datatype , Data, samples.select, threshold){
  if (Datatype == "miR"){
    p <- 43
    v <- seq(2,43,3)}else if(Datatype == "mRNA"){
      p <- 103
      v <- c(seq(2,13,3), seq(14,103,9))}
  
  if (samples.select == "All"){samples <- c(2:p)}else{
    if (samples.select == "Healthy"){
      j <- 1
    }else if(samples.select == "DMD"){
      j <- 2 
    }
    samples <- c()
    for (i in c(1:7)){
      if(j == 14){k <- p}else{k <- v[j+1]-1}
      samples <- c(samples, v[j]: k)
      j <- j +2
    }
  }
  
  data.to.transform <- Data[apply(Data[,c(1,samples)], 1, max) >= threshold,] 
  expr.PCs <- PCA(t(data.to.transform[, samples]), scale.unit = TRUE, ncp = 6, graph = FALSE)
  return(expr.PCs)
}
#PCA graph
PCA.graph <- function(Datatype , samples.select, expr.PCs, x, y, color){
  
  if (Datatype == "miR"){
    SampleDescription <- SampleDescription[c(1:12,seq(13,102,3)),]
    p <- 43
    v <- seq(2,43,3)}else if(Datatype == "mRNA"){
      p <- 103
      v <- c(seq(2,13,3), seq(14,103,9))}
  
  if (samples.select == "All"){samples <- c(2:p)}else{
    if (samples.select == "Healthy"){
      j <- 1
    }else if(samples.select == "DMD"){
      j <- 2 
    }
    samples <- c()
    for (i in c(1:7)){
      if(j == 14){k <- p}else{k <- v[j+1]-1}
      samples <- c(samples, v[j]: k)
      j <- j +2
    }
    SampleDescription <- SampleDescription[samples-1, ]
  }
  
  fig <- plot_ly(data = as.data.frame(expr.PCs$ind$coord), 
                 x = expr.PCs$ind$coord[,x], 
                 y = expr.PCs$ind$coord[,y], 
                 color = SampleDescription[,as.integer(color)], 
                 text = paste("Cell stage:", SampleDescription[,1], 
                              '<br>Phenotype:', SampleDescription[,2],
                              '<br>Cell line:', SampleDescription[,3],
                              '<br>Differentiation:', SampleDescription[,4]),
                 type = 'scatter',
                 mode = "markers") %>% layout(
                   margin = m,
                   title = "PCA",
                   xaxis = list(title = paste("Dim",x, paste0("(", round(expr.PCs$eig[x,2], 1)), "%)")),
                   yaxis = list(title = paste("Dim",y, paste0("(", round(expr.PCs$eig[y,2], 1)), "%)")))
  
  fig
}

#Correlation data
Corr.data <- function(Data, Datatype, samples.select, threshold = 5, method = "spearman"){
  if (Datatype == "miR"){
    p <- 43
    v <- seq(2,43,3)}else if(Datatype == "mRNA"){
      p <- 103
      v <- c(seq(2,13,3), seq(14,103,9))}
  
  if (samples.select == "All"){samples <- c(2:p)}else{
    if (samples.select == "Healthy"){
      j <- 1
    }else if(samples.select == "DMD"){
      j <- 2 
    }
    samples <- c()
    for (i in c(1:7)){
      if(j == 14){k <- p}else{k <- v[j+1]-1}
      samples <- c(samples, v[j]: k)
      j <- j +2
    }
  }
  
  data.to.transform <- Data[apply(Data[,c(1,samples)], 1, max) >= threshold,] 
  StandardizedData <- scale(t(data.to.transform[,samples]), center = TRUE, scale = TRUE)
  CorrData <- cor(t(StandardizedData), method = method, use = "pairwise.complete.obs")
  
  return(CorrData)

}
#Correlation graph
Corr.graph <- function(CorrData){
  
  #do this before the transformation!
  CorrData[upper.tri(CorrData, diag = FALSE)] <- NA
  #CorrData <- CorrData[-1, -ncol(CorrData)]
  
  #Store our variable names for later use
  x_labels <- colnames(CorrData)
  y_labels <- rownames(CorrData)
  
  #Change the variable names to numeric for the grid
  colnames(CorrData) <- 1:ncol(CorrData)
  rownames(CorrData) <- nrow(CorrData):1
  
  #Melt the data into the desired format
  plotdata <- reshape2::melt(CorrData)
  
  #Adding the size variable & scaling it
  plotdata$size <- (abs(plotdata$value))
  scaling <- 500 / ncol(CorrData) / 2
  plotdata$size <- plotdata$size * scaling
  
  #Setting x and y ranges for the chart
  xrange <- c(0.5, length(x_labels)+0.5)
  yrange <- c(0.5, length(y_labels)+0.5)
  
  #Setting the gridlines
  x_grid <- seq(1.5, length(x_labels)-0.5, 1)
  y_grid <- seq(1.5, length(y_labels)-0.5, 1)
  
  
  #Axes definitions
  xAx1 <- list(showgrid = FALSE,
               showline = FALSE,
               zeroline =  FALSE,
               tickvals = colnames(CorrData),
               ticktext = x_labels,
               title = "",
               tickfont = list(size = 8),
               range = xrange,
               rangemode = "tozero")
  
  xAx2 <- list(showgrid = TRUE,
               showline = FALSE,
               zeroline =  FALSE,
               overlaying = "x",
               showticklabels = FALSE,
               tickfont = list(size = 8),
               range = xrange,
               tickvals = x_grid)
  
  yAx1 <- list(autoaxis = FALSE,
               showgrid = FALSE,
               showline = FALSE,
               zeroline =  FALSE,
               tickvals = rownames(CorrData),
               ticktext = y_labels,
               tickfont = list(size = 8),
               title = FALSE,
               rangemode = "tozero",
               range = yrange)
  
  yAx2 <- list(showgrid = TRUE,
               showline = FALSE,
               zeroline =  FALSE,
               tickfont = list(size = 8),
               overlaying = "y",
               showticklabels = FALSE,
               range = yrange,
               tickvals = y_grid)
  
  
  fig <- plot_ly(data = plotdata, height = 525)
  
  fig <- fig %>% add_trace(x = ~Var2, y = ~Var1, type = "scatter", mode = "markers",
                           color = ~value,
                           marker = list(size = ~size, opacity = 1),
                           symbol = I("circle"),
                           text = ~value,
                           hovertemplate = "%{text:.2f} <extra></extra>",
                           xaxis = "x1",
                           yaxis = "y1")
  
  fig <- fig %>% layout(xaxis = xAx1,
                        yaxis = yAx1, 
                        xaxis2 = xAx2,
                        yaxis2 = yAx2#,
                        #plot_bgcolor = "rgba(0,0,0,0)",
                        #paper_bgcolor = "rgba(0, 0, 0, 0.03)"
  )
  
  fig <- fig %>% colorbar(title = "", limits = c(-1,1), x = 1.1, y = 0.75)
  
  fig
}



