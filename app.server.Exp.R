id.query <- reactiveValues()

#### ID list #### 

output$Product.select <- renderUI({
  if (input$Product.type == "miR"){
    list <- list("EntrezGene ID",
                 "Ensembl ID",
                 "Official Symbol",
                 "Official Name",
                 "mirBase ID",
                 "mirBase Name")
    
  }else if(input$Product.type == "mRNA / Protein"){
    list <- list("EntrezGene ID",
                 "Ensembl ID",
                 "Official Symbol",
                 "Official Name",
                 "Uniprot ID")
  }
  selectInput("ID.type",
              label = NULL,
              choices = list,
              selected = "Official Symbol",
              multiple = FALSE , 
              width = 150)
})


#### ID select #### 
output$ID.select <- renderUI({
  
  if (input$Product.type == "miR"){
    val <- "MIR206"
  }else if(input$Product.type == "mRNA / Protein"){
    val <- "DMD"
  }
  
  textInput("ID.selected", label = NULL , value = val, width = '150px')

}) 



#### Observe #### 

exp1.Status <- reactive({

  if (input$ID.type == "Ensembl ID" | input$ID.type == "Official Symbol" | input$ID.type == "Uniprot ID" | input$ID.type == "mirBase ID"){
    id.query$ID.selected <- toupper(input$ID.selected)
  }else{id.query$ID.selected <- input$ID.selected}
  
  id.query$Id.data <- Infos(input$Product.type, id.query$ID.selected, input$ID.type, con)
  
})



#### mRNA db links #### 


output$RNA_info <- renderUI({
  
  exp1.Status()
  if (input$Product.type == "miR"){
    p(style = "text-align:left", 
      tags$b("Full name: "), br(),  paste(id.query$Id.data$name, collapse = ", "), br() , br() ,
      tags$b("Official Symbol: "), br(), paste(id.query$Id.data$symbol, collapse = ", "), br() , br() ,
      tags$b("Ensembl ID: "), br(), paste(id.query$Id.data$ensembl, collapse = ", "), br() , br() ,
      tags$b("EntrezGene ID: "), br(),  paste(id.query$Id.data$entrezgeneid, collapse = ", "), br() , br() ,
      tags$b("mirBase ID: "), br(),  paste(id.query$Id.data$human_mirbase_id, collapse = ", "), br() , br() ,
      tags$b("mirBase name: "), br(),  paste(id.query$Id.data$human_mirbase_name, collapse = ", "), br() , br() ,
      tags$b("Chromosome: "), br(), id.query$Id.data$chr , br() , br() ,
      tags$b("Strand: "), br(), id.query$Id.data$strand)
  }else if(input$Product.type == "mRNA / Protein"){
    p(style = "text-align:left", 
      tags$b("Full name: "), br(), paste(id.query$Id.data$name, collapse = ", "), br() , br() ,
      tags$b("Official Symbol: "), br(),  paste(id.query$Id.data$symbol, collapse = ", "), br() , br() ,
      tags$b("Ensembl ID: "), br(),  paste(id.query$Id.data$ensembl, collapse = ", "), br() , br() ,
      tags$b("EntrezGene ID: "), br(),  paste(id.query$Id.data$entrezgeneid, collapse = ", "), br() , br() ,
      tags$b("Uniprot ID: "), br(),  paste(id.query$Id.data$uniprot, collapse = ", "), br() , br() ,
      tags$b("Chromosome: "), br(), id.query$Id.data$chr , br() , br() ,
      tags$b("Strand: "), br(), id.query$Id.data$strand)
  }
  
})


output$db_links <- renderUI({
  
  exp1.Status()
  if (input$Product.type == "miR"){
    fluidRow(
      column(3, p(" " , style = "text-align:center", uiOutput("RNA_info"))),
      column(9, ) 
    )
  }else if(input$Product.type == "mRNA / Protein"){
    fluidRow(
      column(2, p(" " , style = "text-align:center", uiOutput("RNA_info"))),
      column(2, p(tags$b("Gene/mRNA databases") , style = "text-align:center", 
                  p(style = "text-align:center", 
                    tags$a(tags$img(src = db_gene_ALL , style="height:90%; width:90%"), href = db_Links(input$Product.type, id.query$Id.data)[18,3] , target = "_blank"), br() , br() ,
                    tags$a(tags$img(src = db_Gene_GeneCards , style="height:90%; width:90%"), href = db_Links(input$Product.type, id.query$Id.data)[1,3] , target = "_blank"), br() , br() ,
                    tags$a(tags$img(src = db_Gene_NCBI , style="height:90%; width:90%"), href = db_Links(input$Product.type, id.query$Id.data)[2,3] , target = "_blank") ,br() ,br() ,
                    tags$a(tags$img(src = db_Gene_Ensembl , style="height:90%; width:90%"), href = db_Links(input$Product.type, id.query$Id.data)[3,3] , target = "_blank"),br() ,br() ,
                    tags$a(tags$img(src = db_Gene_HumanMine , style="height:90%; width:90%"), href = db_Links(input$Product.type, id.query$Id.data)[11,3] , target = "_blank"),br() ,br() ,
                    tags$a(tags$img(src = db_Gene_TargetMine , style="height:90%; width:90%"), href = db_Links(input$Product.type, id.query$Id.data)[10,3] , target = "_blank")))), 
      column(2, p(tags$b("Protein databases") , style = "text-align:center", 
                  p(style = "text-align:center", 
                    tags$a(tags$img(src = db_Prot_PAXdb , style="height:90%; width:90%"), href = db_Links(input$Product.type, id.query$Id.data)[4,3] , target = "_blank"), br() ,br() ,
                    tags$a(tags$img(src = db_Prot_UniProt , style="height:90%; width:90%"), href = db_Links(input$Product.type, id.query$Id.data)[5,3] , target = "_blank") , br() ,br() ,
                    tags$a(tags$img(src = db_Prot_TheHumanProteinAtlas , style="height:90%; width:90%"), href = db_Links(input$Product.type, id.query$Id.data)[6,3] , target = "_blank")))), 
      column(2, p(tags$b("Disease databases") , style = "text-align:center", 
                  p(style = "text-align:center", 
                    tags$a(tags$img(src = db_Disease_DisGeNET , style="height:90%; width:90%"), href = db_Links(input$Product.type, id.query$Id.data)[7,3] , target = "_blank"), br() ,br() ,
                    tags$a(tags$img(src = db_Disease_GeneticsHomeReference , style="height:90%; width:90%"), href = db_Links(input$Product.type, id.query$Id.data)[8,3] , target = "_blank") , br() ,br() ,
                    tags$a(tags$img(src = db_Disease_OMIM , style="height:90%; width:90%"), href = db_Links(input$Product.type, id.query$Id.data)[9,3] , target = "_blank") , br() ,br() ,
                    tags$a(tags$img(src = db_Disease_ClinVar , style="height:90%; width:90%"), href = db_Links(input$Product.type, id.query$Id.data)[21,3] , target = "_blank") , br() ,br() ,
                    tags$a(tags$img(src = db_Disease_ClinicalTrials , style="height:90%; width:90%"), href = db_Links(input$Product.type, id.query$Id.data)[20,3] , target = "_blank")))),
      column(2, p(tags$b("Interaction databases") , style = "text-align:center", 
                  p(style = "text-align:center", 
                    tags$a(tags$img(src = db_links_BioGRID , style="height:90%; width:90%"), href = db_Links(input$Product.type, id.query$Id.data)[14,3] , target = "_blank"), br() ,br() ,
                    tags$a(tags$img(src = db_links_KEGG , style="height:90%; width:90%"), href = db_Links(input$Product.type, id.query$Id.data)[13,3] , target = "_blank") , br() ,br() ,
                    tags$a(tags$img(src = db_links_TRRUST , style="height:90%; width:90%"), href = db_Links(input$Product.type, id.query$Id.data)[12,3] , target = "_blank")))),
      column(2, p(tags$b("miR databases") , style = "text-align:center", 
                  p(style = "text-align:center", 
                    tags$a(tags$img(src = db_mir.mrna_DIANA , style="height:90%; width:90%"), href = db_Links(input$Product.type, id.query$Id.data)[15,3] , target = "_blank"), br() ,br() ,
                    tags$a(tags$img(src = db_mir.mrna_mirtarbase , style="height:90%; width:90%"), href = db_Links(input$Product.type, id.query$Id.data)[16,3] , target = "_blank") , br() ,br() ,
                    tags$a(tags$img(src = db_mir.mrna_TargetScanHuman , style="height:90%; width:90%"), href = db_Links(input$Product.type, id.query$Id.data)[17,3] , target = "_blank"), br() ,br() ,
                    tags$b("Other databases"), br() ,br() ,
                    tags$a(tags$img(src = db_BioProject , style="height:90%; width:90%"), href = db_Links(input$Product.type, id.query$Id.data)[19,3] , target = "_blank"))))
    )
  }
})


#### Publi table ####

output$publi.table <- DT::renderDataTable({
  exp1.Status()
  url.api <- paste0("https://www.ebi.ac.uk/europepmc/webservices/rest/search?query=", id.query$Id.data$symbol, "%20sort_date:y&format=json")
  data.api <- GET(url.api)
  if (data.api$status == 200) {
    data.use <- fromJSON(rawToChar(data.api$content))
    data.list <- as.data.frame(data.use$resultList)
    data.list$result.pmid <- replace(data.list$result.pmid, 
                                     c(1:length(data.list$result.pmid)), 
                                     paste0("<a href='", "https://europepmc.org/article/MED/", 
                                            data.list$result.pmid, "?singleResult=true", "' target='_blank'>", data.list$result.pmid,"</a>")) #to add links instead of only the accession nb
  }else{
    message("There is an issue with your request.")
  }
  Render_Table(as.data.frame(data.list[, c(1:4, 6:9, 11, 14)]))
})
