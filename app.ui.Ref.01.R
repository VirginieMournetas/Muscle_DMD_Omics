boxPlus(
  title = tags$b(icon("calendar-plus") , "Preprints" , style="color:white")  , status = "primary" ,
  closable = FALSE, 
  solidHeader = TRUE, 
  collapsible = TRUE,
  collapsed = FALSE,
  width = 12 ,
  enable_label = TRUE,
  label_text = 1,
  label_status = "warning" ,
  timelineBlock
  (
    reversed = TRUE,
    timelineEnd(color = "danger"),
    
    timelineLabel(2019, color = "orange"),
    timelineItem(
      title = p(tags$a(
        'href' = "https://www.biorxiv.org/content/10.1101/720920v2.full" , 
        'style' = "text-decoration:none" , 
        'target' = "_blank" , 
        tags$b("Myogenesis modelled by human pluripotent stem cells uncovers Duchenne muscular dystrophy phenotypes prior to skeletal muscle commitment") #here
      )) ,
      icon = "file-invoice",
      color = "gray",
      #here
      p(tags$b("bioRxiv"), ". DOI:10.1101/720920" , br() , br() , tags$u("V. Mournetas") , ", E. Massouridès, J.-B. Dupont, E. Kornobis, H. Polvèche, M. Jarrige, M. Gosselin, S. D. Garbis, D. C. Górecki, C. Pinset" , 'style' = "font-size:10px")
    ) ,
    timelineStart(color = "gray")
  )
  
)