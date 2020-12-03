boxPlus(
  title = tags$b(icon("calendar-check") , "Other" , style="color:white")  , status = "primary" ,
  closable = FALSE, 
  solidHeader = TRUE, 
  collapsible = TRUE,
  collapsed = TRUE,
  width = 12 ,
  enable_label = TRUE,
  label_text = 1,
  label_status = "warning" ,
  timelineBlock
  (
    reversed = TRUE,
    timelineEnd(color = "danger"),
    
    timelineLabel(2018, color = "orange"),
    timelineItem(
      title = p(tags$a(
        'href' = "https://www.cahiers-myologie.org/articles/myolog/full_html/2018/01/myolog201817p49/myolog201817p49.html" , #here
        'style' = "text-decoration:none" , 
        'target' = "_blank" , 
        tags$b("Duchenne muscular dystrophy: a developmental disease?La dystrophie musculaire de Duchenne : une maladie du développement ?") #here
      )) ,
      icon = "file-invoice",
      color = "gray",
      #here
      p(tags$b("Cah. Myol."), ". doi: 0.1051/myolog/201817016" , br() , br() , tags$u("V. Mournetas") , ", E. Massouridès, E. Kornobis, C. Pinset" , 'style' = "font-size:10px")
    ) ,
    
    timelineStart(color = "gray")
  ))