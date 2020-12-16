div(
  h3(tags$b("How does this shiny application work?")),
  br(),
  
  h4(icon("microscope"), tags$b("Find a brief data overview under the 'Data presentation' tab"), style = "color:#367FA9"),
  br(),
  h4(icon("chart-bar"), tags$b("Find the actual data under the 'Expression data' tab"), style = "color:#367FA9"),
  h5(p(icon("exclamation-triangle"),"Please be patient", style = "color:#CB4A4A"), "The web server is limited, therefore some graphics can take a little while to appear on your screen."), 
  br(),
  
  h4(tags$b(icon("download"),"You have R installed on your computer and you want this app to be quicker", style = "color:#367FA9")),
  h5("Go to the 'Download the app' tab for instructions."),
  br(),
  
  h4(tags$b(icon("file-invoice"),"You use data or code from this app in your research", style = "color:#367FA9")),
  h5("Please cite the paper on the 'References' tab."),
  br(),
  
  h4(tags$b(icon("thumbs-up"),"You like this app", style = "color:#367FA9")),
  h5("Share it", icon("share-alt")), 
  h5("Use the 'Contact' tab to tell me", icon("smile")),
  br()
)