fluidRow(
  column(3) ,
  column(6 , 
         h3(tags$b("You like this app", icon("thumbs-up")),style = "color:#367FA9 ; text-align:center"),
         p(tags$b(icon("share-alt"),"Share it & ", icon("smile"),"Tell me"), style = "color:#367FA9 ; text-align:center"),
         box( #email
           width = 12 ,      
           title = p(icon("envelope") , tags$b("By email")) ,
           style="text-align:center" ,
           "shiny@virginie-mournetas.fr" 
         ),
         box( #Social media
               width = 12 ,      
               title = p(icon("comments") , tags$b("Through social media")) ,
               style="text-align:center" ,
               p( 
                 title = p(icon("comments") , tags$b("Through social media")) ,
                 socialButton(
                   url = "https://github.com/VirginieMournetas",
                   type = "github"),
                 "  " ,
                 tags$a(href = "https://www.researchgate.net/profile/Virginie_Mournetas2", target = "_blank" , icon("researchgate", "fa-3x")) ,
                 "  " ,
                 socialButton(
                   url = "https://www.linkedin.com/in/virginie-mournetas-98274b52/",
                   type = "linkedin"),
                 "  " ,
                 tags$a(href = "https://scholar.google.co.uk/citations?user=xjiT9CkAAAAJ&hl=en", target = "_blank" , icon("google", "fa-3x")) ,
                 "  " ,
                 socialButton(
                   url = "https://twitter.com/VMournet",
                   type = "twitter"),
                 style = "text-align:center" 
               )
             ), br(),
         box( #website
           width = 12 ,      
           title = p(icon("globe-europe") , tags$b("Website")) ,
           style="text-align:center" ,
           p("Have a look at my website",tags$a(href = "https://virginie-mournetas.fr", target = "_blank" , "virginie-mournetas.fr"))
         )),
  column(3)
)