requiredLib <- c(
  "shiny",
  "sctransform",
  "shinydashboard",
  "shinydashboardPlus",
  "shinyWidgets",
  "bsplus",
  "shinyBS",
  "shinyjs",
  "stringr", #regular expression
  "DT", #for tables
  "DBI", #For file gestion & postgreSQL
  "fs", #For file gestion
  "shinyFiles", #For file gestion
  #"RMySQL", #for mySQL
  "htmltools",
  "ggplot2", #For plots
  "plotly", #For plots
  "scales",
  "factoextra", #PCA
  "FactoMineR", #PCA
  "data.table", #Correlations
  "jsonlite", #API
  "httr", #API
  "Seurat", #Single-cell
  "knitr" #rmarkdown
) 
for (lib in requiredLib) {
  if (!require(lib, character.only = TRUE)) {
    install.packages(lib,repos = "http://cran.us.r-project.org")
  }
  require(lib, character.only = TRUE)
}

