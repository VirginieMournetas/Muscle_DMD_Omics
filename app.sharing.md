---
title: "How to run this app from your own computer?"
output: html_document
runtime: shiny
---

```{r libraries, include=FALSE, eval=FALSE}
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
```

<br/><br/>

#### **First method**: run from GitHub - no need to download anything

```{r github, eval=FALSE}
require(shiny)
runGitHub("Muscle_DMD_Omics", "VirginieMournetas")
```

<br/><br/>

#### **Second method**: run from local folder

- Download and unzip the files: https://nextcloud.virginie-mournetas.ovh/index.php/s/b2GSogTGT6Z8pM5
- Run the app: 

```{r local, eval=FALSE}
require(shiny)
setwd("/home/Downloads") #set the directory where is the app folder on your computer
runApp("Muscle-DMD-Omics.app")
```

<br/><br/>

**If you have any issue, please contact shiny@virginie-mournetas.fr**

<br/><br/>

```{r sessionInfo, eval=TRUE}
sessionInfo()
```

<br/><br/>