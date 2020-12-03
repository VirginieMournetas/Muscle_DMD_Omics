## Create a vector with all the user-specific directories, which can be exported in the report afterwards
dir <- vector()

## Specify your home directory

#dir["home"] <- "/home/virginie" #IFB cluster + MAC
#C:\Users\virgi\Nextcloud\WebDAV\home\Formation\Module7_Projet\Scripts

## Specify the local directory for the personal work
#dir["base"] <- file.path(dir["home"], "shared", "Muscle_DMD_Omics")
dir["base"] <- "."

## Directory with the expression data
#dir["Data"] <- file.path(dir["base"], "Data")
dir["Data"] <- file.path("Data")

## Directory with the expression data
#dir["Tables"] <- file.path(dir["base"], "Tables")
dir["Tables"] <- file.path("Tables")


