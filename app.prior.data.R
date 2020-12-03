#### GENERAL -  Prepare files for SQL db #### 

string.for.sql <- function(file){
  names <- tolower(names(file)) 
  names <- str_replace_all(names, '[.]', "_")
  return(names)
}

#### SQL DB ####

source(file.path(dir["base"], "db.R"), local = TRUE, encoding = "UTF-8")
con <- c(db, host_db, db_port, db_user, db_password)

#### Files ####

#load(file.path(dir["Scripts"], "DBs_PostgreSQLR_app.RData"))

Data.files <- c("miR_Ratios_Stats.short.csv", "miRseqData.short.csv", "miRseqData.full.csv", 
                "mRNA_Ratios_Stats.short.csv", "mRNAseqData.short.csv", "mRNAseqData.full.csv", 
                "ProteinData.short.csv", "ProteinData.full.csv",
                "SampleDescription.csv")


for (name in Data.files){
  var <- assign(str_replace(name, ".csv", ""), read.table(file.path(dir["Data"], name), sep = ",", header = TRUE))
  colnames(var) <- string.for.sql(var)
  var <- as.symbol(str_replace(name, ".csv", ""))
}

Tables.files <- c("UniquemRNAData.csv", "UniquemiRData.csv", 
                  "UniquemRNAmeanData.csv", "UniquemiRmeanData.csv", 
                  "mRNA_RatioStat_Data_Myogenesis.csv", "miR_RatioStat_Data_Myogenesis.csv", 
                  "mRNA_RatioStat_Data_Phenotype.csv", "miR_RatioStat_Data_Phenotype.csv",
                  "ProteinData_Table.csv","ProteinData_Boxplot.csv")

for (name in Tables.files){
  var <- assign(str_replace(name, ".csv", ""), read.table(file.path(dir["Tables"], name), sep = "\t"))
  colnames(var) <- string.for.sql(var)
  var <- as.symbol(str_replace(name, ".csv", ""))
}


UniquemRNAData[ , 7] <- factor(UniquemRNAData[ , 7] , 
                              levels = c("D0 Healthy M180","D0 Healthy M194","D0 Healthy M398","D0 DMD M197","D0 DMD M202","D0 DMD M418" , 
                                         "D03 Healthy M180","D03 Healthy M194","D03 Healthy M398","D03 DMD M197","D03 DMD M202","D03 DMD M418" , 
                                         "D10 Healthy M180","D10 Healthy M194","D10 Healthy M398","D10 DMD M197","D10 DMD M202","D10 DMD M418" , 
                                         "D17 Healthy M180","D17 Healthy M194","D17 Healthy M398","D17 DMD M197","D17 DMD M202","D17 DMD M418" , 
                                         "D25 Healthy M180","D25 Healthy M194","D25 Healthy M398","D25 DMD M197","D25 DMD M202","D25 DMD M418" , 
                                         "Myoblast Healthy M180","Myoblast Healthy M194","Myoblast Healthy V1024" , 
                                         "Myoblast DMD M197","Myoblast DMD M202","Myoblast DMD M418" , 
                                         "Myotube Healthy M180","Myotube Healthy M194","Myotube Healthy V1024" , 
                                         "Myotube DMD M197","Myotube DMD M202","Myotube DMD M418"))
UniquemRNAData[ , 8] <- factor(UniquemRNAData[ , 8],
                              levels = c("Healthy D0","DMD D0" , 
                                         "Healthy D03","DMD D03" , 
                                         "Healthy D10","DMD D10" , 
                                         "Healthy D17","DMD D17" , 
                                         "Healthy D25","DMD D25" , 
                                         "Healthy Myoblast","DMD Myoblast" , 
                                         "Healthy Myotube","DMD Myotube"))

UniquemiRData[ , 7] <- factor(UniquemiRData[ , 7] , 
                                levels = c("D0 Healthy M180","D0 Healthy M194","D0 Healthy M398","D0 DMD M197","D0 DMD M202","D0 DMD M418" , 
                                           "D03 Healthy M180","D03 Healthy M194","D03 Healthy M398","D03 DMD M197","D03 DMD M202","D03 DMD M418" , 
                                           "D10 Healthy M180","D10 Healthy M194","D10 Healthy M398","D10 DMD M197","D10 DMD M202","D10 DMD M418" , 
                                           "D17 Healthy M180","D17 Healthy M194","D17 Healthy M398","D17 DMD M197","D17 DMD M202","D17 DMD M418" , 
                                           "D25 Healthy M180","D25 Healthy M194","D25 Healthy M398","D25 DMD M197","D25 DMD M202","D25 DMD M418" , 
                                           "Myoblast Healthy M180","Myoblast Healthy M194","Myoblast Healthy V1024" , 
                                           "Myoblast DMD M197","Myoblast DMD M202","Myoblast DMD M418" , 
                                           "Myotube Healthy M180","Myotube Healthy M194","Myotube Healthy V1024" , 
                                           "Myotube DMD M197","Myotube DMD M202","Myotube DMD M418"))
UniquemiRData[ , 8] <- factor(UniquemiRData[ , 8],
                                levels = c("Healthy D0","DMD D0" , 
                                           "Healthy D03","DMD D03" , 
                                           "Healthy D10","DMD D10" , 
                                           "Healthy D17","DMD D17" , 
                                           "Healthy D25","DMD D25" , 
                                           "Healthy Myoblast","DMD Myoblast" , 
                                           "Healthy Myotube","DMD Myotube"))

#PCA
mRNA.expr.PCs <- readRDS(file = file.path(dir["Data"], "mRNA.expr.PCs.rds"))
miR.expr.PCs <- readRDS(file = file.path(dir["Data"], "miR.expr.PCs.rds"))

#Single-cell data
singlecell.matrix <- readRDS(file = file.path(dir["Data"], "singlecell.matrix.rds"))
singlecell <- readRDS(file = file.path(dir["Data"], "singlecell.rds"))

#### Nextcloud pics ####
nextcloud.links <- function(code){
  nextcloud.link <- paste0("https://nextcloud.virginie-mournetas.ovh/index.php/s/",code,"/preview")
  return(nextcloud.link)
}


Logo <- nextcloud.links("weNLHnqrn3YDx9b") 
logo_bottom <- nextcloud.links("BGwJXGCjjt9NPF9") 

img_exp <- nextcloud.links("gL6wBT8tAML6Ze9") 
img_net.db <- nextcloud.links("QBJLte2H5dG5pR7") 

db_Disease_DisGeNET <- nextcloud.links("eFp5ys3oFPGi9qx") 
db_Disease_GeneticsHomeReference <- nextcloud.links("zQSf6cQ9fFZaaNE") 
db_Disease_MalaCards <- nextcloud.links("GFCj2bGoJZ9a2tj") 
db_Disease_OMIM <- nextcloud.links("aD6Ks785kPJ5D9q") 
db_Gene_Ensembl <- nextcloud.links("A4o9WxmcNictgS5") 
db_Gene_GeneCards <- nextcloud.links("orywBmP9H74fGmz") 
db_Gene_NCBI <- nextcloud.links("JsLGfB4dbatGFH7") 
db_Prot_PAXdb <- nextcloud.links("xETtCda6Qwtpzwd") 
db_Prot_TheHumanProteinAtlas <- nextcloud.links("szE7JAMzGH3J8iC") 
db_Prot_UniProt <- nextcloud.links("FNCDqsjNjQ7Re6X") 
db_Gene_HumanMine <- nextcloud.links("GbSSZwkmpdYP5HP") 
db_Gene_TargetMine <- nextcloud.links("TwDDQEM79HBHLM6") 
db_links_BioGRID <- nextcloud.links("FmQxKCp5XnbCcpE") 
db_links_KEGG <- nextcloud.links("T7kaPJbPSt3jSZm") 
db_links_TRRUST <- nextcloud.links("PiBtRsaZLJfaRT4") 
db_mir_mirbase <- nextcloud.links("PTPgbBzgMe9awxf") 
db_mir.mrna_DIANA <- nextcloud.links("FEteeBSgfe2LCdT") 
db_mir.mrna_mirtarbase <- nextcloud.links("X6wM7Wc7zFiCTgx") 
db_mir.mrna_TargetScanHuman <- nextcloud.links("SPi7CxGTNDCjDNS") 
db_gene_ALL <- nextcloud.links("zxrTRbpCsNKrSHg") 
db_BioProject <- nextcloud.links("iyDcqgRkyAPRx8X") 
db_Disease_ClinVar <- nextcloud.links("2Dqf5sbpZBC26SB") 
db_Disease_ClinicalTrials <- nextcloud.links("Xg5SyzeXgyik8ZN") 
#db_miRGator <- nextcloud.links("iAmnLbC5SMkLy6z") 

#Database links
{
  links_list <- matrix(NA, nrow = 9 , ncol = 3)
  colnames(links_list) <- c("Database type" , "Database name" , "link")
  links_list[ ,1] <- c(rep("Gene", 3), rep("Protein", 3), rep("Disease", 3))
  links_list[ ,2] <- c("GeneCards" , "NCBI" , "Ensembl" , "PAXdb" , "UniProt" , "The Human Protein Atlas" , "DisGeNET" , "Genetics Home Reference" , "OMIM")
  
}