---
title: "How to run this app from your own computer?"
output: html_document
#runtime: shiny
---



<br/><br/>

#### **First method**: run from GitHub - no need to download anything


```r
require(shiny)
runGitHub("Muscle_DMD_Omics", "VirginieMournetas")
```

<br/><br/>

#### **Second method**: run from local folder

- Download and unzip the files: https://nextcloud.virginie-mournetas.ovh/index.php/s/b2GSogTGT6Z8pM5
- Run the app: 


```r
require(shiny)
setwd("/home/Downloads") #set the directory where is the app folder on your computer
runApp("Muscle-DMD-Omics.app")
```

<br/><br/>

**If you have any issue, please contact shiny@virginie-mournetas.fr**

<br/><br/>


```r
sessionInfo()
```

```
## R version 3.6.3 (2020-02-29)
## Platform: x86_64-pc-linux-gnu (64-bit)
## Running under: Ubuntu 18.04.5 LTS
## 
## Matrix products: default
## BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.7.1
## LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.7.1
## 
## locale:
##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C               LC_TIME=fr_FR.UTF-8        LC_COLLATE=en_US.UTF-8     LC_MONETARY=fr_FR.UTF-8    LC_MESSAGES=en_US.UTF-8   
##  [7] LC_PAPER=fr_FR.UTF-8       LC_NAME=C                  LC_ADDRESS=C               LC_TELEPHONE=C             LC_MEASUREMENT=fr_FR.UTF-8 LC_IDENTIFICATION=C       
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] knitr_1.30               Seurat_3.2.2             httr_1.4.2               jsonlite_1.7.1           data.table_1.13.2        FactoMineR_2.3           factoextra_1.0.7        
##  [8] scales_1.1.1             plotly_4.9.2.1           ggplot2_3.3.2            htmltools_0.5.0          shinyFiles_0.9.0         fs_1.5.0                 DBI_1.1.0               
## [15] DT_0.16                  stringr_1.4.0            shinyjs_2.0.0            shinyBS_0.61             bsplus_0.1.2             shinyWidgets_0.5.4       shinydashboardPlus_0.7.5
## [22] shinydashboard_0.7.1     sctransform_0.3.1        shiny_1.5.0             
## 
## loaded via a namespace (and not attached):
##  [1] Rtsne_0.15            colorspace_2.0-0      deldir_0.2-3          ellipsis_0.3.1        ggridges_0.5.2        rsconnect_0.8.16-9002 markdown_1.1          rstudioapi_0.13      
##  [9] spatstat.data_1.5-2   leiden_0.3.5          listenv_0.8.0         ggrepel_0.8.2         lubridate_1.7.9.2     codetools_0.2-18      splines_3.6.3         leaps_3.1            
## [17] polyclip_1.10-0       ica_1.0-2             cluster_2.1.0         png_0.1-7             uwot_0.1.9            compiler_3.6.3        Matrix_1.2-18         fastmap_1.0.1        
## [25] lazyeval_0.2.2        later_1.1.0.1         tools_3.6.3           rsvd_1.0.3            igraph_1.2.6          gtable_0.3.0          glue_1.4.2            RANN_2.6.1           
## [33] reshape2_1.4.4        dplyr_1.0.2           tinytex_0.27          Rcpp_1.0.5            spatstat_1.64-1       vctrs_0.3.5           nlme_3.1-150          crosstalk_1.1.0.1    
## [41] lmtest_0.9-38         xfun_0.19             globals_0.14.0        mime_0.9              miniUI_0.1.1.1        lifecycle_0.2.0       irlba_2.3.3           goftest_1.2-2        
## [49] future_1.20.1         MASS_7.3-53           zoo_1.8-8             promises_1.1.1        spatstat.utils_1.17-0 parallel_3.6.3        RColorBrewer_1.1-2    yaml_2.2.1           
## [57] reticulate_1.18       pbapply_1.4-3         gridExtra_2.3         rpart_4.1-15          stringi_1.5.3         rlang_0.4.9           pkgconfig_2.0.3       matrixStats_0.57.0   
## [65] evaluate_0.14         lattice_0.20-41       tensor_1.5            ROCR_1.0-11           purrr_0.3.4           patchwork_1.1.0       htmlwidgets_1.5.2     cowplot_1.1.0        
## [73] tidyselect_1.1.0      parallelly_1.21.0     RcppAnnoy_0.0.17      plyr_1.8.6            magrittr_2.0.1        R6_2.5.0              generics_0.1.0        mgcv_1.8-33          
## [81] pillar_1.4.7          withr_2.3.0           fitdistrplus_1.1-1    abind_1.4-5           survival_3.2-7        scatterplot3d_0.3-41  tibble_3.0.4          future.apply_1.6.0   
## [89] crayon_1.3.4          KernSmooth_2.23-18    rmarkdown_2.5         grid_3.6.3            digest_0.6.27         flashClust_1.01-2     xtable_1.8-4          tidyr_1.1.2          
## [97] httpuv_1.5.4          munsell_0.5.0         viridisLite_0.3.0
```

<br/><br/>