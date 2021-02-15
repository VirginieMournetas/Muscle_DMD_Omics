# muscle-DMD.omics

![](https://nextcloud.virginie-mournetas.ovh/index.php/s/BGwJXGCjjt9NPF9/preview)

- muscle-DMD.omics is a shiny-based web application for exploring multi-omics data published in [Myogenesis modelled by human pluripotent stem cells: a multi‐omic study of Duchenne myopathy early onset, Mournetas et al., JCSM, 2021](https://onlinelibrary.wiley.com/doi/10.1002/jcsm.12665).

- If you use this application or its code (licensed under GNU General Public License v3.0), please cite [Myogenesis modelled by human pluripotent stem cells: a multi‐omic study of Duchenne myopathy early onset, Mournetas et al., JCSM, 2021](https://onlinelibrary.wiley.com/doi/10.1002/jcsm.12665).

- If you have any issue, please do not hesitate to report it either through GitHub or by email at shiny@virginie-mournetas.fr



* * *
  
## Latest Update
  
 - 2021/02/15: this repository is made public.
 - 2021/02/14: the scientific publication incorporating this app is publicly available : [Myogenesis modelled by human pluripotent stem cells: a multi‐omic study of Duchenne myopathy early onset](https://onlinelibrary.wiley.com/doi/10.1002/jcsm.12665)

* * *
  
## This app on the web
  
muscle-DMD.omics is available at https://muscle-dmd.omics.ovh/
  
* * *
  
## This app on your computer
  
- ### Method 1: run from GitHub - no need to download anything
  
1) Install RStudio 
2) Install shiny R package
3) Run the following code: 
  
  ```r
require(shiny)
runGitHub("Muscle_DMD_Omics", "VirginieMournetas")
```

- ### Method 2: run from local folder

1) Install RStudio 
2) Install shiny R package
3) Download and unzip the files: https://nextcloud.virginie-mournetas.ovh/index.php/s/b2GSogTGT6Z8pM5
4) Run the following code: 
  
  ```r
require(shiny)
setwd("/home/Downloads") #set the directory where is the app folder on your computer
runApp("Muscle-DMD-Omics.app")
```
