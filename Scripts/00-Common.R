## Functions and Common Parameters
## Some useful functions, libraries and common parameters to load
## PBH Oct. 2021


## Libraries --------------
list_libraries <- c("tidyverse", "kableExtra","chilemapas","lwgeom","sf","htmltools",
                    "flextable", "filesstrings","purrr", "readr","extrafont",
                    "stringr", "ggmap", "readxl", "leaflet", "mapview", "purrr",
                    "skimr", "readr", "patchwork", "ggridges", "lubridate",
                    "ggrepel","mapview","officer")
lapply(list_libraries, require, character.only = TRUE)
rm(list_libraries) 

## Functions ----------

## Save Plot
f_savePlot <- function(p1, file_path, dpi=600){
  cat("Saving: ",file_path)
  ggsave(file_path, {{p1}},dpi=dpi,
         width = 14.87, height = 9.30, units = "in")
}

## Save CSV
f_saveCsv <- function(datos, file_path){
  cat("Saving: ",file_path)
  cat('sep=; \n',file = file_path)
  write.table(datos, file_path,
              sep=';',row.names = F, append = T)
}

## Parameters ---------
theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))


url_file <- "C:/Users/Pablo/Box/ENV203-Extreme Policy Heat Engagement/Data and Spatial Analysis/Spatial Data/%s"

## EoF