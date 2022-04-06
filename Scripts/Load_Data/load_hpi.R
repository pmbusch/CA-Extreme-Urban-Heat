## Load and Clean HPI Data
## PBH April 2022

# Common functions and parameters
# source("Scripts/00-Common.R", encoding = "UTF-8")

# load data --------
hpi <- read_delim(sprintf(url_file,"HPI/hpi.csv"),
                  delim = ",")

# columns of interest
col_location <- c("city","county_name",
                  "geoid","zip")
col_indexScore <- c("ces30score",
                    "hpi2score","uhii")
col_health <- c("bphigh_pct","chd_pct",
               "leb_pct","heartattack")
col_exposure <- c("pop2010","elders_pct",
                  "outdoors_pct","heatdays")
col_protection <- c("aircon_pct","treecanopy",
                    "impervsurf_pct","parkaccess")

# filter by columns of interest
hpi <- hpi[,c(col_location,col_indexScore,col_health,
            col_exposure,col_protection)]

rm(col_location,col_indexScore,col_health,
   col_exposure,col_protection)

# hpi_metadata
hpi_metadata <- read_excel(sprintf(url_file,"HPI/hpi_metadata.xlsx"),
                  sheet="hpi_metadata")

# description for HPI
f.getDescHPI <- function(var){
  hpi_metadata %>% filter(variable==var) %>% pull(shorttitle)
}

# definition for HPI
f.getDefinitionHPI <- function(var){
  hpi_metadata %>% filter(variable==var) %>% pull(definition)
}


## EoF