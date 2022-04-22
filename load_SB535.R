# Load SB535 Data: Disadvantaged communities in California
# PBH April 2022
# Source of shapefile https://oehha.ca.gov/calenviroscreen/sb535


# source("Scripts/00-Common.R", encoding = "UTF-8")

# load
file_name <- "Data/SB535_CES3_June2018updateGDB.gdb"
# layers available
st_layers(file_name)
# point data is the only layer that contains info for the programs of interest
map_dac <- st_read(file_name,
                    "SB535_CES3_June2018updateGDB")

# mapview(map_dac)

# load GGRF projects
ggrf <- read_rds("Data/ggrf_zip.rds")

# number of implemented actions
(total_actions <- table(ggrf$subprogram_name))

# filter by location in DAC
map_dac <- st_transform(map_dac,"EPSG:4326") %>% st_make_valid()
ggrf_dac <- st_filter(ggrf,map_dac)

(dac_actions <- table(ggrf_dac$subprogram_name))

# percentage DAC locations
dac_actions/total_actions*100



# EoF