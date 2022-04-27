## Load GGRF Map data
## Database contains all information on projects, with locations for each tree
# https://webmaps.arb.ca.gov/ccimap/
# The geodatabase contains all projects reported by agencies administering 
# California Climate Investments through November 30, 2021.
## PBH April 2022

source("Scripts/00-Common.R", encoding = "UTF-8")

# load GGRF Map Data
file_name <- "Data/cci_2022ar_publicmapdata/CCIRTS_2022AR.gdb"
# layers available
st_layers(file_name)
# point data is the only layer that contains info for the programs of interest
map_ggrf <- st_read(file_name,
                    "Point_Data")


# filter by programs of interest
map_ggrf <- map_ggrf %>% 
  filter(subprogram_name %in% c("Urban and Community Forestry",
                                "Urban Greening Program"))

names(map_ggrf)
map_ggrf$census_tract %>% unique() # no census track information

table(map_ggrf$subprogram_name)
# mapview(map_ggrf)

## read zip code information -------------
# Soruce: https://www.census.gov/cgi-bin/geo/shapefiles/index.php?year=2021&layergroup=ZIP+Code+Tabulation+Areas

file_name <- "Data/zip/v107/zip_poly.gdb"
st_layers(file_name)
zip <- st_read(file_name,"zip_poly")
names(zip)
# zip$ZIP_CODE  %>% str_length() %>% unique()
# mapview(zip)

# filter based on state
table(zip$STATE)
zip <- zip %>% filter(STATE=="CA")

# spatial join to map_ggrf
zip <- zip %>% select(ZIP_CODE,Shape_Length,Shape_Area,Shape)
# same CRS
map_ggrf <- st_transform(map_ggrf,"EPSG:4326") %>% st_make_valid()
map_ggrf <- st_join(map_ggrf,zip)
names(map_ggrf)

a <- map_ggrf[1:1000,]
mapview(a)

# save object
saveRDS(map_ggrf,"Data/ggrf_zip.rds")
