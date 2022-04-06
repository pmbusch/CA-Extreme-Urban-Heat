## Load ZIP Data geo-located in Arcgis for GGRF Projects
## PBH April 2022

# Common functions and parameters
# source("Scripts/00-Common.R", encoding = "UTF-8")

zip <- read_excel(sprintf(url_file,"../ProjectZipCodes_1.xls"),
                  sheet = "ProjectZipCodes_1")


# create dictionary of zip variables - project id to zip code
zip_dict <- zip %>% 
  rename(zip=ZCTA5CE20) %>% 
  group_by(ProjectIDNumber,zip) %>% 
  tally()

# some projects have different zip codes
# for now, select the highest
zip_dict <- zip_dict %>% 
  slice(which.max(n)) %>% 
  ungroup()


# zip$ProjectIDNumber %>% unique() %>% length()
# zip_dict$ProjectIDNumber %>% unique() %>% length()

rm(zip)
# EoF