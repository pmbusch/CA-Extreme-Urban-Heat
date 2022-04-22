## Analysis on GGFR Projects
## PBH April 2022

# LOAD DATA
source("Scripts/00-Common.R", encoding = "UTF-8")
source("Scripts/Load_Data/load_hpi.R", encoding = "UTF-8")
ggrf <- read_rds("Data/ggrf_zip.rds")
library(spatstat)

## Table 2 Comparison of conditions to CA -----
# EACH POINT REPRESENTS SOMETHING IMPLEMENTED BY THE PROJECT, like a tree or park
# This method gives greater weight to projects with more trees planted (or other)

ggrf <- ggrf %>% rename(zip=ZIP_CODE,
                        ProgramName=subprogram_name)
names(ggrf)

# remove spatial dependency (less memory)
ggrf$Shape <- NULL
class(ggrf)

table(ggrf$ProgramName)
table(ggrf$zip)
# number of distinct ZIP codes
# ggrf %>% group_by(ProgramName) %>% 
#   summarise(n=n_distinct(zip))

hpi %>% 
  filter(str_length(zip)==5) %>% 
  pull(zip) %>% unique() %>% length()

# select relevant variables to include in the summary
# names(hpi)
comp_var <- c("ces30score","hpi2score","uhii",
              "leb_pct","elders_pct","outdoors_pct",
              "heatdays","aircon_pct","treecanopy","impervsurf_pct","parkaccess")

# need to aggregate all metrics to zip code values, using the mean
# this will give me a list of all the metric values at zip code level
hpi_zip <- hpi %>% 
  select(c("zip","pop2010",comp_var)) %>%
  group_by(zip) %>% 
  summarise(across(-pop2010,
                   list(mean=~weighted.mean(.,w=pop2010,na.rm=T))),
            .groups = 'drop')

names(hpi_zip) <- names(hpi_zip) %>% str_remove("_mean")

# add total population for each zip code
zip_pop <- hpi %>% 
  group_by(zip) %>% 
  summarise(pop2010=sum(pop2010,na.rm = T))
hpi_zip <- hpi_zip %>% 
  left_join(zip_pop)
rm(zip_pop)

# california average and median (weighted by population)
ca_score <- hpi_zip %>% 
  mutate(state="CA") %>% 
  select(c("state","pop2010",comp_var)) %>%
  group_by(state) %>% 
  summarise(across(-pop2010,
                   list(mean=~weighted.mean(.,w=pop2010,na.rm=T),
                        median=~weighted.median(.,w=pop2010,na.rm=T),
                        p5=~weighted.quantile(.,w=pop2010,probs=0.05,na.rm=T),
                        p95=~weighted.quantile(.,w=pop2010,probs=0.95,na.rm=T))),
            .groups = 'drop')

# make it into tidy format
ca_score <- ca_score %>% 
  pivot_longer(c(-state), names_to = "key", values_to = "value") %>% 
  mutate(metric=str_extract(key,"mean|median|p5|p95"),
         variable=str_remove(key,"_mean|_median|_p5|_p95")) %>% 
  select(-key)

## same summaries, but for the zip codes where the projects are

# first, need to join to GGRF the HPI info
# as it is a left join, all the info for HPI will joined to each project
# each zip code could be repeated due to multiple projects in the area
ggrf_hpi <- ggrf %>% 
  mutate(zip=as.numeric(zip)) %>% 
  left_join(hpi_zip,by=c("zip"))

# average and median (weighted by population)
ggrf_score <- ggrf_hpi %>% 
  select(c("ProgramName","pop2010",comp_var)) %>%
  group_by(ProgramName) %>% 
  summarise(across(-pop2010,
                   list(mean=~weighted.mean(.,w=pop2010,na.rm=T),
                        median=~weighted.median(.,w=pop2010,na.rm=T),
                        p5=~weighted.quantile(.,w=pop2010,probs=0.05,na.rm=T),
                        p95=~weighted.quantile(.,w=pop2010,probs=0.95,na.rm=T))),
            .groups = 'drop')

# make it into tidy format
ggrf_score <- ggrf_score %>% 
  pivot_longer(c(-ProgramName), names_to = "key", values_to = "value") %>% 
  mutate(metric=str_extract(key,"mean|median|p5|p95"),
         variable=str_remove(key,"_mean|_median|_p5|_p95")) %>% 
  select(-key)

# join scores to generate table
scores <- ca_score %>% rename(ProgramName=state) %>% 
  rbind(ggrf_score)

# get formal titles and desc
scores$Metric <- sapply(scores$variable,f.getDescHPI)
scores$Description <- sapply(scores$variable,f.getDefinitionHPI)
scores$variable <- NULL

# number of agg function
number_agg <- scores$metric %>% unique() %>% length()

## add clasification of categories
scores <- scores %>% 
  mutate(Category=case_when(
    Metric %in% c("CalEnviroScreen 3.0 Score",
                  "HPI Score",
                  "Urban Heat Island Index") ~ "Index Score",
    Metric %in% c("Life Expectancy at Birth",
                  "Elderly") ~ "Vulnerability",
    Metric %in% c("Outdoor Workers",
                  "Extreme Heat Days") ~ "Exposure",
    Metric %in% c("Air Conditioning",
                  "Tree Canopy",
                  "Impervious Surface Cover",
                  "Park Access") ~ "Protection"))
                    
# final table with median or mean
scores %>% 
  relocate(Category) %>% 
  filter(metric %in% c("mean")) %>% select(-metric) %>%
  pivot_wider(names_from = ProgramName, values_from = value) %>% 
  flextable() %>% autofit() %>% 
  colformat_double(j=4:6,digits=2) %>% 
  colformat_double(i=3,j=4:6,digits=0) %>% 
  merge_v(j=1) %>% 
  hline(i=c(3,5,7,11)) %>% 
  # highlight rows with undesirable outcomes
  bold(i=c(3,5,7,11),j=5) %>% 
  bold(i=c(3,5,8,11),j=6) %>% 
  footnote(part="header",j=c(4,5,6),
           value = as_paragraph(c(
             "Highlighted are values in with an unwanted difference and direction with respect to CA average",
             "Data from 81,332 locations where project was implemented",
             "Data from 4,564 locations where project was implemented"
             ))) 
  # print(preview="docx")


table(ggrf_hpi$ProgramName)

# final table with all aggregate function: meadin, avg, p95
scores %>% 
  relocate(metric,.after=Description) %>% 
  rename(`Agg. Function`=metric) %>% 
  pivot_wider(names_from = ProgramName, values_from = value) %>% 
  flextable() %>% autofit() %>% 
  colformat_double(j=4:6,digits=2) %>% 
  colformat_double(i=2*number_agg+(1:number_agg),j=4:6,digits=0) %>% 
  merge_v(j=1:2) %>% 
  hline(i=seq(number_agg,nrow(scores)/3,number_agg)) %>% 
  vline(j=2)


rm(ca_score,ggrf_score)
## EoF