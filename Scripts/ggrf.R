## Analysis on GGFR Projects
## PBH April 2022

source("Scripts/00-Common.R", encoding = "UTF-8")
source("Scripts/Load_Data/load_GGRF.R", encoding = "UTF-8")
library(spatstat)

# TABLES  ------------

## Table Summary of Project data -----

## unique projects
ggrf %>% group_by(ProgramName) %>% tally()
# funds
ggrf %>% group_by(ProgramName) %>% 
  summarise(TotalProjectCost_MM=sum(TotalProjectCost)/1e6)
# funds for DAC
ggrf %>% group_by(ProgramName) %>% 
  summarise(TotalGGRFDisadvantagedCommunityFunding_MM=
              sum(TotalGGRFDisadvantagedCommunityFunding)/1e6)
# tree planted
ggrf %>% group_by(ProgramName) %>% 
  summarise(EstimatedNumberOfTreesToBePlanted=sum(EstimatedNumberOfTreesToBePlanted))
# % of projects that planted trees
ggrf %>% group_by(ProgramName) %>% 
  summarise(sum(EstimatedNumberOfTreesToBePlanted>0)/n()*100)
# avg project area
ggrf %>% group_by(ProgramName) %>% 
  summarise(ProjectAcreage=mean(ProjectAcreage))
# different grantees
ggrf %>% group_by(ProgramName) %>% 
  summarise(count=n_distinct(FundingRecipient))
# life years of projects (freq)
ggrf %>% group_by(ProgramName, ProjectLifeYears) %>% tally()

# mentions urban heat island in benefits
string_UHI <- c("urban heat|urban heat island|extreme heat")
# note: urban heat island brings all the results
ggrf <- ggrf %>% 
  mutate(UHI_benefit=DisadvantagedCommunityBenefitsDescription %>% 
           str_to_lower() %>% 
           str_detect(string_UHI),
         UHI_otherBenefit=OtherProjectBenefitsDescription %>% 
           str_to_lower() %>% 
           str_detect(string_UHI),
         both=UHI_benefit*UHI_otherBenefit,
         any=UHI_benefit|UHI_otherBenefit)
# % of projects that mention UHI
ggrf %>% group_by(ProgramName) %>% 
  summarise(UHI_benefit=sum(UHI_benefit)/n()*100,
            UHI_otherBenefit=sum(UHI_otherBenefit)/n()*100,
            both=sum(both)/n()*100,
            any=sum(any)/n()*100)
# interaction between trees and urban heat island
sum(ggrf$any)
table(ggrf$any, ggrf$EstimatedNumberOfTreesToBePlanted>0)

# grantee
table(ggrf$FundingRecipient) %>% sort()

## Table 2 Comparison of conditions to CA -----

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

# final table with median
scores %>% 
  filter(metric %in% c("median")) %>% select(-metric) %>%
  pivot_wider(names_from = ProgramName, values_from = value) %>% 
  flextable() %>% autofit() %>% 
  colformat_double(j=3:5,digits=2) %>% 
  colformat_double(i=3,j=3:5,digits=0)


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