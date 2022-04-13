## Figures to generate Analysis on GGFR Projects
## PBH April 2022

source("Scripts/00-Common.R", encoding = "UTF-8")
source("Scripts/Load_Data/load_GGRF.R", encoding = "UTF-8")

# FIGURES  ----

# exploratory - funds vs tree planted
ggrf %>% 
  mutate(TotalProjectCost=TotalProjectCost/1e3) %>% 
  ggplot(aes(EstimatedNumberOfTreesToBePlanted,TotalProjectCost,
             col=ProgramName)) +
  geom_point()+
  facet_wrap(~ProgramName,scales="free")+
  labs(x="Estimated # of Tree planted",y="Total project cost [thousands $USD]")+
  theme(legend.position = "none")


## CalEnviroScreen vs Funds (at zip level) -----

# weighted mean by pop
hpi_score <- hpi %>% 
  mutate(zip=as.character(zip)) %>% 
  group_by(zip) %>% 
  summarise(ces30score=weighted.mean(ces30score,pop2010)) %>% 
  ungroup()

ggrf_score <- ggrf %>% 
  left_join(hpi_score, by=c("zip")) %>% 
  mutate(TotalProjectCost=TotalProjectCost/1e3,
         ProgramName=str_replace_all(ProgramName,
                                     "Forestry",
                                     "\n Forestry"))
# Scatter
ggplot(ggrf_score,aes(ces30score,TotalProjectCost,col=ProgramName))+
  geom_point(size=2,alpha=.5)+
  facet_wrap(~ProgramName,scales ="free_y")+
  scale_y_continuous(labels=function(x) format(x, 
                                               big.mark = " ", scientific = FALSE))+
  xlim(0,100)+
  coord_cartesian(expand=T)+
  labs(x="CalEnviroScreen 3.0 Score (Zip level)",
       y="Total project cost [thousands $USD]",
       caption="Note: Project cost range (Y-axis) has different scales.")+
  theme(legend.position = "none")

# boxplot
ggrf_score %>% 
  filter(!is.na(ces30score)) %>% 
  mutate(ces30score=case_when(
    ces30score<20 ~ "0-20",
    ces30score<40 ~ "20-40",
    ces30score<60 ~ "40-60",
    ces30score<80 ~ "60-80",
    ces30score<100 ~ "80-100",
    T ~ "other") %>% as.factor()) %>% 
  ggplot(aes(ces30score,TotalProjectCost,
             fill=as.factor(IsBenefitDisadvantagedCommunities)))+
  geom_boxplot(alpha=0.5)+
  facet_grid(~ProgramName)+
  scale_y_continuous(labels=function(x) format(x, 
                                               big.mark = " ", scientific = FALSE))+
  coord_cartesian(expand=T)+
  labs(x="CalEnviroScreen 3.0 Score (Zip level)",
       y="Total project cost [thousands $USD]",
       fill="Benefit DAC")




## Function -----

# function that plot funds vs a selected variable from hpi database
f.scatter.funds <- function(var){
  # get weighted mean by population at zip level
  hpi_score <- hpi %>% 
    mutate(zip=as.character(zip)) %>% 
    group_by(zip) %>% 
    summarise(var_int=weighted.mean({{var}},pop2010)) %>% 
    ungroup()
  # join to project data
  ggrf_score <- ggrf %>% 
    left_join(hpi_score, by=c("zip")) %>% 
    mutate(TotalProjectCost=TotalProjectCost/1e3,
           ProgramName=str_replace_all(ProgramName,
                                       "Forestry",
                                       "\n Forestry"))
  # get title and def. from dictionary
  title <- f.getDescHPI(deparse(substitute(var)))
  definition <- f.getDefinitionHPI(deparse(substitute(var)))
  
  # Scatter
  ggplot(ggrf_score,aes(var_int,TotalProjectCost,col=ProgramName))+
    geom_point(size=2,alpha=.5)+
    scale_y_continuous(labels=function(x) format(x, big.mark = " ", 
                                                 scientific = FALSE))+
    scale_x_continuous(labels=function(x) format(x, big.mark = " ", 
                                                 scientific = FALSE))+
    coord_cartesian(expand=T)+
    labs(x=paste0(title," (ZIP Level)"),
         y="Total project cost [thousands $USD]",
         caption=definition)+
    ggtitle(title)
}


# Generate PDF of summary
# names(hpi)
pdf("Scatter_Funds_vsHPI.pdf",
    width = 11,height = 6)
f.scatter.funds(ces30score)
f.scatter.funds(hpi2score)
f.scatter.funds(uhii)
f.scatter.funds(bphigh_pct)
f.scatter.funds(chd_pct)
f.scatter.funds(leb_pct)
f.scatter.funds(heartattack)
f.scatter.funds(pop2010)
f.scatter.funds(elders_pct)
f.scatter.funds(outdoors_pct)
f.scatter.funds(heatdays)
f.scatter.funds(aircon_pct)
f.scatter.funds(treecanopy)
f.scatter.funds(impervsurf_pct)
f.scatter.funds(parkaccess)
dev.off()


## EoF




## EoF