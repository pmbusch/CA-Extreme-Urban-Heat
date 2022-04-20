## Load and Clean GGRF Projects Data
## PBH April 2022


# Common functions and parameters
# source("Scripts/00-Common.R", encoding = "UTF-8")

# load data --------
ggrf <- read_excel(sprintf(url_file,"../GGFR Project Analysis.xlsx"),
                  sheet = "Implemented Projects")

nrow(ggrf)
ggrf$Lat %>% unique %>% length()
ggrf$ProjectIDNumber %>% unique %>% length()


ggrf %>% group_by(ProjectIDNumber,Lat) %>% tally()


ggrf %>% filter(!is.na(Lat)) %>% 
  pull(ProjectIDNumber) %>% unique() %>% length()
  


names(ggrf) <- names(ggrf) %>% str_replace_all(" ","_")

# columns of interest
col_id <- c("ProjectIDNumber","ReportingCycleName","ProjectCount",
            "AgencyName","ProgramName","ProjectName")
col_description <- c("ProjectType","ProjectDescription",
                     "FundingRecipient","FiscalYearFundingProject")
col_temporal <- c("DateOperational","ProjectCompletionDate",
                  "ProjectLifeYears")
col_spatial <- c("CensusTract","Address","Lat","Long")
col_funds <- c("TotalProjectCost","TotalProgramGGRFFunding")
col_implementation <- c("EstimatedNumberOfTreesToBePlanted",
                        "ProjectAcreage",
                        "DisadvantagedCommunityBenefitsDescription",
                        "OtherProjectBenefitsDescription")
col_benefits <- c("EstimatedEnergySavedKWH","EstimatedAcresPreserved",
                  "EstimatedAcresRestored","EstimatedAcresTreated")
col_EJ <- c("IsBenefitDisadvantagedCommunities",
            "DisadvantagedCommunityCensusTracts",
            "TotalGGRFDisadvantagedCommunityFunding")

# filter by columns of interest
ggrf <- ggrf[,c(col_id, col_description, col_temporal,
                col_spatial, col_funds,col_implementation,
                col_benefits,col_EJ)]

rm(col_id, col_description, col_temporal,
   col_spatial, col_funds,col_implementation,
   col_benefits,col_EJ)


# Data wrangling ----

# convert to unique projects
ggrf <- ggrf %>% 
  group_by(ProjectIDNumber,                       
           AgencyName,                            
           ProgramName,                           
           ProjectName,                           
           ProjectDescription,                    
           FundingRecipient,                      
           ProjectCompletionDate,                 
           CensusTract
           ) %>%
  summarise(
    ReportingCycleName=paste0(ReportingCycleName,collapse=", "),
    Address=min(Address),
    Lat=min(Lat,na.rm=T),
    Long=min(Long,na.rm=T),
    DisadvantagedCommunityCensusTracts=paste0(DisadvantagedCommunityCensusTracts,collapse=", "),
    ProjectType=paste0(ProjectType,collapse = ", "),
    ProjectLifeYears=max(ProjectLifeYears),
    ProjectCount=max(ProjectCount),
    DateOperational=min(DateOperational),
    FiscalYearFundingProject=min(FiscalYearFundingProject),
    TotalProjectCost=sum(TotalProjectCost,na.rm=T),
    TotalProgramGGRFFunding=sum(TotalProgramGGRFFunding,na.rm=T),
    DisadvantagedCommunityBenefitsDescription=paste0(DisadvantagedCommunityBenefitsDescription,collapse="; "),
    OtherProjectBenefitsDescription=paste0(OtherProjectBenefitsDescription,collapse = "; "),
    IsBenefitDisadvantagedCommunities=max(IsBenefitDisadvantagedCommunities),
    EstimatedNumberOfTreesToBePlanted=sum(EstimatedNumberOfTreesToBePlanted,na.rm=T),
    ProjectAcreage=sum(ProjectAcreage,na.rm=T),
    EstimatedEnergySavedKWH=sum(EstimatedEnergySavedKWH,na.rm=T),
    EstimatedAcresPreserved=sum(EstimatedAcresPreserved,na.rm=T),
    EstimatedAcresRestored=sum(EstimatedAcresRestored,na.rm=T),
    EstimatedAcresTreated=sum(EstimatedAcresTreated,na.rm=T),
    TotalGGRFDisadvantagedCommunityFunding=sum(TotalGGRFDisadvantagedCommunityFunding,na.rm=T)) %>% 
  ungroup()

# extract zip code GGFR
ggrf <- ggrf %>% 
  mutate(zip=str_extract(Address,"CA \\d{5}") %>% 
           str_remove("CA ") %>% as.numeric())

# identified records
sum(!is.na(ggrf$zip))/nrow(ggrf)*100
# 12%, too low

table(ggrf$ProgramName)

## load other Databases ------
source("Scripts/Load_Data/load_zip.R", encoding = "UTF-8")
source("Scripts/Load_Data/load_hpi.R", encoding = "UTF-8")


## Data wrangling -------------

# add zip code to GGRF
ggrf$zip <- NULL
ggrf <- ggrf %>% left_join(zip_dict,by=c("ProjectIDNumber"))


## EoF