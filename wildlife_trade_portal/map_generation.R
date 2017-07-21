##Preparing RDS file for map filtering app
library(dplyr)
library(tidyr)
library(stringr)
load("~/Documents/github/forest_poverty_portal/data/PROFOR_Evidence_Map.RData")

#Attach final data frames
map_data_final <- left_join(data.biblio, data.biomes,by="aid")
map_data_final <- distinct(map_data_final)
map_data_final <- left_join(map_data_final,data.interv,by="aid")
map_data_final <- distinct(map_data_final)
map_data_final <- left_join(map_data_final,data.outcome,by="aid")
map_data_final <- distinct(map_data_final)
map_data_final <- left_join(map_data_final,data.pathways,by="aid")
map_data_final <- distinct(map_data_final)

#Melt multi-valued country data
melted <- data.study %>% mutate(Study_country = strsplit(as.character(Study_country), ", ")) %>% unnest(Study_country)
melted <- melted %>% as.data.frame() %>% distinct()

#Create region column
reg <- read.csv("data/allcountries.csv",header=TRUE)
#create blank data matrix
rows <- c(1:nrow(melted))
reg_data <- matrix(nrow=nrow(melted),ncol=2)
rownames(reg_data) <- rows
colnames(reg_data) <- c("region","subregion")

#Assign regions
for (i in rows){
  country <- melted$Study_country[i]
  sub <- filter(reg, Country == country)
  reg_data[i,"region"] <- as.character(sub$Region)
  reg_data[i,"subregion"] <- as.character(sub$Subregion)
}

reg_data <- as.data.frame(reg_data)
study_step1 <- bind_cols(melted,reg_data)

##Assign study types
study <- study_step1
study$study_type <- c("")

BACI <- filter(study, Design.control == "Yes" & Comps_type == "ba")
BACI <- distinct(BACI)
BACI$study_type <- "BACI"
BA <- filter(study, Design.control == "No" & Comps_type == "ba")
BA <- distinct(BA)
BA$study_type <- "BA"
CT <- filter(study, (Comps_type == "punc" | Comps_type == "cont"),Design.control == "No")
CT <- distinct(CT)
CT$study_type <- "CT"
CG <- filter(study, Design.control == "No", (Comps_type == "between" | Comps_type == "spat" | Comps_type == "other" | Comps_type == "pa"))
CG <- distinct(CG)
CG$study_type <- "CG"
CGBA <- filter(study, (Comps_type == "between" | Comps_type == "spat" | Comps_type == "other" | Comps_type == "pa"),(Comps_type == "ba"))
CGBA <- distinct(CGBA)
CGBA$study_type <- "CGBA"
CGCI <- filter(study, (Comps_type == "between" | Comps_type == "spat" | Comps_type == "other" | Comps_type == "pa"), Design.control == "Yes")
CGCI <- distinct(CGCI)
CGCI$study_type <- "CGCI"
NONE <- filter(study,Design.comps == "No")
NONE <- distinct(NONE)
NONE$study_type <- "None"

study_final <- bind_rows(BACI, BA, CT, CG, CGBA, CGCI, NONE)
study_final <- distinct(study_final)
study_final <- arrange(study_final,aid)

map_data_final <- left_join(map_data_final,study_final,by="aid")
map_data_final <- distinct(map_data_final)

# #read in DOI info
# doi <- readRDS("data/map_data_final_doi_sent.rds")
# doi <- select(doi, aid,DOI, Title.formatted, Fullcitation)
# doi <- distinct(doi)
# 
# map_data_final <- select(map_data_final, -DOI)
# map_data_final <- left_join(map_data_final,doi,by="aid")
# map_data_final <- distinct(map_data_final)
# map_data_final <- map_data_final[apply(map_data_final,1,function(x)any(!is.na(x))),]
# 
# #Read in open access info
# open <- read.csv("data/data+openaccess.csv",header=TRUE)
# map_data_final <- left_join(map_data_final,open,by="aid")

map_data_final <- map_data_final %>% dplyr::select(-Outcome.direction, -Outcome.direction_notes, -Expl_mechanism,-Stated_mech,-Stated_model,-Model_use, -Assessor, -Assess_date, -Assessor_2)

map_data_final$region <- as.character(map_data_final$region)
map_data_final <- map_data_final[!apply(is.na(map_data_final) | map_data_final == "", 1, all),]
map_data_final <- map_data_final %>% distinct()

saveRDS(map_data_final,file="data/map_data_final.rds")
