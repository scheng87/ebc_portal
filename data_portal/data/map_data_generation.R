##Preparing RDS file for map filtering app
library(dplyr)
library(tidyr)
library(stringr)
load("~/Documents/CI_Projects_LA/Knowledge_Base/evidence_based_7_14_16.RData")

#Join intervention and outcome data
int_out_data <- left_join(data.interv,data.outcome,by="aid")
int_out_data <- int_out_data %>% select(aid,Int_type,Outcome) %>% distinct()
#Add intervention group column
#Create blank data matrix
rows <- c(1:nrow(int_out_data))
int_groups <- matrix(nrow=nrow(int_out_data),ncol=1)
rownames(int_groups) <- rows
colnames(int_groups) <- c("int_group")

#Assign intervention groups
for (i in rows){
  int <- int_out_data$Int_type[i]
  if (int == "area_mgmt" | int == "sp_control" | int == "restoration") {
    group <- "land_wat_mgmt"
  } else if (int == "sp_mgmt" | int == "sp_recov" | int == "sp_reint" | int == "ex_situ") {
    group <- "species_mgmt"
  } else if (int == "form_ed" | int == "training" | int == "aware_comm") {
    group <- "education"
  } else if (int == "legis" | int == "pol_reg" | int == "priv_codes" | int == "compl_enfor") {
    group <- "law_policy"
  } else if (int == "liv_alt" | int == "sub" | int == "market" | int == "non_mon") {
    group <- "liv_eco_inc"
  } else if (int == "inst_civ_dev" | int == "part_dev" | int == "cons_fin") {
    group <- "ext_cap_build"
  } else
    group <- int
  int_groups[i,"int_group"] <- group
}

int_groups <- as.data.frame(int_groups)
int_out_final <- bind_cols(int_out_data,int_groups)
int_out_final <- filter(int_out_final,!is.na(int_groups))

#Isolate country data and add region column
country_data <- select(data.study,aid,Study_country)
country_data <- distinct(country_data)
reg <- read.csv("~/Documents/CI_Projects_LA/Knowledge_Base/allcountries.csv",header=TRUE)
#create blank data matrix
rows <- c(1:nrow(country_data))
reg_data <- matrix(nrow=nrow(country_data),ncol=2)
rownames(reg_data) <- rows
colnames(reg_data) <- c("region","subregion")

#Spot-checking misspellings and fixing
country_data[64,2] <- c("Congo, The Democratic Republic Of The")
country_data[117,2] <- c("Honduras")
country_data[253,2] <- c("Serbia")
country_data[308,2] <- c("Congo, The Democratic Republic Of The")
country_data[785,2] <- c("Bahamas")
country_data[936,2] <- c("Tanzania, United Republic Of")
country_data[942,2] <- c("Belize")
country_data[957,2] <- c("Tanzania, United Republic Of")
country_data[1042,2] <- c("Lao People's Democratic Republic")
country_data[1170,2] <- c("Trinidad and Tobago")

#Assign regions
for (i in rows){
  country <- country_data$Study_country[i]
  sub <- filter(reg,Country == country)
  reg_data[i,"region"] <- as.character(sub$Region)
  reg_data[i,"subregion"] <- as.character(sub$Subregion)
}

reg_data <- as.data.frame(reg_data)
country_data_final <- bind_cols(country_data,reg_data)

#Read in biomes
biome_data <- distinct(data.biomes)

#Attach final data frames
map_data_final <- left_join(int_out_final,country_data_final,by="aid")
map_data_final <- distinct(map_data_final)
map_data_final <- left_join(map_data_final,biome_data,by="aid")
map_data_final <- distinct(map_data_final)

#Total countries
rows <- c(1:nrow(map_data_final))
tots <- matrix(nrow=nrow(map_data_final),ncol=1)
rownames(tots) <- rows
colnames(tots) <- "total_countries"

for (i in rows){
  if (map_data_final$region[i] == "Africa"){
    tots[i] <- 56
  } else if (map_data_final$region[i] == "Asia"){
    tots[i] <- 43
  } else if (map_data_final$region[i] == "Europe"){
    tots[i] <- 14
  } else if (map_data_final$region[i] == "Latin America"){
    tots[i] <- 32
  } else if (map_data_final$region[i] == "Oceania"){
    tots[i] <- 12
  } else
    tots[i] <- NA
}

tots <- as.data.frame(tots)
map_data_final <- bind_cols(map_data_final,tots)

#Append study design and impact evaluation information

ie <- select(data.study,aid,Comps,Data_type)
ie <- distinct(ie)
ie_e <- filter(ie,Comps==1)
ie_e <- distinct(ie_e)
ie_e_e <- filter(ie_e,Data_type == "Quant")
ie_aid <- select(ie_e_e,aid)
ie_aid[,"IE"] <- "Y"

map_data_final <- full_join(map_data_final,ie_aid, by="aid")
map_data_final[,"IE"][is.na(map_data_final[,"IE"])] <- "N"

bib <- select(data.biblio,aid,Pub_type,Authors,DOI,Pub_year,Title,Journal)
bib <- distinct(bib)
map_data_final <- left_join(map_data_final,bib,by="aid")

map_data_final <- assignBiomeGroups(map_data_final,out)

study <- select(data.study, aid, Comps, Comps.type, Comps.time, Design.qual_only, Design.assigned, Design.control)
study <- distinct(study)
study$study_type <- c("")
row <- c(1:nrow(study))

BACI <- filter(study, Design.control == 1,(Comps.time == "Punctuated" | Comps.time == "Yes, punctuated"))
BACI <- distinct(BACI)
BACI$study_type <- "BACI"
BA <- filter(study, Design.control == 0,(Comps.time == "Punctuated" | Comps.time == "Yes, punctuated"))
BA <- distinct(BA)
BA$study_type <- "BA"
CT <- filter(study, (Comps.time == "Continual" | Comps.time == "Continuous" | Comps.time == "Yes, continual"),Design.control == 0)
CT <- distinct(CT)
CT$study_type <- "CT"
CG <- filter(study, Design.control == 0, Comps == 1, (Comps.type == "Cultural/ethnic groups" | Comps.type == "Demographic groups" | Comps.type == "Presence/Absence of intervention" | Comps.type == "Presence/absence of intervention" | Comps.type == "Projects" | Comps.type == "Sites" | Comps.type == "Socio-economic groups/levels" | Comps.type == "User groups"| Comps.type == "cultural/ethnic groups" | Comps.type == "demographic groups" | Comps.type == "presence/absence of intervention" | Comps.type == "project" | Comps.type == "projects" | Comps.type == "sites" | Comps.type == "socio-economic" | Comps.type == "socio-economic groups" | Comps.type == "user groups"))
CG <- distinct(CG)
CG$study_type <- "CG"
CGBA <- filter(study, (Comps.type != "" | Comps.type != "None" | Comps.type != "Unspecified"),(Comps.time == "Punctuated" | Comps.time == "Yes, punctuated"), Design.control == 0)
CGBA <- distinct(CGBA)
CGBA$study_type <- "CGBA"
CGCI <- filter(study, (Comps.type != "" | Comps.type != "None" | Comps.type != "Unspecified"), Design.control == 1)
CGCI <- distinct(CGCI)
CGCI$study_type <- "CGCI"
NONE <- filter(study,Comps ==0)
NONE <- distinct(NONE)
NONE$study_type <- "None"

study <- bind_rows(BACI, BA, CT, CG, CGBA, CGCI, NONE)
study <- distinct(study)
study <- arrange(study,aid)

map_data_final <- left_join(map_data_final,study,by="aid")
map_data_final <- distinct(map_data_final)

#read in DOI info
doi <- readRDS("data/map_data_final_doi_sent.rds")
doi <- select(doi, aid,DOI, Title.formatted, Fullcitation)
doi <- distinct(doi)

map_data_final <- select(map_data_final, -DOI)
map_data_final <- left_join(map_data_final,doi,by="aid")
map_data_final <- distinct(map_data_final)
map_data_final <- map_data_final[apply(map_data_final,1,function(x)any(!is.na(x))),]

#Read in open access info
open <- read.csv("data/data+openaccess.csv",header=TRUE)
map_data_final <- left_join(map_data_final,open,by="aid")

saveRDS(map_data_final,file="~/Documents/github/visualizations/beta_tool/data/map_data_final_2.rds")

int_type_def = as.data.frame(c("area_protect", "area_mgmt", "res_mgmt", "sp_control", "restoration", "sp_mgmt", "sp_recov", "sp_reint", "ex_situ", "form_ed", "training", "aware_comm", "legis", "pol_reg", "priv_codes", "compl_enfor", "liv_alt", "sub", "market", "non_mon", "inst_civ_dev", "part_dev", "cons_fin", "sus_use", "other"))

group_type_def = as.data.frame(c("area_protect", "res_mgmt", "land_wat_mgmt", "species_mgmt", "education", "law_policy", "liv_eco_inc", "ext_cap_build", "sus_use", "other"))

out_type_def = as.data.frame(c("env", "mat_liv_std", "eco_liv_std", "health", "education", "soc_rel", "sec_saf", "gov", "sub_well", "culture", "free_choice", "other"))
