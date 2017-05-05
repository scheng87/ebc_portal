##Define functions

##Define functions

##Assign intervention type to intervention group
assignIntGroup <- function(input,output){
  rows <- c(1:nrow(input))
  int_groups <- matrix(nrow=nrow(input),ncol=1)
  rownames(int_groups) <- rows
  colnames(int_groups) <- c("int_group")
  for (i in rows){
    int <- as.vector(input$Int_type[i])
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
  output <- bind_cols(input,int_groups)
  output <- filter(output,!is.na(int_groups))
  return(output)
}

assignIntLabel <- function(input,output){
  rows <- c(1:nrow(input))
  int_labels <- matrix(nrow=nrow(input),ncol=1)
  rownames(int_labels) <- rows
  colnames(int_labels) <- c("int_labels")
  for (i in rows){
    int <- as.vector(input$Int_type[i])
    if (int == "area_mgmt") {
      group <- "Area management"
    } else if (int == "sp_control") {
      group <- "Species control"
    } else if (int == "restoration") {
      group <- "Restoration"
    } else if (int == "sp_mgmt") {
      group <- "Species management"
    } else if (int == "sp_reint") {
      group <- "Species reintroduction"
    } else if (int == "ex_situ") {
      group <- "Ex-situ conservation"
    } else if (int == "sp_recov") {
      group <- "Species recovery"
    } else if (int == "form_ed") {
      group <- "Formal education"
    } else if (int == "training") {
      group <- "Training"
    } else if (int == "aware_comm") {
      group <- "Awareness & communication"
    } else if (int == "legis") {
      group <- "Legislation"
    } else if (int == "pol_reg") {
      group <- "Policies & regulations"
    } else if (int == "priv_codes") {
      group <- "Private sector standards & codes"
    } else if (int == "compl_enfor") {
      group <- "Compliance & enforcement"
    } else if (int == "liv_alt") {
      group <- "Enterprises & livelihood alternatives"
    } else if (int == "sub") {
      group <- "Substitution"
    } else if (int == "market") {
      group <- "Market-based forces"
    } else if (int == "non_mon") {
      group <- "Non-monetary values"
    } else if (int == "inst_civ_dev") {
      group <- "Institution & civil society development"
    } else if (int == "part_dev") {
      group <- "Alliance & partnership development"
    } else if (int == "sus_use") {
      group <- "Sustainable use"
    } else if (int == "area protect") {
      group <- "Area protection"
    } else if (int == "res_mgmt") {
      group <- "Resource management"
    } else if (int == "other") {
      group <- "Other"
    } else 
      group <- int
    int_labels[i,"int_labels"] <- group
  }
  int_labels <- as.data.frame(int_labels)
  output <- bind_cols(input,int_labels)
  output <- filter(output,!is.na(int_labels))
  return(output)
}

saveData <- function(data) {
  # Grab the Google Sheet
  sheet <- gs_title(table)
  # Add the data as a new row
  gs_add_row(sheet, input = data)
}

loadData <- function() {
  # Grab the Google Sheet
  sheet <- gs_title(table)
  # Read the data
  gs_read_csv(sheet)
}


##===========
## Assign biome groups
## Need to pull out 
assignBiomeGroups <- function(input,output){
  rows <- c(1:nrow(input))
  biome_groups <- matrix(nrow=nrow(input),ncol=1)
  rownames(biome_groups) <- rows
  colnames(biome_groups) <- c("biome_group")
  
  for (i in rows){
    biome <- input$Biome.[i]
    if (is.na(biome)) {
      group <- "NA"
    } else if (biome == "M_P" | biome == "M_TSS" | biome == "M_TU" | biome == "M_TRU" | biome == "M_TRC" | biome == "M_TSTSS") {
      group <- "MAR"
    } else if (biome == "FW_LL" | biome == "FW_LRD" | biome == "FW_PF" | biome == "FW_MF" | biome == "FW_TCR" | biome == "FW_TFRW" | biome == "FW_TUR" | biome == "FW_TSTCR" | biome == "FW_TSTFRW" | biome == "FW_TSTUR" | biome == "FW_XFEB" | biome == "FW_OI") {
      group <- "FRW"
    } else if (biome == "T_TSTMBF" | biome == "T_TSTDBF" | biome == "T_TSTCF" | biome == "T_TBMF" | biome == "T_TCF" | biome == "T_BFT" | biome == "T_MFWS") {
      group <- "FOR"
    } else if (biome == "T_TSTGSS" | biome == "T_TGSS" | biome == "T_FGS" | biome == "T_MGS") {
      group <- "GRS"
    } else if (biome == "T_T") {
      group <- "TUN"
    } else if (biome == "T_DXS") {
      group <- "DES"
    } else if (biome == "T_M") {
      group <- "MAN"
    } 
    biome_groups[i,"biome_group"] <- group
  }
  biome_groups <- as.data.frame(biome_groups)
  output <- bind_cols(input,biome_groups)
  output <- filter(output,!is.na(biome_groups))
  return(output) 
}

assignBiomeLabels <- function(input,output){
  rows <- c(1:nrow(input))
  biome_labels <- matrix(nrow=nrow(input),ncol=1)
  rownames(biome_labels) <- rows
  colnames(biome_labels) <- c("biome_label")
  
  for (i in rows){
    biome <- input$Biome.[i]
    if (is.na(biome)) {
      group <- "NA"
    } else if (biome == "M_P") {
      group <- "Polar"
    } else if (biome == "M_TSS") {
      group <- "Temperate shelfs & seas"
    } else if (biome == "M_TU") {
      group <- "Temperate upwelling"
    } else if (biome == "M_TRU") {
      group <- "Tropical upwelling"
    } else if (biome == "M_TRC") {
      group <- "Tropical coral reefs"
    } else if (biome == "M_TSTSS") {
      group <- "Tropical/subtropial shelfs & seas"
    } else if (biome == "FW_LL") {
      group <- "Large lakes"
    } else if (biome == "FW_LRD") {
      group <- "Large river deltas"
    } else if (biome == "FW_PF") {
      group <- "Polar freshwaters"
    } else if (biome == "FW_MF") {
      group <- "Montane freshwaters"
    } else if (biome == "FW_TCR") {
      group <- "Temperate coastal rivers"
    } else if (biome == "FW_TFRW") {
      group <- "Temperate floodplain rivers & wetland complexes"
    } else if (biome == "FW_TUR") {
      group <- "Temperate upland rivers"
    } else if (biome == "FW_TSTCR") {
      group <- "Tropical/subtropical coastal rivers"
    } else if (biome == "FW_TSTFRW") {
      group <- "Tropical/subtropical floodplain rivers & wetland complexes"
    } else if (biome == "FW_TSTUR") {
      group <- "Tropical/subtropical upland rivers"
    } else if (biome == "FW_XFEB") {
      group <- "Xeric freshwaters & Endorheic (Closed) Basins"
    } else if (biome == "FW_OI") {
      group <- "Oceanic Islands"
    } else if (biome == "T_TSTMBF") {
      group <- "Tropical/subtropical moist broadleaf forests"
    } else if (biome == "T_TSTDBF") {
      group <- "Tropical/subtropical dry broadleaf forests"
    } else if (biome == "T_TSTCF") {
      group <- "Tropical/subtropical coniferous forests"
    } else if (biome == "T_TBMF") {
      group <- "Temperate broadleaf & mixed forests"
    } else if (biome == "T_TCF") {
      group <- "Temperate coniferous forests"
    } else if (biome == "T_BFT") {
      group <- "Boreal forests/taiga"
    } else if (biome == "T_MFWS") {
      group <- "Mediterranean forests, woodlands, and scrublands"
    } else if (biome == "T_TSTGSS") {
      group <- "Tropical/subtropical grasslands, savannas, & shrublands"
    } else if (biome == "T_TGSS") {
      group <- "Temperate grasslands, savannas, & shrublands"
    } else if (biome == "T_FGS") {
      group <- "Flooded grasslands & savannas"
    } else if (biome == "T_MGS") {
      group <- "Montane grasslands & shrublands"
    } else if (biome == "T_T") {
      group <- "Tundra"
    } else if (biome == "T_DXS") {
      group <- "Deserts"
    } else if (biome == "T_M") {
      group <- "Mangroves"
    } 
    biome_labels[i,"biome_label"] <- group
  }
  biome_labels <- as.data.frame(biome_labels)
  output <- bind_cols(input,biome_labels)
  output <- filter(output,!is.na(biome_labels))
  return(output) 
}

