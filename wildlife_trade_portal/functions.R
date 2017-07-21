##Define functions

##Assign intervention type to intervention group
assignIntGroup <- function(input,output){
  rows <- c(1:nrow(input))
  int_groups <- matrix(nrow=nrow(input),ncol=1)
  rownames(int_groups) <- rows
  colnames(int_groups) <- c("int_group")
  for (i in rows){
    int <- as.vector(input$Int_type[i])
    if (int == "laws" | int == "policies") {
      group <- "Establish/refine laws & policies"
    } else if (int == "detection" | int == "prosecution" | int == "civil") {
      group <- "Enforcement/compliance"
    } else if (int == "substitution" | int == "awareness" | int == "market") {
      group <- "Reduce demand/consumption"
    } else if (int == "disincentive" | int == "stewardship" | int == "conflict") {
      group <- "Support livelihoods"
    } else if (int == "spat_protect" | int == "harvest_reg" | int == "culture") {
      group <- "Reduce threats to species"
    } else 
      group <- int
    int_groups[i,"int_group"] <- group
  }
  int_groups <- as.data.frame(int_groups)
  output <- bind_cols(input,int_groups)
  output <- filter(output,!is.na(int_groups))
  return(output)
}

##Assign outcome type to outcome group
assignOutGroup <- function(input,output){
  rows <- c(1:nrow(input))
  out_groups <- matrix(nrow=nrow(input),ncol=1)
  rownames(out_groups) <- rows
  colnames(out_groups) <- c("out_group")
  for (i in rows){
    out <- as.vector(input$Outcome[i])
    if (out == "Management" | out == "Protection" | out == "Trade" | out == "Behavior change") {
      group <- "Behavioural"
    } else if (out == "Population" | out == "Species") {
      group <- "Biological"
    } else if (out == "Economic living standards" | out == "Material living standards" | out == "Health" | out == "Education" | out == "Social relations" | out == "Security and safety" | out == "Governance and empowerment" | out == "Subjective well-being" | out == "Culture & spirituality" | out == "Freedom of choice & action") {
      group <- "Human well-being"
    } else 
      group <- out
    out_groups[i,"out_group"] <- group
  }
  out_groups <- as.data.frame(out_groups)
  output <- bind_cols(input,out_groups)
  output <- filter(output,!is.na(out_groups))
  return(output)
}

assignIntLabel <- function(input,output){
  rows <- c(1:nrow(input))
  int_labels <- matrix(nrow=nrow(input),ncol=1)
  rownames(int_labels) <- rows
  colnames(int_labels) <- c("int_label")
  for (i in rows){
    int <- as.vector(input$Int_type[i])
    if (int == "laws") {
      group <- "Laws, regulations & codes"
    } else if (int == "policies") {
      group <- "Policies & regulations"
    } else if (int == "substitution") {
      group <- "Substitution"
    } else if (int == "awareness") {
      group <- "Awareness raising"
    } else if (int == "market") {
      group <- "Market-based incentives"
    } else if (int == "disincentive"){
      group <- "Disincentives for illegal behavior"
    } else if (int == "stewardship"){
      group <- "Incentives for stewardship of wildlife"
    } else if (int == "conflict"){
      group <- "Decrease human-wildlife conflict"
    } else if (int == "detection"){
      group <- "Detection"
    } else if (int == "prosecution"){
      group <- "Prosecution"
    } else if (int == "civil"){
      group <- "Civil action"
    } else if (int == "spat_protect"){
      group <- "Spatial areas of protection"
    } else if (int == "harvest_reg"){
      group <- "Regulate harvest"
    } else if (int == "culture"){
      group <- "Culturing of species"
    } else
      group <- int
    int_labels[i,"int_label"] <- group
  }
  int_labels <- as.data.frame(int_labels)
  output <- bind_cols(input,int_labels)
  output <- filter(output,!is.na(int_labels))
  return(output)
}

assignSupLabel <- function(input,output){
  rows <- c(1:nrow(input))
  int_labels <- matrix(nrow=nrow(input),ncol=1)
  rownames(int_labels) <- rows
  colnames(int_labels) <- c("supply_label")
  for (i in rows){
    int <- as.vector(input$Design.type[i])
    if (int == "supply") {
      group <- "Supply-side"
    } else if (int == "trade") {
      group <- "Trade controls"
    } else if (int == "consumer") {
      group <- "End-market"
    } else
      group <- int
    int_labels[i,"supply_label"] <- group
  }
  int_labels <- as.data.frame(int_labels)
  output <- bind_cols(input,int_labels)
  output <- filter(output,!is.na(int_labels))
  return(output)
}
# reLabelInt <- function(input,output){
#   rows <- c(1:nrow(input))
#   new_matrix <- matrix(nrow=nrow(input),ncol=3)
#   rownames(new_matrix) <- rows
#   colnames(new_matrix) <- c("labels","def","example")
#     for (i in rows){
#       int <- as.vector(input$Int_type[i])
#       if (int == "area_mgmt"){
#         label <- "Site/Area management"
#         def <- "Management of protected areas and other resource lands for conservation"
#         example <- "For example: maintenenance of habitat, site design, demarcating borders, erecting fences, training park staff, control of poachers, etc..."
#       } else if (int == "area_protect"){
#         label <- "Site/Area protection"
#         def <- "Establishing or expanding public or private parks, reserves, and other protected areas roughly equivalent to IUCN Categories I-VI (includes marine protected areas)"
#         example <- "national parks, nature reserves, marine protected areas (MPAs), town wildlife sanctuaries, private reserves, tribally owned hunting grounds, communal protected areas, etc."
#       } else if (int == "res_mgmt"){
#         label <- "Resource & habitat protection"
#         def <- "Establishing protection or easements of some specific aspect of the resource on public or private lands outside of IUCN Categories I-VI"
#         example <- "easements, development rights, water rights, instream flow rights, wild & scenic river designation, etc..."
#       } else if (int == "sp_control"){
#         label <- "Invasive/problematic species control"
#           def <- "Controlling and/or preventing invasive and/or other problematic plants, animals, and pathogens"
#           example <- "cutting vines off trees, preventing ballast water discharge, etc..."
#       } else if (int == "restoration"){
#         label <- "Restoration"
#           def <- "Enhancing degraded or restoring missing habitats and ecosystem functions; dealing with pollution"
#           example <- "creating forest corridors, prairie re-creation, riparian tree plantings, coral reef restoration, mangrove replanting, prescribed burns, breaching levees, dam removal, installing fish ladders, liming acid lakes, cleaning up oil spills , modifying land use policy (to reduce or stop logging and sedimentation), etc..."
#       } else if (int == "sp_mgmt"){
#         label <- "Species management"
#           def <- "Managing specific plant and animal populations of concern - includes harvest and trade management, and limiting population growth"
#           example <- "Harvest management: harvest management of wild mushrooms, setting fishing quotas, seting catch-size limits, etc...; Trade management: setting harvest quotas, trade regulations for specific populations, non CITES trade regulations, aquarium trade regulation, regulation of trade in non-timber forest products, etc...; Limiting population growth: culling buffalo to keep population size within park carrying capacity, sterilization of animals, etc..."
#       } else if (int == "sp_recov"){
#         label <- "Species recovery"
#           def <- "Manipulating, enhancing or restoring specific plant and animal populations, vaccination programs"
#           example <- "manual pollination of trees, artificial nesting boxes/platforms, clutch manipulation, supplementary feeding, disease/pathogen/parasite management, etc..."
#       } else if (int == "sp_reint"){
#         label <- "Species reintroduction"
#           def <- "Re-introducing species to places where they formally occurred or benign introductions"
#           example <- NA
#       } else if (int == "ex_situ"){
#         label <- "Ex-situ conservation"
#           def <- "Protecting biodiversity out of its native habitats"
#           example <- "captive breeding of animals, propagation of plants from seeds or cuttings, artificial propagation of plants, gene-banking, cryopreservation, etc..."
#       } else if (int == "form_ed"){
#         label <- "Formal education"
#           def <- "Enhancing knowledge and skills of students in a formal degree programme"
#           example <- "public schools, colleges & universities, continuing education, etc."
#       } else if (int == "training"){
#         label <- "Training"
#           def <- "Enhancing knowledge, skills and information exchange for practitioners, stakeholders, and other relevant individuals in structured settings outside of degree programmes"
#           example <- "monitoring workshops or training courses in reserve design for park managers, learning networks or writing how-to manuals for project managers, stakeholder education on specific issues, improving species identification skills (especially of animal or plant parts in trade), training on how to set shark nets for beach protection to minimize bycatch of sharks and cetaceans, etc..."
#       } else if (int == "aware_comm"){
#         label <- "Awareness & communications"
#           def <- "Raising environmental awareness and providing information through various media or through civil disobedience"
#           example <- "radio soap operas, environmental publishing, web blogs, puppet shows, door-to-door canvassing, tree sitting, protest marches, etc..."
#       } else if (int == "legis"){
#         label <- "Legislation"
#           def <- "Making, implementing, changing, influencing, or providing input into formal government sector legislation or polices at all levels: international, national, state/provincial, local, tribal"
#           example <- "Promoting conventions on biodiversity, wildlife trade laws like CITES, regional legislation like the European Habitats Directive, work for or against government laws such as the US Endangered Species Act, influencing legislative appropriations, State/provincial: state ballot initiatives, providing data to state policy makers, developing pollution permitting systems, dam relicensing, Local: developing zoning regulations, countryside laws, species protection laws, hunting bans Tribal: creating tribal laws, etc..."
#       } else if (int == "pol_reg"){
#         label <- "Policies & regulations"
#           def <- "Making, implementing, changing, influencing, or providing input into policies and regulations affecting the implementation of laws at all levels: international, national, state/provincial, local/community, tribal"
#           example <- "Input into agency plans regulating certain species or resources, working with local governments or communities to implement zoning regulations; promoting sustainable harvest of timber on state forest lands, etc..."
#       } else if (int == "priv_code"){
#         label <- "Private sector standards & codes"
#           def <- "Setting, implementing, changing, influencing, or providing input into voluntary standards & professional codes that govern private sector practice"
#           example <- "Marine & Forest Stewardship Councils, Marine Aquarium Council, Conservation Measures Partnership (CMP) Open Standards, corporate adoption of forestry best management practices, sustainable grazing by a rancher, etc..."
#       } else if (int == "compl_enfor"){
#         label <- "Compliance & enforcement"
#           def <- "Monitoring and enforcing compliance with laws, policies & regulations, and standards & codes at all levels"
#           example <- NA
#       } else if (int == "liv_alt"){
#         label <- "Linked enterprises & livelihood alternatives"
#           def <- "Developing enterprises that directly depend on the maintenance of natural resources or provide substitute livelihoods as a means of changing behaviours and attitudes"
#           example <- "ecotourism, non-timber forest product harvesting, harvesting wild salmon to create value for wild population, etc..."
#       } else if (int == "sub"){
#         label <- "Substitution"
#           def <- "Promoting alternative products and services that substitute for environmentally damaging ones"
#           example <- "Viagra for rhino horn, farmed salmon as a replacement for pressure on wild populations, promoting recycling and use of recycled materials, etc..."
#       } else if (int == "market"){
#         label <- "Market forces"
#           def <- "Using market mechanisms to change behaviours and attitudes"
#           example <- "certification, positive incentives, boycotts, negative incentives, grass & forest banking, valuation of ecosystem services such as flood control, etc."
#       } else if (int == "non_mon"){
#         label <- "Non-monetary values"
#           def <- "Using intangible values to change behaviours and attitudes"
#           example <- "spiritual, cultural, links to human health, etc..."
#       } else if (int == "inst_civ_dev"){
#         label <- "Institutional & civil society development"
#           def <- "Creating or providing non-financial support & capacity building for non-profits, government agencies, communities, and for-profits"
#           example <- "creating new local land trusts, providing circuit riders to help develop organizational capacity, etc.
# .."
#       } else if (int == "part_dev"){
#         label <- "Alliance & partnership development"
#           def <- "Forming and facilitating partnerships, alliances, and networks of organizations"
#           example <- "country networks, Conservation Measures Partnership (CMP), Cambridge Conservation Initiative (CCI), etc..."
#       } else if (int == "cons_fin"){
#         label <- "Conservation finance"
#           def <- "Raising and providing funds for conservation work"
#           example <- "private foundations, debt-for-nature swaps"
#       } else if (int == "sus_use"){
#         label <- "Sustainable use"
#           def <- "Promoting sustainable use of resources"
#           example <- NA
#       } else if (int == "other"){
#         label <- "Other"
#         def <- "Other actions that don't fit into the IUCN-CMP typology"
#         example <- NA
#       }     
#       new_matrix[i,"labels"] <- label
#       new_matrix[i,"def"] <- def
#       new_matrix[i,"example"] <- example
#     }
#     new_matrix <- as.data.frame(new_matrix)
#     output <- bind_cols(input,new_matrix)
#     return(output)
#   } 
# 
# reLabelOut <- function(input,output){
#   rows <- c(1:nrow(input))
#   new_matrix <- matrix(nrow=nrow(input),ncol=3)
#   rownames(new_matrix) <- rows
#   colnames(new_matrix) <- c("labels","def","example")
#     for (i in rows){
#       out <- as.vector(input$Outcome[i])
#       if (out == "env"){
#         label <- "Environmental"
#         def <- "Ecological and biological outcomes"
#         example <- "Trees planted, percent forest cover, percent coral cover, biomass, etc..."
#       } else if (out == "mat_liv_std"){
#         label <- "Material living standards"
#         def <- "Encompasses the living standards of basic life"
#         example <- "Material assets owned, basic infrastructure (electricity, water, telecommunication and transportation), shelter, resource use (this can incude sustainable use/harvest)"
#       } else if (out == "eco_liv_std"){
#         label <- "Economic living standards"
#         def <- "Encompasses the living standards of basic life"
#         example <- "Income, employment, employment opportunities, wealth, poverty, savings"
#       } else if (out == "health"){
#         label <- "Health"
#         def <-"Any component of mental or physical health"
#         example <- "Physical health, longevity/life expectancy, maternal health, child health, access to health care, nutrition, occurrence of diseases, mental health"
#       } else if (out == "education"){
#         label <- "Education"
#         def <-"Any component relating to level of or access to education or training, knowledge gained"
#         example <- "Education infrastructure: Access to school, access to training, quality of education; Informal education: transfer of knowledge, livelihood skills, traditional ecological knowledge/local ecological knowledge; Formal education: Degrees awarded, students enrolled"
#       } else if (out == "soc_rel"){
#         label <- "Social relations"
#         def <-"Relations between members of society. Clarify at: between individuals, within and/or between groups (communities, stakeholders, ethnic groups, genders)"
#         example <- "Conflict, relationships, connectedness, ability to work together, ability to help others, resilience to disturbances or shock"
#       } else if (out == "sec_saf"){
#         label <- "Safety & security"
#         def <- NA
#         example <- "Physical security (includes personal security and safety?), economic security, political security, human rights, land tenure, resource management rights, access to natural resources, food and water security, vulnerability, resilience and adaptive capacity"
#       } else if (out == "gov"){
#         label <- "Governance & empowerment"
#         def <- NA
#         example <- "Structures and processes for decision making including both formal and informal rules; incl. participation and control in decision-making, accountability, justice, transparency, governanace skills"
#       } else if (out == "sub_well"){
#         label <- "Subjective well-being"
#         def <-NA
#         example <- "Measures of happiness, quality of life, satisfaction supported by some value of ecosystem and/or resource"
#       } else if (out == "culture"){
#         label <- "Culture"
#         def <-NA
#         example <- "Cultural, societal and traditional values of natural resources and nature to the community; Sense of home; Cultural identity and heritage"
#       } else if (out == "free_choice"){
#         label <- "Freedom of choice and action/autonomy"
#         def <-"Ability to pursue what you value doing and being"
#         example <- NA
#       } else if (out == "other"){
#         label <- "Other"
#         def <- "Other human well-being outcomes that do not fall into this typology"
#         example <- NA
#       }
#       new_matrix[i,"labels"] <- label
#       new_matrix[i,"def"] <- def
#       new_matrix[i,"example"] <- example
#     }
#     new_matrix <- as.data.frame(new_matrix)
#     output <- bind_cols(input,new_matrix)
#     return(output)
# }

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


