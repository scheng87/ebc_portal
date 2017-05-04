##Interactive webportal for international wildlife trade evidence map

##server

##Main server.R for beta tool
source("sources.R")
source("server_explore.R")
source("server_pages.R")
# source("server_about.R")
# source("server_biome.R")

shinyServer(function(input,output,session){
  env_serv=environment()
  ########=======#######
  ##====BIOME PAGE====##
  ########=======#######
  
  ##=====Read in shapefiles for WWF terrestrial, freshwater, and marine ecoregions=====##
#   ter <- readOGR("data/Simplified", layer="wwf_1km")
#   terr <- data.frame(BIOME=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,98,99),biome_label=c("Tropical/Subtropical Moist Broadleaf Forests","Tropical/Subtropical Dry Broadleaf Forests","Tropical/Subtropical Coniferous Forests","Temperate Broadleaf & Mixed Forests","Temperate Coniferous Forests","Boreal Forests/Taiga","Tropical/Subtropical Grasslands, Savannas, & Shrublands","Temperate Grasslands, Savannas & Shrublands","Flooded Grasslands & Savannas","Montane Grasslands & Shrublands","Tundra","Mediterranean Forests, Woodlands & Scrubs","Deserts & Xeric Shrublands","Mangroves","Unknown","Unidentified"))
#   ter <- merge(ter,terr,by="BIOME")
#   ter2 <- subset(ter,ter$BIOME %in% c(1:14))
#   fre <- readOGR("data/Simplified", layer="FEOW_1km")
#   mar <- readOGR("data/Simplified", layer="meow_1km")
#   
#   ##=====Subset into biomes========##
#   FOR <- subset(ter,ter$BIOME %in% c(1:6,12))
#   GRS <- subset(ter,ter$BIOME %in% c(7:10))
#   DES <- subset(ter,ter$BIOME %in% c(13))
#   TUN <- subset(ter,ter$BIOME %in% c(11))
#   MAN <- subset(ter,ter$BIOME %in% c(14))
  
  startp(env_serv)
#   adata(env_serv)
#   elinkexplore(env_serv)
  elink(env_serv)
  eglob(env_serv)
  eintout(env_serv)
#   ebiom(env_serv)
  
  code_def <- reactive({
    t <- filter(var.labels,code == as.character(input$code))
    p <- t$code_def
  })
  
  observeEvent(input$toggle_lu, {
    toggle("gloss")
  })
  
  observeEvent(input$user_reg1, {
    updateNavbarPage(session, "mainpage", selected="regist")
  })
  
  observeEvent(input$user_reg2, {
    updateNavbarPage(session, "mainpage", selected="regist")
  })
  
  observeEvent(input$user_reg3, {
    updateNavbarPage(session, "mainpage", selected="regist")
  })
  
  observeEvent(input$user_reg4, {
    updateNavbarPage(session, "mainpage", selected="regist")
  })
  
  observeEvent(input$submit, {
    output$thankyou <- renderText({
      "Thank you for registering. You may now go back to the exploration pages to download your data."
    })
  })
  
  output$code_out <- renderText({ paste(as.character(code_def())) })
  
  intdef1 <- reactive({
    data <- filter(definitions,Term == input$intervention_def1)
    def <- data$Definition
  })
  
  intex1 <- reactive({
    data <- filter(definitions,Term == input$intervention_def1)
    ex <- data$Example
  })
  
  output$int_def21 <- renderText({
    paste(as.character(intdef1()))
  })
  
  output$int_def31 <- renderText({
    paste(as.character(intex1()))
  })
  
  outdef1 <- reactive({
    data <- filter(definitions,Term == input$outcome_def1)
    def <- data$Definition
  })
  outex1 <- reactive({
    data <- filter(definitions,Term == input$outcome_def1)
    ex <- data$Example
  })
  
  output$out_def21 <- renderText({
    paste(as.character(outdef1()))
  })
  
  output$out_def31 <- renderText({
    paste(as.character(outex1()))
  })
  
  biodef1 <- reactive({
    data <- filter(definitions,Term == input$biome_def1)
    def <- data$Definition
  })
  bioex1 <- reactive({
    data <- filter(definitions,Term == input$biome_def1)
    ex <- data$Example
  })
  
  output$bio_def21 <- renderText({
    paste(as.character(biodef1()))
  })
  
  output$bio_def31 <- renderText({
    paste(as.character(bioex1()))
  })
  
  studydef1 <- reactive({
    data <- filter(definitions,Term == input$study_def1)
    def <- data$Definition
  })
  studyex1 <- reactive({
    data <- filter(definitions,Term == input$study_def1)
    ex <- data$Example
  })
  
  output$study_def21 <- renderText({
    paste(as.character(studydef1()))
  })
  
  output$study_def31 <- renderText({
    paste(as.character(studyex1()))
  })
  
  # Whenever a field is filled, aggregate all form data for user registration
  formData <- reactive({
    data <- sapply(fields, function(x) input[[x]])
    data
  })
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$submit, {
    saveData(formData())
  })
  
})