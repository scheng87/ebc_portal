ebiom = function(env_serv) with (env_serv, {  

  ##=====Set classes======##
  terr_labs$BIOME <- as.character(as.vector(terr_labs$BIOME))
  terr_labs$ECOREGION <- as.character(as.vector(terr_labs$ECOREGION))
  
  ##=====Select ecosystem for summaries and render controls=====##
  biome_choice <- reactiveValues(data="all")
  
  observeEvent(input$ter, {
    toggle("terr")
    hide("mari")
    hide("fres")
    biome_choice$data <- as.character("terrestrial")
  })
  
  observeEvent(input$mar, {
    toggle("mari")
    hide("terr")
    hide("fres")
    biome_choice$data <- as.character("marine")
  })
  
  observeEvent(input$fre, {
    toggle("fres")
    hide("mari")
    hide("terr")
    biome_choice$data <- as.character("freshwater")
  })

  observe({
    b_eco <- if (input$b_biome == "All") character(0) else{
      filter(terr_labs, BIOME == input$b_biome) %>%
        select(ECOREGION) %>%
        unique()
    }
    stillSelected <- isolate(input$b_eco[input$b_eco %in% b_eco])
    rownames(stillSelected) <- NULL
    rownames(b_eco) <- NULL
    updateSelectInput(session,"b_eco", choices=as.vector(b_eco), selected=stillSelected)
  })
  
  eco <- reactive({
    if (biome_choice$data == "terrestrial"){
      bio <- input$b_biome
      if (input$b_eco == ""){
        eco <- "all"
      } else eco <- input$b_eco
      int <- input$b_intervention
      out <- input$b_outcome
    } else if (biome_choice$data == "marine"){
      eco <- input$m_eco
      int <- input$m_intervention
      out <- input$m_outcome
      bio <- "all"
    } else if (biome_choice$data == "freshwater"){
      eco <- input$f_eco
      int <- input$f_intervention
      out <- input$f_outcome
      bio <- "all"
    } else if (biome_choice$data == "all"){
      eco <- "all"
      int <- "all"
      out <- "all"
      bio <- "all"
    }
    
    var <- c(eco,int,out,bio)
  })

  ##==========
  ## Creating reactive data table
  ##==========
  
  ebiom_data1 <- reactive({
    data <- map_data_final
    
    if (biome_choice$data == "terrestrial"){
      sub <- filter(data,biome_group != "FRW", biome_group != "MAR")
    } else if (biome_choice$data == "marine"){
      sub <- filter(data,biome_group == "MAR")
    } else if (biome_choice$data == "freshwater"){
      sub <- filter(data,biome_group == "FRW")
    } else sub <- data
    
   sub <- distinct(sub)
  })

  ebiom_data2 <- reactive({
    data <- as.data.frame(ebiom_data1())
    var <- c(eco())
    
    if (var[[4]] == "all"){
      sub <- data
    } else if (var[[4]] == "forest"){
      sub <- data[data$biome_group == "FOR",]
    } else if (var[[4]] == "grass"){
      sub <- data[data$biome_group == "GRS",]
    } else if (var[[4]] == "tundra"){
      sub <- data[data$biome_group == "TUN",]
    } else if (var[[4]] == "mangrove"){
      sub <- data[data$biome_group == "MAN",]
    } else if (var[[4]] == "desert"){
      sub <- data[data$biome_group == "DES",]
    }

    sub <- distinct(sub)
  })
  
  ebiom_data3 <- reactive({
    data <- as.data.frame(ebiom_data2())
    var <- c(eco())
    
    if (var[[1]] == "all"){
      sub <- data
    } else if (var[[1]] == "Tropical & Subtropical Moist Broadleaf Forests"){
      sub <- data[data$Biome. == "T_TSTMBF",]
    } else if (var[[1]] == "Tropical & Subtropical Dry Broadleaf Forests"){
      sub <- data[data$Biome. == "T_TSTDBF",]
    } else if (var[[1]] == "Tropical & Subtropical Coniferous Forests"){
      sub <- data[data$Biome. == "T_TSTCF",]
    } else if (var[[1]] == "Temperate Broadleaf & Mixed Forests"){
      sub <- data[data$Biome. == "T_TBMF",]
    } else if (var[[1]] == "Temperate Conifer Forests"){
      sub <- data[data$Biome. == "T_TCF",]
    } else if (var[[1]] == "Boreal Forests/Taiga"){
      sub <- data[data$Biome. == "T_BFT",]
    } else if (var[[1]] == "Mediterranean Forests, Woodlands & Scrubs"){
      sub <- data[data$Biome. == "T_MFWS",]
    } else if (var[[1]] == "Tropical & Subtropical Grasslands, Savannas & Shrublands"){
      sub <- data[data$Biome. == "T_TSTGSS",]
    } else if (var[[1]] == "Temperate Grasslands, Savannas & Shrublands"){
      sub <- data[data$Biome. == "T_TGSS",]
    } else if (var[[1]] == "Flooded Grasslands & Savannas"){
      sub <- data[data$Biome. == "T_FGS",]
    } else if (var[[1]] == "Montane Grasslands & Shrublands"){
      sub <- data[data$Biome. == "T_MGS",]
    } else if (var[[1]] == "Deserts & Xeric Shrublands"){
      sub <- data[data$biome_group == "DES",]
    } else if (var[[1]] == "Mangrove"){
      sub <- data[data$biome_group == "MAN",]
    } else if (var[[1]] == "Tundra"){
      sub <- data[data$biome_group == "TUN",]
    } else sub <- data[data$Biome. == var[[1]],]
    
    sub <- distinct(sub)
  })

  ebiom_data4 <- reactive({
    data <- as.data.frame(ebiom_data3())
    var <- c(eco())
    
    if (var[[2]] == "all" & var[[3]] == "all"){
      sub <- data
    } else if (var[[2]] != "all" & var[[3]] == "all"){
      sub <- data[data$Int_type == var[[2]],]
    } else if (var[[2]] == "all" & var[[3]] != "all"){
      sub <- data[data$Outcome == var[[3]],]
    } else if (var[[2]] != "all" & var[[3]] != "all"){
      sub <- data %>% filter(Int_type == var[[2]], Outcome == var[[3]])
    }
    
    sub <- distinct(sub)
    
  })

  output$ebiom_table <- DT::renderDataTable({
    data <- as.data.frame(ebiom_data4())
    data <- data %>% select(aid,IE,region,Study_country,Biome.,biome_group,int_group,Int_type,Outcome)
    data <- data[,c("aid","IE","region","Study_country","Biome.","biome_group","int_group","Int_type","Outcome")]
    colnames(data) <- c("Article ID", "Impact evaluation?","Region","Country","Biome","Biome group","Intervention group type", "Intervention sub-type", "Outcome type")
    DT::datatable(distinct(data))
  })
  
  ##=========
  ## Creating reactive bibliography
  ##=========
  
  ebiom_bib <- reactive({
    data <- as.data.frame(ebiom_data4())
    data <- data %>% select(aid, Pub_type, Authors, Pub_year, Title, Journal, DOI, FullText)
    data <- data[,c("aid","Pub_type","Authors","Pub_year","Title","Journal","DOI","FullText")]
    data <- distinct(data)
    data$DOI <- paste0("<a href='",data$DOI,"' target='_blank'>",data$DOI,"</a>")
    data <- data
    
  })
  
  output$ebiom_bib <- DT::renderDataTable({
    data <- as.data.frame(ebiom_bib())
    colnames(data) <- c("Article ID", "Publication type","Author(s)","Publication year","Title","Journal","DOI","Open Access")
    DT::datatable(distinct(data),escape=FALSE)
  })
  
  ##==============
  ## Download buttons
  ##==============
  
  output$downloadFullData_B <- downloadHandler(
    filename = function() {
      paste("biome_subset_dataset.csv",sep="")
    },
    content = function(file) {
      write.csv(ebiom_data4(), file)
    }
  )
  
  output$downloadBiblio_B <- downloadHandler(
    filename = function() {
      paste("biome_subset_biblio.csv",sep="")
    },
    content = function(file) {
      write.csv(ebiom_bib(), file)
    }
  )
  
  observeEvent(input$submit, {
    output$download_opts7 <- renderUI({
      downloadButton("downloadBiblio_B", "Bibliography as .csv")
    })
  })
  
  observeEvent(input$submit, {
    output$download_opts8 <- renderUI({
      downloadButton("downloadFullData_B", "Data as .csv")
    })
  })
  
  ##=====Count total countries=========##
country_dat1 <- reactive({
  dat <- as.data.frame(ebiom_data4())
  
  country_count <- matrix(nrow=nrow(regions), ncol=2)
  rownames(country_count) <- regions$COUNTRY
  colnames(country_count) <- c("COUNTRY", "COUNTS")
  
  for (c in regions$COUNTRY){
    subset <- filter(dat, Study_country == c)
    country_count[c,1] <- c
    country_count[c,2] <- as.numeric(n_distinct(subset$aid))
  }
  
  rownames(country_count) = NULL
  country_count <- as.data.frame(country_count)
  country_count <- inner_join(country_count,regions,by="COUNTRY")
  country_count <- filter(country_count,CODE != "")
  country_count <- as.data.frame(country_count)
  country_count <- select(country_count,COUNTRY,COUNTS,CODE)
  country_count$labels <- country_count$COUNTRY
  colnames(country_count) <- c("var","count","ISO3","labels")
  country_count$count <- as.numeric(as.vector(country_count$count))
  country_count <- country_count
})

##=====Merge with country shapefile into new shapefile with count attribute layer======##
  
  merged <- reactive({
    dat <- as.data.frame(country_dat1())
    merged <- merge(countries_shape,dat,by='ISO3')
    merged <- merged[merged$count > 0,]
    test <- merged %>% select(LON,LAT,count,labels) %>% na.omit()
    rownames(test) <- NULL
    test$interval <- NA
    for (i in 1:nrow(test)){
      if (test[i,3] < 6){
        test[i,5] <- 5
      } else if (test[i,3] %in% c(6:10)){
        test[i,5] <- 10
      } else if (test[i,3] %in% c(11:25)){
        test[i,5] <- 25
      } else if (test[i,3] %in% c(26:50)){
        test[i,5] <- 50
      } else if (test[i,3] %in% c(51:75)){
        test[i,5] <- 75
      } else if (test[i,3] %in% c(76:100)){
        test[i,5] <- 100
      } else if (test[i,3] > 100){
        test[i,5] <- 150
      }
    }
    
    test <- test
  })

  #=====Reactively choose ecoregion layer to plot==============##
  ecoregion_shape <- reactive({
    var <- c(eco())
    if (biome_choice$data == "all"){
      shape <- NULL
      } else if (biome_choice$data == "terrestrial"){
      if (var[[4]] == "all"){
        shape <- ter
      } else if (var[[4]] == "forest" & var[[1]] == "all"){
        shape <- FOR
      } else if (var[[4]] == "forest" & var[[1]] != "all"){
        if (var[[1]] == "Tropical & Subtropical Moist Broadleaf Forests"){
        shape <- subset(ter,ter$BIOME %in% c(1))
      } else if (var[[1]] == "Tropical & Subtropical Dry Broadleaf Forests"){
        shape <- subset(ter,ter$BIOME %in% c(2))
      } else if (var[[1]] == "Tropical & Subtropical Coniferous Forests"){
        shape <- subset(ter,ter$BIOME %in% c(3))
      } else if (var[[1]] == "Temperate Broadleaf & Mixed Forests"){
        shape <- subset(ter,ter$BIOME %in% c(4))
      } else if (var[[1]] == "Temperate Conifer Forests"){
        shape <- subset(ter,ter$BIOME %in% c(5))
      } else if (var[[1]] == "Boreal Forests/Taiga"){
        shape <- subset(ter,ter$BIOME %in% c(6))
      } else if (var[[1]] == "Mediterranean Forests, Woodlands & Scrubs"){
        shape <- subset(ter,ter$BIOME %in% c(12))
      }
      } else if (var[[4]] == "grass" & var[[1]] == "all"){
        shape <- GRS
      } else if (var[[4]] == "grass" & var[[1]] != "all"){
        if (var[[1]] == "Tropical & Subtropical Grasslands, Savannas & Shrublands"){
        shape <- subset(ter,ter$BIOME %in% c(7))
      } else if (var[[1]] == "Temperate Grasslands, Savannas & Shrublands"){
        shape <- subset(ter,ter$BIOME %in% c(8))
      } else if (var[[1]] == "Flooded Grasslands & Savannas"){
        shape <- subset(ter,ter$BIOME %in% c(9))
      } else if (var[[1]] == "Montane Grasslands & Shrublands"){
        shape <- subset(ter,ter$BIOME %in% c(10))
      }
      } else if (var[[4]] == "desert"){
        shape <- DES
      } else if (var[[4]] == "mangrove"){
        shape <- MAN
      } else if (var[[4]] == "tundra"){
        shape <- TUN
      }
    } else if (biome_choice$data == "marine"){
      shape <- mar
    } else if (biome_choice$data == "freshwater"){
      if (var[[1]] == "all"){
        shape <- fre
      } else if (var[[1]] == "FW_LL"){
        shape <- subset(fre,fre$MHT_TXT %in% c("large lakes"))
      } else if (var[[1]] == "FW_LRD"){
        shape <- subset(fre,fre$MHT_TXT %in% c("large river deltas"))
      } else if (var[[1]] == "FW_MF"){
        shape <- subset(fre,fre$MHT_TXT %in% c("montane freshwaters"))
      } else if (var[[1]] == "FW_OI"){
        shape <- subset(fre,fre$MHT_TXT %in% c("oceanic islands"))
      } else if (var[[1]] == "FW_PF"){
        shape <- subset(fre,fre$MHT_TXT %in% c("polar freshwaters"))
      } else if (var[[1]] == "FW_TCR"){
        shape <- subset(fre,fre$MHT_TXT %in% c("temperate coastal rivers"))
      } else if (var[[1]] == "FW_TFRW"){
        shape <- subset(fre,fre$MHT_TXT %in% c("temperate floodplain rivers and wetlands"))
      } else if (var[[1]] == "FW_TUR"){
        shape <- subset(fre,fre$MHT_TXT %in% c("temperate upland rivers"))
      } else if (var[[1]] == "FW_TSTCR"){
        shape <- subset(fre,fre$MHT_TXT %in% c("tropical and subtropical coastal rivers"))
      } else if (var[[1]] == "FW_TSTFRWC"){
        shape <- subset(fre,fre$MHT_TXT %in% c("tropical and subtropical floodplain rivers and wetland complexes"))
      } else if (var[[1]] == "FW_TSTUR"){
        shape <- subset(fre,fre$MHT_TXT %in% c("tropical and subtropical upland rivers"))
      } else if (var[[1]] == "FW_XFEB"){
        shape <- subset(fre,fre$MHT_TXT %in% c("xeric freshwaters and endorheic (closed) basins"))
      }
    }
    shape <- shape
  })
  
##=====Create interactive map with leaflet and leaflet proxy========##
output$biome_map <- renderLeaflet({
  leaflet() %>%
    addProviderTiles("Esri.WorldGrayCanvas") %>%
    setView(lng=0,lat=30,zoom=2)
})

observe({
  shp <- ecoregion_shape()
  counts <- merged()
  
  update <- leafletProxy("biome_map",data=shp)
  update %>% clearShapes() %>%
    clearMarkers()
  
  if (biome_choice$data == "terrestrial"){
    factpal <- colorFactor("Greens",shp@data$biome_label)
    update %>% 
      addPolygons(stroke=FALSE, fillOpacity=1, smoothFactor = 0.5, color=~factpal(biome_label), popup = ~paste0("Biome: ",biome_label,"\n","Ecoregion: ",ECO_NAME)) %>%
      addCircleMarkers(data=counts, ~LON, ~LAT, popup = ~paste0("Country: ",labels,"                   No. articles: ",as.character(count)), stroke=FALSE, color="orange", radius = ~interval/4, fillOpacity=0.75)
  } else if (biome_choice$data == "marine"){
    factpal <- colorFactor("Blues",shp@data$REALM)
    update %>% 
      addPolygons(stroke=FALSE, fillOpacity=1, smoothFactor = 0.5, color=~factpal(REALM), popup = ~paste0("Realm: ",REALM,"\n","Province: ",PROVINCE)) %>%
      addCircleMarkers(data=counts, ~LON, ~LAT, popup = ~paste0("Country: ",labels,"                   No. articles: ",as.character(count)), stroke=FALSE, color="orange", radius = ~interval/4, fillOpacity=0.75)
  } else if (biome_choice$data == "freshwater"){
    factpal <- colorFactor("GnBu",shp@data$MHT_TXT)
    update %>%
      addPolygons(stroke=FALSE, fillOpacity=1, smoothFactor = 0.5, color=~factpal(MHT_TXT), popup = ~paste0("Major habitat type: ",MHT_TXT,"\n","Ecoregion: ",ECOREGION)) %>%
      addCircleMarkers(data=counts, ~LON, ~LAT, popup = ~paste0("Country: ",labels,"                   No. articles: ",as.character(count)), stroke=FALSE, color="orange", radius = ~interval/4, fillOpacity=0.75)
  }
})

observe({
  shp <- ecoregion_shape()
  update <- leafletProxy("biome_map",data=shp)
  update %>% clearControls()
  
  if (biome_choice$data == "terrestrial"){
    factpal <- colorFactor("Greens",shp@data$biome_label)
    update %>% 
      addLegend("topleft",pal = factpal ,values = ~biome_label, title = "Biome types",opacity = 1)
  } else if (biome_choice$data == "marine"){
    factpal <- colorFactor("Blues",shp@data$REALM)
    update %>% 
      addLegend("topleft",pal = factpal ,values = ~REALM, title = "Realm types",opacity = 1)
  } else if (biome_choice$data == "freshwater"){
    factpal <- colorFactor("GnBu",shp@data$MHT_TXT)
    update %>%
      addLegend("topleft",pal = factpal ,values = ~MHT_TXT, title = "Major habitat types",opacity = 1)
  }
})
  
##=========
## Info box summaries
##=========
us3 <- reactive({
  n <- n_distinct(ebiom_data4()$aid)
})

ie3 <- reactive({
  data <- select(ebiom_data4(),aid,IE)
  IE <- filter(data,IE == "Y")
  IE <- distinct(IE)
  n <- n_distinct(IE$aid)
})

oa3 <- reactive({
  data <- select(ebiom_data4(),aid,FullText)
  FT <- filter(data, FullText == "Y")
  FT <- distinct(FT)
  n <- n_distinct(FT$aid)
})

output$elink_us_3 <- renderText({
  as.character(us3())
})

output$elink_ie_3 <- renderText({
  as.character(ie3())
})

output$elink_oa_3 <- renderText({
  as.character(oa3())
})

##========
## Data summary plots
##========

output$bio_int <- renderPlotly({
  dat <- as.data.frame(ebiom_data4())
  
  if (input$show_subtypes2 == FALSE){
    dat <- dat %>% select(aid,int_group) %>% distinct()
    colnames(dat) <- c("aid","var")
    i_counts = matrix(nrow=10, ncol=2)
    rownames(i_counts) <- group_type
    var_type <- group_type
  } else {
    dat <- dat %>% select(aid,Int_type) %>% distinct()
    colnames(dat) <- c("aid","var")
    i_counts = matrix(nrow=25, ncol=2)
    rownames(i_counts) <- int_type
    var_type <- int_type
  }
  
  colnames(i_counts) <- c("var","count")
  
  for (i in var_type){
    subset <- filter(dat, var == i)
    i_counts[i,1] <- i
    i_counts[i,2] <- n_distinct(subset$aid)
  }
  
  i_counts <- as.data.frame(i_counts)
  rownames(i_counts) <- NULL
  i_counts$count <- as.numeric(as.vector(i_counts$count))
  
  if (input$show_subtypes2 == FALSE){
    i_counts$labels <- c("Area protection", "Land/Water management", "Resource management", "Species management", "Education", "Law & Policy", "Livelihood, economic & other incentives", "External capacity building", "Sustainable use", "Other")
  } else {
    i_counts$labels <- c("Area protection", "Area management", "Resource management/protection", "Species control", "Restoration", "Species management", "Species recovery", "Species reintroduction", "Ex-situ conservation", "Formal education", "Training", "Awareness & Communications", "Legislation", "Policies & Regulations", "Private sector standards and codes", "Compliance & enforcement", "Enterprises & livelihood alternatives", "Substitution", "Market-based forces", "Non-monetary values", "Institutional & civil society development", "Alliance & partnership development", "Conservation finance", "Sustainable use", "Other")
  }
  
  p <- ggplot(data=i_counts, aes(x=labels,y=count)) +
    geom_bar(stat="identity",fill="turquoise") +
    theme(axis.text.x = element_text(angle=45,hjust=1,size=10),axis.title.x = element_text(vjust=0.5,size=12),axis.title.y = element_text(hjust=0.5,size=12),axis.text.y=element_text(size=10),plot.margin=unit(c(1,1,1,2),"cm")) +
    ylab("Number of articles") +
    xlab("IUCN conservation interventions")
  
  m=list(l=120,
         b=280,
         t=25,
         pad=4
  )
  
  ggplotly(p) %>% layout(margin=m)
})

output$bio_out <- renderPlotly({
  dat <- as.data.frame(ebiom_data4())
  dat <- dat %>% select(aid,Outcome) %>% distinct()
  colnames(dat) <- c("aid","var")
  i_counts = matrix(nrow=11, ncol=2)
  rownames(i_counts) <- out_type
  colnames(i_counts) <- c("var","count")
  
  for (i in out_type){
    subset <- filter(dat, var == i)
    i_counts[i,1] <- i
    i_counts[i,2] <- n_distinct(subset$aid)
  }
  
  i_counts <- as.data.frame(i_counts)
  rownames(i_counts) <- NULL
  i_counts$count <- as.numeric(as.vector(i_counts$count))
  i_counts$labels <- out_labels
  
  p <- ggplot(data=i_counts, aes(x=labels,y=count)) +
    geom_bar(stat="identity",fill="turquoise") +
    theme(axis.text.x = element_text(angle=45,hjust=1,size=10),axis.title.x = element_text(vjust=-2,size=12),axis.title.y = element_text(size=12),axis.text.y=element_text(size=10),plot.margin=unit(c(1,1,1,2),"cm")) +
    ylab("Number of articles") +
    xlab("Human well-being outcomes") 
  
  m=list(l=120,
         b=280,
         t=25,
         pad=4
  )
  
  ggplotly(p) %>% layout(margin=m)
})

output$bio_country <- renderPlotly({
  dat <- as.data.frame(ebiom_data4())
  
  i_counts <- reactive({
    dat <- as.data.frame(country_dat1())
    final <- arrange(dat,desc(count))
    n <- as.integer(input$b_show_country)
    final <- slice(final,1:n)
  })
  
  p <- ggplot(data=i_counts(), aes(x=labels,y=count)) +
    geom_bar(stat="identity",fill="turquoise") +
    theme(axis.text.x=element_text(angle=45,hjust=1,size=10),axis.title.x = element_text(vjust=-3,size=12),axis.title.y = element_text(size=12),axis.text.y=element_text(size=10),plot.margin=unit(c(1,1,1,2),"cm")) +
    ylab("Number of articles") +
    xlab("Country")
  
  m=list(l=120,
         b=280,
         t=25,
         pad=4
  )
  
  ggplotly(p) %>% layout(margin=m)
})

output$bio_study <- renderPlotly({
  dat <- as.data.frame(ebiom_data4())
  dat <- dat %>% select(aid,study_type) %>% distinct()
  colnames(dat) <- c("aid","var")
  i_counts = matrix(nrow=7, ncol=2)
  rownames(i_counts) <- study_types
  colnames(i_counts) <- c("var","count")
  
  for (i in study_types){
    subset <- filter(dat, var == i)
    i_counts[i,1] <- i
    i_counts[i,2] <- n_distinct(subset$aid)
  }
  
  i_counts <- as.data.frame(i_counts)
  rownames(i_counts) <- NULL
  i_counts$count <- as.numeric(as.vector(i_counts$count))
  i_counts$labels <- study_labels
  
  p <- ggplot(data=i_counts, aes(x=labels,y=count)) +
    geom_bar(stat="identity",fill="turquoise") +
    theme(axis.text.x = element_text(angle=45,hjust=1,size=10),axis.title.x = element_text(vjust=-2,size=12),axis.title.y = element_text(size=12),axis.text.y=element_text(size=10),plot.margin=unit(c(1,1,1,2),"cm")) +
    ylab("Number of articles") +
    xlab("Study design type") 
  
  m=list(l=120,
         b=280,
         t=25,
         pad=4
  )
  
  ggplotly(p) %>% layout(margin=m)
  
})

})