elinkexplore = function(env_serv) with (env_serv, {
  page <- ("https://www.nceas.ucsb.edu/~brun/heatmap/heatmap.html")
  
  output$map_page <- renderUI({
    my_test <- tags$iframe(src=page, height=800, width=1300,scrolling="no")
    print(my_test)
    my_test
  })
})

elink = function(env_serv) with (env_serv, {
  data <- map_data_final
  
  subR <- reactive({
    if (input$region != "All"){
      sub <- data[data$region == input$region,]
    } else 
      sub <- data
    sub <- distinct(sub)
  })
  
  dataInputB <- reactive({
    if (input$biome == "MAR"){
      subB <- subR()[subR()$Biome. == "M_P" | subR()$Biome. == "M_TSS" | subR()$Biome. == "M_TU" | subR()$Biome. == "M_TRU" | subR()$Biome. == "M_TRC" | subR()$Biome. == "M_TSTSS",]
    } else if (input$biome == "FRW"){
      subB <- subR()[subR()$Biome. == "FW_LL" | subR()$Biome == "FW_LRD" | subR()$Biome == "FW_PF" | subR()$Biome == "FW_MF" | subR()$Biome == "FW_TCR" | subR()$Biome == "FW_TFRW" | subR()$Biome == "FW_TUR" | subR()$Biome == "FW_TSTCR" | subR()$Biome == "FW_TSTFRW" | subR()$Biome == "FW_TSTUR" | subR()$Biome == "FW_XFEB" | subR()$Biome == "FW_OI",]
    } else if (input$biome == "FOR"){
      subB <- subR()[subR()$Biome. == "T_TSTMBF" | subR()$Biome. == "T_TSTDBF" | subR()$Biome. == "T_TSTCF" | subR()$Biome. == "T_TBMF" | subR()$Biome. == "T_TCF" | subR()$Biome. == "T_BFT" | subR()$Biome. == "T_MFWS",]
    } else if (input$biome == "GRS"){
      subB <- subR()[subR()$Biome. == "T_TSTGSS" | subR()$Biome. == "T_TGSS" | subR()$Biome. == "T_FGS" | subR()$Biome. == "T_MGS",]
    } else if (input$biome == "TUN"){
      subB <- subR()[subR()$Biome. == "T_T",]
    } else if (input$biome == "DES"){
      subB <- subR()[subR()$Biome. == "T_DXS",]
    } else if (input$biome == "MAN"){
      subB <- subR()[subR()$Biome. == "T_M",]
    } else
      subB <- subR()
    subB <- distinct(subB)
  })
      
  final_input <- reactive({
    if (input$expand == TRUE){
      io_counts = matrix(nrow=11, ncol=25)
      rownames(io_counts) <- out_type
      colnames(io_counts) <- int_type
      io_counts <- io_counts
    } else if (input$expand == FALSE){
      io_counts = matrix(nrow=11, ncol=10)
      rownames(io_counts) <- out_type
      colnames(io_counts) <- group_type
      io_counts <- io_counts
    }
  })
    
  final_matrix <- reactive({
    io_counts <- final_input()
    if (input$expand == FALSE){
      for (i in group_type){
        for (j in out_type){
          subset <- filter(dataInputB(), Outcome == j, int_group == i)
          io_counts[j,i] <- n_distinct(subset$aid)
        }
      }
      io_counts <- as.data.frame(io_counts)
      io_counts$outcome <- rownames(io_counts)
      io_counts <- gather(io_counts,int,counts,1:(ncol(io_counts)-1)) 
      io_counts <- as.data.frame(io_counts)
    } else if (input$expand == TRUE) {
      for (i in int_type){
        for (j in out_type){
          subset <- filter(dataInputB(), Outcome == j, Int_type == i)
          io_counts[j,i] <- n_distinct(subset$aid)
        }
      }
      io_counts <- as.data.frame(io_counts)
      io_counts$outcome <- rownames(io_counts)
      io_counts <- gather(io_counts,int,counts,1:(ncol(io_counts)-1))
      io_counts <- as.data.frame(io_counts)
    }
    full_data <- io_counts
    colnames(full_data) <- c("outcome","int","aid_count")
    full_data[] <- lapply(full_data,function(x){replace(x,x == 0,NA)})
    full_data <- as.data.frame(full_data)
  })

  output$heatmap <- renderPlot({
    DATA <- as.data.frame(final_matrix())
    if (input$expand == FALSE){
      ggplot(DATA, aes(y=outcome,x=int,label=aid_count)) +
        geom_tile(aes(fill=aid_count),colour="black") +
        theme(axis.text.x = element_text(family="Helvetica",angle=45,hjust=1,size=16),axis.text.y=element_text(family="Helvetica",size=16),axis.title.x=element_text(family="Helvetica",size=20,vjust=-2),axis.title.y=element_text(family="Helvetica",size=20,vjust=-2)) +
        scale_fill_gradient2(name="No. of unique articles",midpoint=(max(DATA$aid_count,na.rm=TRUE)/2),low="#c7e9c0",mid="#41ab5d",high="#00441b",na.value="white") +
        coord_fixed(ratio=5/6) +
        geom_text(size=5,family="Helvetica") +
        scale_x_discrete(name="IUCN conservation intervention types",limits=c("area_protect", "land_wat_mgmt", "res_mgmt", "species_mgmt", "education", "law_policy", "liv_eco_inc", "ext_cap_build", "sus_use", "other"),labels=c("Area protection", "Land/Water management", "Resource management", "Species management", "Education", "Law & Policy", "Livelihood, economic & other incentives", "External capacity building", "Sustainable use", "Other")) +
        scale_y_discrete(name="Human well-being outcomes",limits=c("other","free_choice","culture","sub_well","gov","sec_saf","soc_rel","education","health","mat_liv_std","eco_liv_std"),labels=c("Other","Freedom of choice/action","Cultural & spiritual","Subjective well-being","Governance & empowerment","Security & safety","Social relations","Education","Health","Material living standards","Economic living standards"))
    } else if (input$expand == TRUE){
      ggplot(DATA, aes(y=outcome,x=int,label=aid_count)) +
        geom_tile(aes(fill=aid_count),colour="black") +
        theme(axis.text.x = element_text(family="Helvetica",angle=45,hjust=1,size=14),axis.text.y=element_text(family="Helvetica",size=14),axis.title.x=element_text(family="Helvetica",size=20,vjust=-2),axis.title.y=element_text(family="Helvetica",size=20,vjust=-2)) +
        scale_fill_gradient2(name="No. of unique articles",midpoint=(max(DATA$aid_count,na.rm=TRUE)/2),low="#c7e9c0",mid="#41ab5d",high="#00441b",na.value="white") +
        coord_fixed(ratio=5/6) +
        geom_text(size=4,family="Helvetica",color="gray") +
        scale_x_discrete(name="IUCN conservation intervention sub-types",limits=c("area_protect", "area_mgmt", "res_mgmt", "sp_control", "restoration", "sp_mgmt", "sp_recov", "sp_reint", "ex_situ", "form_ed", "training", "aware_comm", "legis", "pol_reg", "priv_codes", "compl_enfor", "liv_alt", "sub", "market", "non_mon", "inst_civ_dev", "part_dev", "cons_fin", "sus_use", "other"),labels=c("Area protection", "Area management", "Resource management/protection", "Species control", "Restoration", "Species management", "Species recovery", "Species reintroduction", "Ex-situ conservation", "Formal education", "Training", "Awareness & Communications", "Legislation", "Policies & Regulations", "Private sector standards and codes", "Compliance & enforcement", "Enterprises & livelihood alternatives", "Substitution", "Market-based forces", "Non-monetary values", "Institutional & civil society development", "Alliance & partnership development", "Conservation finance", "Sustainable use", "Other")) +
        scale_y_discrete(name="Human well-being outcomes",limits=c("other","free_choice","culture","sub_well","gov","sec_saf","soc_rel","education","health","mat_liv_std","eco_liv_std"),labels=c("Other","Freedom of choice/action","Cultural & spiritual","Subjective well-being","Governance & empowerment","Security & safety","Social relations","Education","Health","Material living standards","Economic living standards"))
    }
  })
  
  output$map_data <- DT::renderDataTable({
    data <- dataInputB()
    data <- data %>% select(-Title.formatted,-Fullcitation,-Pub_type,-Authors,-Title,-FullText,-Pub_year,-DOI,-Journal,-total_countries) %>% distinct()
    colnames(data) <- c("Article ID","Intervention sub-type", "HWB Outcome type", "Intervention type","Study country","Study region","Study subregion","Biome of study","Impact evaluation?","Major habitat type","Does the study conduct comparisons?","Type of comparisons","Comparisons over time","Qualitative data only","Assigned groups to interventions","Control","Study design type")
    DT::datatable(data)
  })

  biblio_maptab <- reactive({
    data <- dataInputB()
    bib <- data %>% select(aid,Biome.,Pub_type,Authors,Title,Pub_year,DOI,Journal,FullText) %>% distinct()
    colnames(bib) <- c("Article ID", "Study_country","Publication type","Authors","Title","Year of publication","DOI","Journal","Open Access")
  })
  
  output$downloadFullData <- downloadHandler(
    filename = function() {
      paste(input$region,"_",input$biome,"_dataset.csv",sep="")
    },
    content = function(file) {
      write.csv(dataInputB(), file)
    }
  )
  
  output$downloadBiblio <- downloadHandler(
    filename = function() {
      paste(input$region,"_",input$biome,"_biblio.csv",sep="")
    },
    content = function(file) {
      write.csv(biblio_maptab(), file)
    }
  )
  
  observeEvent(input$submit, {
    output$download_opts1 <- renderUI({
      downloadButton("downloadBiblio", "Bibliography as .csv")
    })
  })
  
  observeEvent(input$submit, {
    output$download_opts2 <- renderUI({
      downloadButton("downloadFullData", "Data as .csv")
    })
  })
  
  us1 <- reactive({
    n <- n_distinct(dataInputB()$aid)
  })
  
  ie1 <- reactive({
    data <- select(dataInputB(),aid,IE)
    IE <- filter(data,IE == "Y")
    IE <- distinct(IE)
    n <- n_distinct(IE$aid)
  })
  
  oa1 <- reactive({
    data <- select(dataInputB(),aid,FullText)
    FT <- filter(data, FullText == "Y")
    FT <- distinct(FT)
    n <- n_distinct(FT$aid)
  })
  
  output$elink_us <- renderText({
    as.character(us1())
  })
  
  output$elink_ie <- renderText({
    as.character(ie1())
  })
  
  output$elink_oa <- renderText({
    as.character(oa1())
  })
})

eglob = function(env_serv) with (env_serv,{
  dataInputC <- reactive({
    dataC <- map_data_final
    if (input$intervention_c == "all" & input$outcome_c == "all" & input$biome_filter == "ALL"){
      dataC <- dataC
    } else if (input$intervention_c != "all" & input$outcome_c == "all" & input$biome_filter == "ALL"){
      dataC <- dataC[dataC$Int_type == input$intervention_c,]
    } else if (input$intervention_c == "all" & input$outcome_c != "all" & input$biome_filter == "ALL"){
      dataC <- dataC[dataC$Outcome == input$outcome_c,]
    } else if (input$intervention_c == "all" & input$outcome_c == "all" & input$biome_filter != "ALL"){
      dataC <- dataC[dataC$biome_group == input$biome_filter,]
    } else if (input$intervention_c != "all" & input$outcome_c != "all" & input$biome_filter == "ALL"){
      dataC <- dataC[dataC$Int_type == input$intervention_c,]
      dataC <- dataC[dataC$Outcome == input$outcome_c,]
    } else if (input$intervention_c != "all" & input$outcome_c == "all" & input$biome_filter != "ALL"){
      dataC <- dataC[dataC$Int_type == input$intervention_c,]
      dataC <- dataC[dataC$biome_group == input$biome_filter,]
    } else if (input$intervention_c == "all" & input$outcome_c != "all" & input$biome_filter != "ALL"){
      dataC <- dataC[dataC$Outcome == input$outcome_c,]
      dataC <- dataC[dataC$biome_group == input$biome_filter,]
    } else if (input$intervention_c != "all" & input$outcome_c != "all" & input$biome_filter != "ALL"){
      dataC <- dataC[dataC$Outcome == input$outcome_c,]
      dataC <- dataC[dataC$Int_type == input$intervention_c,]
      dataC <- dataC[dataC$biome_group == input$biome_filter,]
    }
    distinct(dataC)
  })
  
  country_dat <- reactive({
    country_count <- matrix(nrow=nrow(regions), ncol=2)
    rownames(country_count) <- regions$COUNTRY
    colnames(country_count) <- c("COUNTRY", "COUNTS")
    
    for (c in regions$COUNTRY){
      subset <- filter(dataInputC(), Study_country == c)
      country_count[c,1] <- c
      country_count[c,2] <- as.numeric(n_distinct(subset$aid))
    }
    
    rownames(country_count) = NULL
    country_count <- as.data.frame(country_count)
    country_count <- inner_join(country_count,regions,by="COUNTRY")
    country_count <- filter(country_count,CODE != "")
    country_count <- as.data.frame(country_count)
  })

#===================
# Plotting interactive map with plotly
#===================

output$map_plotly <- renderPlotly({
  country_count <- as.data.frame(country_dat())
  country_count$COUNTS <- as.numeric(as.vector(country_count$COUNTS))
  country_count <- bind_rows(country_count, oecd)
  
  l <- list(color = toRGB("grey"), width = 0.5)
  g <- list(
    showframe = FALSE,
    resolution=50,
    projection = list(type = 'Mercator'),
    showcoastlines = T,
    showcountries = T,
    countrycolor = toRGB("white"),
    coastlinecolor = toRGB("white")
  )
  
  col <- colorRampPalette(c("white", "#74c476","#41ab5d", "#238b45", "#006d2c", "#00441b")) (200)
  
  p <- plot_ly(country_count, z=COUNTS, type = 'choropleth', source="poop", locations = CODE, 
          text = COUNTRY, color = COUNTS, colors=col, marker=list(line=l), 
          colorbar = list(title="No. of unique articles"))
  
  layout(p, geo = g)
})

country_click <- reactive({
  oecd <- select(oecd,-COUNTS)
  all <- bind_rows(regions,oecd)
  colnames(all) <- c("COUNTRY","REGION","CODE","SUBREGION","POINT")
  
  d <- plotly::event_data("plotly_click", source="poop")
  if (is.null(d)){
    country <- as.data.frame("none")
  } else country <- filter(all,POINT == as.integer(d[2]))
  country <- as.character(country[1,1])
  return(country)
})
  
  biblio_countrytab <- reactive({
    data <- dataInputC()
    data <- data %>% select(aid) %>% distinct()
    bib <- left_join(data,map_data_final,by="aid")
    bib <- bib %>% select(aid,Study_country,biome_group,Pub_type,Authors,Title,Pub_year,DOI,Journal,FullText) %>% distinct()
    colnames(bib) <- c("Article ID", "Publication type","Authors","Title","Year of publication","DOI","Journal","Open Access")
  })

  output$biblio_glob <- DT::renderDataTable({
    data <- as.data.frame(dataInputC())
    click <- as.character(country_click())
    if (click != "none"){
      sub <- filter(data,Study_country == click)
      bib <- sub %>% select(aid,Study_country,Pub_type,Authors,Title,Pub_year,DOI,Journal,FullText) %>% distinct()
      bib$DOI <- paste0("<a href='",bib$DOI,"' target='_blank'>",bib$DOI,"</a>")
      colnames(bib) <- c("Article ID", "Study_country","Publication type","Authors","Title","Year of publication","DOI","Journal","Open Access")
      DT::datatable(distinct(bib),escape=FALSE)
    } else if (click == "none"){
      bib <- matrix(nrow=1,ncol=9)
      colnames(bib) <- c("Article ID", "Study_country","Publication type","Authors","Title","Year of publication","DOI","Journal","Open Access")
      DT::datatable(distinct(as.data.frame(bib),escape=FALSE))
    }
  })

  output$downloadFullDataC <- downloadHandler(
    filename = function() {
      paste(input$intervention_c,"_",input$outcome_c,"_",input$biome_filter,"_dataset.csv",sep="")
    },
    content = function(file) {
      write.csv(dataInputC(), file)
    }
  )
  
  output$downloadBiblioC <- downloadHandler(
    filename = function() {
      paste(input$intervention_c,"_",input$outcome_c,"_",input$biome_filter,"_biblio.csv",sep="")
    },
    content = function(file) {
      write.csv(biblio_countrytab(), file)
    }
  )

observeEvent(input$submit, {
  output$download_opts3 <- renderUI({
    downloadButton("downloadBiblioC", "Bibliography as .csv")
  })
})

observeEvent(input$submit, {
  output$download_opts4 <- renderUI({
    downloadButton("downloadFullDataC", "Data as .csv")
  })
})
})

eregi = function(env_serv) with (env_serv,{

  ##==========
  ## Converting to appropriate classes
  ##==========
  regions$COUNTRY <- as.character(as.vector(regions$COUNTRY))
  regions$REGION <- as.character(as.vector(regions$REGION))
  regions$CODE <- as.character(as.vector(regions$CODE))
  regions$SUBREGION <- as.character(as.vector(regions$SUBREGION))
  
  ##==========
  ## Setting region parameters
  ##==========
  observe({
    eregi_subreg <- if (input$eregi_region == "All") character(0) else{
      filter(regions, REGION == input$eregi_region) %>%
        select(SUBREGION) %>%
        unique()
    }
    stillSelected <- isolate(input$eregi_subreg[input$eregi_subreg %in% eregi_subreg])
    rownames(stillSelected) <- NULL
    rownames(eregi_subreg) <- NULL
    updateSelectInput(session,"eregi_subreg", choices=as.vector(eregi_subreg), selected=stillSelected)
  })
  
  observe({
    eregi_country <- if (input$eregi_subreg == "All") character(0) else{
        filter(regions, SUBREGION == input$eregi_subreg) %>%
        select(COUNTRY) %>%
        unique()
    }
    
    stillSelected <- isolate(input$eregi_country[input$eregi_country %in% eregi_country])
    rownames(stillSelected) <- NULL
    rownames(eregi_country) <- NULL
    updateSelectInput(session, "eregi_country",choice=as.vector(eregi_country), selected=stillSelected)
  })
    
  ##==========
  ## Creating reactive data table
  ##==========
  
  eregi_data <- reactive({
    data <- map_data_final
    if (input$eregi_region != "All" & input$eregi_subreg == "" & input$eregi_country == "") {
      data <- data[data$region == input$eregi_region,]
    } else if (input$eregi_region != "All" & input$eregi_subreg != "" & input$eregi_country == "") {
      data <- data[data$subregion == input$eregi_subreg,]
    } else if (input$eregi_region != "All" & input$eregi_subreg != "" & input$eregi_country != "") {
      data <- data[data$Study_country == input$eregi_country,]
    }
    distinct(data)
    
  })

  output$eregi_table <- DT::renderDataTable({
    data <- as.data.frame(eregi_data())
    data <- data %>% select(aid,IE,region,subregion,Study_country,Biome.,biome_group,int_group,Int_type,Outcome)
    data <- data[,c("aid","IE","region","subregion","Study_country","Biome.","biome_group","int_group","Int_type","Outcome")]
    colnames(data) <- c("Article ID", "Impact evaluation?","Region","Subregion","Country","Biome","Biome group","Intervention group type", "Intervention sub-type", "Outcome type")
    DT::datatable(distinct(data))
  })
  
  ##=========
  ## Creating reactive bibliography
  ##=========

eregi_bibl <- reactive({
  data <- as.data.frame(eregi_data())
  data <- data %>% select(aid, Pub_type, Authors, Pub_year, Title, Journal, DOI, FullText, Study_country)
  data <- distinct(data)
  data$DOI <- paste0("<a href='",data$DOI,"' target='_blank'>",data$DOI,"</a>")
  data <- data
  
})

  output$eregi_bib <- DT::renderDataTable({
    data <- as.data.frame(eregi_bibl())
    colnames(data) <- c("Article ID", "Publication type","Author(s)","Publication year","Title","Journal","DOI","Open Access", "Study country")
    DT::datatable(distinct(data),escape=FALSE)
  })
  
##==============
## Download buttons
##==============

output$downloadFullData_R <- downloadHandler(
  filename = function() {
    paste(input$eregi_reg,"_",input$eregi_subreg,"_",input$eregi_country,"_dataset.csv",sep="")
  },
  content = function(file) {
    write.csv(eregi_data(), file)
  }
)

output$downloadBiblio_R <- downloadHandler(
  filename = function() {
    paste(input$eregi_reg,"_",input$eregi_subreg,"_",input$eregi_country,"_biblio.csv",sep="")
  },
  content = function(file) {
    write.csv(eregi_bibl(), file)
  }
)

observeEvent(input$submit, {
  output$download_opts5 <- renderUI({
    downloadButton("downloadBiblio_R", "Bibliography as .csv")
  })
})

observeEvent(input$submit, {
  output$download_opts6 <- renderUI({
    downloadButton("downloadFullData_R", "Data as .csv")
  })
})

  ##========
  ## Generating data for interactive map
  ##========
  
  eregi_vars <- reactive({
    if (input$eregi_map_type == "Int_type"){
      int_type
    } else if (input$eregi_map_type == "int_group"){
      group_type
    } else if (input$eregi_map_type == "Outcome"){
      out_type
    } else if (input$eregi_map_type == "Biome."){
      biome_type
    } else if (input$eregi_map_type == "biome_group"){
      biome_group
    }
  })
  
#   eregi_labels <- reactive({
#     if (input$eregi_map_type == "Int_type"){
#       int_labels
#     } else if (input$eregi_map_type == "int_group"){
#       group_labels
#     } else if (input$eregi_map_type == "Outcome"){
#       out_labels
#     } else if (input$eregi_map_type == "Biome."){
#       biome_labels
#     } else if (input$eregi_map_type == "biome_group"){
#       bg_labels
#     }
#   })
  
  eregi_dat2 <- reactive({
    data <- map_data_final
    if (input$eregi_region != "All" & input$eregi_subreg == "" & input$eregi_country == "") {
      data <- data[data$region == input$eregi_region,]
    } else if (input$eregi_region != "All" & input$eregi_subreg != "" & input$eregi_country == "") {
      data <- data[data$subregion == input$eregi_subreg,]
    } else if (input$eregi_region != "All" & input$eregi_subreg != "" & input$eregi_country != "") {
      data <- data[data$subregion == input$eregi_subreg,]
    }
    data <- distinct(data)
  })
  
  eregi_sum <- reactive({
    
    dat <- as.data.frame(eregi_dat2())
  
    types <- as.vector(eregi_vars())
    
    if (input$eregi_map_type == "Int_type"){
      dat <- dat %>% select(aid,Study_country,Int_type) %>% distinct()
    } else if (input$eregi_map_type == "int_group"){
      dat <- dat %>% select(aid,Study_country,int_group) %>% distinct()
    } else if (input$eregi_map_type == "Outcome"){
      dat <- dat %>% select(aid,Study_country,Outcome) %>% distinct()
    } else if (input$eregi_map_type == "Biome."){
      dat <- dat %>% select(aid,Study_country,Biome.) %>% distinct()
    } else if (input$eregi_map_type == "biome_group"){
      dat <- dat %>% select(aid,Study_country,biome_group) %>% distinct()
    }
    
    dat <- dat[complete.cases(dat),]
    
    colnames(dat) <- c("aid","Study_country","Variable")
    l <- as.list(dat$Study_country)
    l <- unique(l)
    
    n <- length(l)
    summary <- matrix(nrow=n,ncol=(length(types)+2))
    rownames(summary) <- l
    colnames(summary) <- c(types,"Country","Code")
    for (c in l){
      for (o in types){
        subset <- filter(dat, Study_country == c, Variable == o)
        test <- filter(regions, COUNTRY == c)
        code <- as.character(as.vector(test$CODE))
        summary[c,"Country"] <- c
        summary[c,"Code"] <- code
        summary[c,o] <- as.numeric(n_distinct(subset$aid))
      }
    }
    
    summary <- as.data.frame(summary)
    summary <- gather(summary,types,counts,1:(ncol(summary)-2))
    summary$counts <- as.numeric(as.vector(summary$counts))
    summary <- summary %>% group_by(Country) %>% arrange(desc(counts))
  })

eregi_map <- reactive({
  dat <- as.data.frame(eregi_sum())
  top <- dat %>% group_by(Country) %>% slice(1:5)
})
  
eregi_art <- reactive({
  dat <- as.data.frame(eregi_dat2())
  dat <- select(dat,aid,Study_country)
  
  allcountries <- filter(reg, Subregion == input$eregi_subreg)
  oecdcountries <- filter(oecd,SUBREGION == input$eregi_subreg)
  oecdcountries <- select(oecdcountries,COUNTS,CODE,COUNTRY)
  colnames(oecdcountries) <- c("n","Code","country")
  allc <- as.list(as.character(allcountries$Country))
  nc <- length(allc)
  
  articles <- matrix(nrow=nc,ncol=(2))
  rownames(articles) <- allc
  colnames(articles) <- c("n","Code")
  
  for (c in allc){
    subset <- filter(dat,Study_country == c)
    subset <- distinct(subset)
    test <- filter(allcountries, Country == c)
    code <- as.character(as.vector(test$code))
    articles[c,1] <- as.numeric(n_distinct(subset$aid))
    articles[c,2] <- code
  }
  articles <- as.data.frame(articles)
  articles$n <- as.numeric(as.vector(articles$n))
  articles$country <- rownames(articles)
  rownames(articles) <- NULL
  articles <- bind_rows(articles,oecdcountries)
})    

  ##========
  ## Set parameters for mapping
  ##========
  
  g <- list(
    showframe = T,
    showland = T,
    landcolor = toRGB("grey90")
  )
  
  g1 <- reactive({
    coord <- filter(coordinates, Location == input$eregi_subreg)
    
    g1 <- c(g,
    resolution = 50,
    showcoastlines = T,
    countrycolor = toRGB("white"),
    coastlinecolor = toRGB("white"),
    projection = list(type = 'Mercator'),
    list(lonaxis = list(range = c(coord[1,4],coord[1,5]))),
    list(lataxis = list(range = c(coord[1,2],coord[1,3])))
    )
    
    g1
    })
  
  col <- colorRampPalette(c("white", "#74c476","#41ab5d", "#238b45", "#006d2c", "#00441b")) (200)

col2 <- reactive({
  top <- as.data.frame(eregi_map())
  col2 <- RColorBrewer::brewer.pal(n_distinct(top$types),"Paired")
})
  
  
  ##========
  ## Map plotly output
  ##========
  
  output$regi_map <- renderPlotly({
    dat <- as.data.frame(eregi_map())
    art <- as.data.frame(eregi_art())
    
    plot_ly(art, type = 'choropleth', locations = Code, z = n, colors = col, source="poop2",text=paste(country,":<br>",n,"total articles"), showscale = F, geo = 'geo') %>%
      add_trace(type="scattergeo",locations=Code, text = paste(Country,":<br>",counts, "articles"), color = types, colors = col2(), marker = list(size = counts*2),data=dat, geo="geo") %>%
      layout(title = 'Source: <a href="https://environmentalevidencejournal.biomedcentral.com/articles/10.1186/2047-2382-3-16">Bottrill et al. (in press)</a>', geo = g1())
  })

##=========
## Info box summaries
##=========
us2 <- reactive({
  n <- n_distinct(eregi_data()$aid)
})

ie2 <- reactive({
  data <- select(eregi_data(),aid,IE)
  IE <- filter(data,IE == "Y")
  IE <- distinct(IE)
  n <- n_distinct(IE$aid)
})

oa2 <- reactive({
  data <- select(eregi_data(),aid,FullText)
  FT <- filter(data, FullText == "Y")
  FT <- distinct(FT)
  n <- n_distinct(FT$aid)
})

output$elink_us_2 <- renderText({
  as.character(us2())
})

output$elink_ie_2 <- renderText({
  as.character(ie2())
})

output$elink_oa_2 <- renderText({
  as.character(oa2())
})

##========
## Data summary plots
##========

output$regi_int <- renderPlotly({
  dat <- as.data.frame(eregi_data())
  
  if (input$show_subtypes == FALSE){
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
  
  if (input$show_subtypes == FALSE){
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

output$regi_out <- renderPlotly({
  dat <- as.data.frame(eregi_data())
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

output$regi_bio <- renderPlotly({
  dat <- as.data.frame(eregi_data())
  if (input$show_ecoregions == "ALL"){
    dat <- dat %>% select(aid,biome_group) %>% distinct()
    colnames(dat) <- c("aid","var")
  } else {
    dat <- dat %>% filter(biome_group == input$show_ecoregions) %>% select(aid,Biome.) %>% distinct()
    colnames(dat) <- c("aid","var")
  }
  
  if (input$show_ecoregions == "ALL"){
    i_counts = matrix(nrow=7, ncol=2)
    rownames(i_counts) <- biome_group
    var_type <- biome_group
    i_counts <- as.data.frame(i_counts)
    i_counts$labels <- bg_labels
  } else if (input$show_ecoregions == "MAR"){
    i_counts = matrix(nrow=length(marine_type), ncol=2)
    rownames(i_counts) <- marine_type
    var_type <- marine_type
    i_counts <- as.data.frame(i_counts)
    i_counts$labels <- marine_labels
  } else if (input$show_ecoregions == "FOR"){
    i_counts = matrix(nrow=length(forest_type), ncol=2)
    rownames(i_counts) <- forest_type
    var_type <- forest_type
    i_counts <- as.data.frame(i_counts)
    i_counts$labels <- forest_labels
  } else if (input$show_ecoregions == "GRS"){
    i_counts = matrix(nrow=length(grass_type), ncol=2)
    rownames(i_counts) <- grass_type
    var_type <- grass_type
    i_counts <- as.data.frame(i_counts)
    i_counts$labels <- grassland_labels
  } else if (input$show_ecoregions == "TUN"){
    i_counts = matrix(nrow=length(tundra_type), ncol=2)
    rownames(i_counts) <- tundra_type
    var_type <- tundra_type
    i_counts <- as.data.frame(i_counts)
    i_counts$labels <- tundra_labels
  } else if (input$show_ecoregions == "DES"){
    i_counts = matrix(nrow=length(desert_type), ncol=2)
    rownames(i_counts) <- desert_type
    var_type <- desert_type
    i_counts <- as.data.frame(i_counts)
    i_counts$labels <- desert_labels
  } else if (input$show_ecoregions == "MAN"){
    i_counts = matrix(nrow=length(mangrove_type), ncol=2)
    rownames(i_counts) <- mangrove_type
    var_type <- mangrove_type
    i_counts <- as.data.frame(i_counts)
    i_counts$labels <- mangrove_labels
  } else if (input$show_ecoregions == "FRW"){
    i_counts = matrix(nrow=length(fresh_type), ncol=2)
    rownames(i_counts) <- fresh_type
    var_type <- fresh_type
    i_counts <- as.data.frame(i_counts)
    i_counts$labels <- freshwater_labels
  } 
  
  colnames(i_counts) <- c("var","count","labels")
  
  for (i in var_type){
    subset <- filter(dat, var == i)
    i_counts[i,1] <- i
    i_counts[i,2] <- n_distinct(subset$aid)
  }
  
  i_counts <- as.data.frame(i_counts)
  rownames(i_counts) <- NULL
  i_counts$count <- as.numeric(as.vector(i_counts$count))
  
  p <- ggplot(data=i_counts, aes(x=labels,y=count)) +
    geom_bar(stat="identity",fill="turquoise") +
    theme(axis.text.x=element_text(angle=45,hjust=1,size=10),axis.title.x = element_text(vjust=-3,size=12),axis.title.y = element_text(size=12),axis.text.y=element_text(size=10),plot.margin=unit(c(1,1,1,2),"cm")) +
    ylab("Number of articles") +
    xlab("Biome/ecoregion type")
  
  m=list(l=120,
         b=280,
         t=25,
         pad=4
  )
  
  ggplotly(p) %>% layout(margin=m)
})

output$regi_study <- renderPlotly({
  dat <- as.data.frame(eregi_data())
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

# etime = function(env_serv) with (env_serv,{
#   
#   t_dat <- reactive({
#     dat <- map_data_final %>% filter(Pub_year <= input$year1) %>% select(aid,Pub_year,Study_country,IE,Int_type,int_group,biome_group,Outcome) %>% distinct()
#   })
#   
#   l1 <- list(color = toRGB("grey"), width = 0.5)
#   g_1 <- list(
#     showframe = FALSE,
#     projection = list(type = 'Mercator'),
#     showland = TRUE,
#     landcolor = toRGB("grey85"),
#     showcoastlines = F,
#     showcountries = T,
#     countrycolor = toRGB("white"),
#     coastlinecolor = toRGB("white")
#   )
#     
#   col1 <- colorRampPalette(c("white", "#74c476","#41ab5d", "#238b45", "#006d2c", "#00441b")) (200)
#   
#   map_t_dat <- reactive({
#     dat <- t_dat()
#     dat <- dat %>% select(aid,Study_country) %>% distinct()
#     
#     country_count <- matrix(nrow=nrow(regions), ncol=2)
#     rownames(country_count) <- regions$COUNTRY
#     colnames(country_count) <- c("COUNTRY", "COUNTS")
#     
#     for (c in regions$COUNTRY){
#       subset <- filter(dat, Study_country == c)
#       country_count[c,1] <- c
#       country_count[c,2] <- as.numeric(n_distinct(subset$aid))
#     }
#     
#     rownames(country_count) = NULL
#     country_count <- as.data.frame(country_count)
#     country_count <- inner_join(country_count,regions,by="COUNTRY")
#     country_count <- filter(country_count,CODE != "")
#     country_count <- as.data.frame(country_count)
#     
#     country_count$COUNTS <- as.numeric(as.vector(country_count$COUNTS))
#     country_count <- bind_rows(country_count, oecd)
#   })
#   
#   output$timemap <- renderPlotly({
#     dat <- as.data.frame(map_t_dat())
#     
#     p <- plot_ly(dat, locations = CODE, z=COUNTS, type = 'scattergeo',
#                  colors = col1,text=COUNTRY, marker=list(size=COUNTS*1.5,line=l1)
#                  ) 
#     layout(p, geo = g_1)
#   
#   })
#   
#   link_t_dat <- reactive({
#     dat <- map_data_final %>% filter(Pub_year <= input$year2) %>% select(aid,Pub_year,int_group,Outcome) %>% distinct()
#     io_counts = matrix(nrow=11, ncol=10)
#     rownames(io_counts) <- out_type
#     colnames(io_counts) <- group_type
#     
#     for (i in group_type){
#       for (j in out_type){
#         subset <- filter(dat, Outcome == j, int_group == i)
#         io_counts[j,i] <- n_distinct(subset$aid)
#       }
#     }
#     io_counts <- as.data.frame(io_counts)
#     io_counts$outcome <- rownames(io_counts)
#     io_counts <- gather(io_counts,int,counts,1:(ncol(io_counts)-1)) 
#     io_counts <- as.data.frame(io_counts)
#     
#     full_data <- io_counts
#     colnames(full_data) <- c("outcome","int","aid_count")
#     full_data[] <- lapply(full_data,function(x){replace(x,x == 0,NA)})
#     full_data <- as.data.frame(full_data)
#   })
#   
#   output$timematrix <- renderPlot({
#     dat <- as.data.frame(link_t_dat())
#     ggplot(dat, aes(y=outcome,x=int,label=aid_count)) +
#       geom_tile(aes(fill=aid_count),colour="black") +
#       theme(axis.text.x = element_text(family="Helvetica",angle=45,hjust=1,size=16),axis.text.y=element_text(family="Helvetica",size=16),axis.title.x=element_text(family="Helvetica",size=20,vjust=-2),axis.title.y=element_text(family="Helvetica",size=20,vjust=-2)) +
#       scale_fill_gradient2(name="Number of studies",midpoint=(max(dat$aid_count,na.rm=TRUE)/2),low="#c7e9c0",mid="#41ab5d",high="#00441b",na.value="white") +
#       coord_fixed(ratio=5/6) +
#       geom_text(size=5,family="Helvetica") +
#       scale_x_discrete(name="IUCN conservation intervention types",limits=c("area_protect", "land_wat_mgmt", "res_mgmt", "species_mgmt", "education", "law_policy", "liv_eco_inc", "ext_cap_build", "sus_use", "other"),labels=c("Area protection", "Land/Water management", "Resource management", "Species management", "Education", "Law & Policy", "Livelihood, economic & other incentives", "External capacity building", "Sustainable use", "Other")) +
#       scale_y_discrete(name="Human well-being outcomes",limits=c("other","free_choice","culture","sub_well","gov","sec_saf","soc_rel","education","health","mat_liv_std","eco_liv_std"),labels=c("Other","Freedom of choice/action","Cultural & spiritual","Subjective well-being","Governance & empowerment","Security & safety","Social relations","Education","Health","Material living standards","Economic living standards"))
#   })
# })