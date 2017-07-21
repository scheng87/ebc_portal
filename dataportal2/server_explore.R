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
  
  dataInputB <- reactive({
    if (input$region1 != "All"){
      sub <- data[data$region == input$region1,]
    } else 
      sub <- data
    sub <- distinct(sub)
    })
  
  dataInputB_C <- reactive({
    data <- dataInputB()
    if (input$comp != "All"){
      sub <- data[data$study_type == input$comp,]
    } else
      sub <- data
    sub <- distinct(sub)
  })
  
  dataInputC <- reactive({
    data <- dataInputB_C()
    if (input$biome != "All"){
      sub <- data[data$biome_group == input$biome,]
    } else 
      sub <- data
    sub <- distinct(sub)
  })
        
  final_input <- reactive({
    if (input$expand == TRUE){
      io_counts = matrix(nrow=length(out_type), ncol=length(int_type))
      rownames(io_counts) <- out_type
      colnames(io_counts) <- int_type
      io_counts <- io_counts
    } else if (input$expand == FALSE){
      io_counts = matrix(nrow=length(out_type), ncol=length(group_type))
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
          subset <- filter(dataInputC(), Outcome == j, int_group == i)
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
          subset <- filter(dataInputC(), Outcome == j, Int_type == i)
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
        theme(axis.text.x = element_text(family="Proxima Nova",angle=45,hjust=1,size=16),axis.text.y=element_text(family="Proxima Nova",size=16),axis.title.x=element_text(family="Proxima Nova",size=20,vjust=-3),axis.title.y=element_text(family="Proxima Nova",size=20,vjust=-4)) +
        labs(x = "Types of actions",y="Types of outcomes") +
        scale_fill_gradient2(name="No. of unique articles",midpoint=(max(DATA$aid_count,na.rm=TRUE)/2),low="#c7e9c0",mid="#41ab5d",high="#00441b",na.value="white") +
        coord_fixed(ratio=5/6) +
        geom_text(size=5,family="Proxima Nova") +
        scale_x_discrete(name="IUCN conservation intervention types",limits=c("area_protect", "land_wat_mgmt", "res_mgmt", "species_mgmt", "education", "law_policy", "liv_eco_inc", "ext_cap_build", "sus_use", "other"),labels=c("Area protection", "Land/Water management", "Resource management", "Species management", "Education", "Law & Policy", "Livelihood, economic & other incentives", "External capacity building", "Sustainable use", "Other")) +
        scale_y_discrete(name="Human well-being outcomes",limits=c("other","free_choice","culture","sub_well","gov","sec_saf","soc_rel","education","health","mat_liv_std","eco_liv_std"),labels=c("Other","Freedom of choice/action","Cultural & spiritual","Subjective well-being","Governance & empowerment","Security & safety","Social relations","Education","Health","Material living standards","Economic living standards"))
    } else if (input$expand == TRUE){
      ggplot(DATA, aes(y=outcome,x=int,label=aid_count)) +
        geom_tile(aes(fill=aid_count),colour="black") +
        theme(axis.text.x = element_text(family="Proxima Nova",angle=45,hjust=1,size=14),axis.text.y=element_text(family="Proxima Nova",size=14),axis.title.x=element_text(family="Proxima Nova",size=20,vjust=-3),axis.title.y=element_text(family="Proxima Nova",size=20,vjust=-4)) +
        labs(x = "Types of actions",y="Types of outcomes") +
        scale_fill_gradient2(name="No. of unique articles",midpoint=(max(DATA$aid_count,na.rm=TRUE)/2),low="#c7e9c0",mid="#41ab5d",high="#00441b",na.value="white") +
        coord_fixed(ratio=5/6) +
        geom_text(size=4,family="Proxima Nova",color="gray") +
        scale_x_discrete(name="IUCN conservation intervention sub-types",limits=c("area_protect", "area_mgmt", "res_mgmt", "sp_control", "restoration", "sp_mgmt", "sp_recov", "sp_reint", "ex_situ", "form_ed", "training", "aware_comm", "legis", "pol_reg", "priv_codes", "compl_enfor", "liv_alt", "sub", "market", "non_mon", "inst_civ_dev", "part_dev", "cons_fin", "sus_use", "other"),labels=c("Area protection", "Area management", "Resource management/protection", "Species control", "Restoration", "Species management", "Species recovery", "Species reintroduction", "Ex-situ conservation", "Formal education", "Training", "Awareness & Communications", "Legislation", "Policies & Regulations", "Private sector standards and codes", "Compliance & enforcement", "Enterprises & livelihood alternatives", "Substitution", "Market-based forces", "Non-monetary values", "Institutional & civil society development", "Alliance & partnership development", "Conservation finance", "Sustainable use", "Other")) +
        scale_y_discrete(name="Human well-being outcomes",limits=c("other","free_choice","culture","sub_well","gov","sec_saf","soc_rel","education","health","mat_liv_std","eco_liv_std"),labels=c("Other","Freedom of choice/action","Cultural & spiritual","Subjective well-being","Governance & empowerment","Security & safety","Social relations","Education","Health","Material living standards","Economic living standards"))
    }
  })
  
  output$map_data <- DT::renderDataTable({
    data <- distinct(dataInputC())
    data <- select(data,aid,Pub_type,Authors,Pub_year,Title,Journal,int_group,Int_type,Outcome,Study_country,subregion,region,Biome.,biome_group,IE,study_type,Comps,Comps.type,Comps.time,Design.qual_only,Design.assigned,Design.control,link,FullText)
    colnames(data) <- c("Article ID","Publication type","Author(s)","Publication year","Title","Journal","Intervention group","Intervention sub-type","Outcome type","Country of study","Subregion of study","Region of study","Ecoregion of study","Major habitat type of study","Impact evaluation?","Does the study conduct comparisons?","What type of comparators are used?","Does the study conduct comparisons over time?","Does the study only use qualitative data?","Does the study assign groups to treatments?","Does the study employ a control?","DOI Link","Open access?")
    data <- distinct(data)
    DT::datatable(data, escape=FALSE)
  })

  biblio_maptab <- reactive({
    data <- dataInputC()
    bib <- data %>% select(aid,Biome.,Pub_type,Authors,Title,Pub_year,DOI,Journal) %>% distinct()
    colnames(bib) <- c("Article ID", "Biome/ecoregion","Publication type","Authors","Title","Year of publication","DOI","Journal")
    bib <- bib
  })
  
  output$downloadFullData <- downloadHandler(
    filename = function() {
      paste(input$region1,"_",input$biome,"_dataset.csv",sep="")
    },
    content = function(file) {
      write.csv(dataInputB(), file)
    }
  )
  
  output$downloadBiblio <- downloadHandler(
    filename = function() {
      paste(input$region1,"_",input$biome,"_biblio.csv",sep="")
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
    n <- n_distinct(dataInputC()$aid)
  })
  
  ie1 <- reactive({
    data <- select(dataInputC(),aid,IE)
    IE <- filter(data,IE == "Y")
    IE <- distinct(IE)
    n <- n_distinct(IE$aid)
  })
  
  oa1 <- reactive({
    data <- select(dataInputC(),aid,FullText)
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

eintout = function(env_serv) with (env_serv,{
  
  ##==========
  ## Setting dynamic population of drop-down lists
  ##==========
  
  ## Set character classes of inputs
  regions$COUNTRY <- as.character(as.vector(regions$COUNTRY))
  regions$REGION <- as.character(as.vector(regions$REGION))
  regions$CODE <- as.character(as.vector(regions$CODE))
  regions$SUBREGION <- as.character(as.vector(regions$SUBREGION))
  
  biomelabels$mht <- as.character(as.vector(biomelabels$mht))
  biomelabels$ecoregion <- as.character(as.vector(biomelabels$ecoregion))
  
  intlabels$Int_type <- as.character(as.vector(intlabels$Int_type))
  intlabels$int_group <- as.character(as.vector(intlabels$int_group))
  
  ## Set dynamic population
  observe({
    eintout_subreg <- if (input$eintout_region == "All") character(0) else{
      filter(regions, REGION == input$eintout_region) %>%
        select(SUBREGION) %>%
        unique()
    }
    stillSelected <- isolate(input$eintout_subreg[input$eintout_subreg %in% eintout_subreg])
    rownames(stillSelected) <- NULL
    rownames(eintout_subreg) <- NULL
    updateSelectInput(session,"eintout_subreg", choices=as.vector(eintout_subreg), selected=stillSelected)
  })
  
  observe({
    eintout_country <- if (input$eintout_subreg == "All") character(0) else{
      filter(regions, SUBREGION == input$eintout_subreg) %>%
        select(COUNTRY) %>%
        unique()
    }
    
    stillSelected <- isolate(input$eintout_country[input$eintout_country %in% eintout_country])
    rownames(stillSelected) <- NULL
    rownames(eintout_country) <- NULL
    updateSelectInput(session, "eintout_country",choice=as.vector(eintout_country), selected=stillSelected)
  })
  
  observe({
    eintout_ecoreg <- if (input$eintout_mht == "All") character(0) else{
      filter(biomelabels, mht == input$eintout_mht) %>%
        select(ecoregion) %>%
        unique()
    }
    stillSelected <- isolate(input$eintout_ecoreg[input$eintout_ecoreg %in% eintout_ecoreg])
    rownames(stillSelected) <- NULL
    rownames(eintout_ecoreg) <- NULL
    updateSelectInput(session,"eintout_ecoreg", choices=as.vector(eintout_ecoreg), selected=stillSelected)
  })
  
  observe({
    eintout_inttype <- if (input$eintout_intgroup == "All") character(0) else{
      filter(intlabels, int_group == input$eintout_intgroup) %>%
        select(Int_type) %>%
        unique()
    }
    stillSelected <- isolate(input$eintout_inttype[input$eintout_inttype %in% eintout_inttype])
    rownames(stillSelected) <- NULL
    rownames(eintout_inttype) <- NULL
    updateSelectInput(session,"eintout_inttype", choices=as.vector(eintout_inttype), selected=stillSelected)
  })
  
  ##==========
  ## Creating reactive data table
  ##==========
  
  map_data_final <- map_data_final
  
  dataInputA <- reactive({
    data <- map_data_final
    if (input$eintout_region != "All" & input$eintout_subreg == "" & input$eintout_country == "") {
      data <- data[data$region == input$eintout_region,]
    } else if (input$eintout_region != "All" & input$eintout_subreg != "" & input$eintout_country == "") {
      data <- data[data$subregion == input$eintout_subreg,]
    } else if (input$eintout_region != "All" & input$eintout_subreg != "" & input$eintout_country != "") {
      data <- data[data$Study_country == input$eintout_country,]
    } else
      data <- data
    distinct(data)
  })
  
  dataInputD <- reactive({
    data <- as.data.frame(dataInputA())
    if (input$eintout_mht != "All" & input$eintout_ecoreg == "") {
      data <- data[data$biome_group == input$eintout_mht,]
    } else if (input$eintout_region != "All" & input$eintout_ecoreg != "") {
      data <- data[data$biome_label == input$eintout_ecoreg,]
    } else
      data <- data
    distinct(data)
  })
  
  dataInputE <- reactive({
    data <- as.data.frame(dataInputD())
    if (input$eintout_intgroup != "All" & input$eintout_inttype == "") {
      data <- data[data$int_group == input$eintout_intgroup,]
    } else if (input$eintout_intgroup != "All" & input$eintout_inttype != "") {
      data <- data[data$int_labels == input$eintout_inttype,]
    } else
      data <- data
    distinct(data)
  })
  
  dataInputF <- reactive({
    data <- as.data.frame(dataInputE())
    if (input$eintout_out == "All"){
      sub <- data
    } else
      sub <- filter(data, Outcome == input$eintout_out)
    distinct(sub)
  })
  
  output$e_table <- DT::renderDataTable({
    data <- distinct(dataInputF())
    data <- select(data,aid,Pub_type,Authors,Pub_year,Title,Journal,int_group,Int_type,Outcome,Study_country,subregion,region,Biome.,biome_group,IE,study_type,Comps,Comps.type,Comps.time,Design.qual_only,Design.assigned,Design.control,link,FullText)
    colnames(data) <- c("Article ID","Publication type","Author(s)","Publication year","Title","Journal","Intervention group","Intervention sub-type","Outcome type","Country of study","Subregion of study","Region of study","Ecoregion of study","Major habitat type of study","Impact evaluation?","Does the study conduct comparisons?","What type of comparators are used?","Does the study conduct comparisons over time?","Does the study only use qualitative data?","Does the study assign groups to treatments?","Does the study employ a control?","DOI link","Open access?")
    data <- distinct(data)
    DT::datatable(data, escape=FALSE)
  })
  
  ##=========
  ## Creating reactive bibliography
  ##=========
  
  e_bibl <- reactive({
    data <- as.data.frame(dataInputA())
    
    data <- data %>% select(aid,Pub_type,Authors,Title,Pub_year,Journal,DOI,FullText) %>% distinct()
    colnames(data) <- c("Article ID", "Publication type","Authors","Title","Year of publication","Journal","DOI","Open access?")
    data <- distinct(data)
  })
  
  ##==============
  ## Download buttons
  ##==============
  
  output$downloadFullData_R <- downloadHandler(
    filename = function() {
      paste(input$eintout_region,"_",input$eintout_int,"_",input$eintout_out,"_dataset.csv",sep="")
    },
    content = function(file) {
      write.csv(dataInputE(), file)
    }
  )
  
  output$downloadBiblio_R <- downloadHandler(
    filename = function() {
      paste(input$eintout_region,"_",input$eintout_int,"_",input$eintout_out,"_biblio.csv",sep="")
    },
    content = function(file) {
      write.csv(e_bibl(), file)
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
  
  #=========
  # Info box summaries
  #=========
  us2 <- reactive({
    n <- n_distinct(dataInputF()$aid)
  })
  
  ie2 <- reactive({
    data <- select(dataInputF(),aid,IE)
    IE <- filter(data,IE == "Y")
    IE <- distinct(IE)
    n <- n_distinct(IE$aid)
  })
  
  oa2 <- reactive({
    data <- select(dataInputF(),aid,FullText)
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
  
  output$e_int <- renderPlotly({
    dat <- as.data.frame(dataInputF())
    
    if (input$show_subtypes == FALSE){
      is_counts = matrix(nrow=length(group_type), ncol=2)
      rownames(is_counts) <- group_type
      var_type <- group_type
      for (i in var_type){
        subset <- filter(dat, int_group == i)
        is_counts[i,1] <- i
        is_counts[i,2] <- n_distinct(subset$aid)
      }
    } else if (input$show_subtypes == TRUE){
      is_counts = matrix(nrow=length(int_type), ncol=2)
      rownames(is_counts) <- int_type
      var_type <- int_type
      for (i in var_type){
        subset <- filter(dat, Int_type == i)
        is_counts[i,1] <- i
        is_counts[i,2] <- n_distinct(subset$aid)
      }
    }
    colnames(is_counts) <- c("var","count")
    is_counts <- as.data.frame(is_counts)
    is_counts$count <- as.numeric(as.vector(is_counts$count))
    if (input$show_subtypes == FALSE){
      is_counts$labels <- c("Area protection", "Resource management",  "Land/Water management", "Species management", "Education", "Law & Policy", "Livelihood, economic & other incentives", "External capacity building", "Sustainable use", "Other")
    } else {
      is_counts$labels <- c("Area protection", "Area management", "Resource management/protection", "Species control", "Restoration", "Species management", "Species recovery", "Species reintroduction", "Ex-situ conservation", "Formal education", "Training", "Awareness & Communications", "Legislation", "Policies & Regulations", "Private sector standards and codes", "Compliance & enforcement", "Enterprises & livelihood alternatives", "Substitution", "Market-based forces", "Non-monetary values", "Institutional & civil society development", "Alliance & partnership development", "Conservation finance", "Sustainable use", "Other")
    }
    
    plot_ly(x=is_counts$labels, y=is_counts$count ,type="bar") %>%
      layout(margin = list(b=200,r=50), xaxis=list(tickangle= 45))
  })
  
  output$e_out <- renderPlotly({
    dat <- as.data.frame(dataInputF())

    io_counts = matrix(nrow=length(out_type), ncol=2)
    rownames(io_counts) <- out_type
    var_type <- out_type
    for (i in var_type){
      subset <- filter(dat, Outcome == i)
      io_counts[i,1] <- i
      io_counts[i,2] <- n_distinct(subset$aid)
      }

    colnames(io_counts) <- c("var","count")
    io_counts <- as.data.frame(io_counts)
    io_counts$count <- as.numeric(as.vector(io_counts$count))
    io_counts$labels <- c(out_labels)

    plot_ly(x=io_counts$labels, y=io_counts$count ,type="bar") %>%
      layout(margin = list(b=200,r=75), xaxis=list(tickangle= 45))
  })

  output$e_comp <- renderPlotly({
    dat <- as.data.frame(dataInputF())

    ic_counts = matrix(nrow=length(study_types), ncol=2)
    rownames(ic_counts) <- study_types
    var_type <- study_types
    for (i in var_type){
      subset <- filter(dat, study_type == i)
      ic_counts[i,1] <- i
      ic_counts[i,2] <- n_distinct(subset$aid)
    }

    colnames(ic_counts) <- c("var","count")
    ic_counts <- as.data.frame(ic_counts)
    ic_counts$count <- as.numeric(as.vector(ic_counts$count))
    ic_counts$labels <- c(study_labels)

    plot_ly(x=ic_counts$labels, y=ic_counts$count ,type="bar") %>%
      layout(margin = list(b=200,r=50), xaxis=list(tickangle= 45))
  })

  output$e_eco <- renderPlotly({
    dat <- as.data.frame(dataInputF())

    ib_counts = matrix(nrow=length(biome_type), ncol=2)
    rownames(ib_counts) <- biome_type
    var_type <- biome_type

    for (i in var_type){
      subset <- filter(dat, Biome. == i)
      ib_counts[i,1] <- i
      ib_counts[i,2] <- n_distinct(subset$aid)
    }

    colnames(ib_counts) <- c("var","count")
    ib_counts <- as.data.frame(ib_counts)
    ib_counts$count <- as.numeric(as.vector(ib_counts$count))
    ib_counts$labels <- c(biome_labels)

    plot_ly(x=ib_counts$labels, y=ib_counts$count ,type="bar") %>%
      layout(margin = list(b=200,r=50), xaxis=list(tickangle= 45))
  })
  
  ##========
  ## Prepping for mapping
  ##========
  
  country_dat <- reactive({
    dat <- as.data.frame(dataInputF())
    regions <- read.csv("data/country_list_all.csv", head=TRUE, sep=",")
    names(regions) <- c("Study_country", "Region", "Code","Subregion","Point")
    
    ##Count number of studies for all countries and arrange by region
    country_count <- matrix(nrow=nrow(regions), ncol=2)
    rownames(country_count) <- regions$Study_country
    colnames(country_count) <- c("Study_country", "counts")
    #Calculate in for loop and write to blank matrix
    for (c in regions$Study_country){
      subset <- filter(dat, Study_country == c)
      country_count[c,1] <- c
      country_count[c,2] <- as.numeric(n_distinct(subset$aid))
    }
    
    rownames(country_count) = NULL
    country_count <- as.data.frame(country_count)
    country_count <- inner_join(country_count,regions,by="Study_country")
    country_count <- filter(country_count,Code != "")
    country_count <- as.data.frame(country_count)
  })
  
  #===================
  # Plotting interactive map with plotly
  #===================
  
  output$map_plotly <- renderPlotly({
    country_count <- as.data.frame(country_dat())
    country_count$counts <- as.numeric(as.vector(country_count$counts))
    
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
    
    p <- plot_geo(country_count) %>%
      add_trace(z=~counts, locations = ~Code, 
                 text = ~Study_country, color = ~counts, colors=col, marker=list(line=l)
                ) %>%
      colorbar(title="No. of unique articles") %>%
      layout(
        geo = g
      )

  })
  
})

# eglob = function(env_serv) with (env_serv,{
#   data <- map_data_final
#   
#   dataInputX <- reactive({
#     df <- data[data$Study_supplychain %in% input$supplychain_c,]
#     df <- distinct(df)
#   })
#   
#   dataInputY <- reactive({
#     data <- as.data.frame(dataInputX())
#     df <- data[data$Int_type %in% input$intervention_c,]
#     df <- distinct(df)
#   })
#   
#   dataInputZ <- reactive({
#     data <- as.data.frame(dataInputY())
#     df <- data[data$Outcome %in% input$outcome_c,]
#     df <- distinct(df)
#   })
#   
#   country_dat <- reactive({
#     dat <- as.data.frame(dataInputZ())
#     regions <- read.csv("data/country_list_all.csv", head=TRUE, sep=",")
#     names(regions) <- c("Study_country", "Region", "Code","Subregion","Point")
#     
#     ##Count number of studies for all countries and arrange by region
#     country_count <- matrix(nrow=nrow(regions), ncol=2)
#     rownames(country_count) <- regions$Study_country
#     colnames(country_count) <- c("Study_country", "counts")
#     #Calculate in for loop and write to blank matrix
#     for (c in regions$Study_country){
#       subset <- filter(dat, Study_country == c)
#       country_count[c,1] <- c
#       country_count[c,2] <- as.numeric(n_distinct(subset$aid))
#     }
#     
#     rownames(country_count) = NULL
#     country_count <- as.data.frame(country_count)
#     country_count <- inner_join(country_count,regions,by="Study_country")
#     country_count <- filter(country_count,Code != "")
#     country_count <- as.data.frame(country_count)
#   })
# 
# #===================
# # Plotting interactive map with plotly
# #===================
# 
# output$map_plotly <- renderPlotly({
#   country_count <- as.data.frame(country_dat())
#   country_count$counts <- as.numeric(as.vector(country_count$counts))
#   
#   l <- list(color = toRGB("grey"), width = 0.5)
#   g <- list(
#     showframe = FALSE,
#     resolution=50,
#     projection = list(type = 'Mercator'),
#     showcoastlines = T,
#     showcountries = T,
#     countrycolor = toRGB("white"),
#     coastlinecolor = toRGB("white")
#   )
#   
#   col <- colorRampPalette(c("white", "#74c476","#41ab5d", "#238b45", "#006d2c", "#00441b")) (200)
#   
#   p <- plot_ly(country_count, z=counts, type = 'choropleth', locations = Code, 
#           text = Study_country, color = counts, colors=col, marker=list(line=l), 
#           colorbar = list(title="No. of unique articles"))
#   
#   layout(p, geo = g)
# })
# 
# country_click <- reactive({
#   
#   all <- read.csv("data/country_list_all.csv", head=TRUE, sep=",")
#   names(all) <- c("Study_country", "Region", "Code","Subregion","Point")
#    
#   d <- plotly::event_data("plotly_click", source="poop")
#   if (is.null(d)){
#     country <- as.data.frame("none")
#   } else country <- filter(all,Point == as.integer(d[2]))
#   country <- as.character(country[1,1])
#   return(country)
# })
#   
#   biblio_countrytab <- reactive({
#     data <- dataInputZ()
#     data <- data %>% select(aid) %>% distinct()
#     bib <- left_join(data,map_data_final,by="aid")
#     bib <- bib %>% select(aid,Study_country,biome_group,Pub_type,Authors,Title,Pub_year,DOI,Journal,FullText) %>% distinct()
#     colnames(bib) <- c("Article ID", "Publication type","Authors","Title","Year of publication","DOI","Journal","Open Access")
#     bib <- bib
#   })
# 
#   output$biblio_glob <- DT::renderDataTable({
#     data <- as.data.frame(dataInputZ())
#     click <- as.character(country_click())
#     if (click != "none"){
#       sub <- filter(data,Study_country == click)
#       bib <- sub %>% select(aid,Study_country,Pub_type,Authors,Title,Pub_year,DOI,Journal) %>% distinct()
#       colnames(bib) <- c("Article ID", "Study_country","Publication type","Authors","Title","Year of publication","DOI","Journal")
#       DT::datatable(distinct(bib),escape=FALSE)
#     } else if (click == "none"){
#       bib <- matrix(nrow=1,ncol=8)
#       colnames(bib) <- c("Article ID", "Study_country","Publication type","Authors","Title","Year of publication","DOI","Journal")
#       DT::datatable(distinct(as.data.frame(bib),escape=FALSE))
#     }
#   })
# 
#   output$downloadFullDataC <- downloadHandler(
#     filename = function() {
#       paste(input$intervention_c,"_",input$outcome_c,"_",input$supplychain_c,"_dataset.csv",sep="")
#     },
#     content = function(file) {
#       write.csv(dataInputZ(), file)
#     }
#   )
#   
#   output$downloadBiblioC <- downloadHandler(
#     filename = function() {
#       paste(input$intervention_c,"_",input$outcome_c,"_",input$supplychain_c,"_biblio.csv",sep="")
#     },
#     content = function(file) {
#       write.csv(biblio_countrytab(), file)
#     }
#   )
# 
# observeEvent(input$submit, {
#   output$download_opts3 <- renderUI({
#     downloadButton("downloadBiblioC", "Bibliography as .csv")
#   })
# })
# 
# observeEvent(input$submit, {
#   output$download_opts4 <- renderUI({
#     downloadButton("downloadFullDataC", "Data as .csv")
#   })
# })
# })


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
#       theme(axis.text.x = element_text(family="Proxima Nova",angle=45,hjust=1,size=16),axis.text.y=element_text(family="Proxima Nova",size=16),axis.title.x=element_text(family="Proxima Nova",size=20,vjust=-2),axis.title.y=element_text(family="Proxima Nova",size=20,vjust=-2)) +
#       scale_fill_gradient2(name="Number of studies",midpoint=(max(dat$aid_count,na.rm=TRUE)/2),low="#c7e9c0",mid="#41ab5d",high="#00441b",na.value="white") +
#       coord_fixed(ratio=5/6) +
#       geom_text(size=5,family="Proxima Nova") +
#       scale_x_discrete(name="IUCN conservation intervention types",limits=c("area_protect", "land_wat_mgmt", "res_mgmt", "species_mgmt", "education", "law_policy", "liv_eco_inc", "ext_cap_build", "sus_use", "other"),labels=c("Area protection", "Land/Water management", "Resource management", "Species management", "Education", "Law & Policy", "Livelihood, economic & other incentives", "External capacity building", "Sustainable use", "Other")) +
#       scale_y_discrete(name="Human well-being outcomes",limits=c("other","free_choice","culture","sub_well","gov","sec_saf","soc_rel","education","health","mat_liv_std","eco_liv_std"),labels=c("Other","Freedom of choice/action","Cultural & spiritual","Subjective well-being","Governance & empowerment","Security & safety","Social relations","Education","Health","Material living standards","Economic living standards"))
#   })
# })