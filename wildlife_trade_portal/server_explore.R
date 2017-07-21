elink = function(env_serv) with (env_serv, {
  data <- map_data_final
  
  dataInputB <- reactive({
    if (input$region1 != "All"){
      sub <- data[data$region == input$region1,]
    } else 
      sub <- data
    sub <- distinct(sub)
    })
  
  dataInputC <- reactive({
    data <- dataInputB()
    if (input$supplychain != "All"){
      sub <- data[data$Int_supplychain == input$supplychain,]
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
      io_counts = matrix(nrow=length(out_group), ncol=length(int_group))
      rownames(io_counts) <- out_group
      colnames(io_counts) <- int_group
      io_counts <- io_counts
    }
  })
    
  final_matrix <- reactive({
    io_counts <- final_input()
    if (input$expand == FALSE){
      for (i in int_group){
        for (j in out_group){
          subset <- filter(dataInputC(), out_group == j, int_group == i)
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
        scale_x_discrete(name="Types of actions",limits=c("Establish/refine laws & policies", "Enforcement/compliance", "Reduce demand/consumption", "Support livelihoods", "Reduce threats to species"),labels=c("Establish/refine laws & policies", "Enforcement/compliance", "Reduce demand/consumption", "Support livelihoods", "Reduce threats to species")) +
        scale_y_discrete(name="Types of outcomes",limits=c("Behavioural","Biological","Human well-being"),labels=c("Behavioural","Biological","Human well-being"))
    } else if (input$expand == TRUE){
      ggplot(DATA, aes(y=outcome,x=int,label=aid_count)) +
        geom_tile(aes(fill=aid_count),colour="black") +
        theme(axis.text.x = element_text(family="Proxima Nova",angle=45,hjust=1,size=14),axis.text.y=element_text(family="Proxima Nova",size=14),axis.title.x=element_text(family="Proxima Nova",size=20,vjust=-3),axis.title.y=element_text(family="Proxima Nova",size=20,vjust=-4)) +
        labs(x = "Types of actions",y="Types of outcomes") +
        scale_fill_gradient2(name="No. of unique articles",midpoint=(max(DATA$aid_count,na.rm=TRUE)/2),low="#c7e9c0",mid="#41ab5d",high="#00441b",na.value="white") +
        coord_fixed(ratio=5/6) +
        geom_text(size=4,family="Proxima Nova",color="gray") +
        scale_x_discrete(limits=c("laws","policies","detection","prosecution","civil","substitution","awareness","market","disincentive","stewardship","conflict","spat_protect","harvest_reg","culture"),labels=c("Laws, regulations & codes","Policies & regulations","Detection","Prosecution","Civil action","Substitutions","Awareness raising","Market-based incentives","Disincentives for illegal behavior","Incentives for stewardship of wildlife","Decrease human-wildlife conflict","Spatial areas of protection","Regulate harvest","Culturing of species")) +
        scale_y_discrete(name="Types of outcomes",limits=c("Management","Protection","Trade","Behavior change","Population","Species","Economic living standards", "Material living standards","Health","Education","Social relations","Security and safety","Governance and empowerment","Subjective well-being","Culture & spirituality","Freedom of choice & action"),labels=c("Management","Protection","Trade","Behavior change","Population","Species","Economic living standards", "Material living standards","Health","Education","Social relations","Security and safety","Governance and empowerment","Subjective well-being","Culture & spirituality","Freedom of choice & action"))
    }
  })
  
  output$map_data <- DT::renderDataTable({
    data <- distinct(dataInputC())
    data <- select(data,aid,Pub_type,Authors,Pub_year,Title,Journal,Species,Desired_outcome,Actions_taken,int_group,Int_type,Parts,Purpose,Int_supplychain,Study_goal,Study_location,Study_country,subregion,region,Design.control,Comps.type,Design.type,Study_supplychain,Stated_outcomes,out_group,Outcome,Indicators,Outcome.data_type,species_group,species_habitat,link,FullText)
    colnames(data) <- c("Article ID","Publication type","Author(s)","Publication year","Title","Journal","Target species","Desired outcome of intervention","Actions taken","Intervention group","Intervention sub-type","Parts used","Purpose","Part of the supply chain","Goal of study","Location of study","Country of study","Subregion of study","Region of study","Does the study employ a control?","What type of comparators are used?","What is the design of the study?","Where along the supply chain does this study examine?","Stated outcomes measured","Outcome categories","Outcome types","Indicators used","Data type","Type of wildlife","Species habitat","DOI link","Open Access?")
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
  
  oa1 <- reactive({
    data <- select(dataInputC(),aid,FullText)
    FT <- filter(data, FullText == "Y")
    FT <- distinct(FT)
    n <- n_distinct(FT$aid)
  })
  
  output$elink_us <- renderText({
    as.character(us1())
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

  intlabels$Int_type <- as.character(as.vector(intlabels$Int_type))
  intlabels$int_group <- as.character(as.vector(intlabels$int_group))
  
  intlabels$Out_type <- as.character(as.vector(intlabels$Out_type))
  intlabels$out_group <- as.character(as.vector(intlabels$out_group))

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
  
  observe({
    eintout_outtype <- if (input$eintout_outgroup == "All") character(0) else{
      filter(intlabels, out_group == input$eintout_outgroup) %>%
        select(Out_type) %>%
        unique()
    }
    stillSelected <- isolate(input$eintout_outtype[input$eintout_outtype %in% eintout_outtype])
    rownames(stillSelected) <- NULL
    rownames(eintout_outtype) <- NULL
    updateSelectInput(session,"eintout_outtype", choices=as.vector(eintout_outtype), selected=stillSelected)
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
    if (input$eintout_mht != "All") {
      data <- data[data$species_habitat == input$eintout_mht,]
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
    if (input$eintout_outgroup != "All" & input$eintout_outtype == "") {
      data <- data[data$out_group == input$eintout_outgroup,]
    } else if (input$eintout_outgroup != "All" & input$eintout_outtype != "") {
      data <- data[data$Outcome == input$eintout_outtype,]
    } else
      data <- data
    distinct(data)
  })

  output$e_table <- DT::renderDataTable({
    data <- distinct(dataInputF())
    data <- select(data,aid,Pub_type,Authors,Pub_year,Title,Journal,Species,Desired_outcome,Actions_taken,int_group,Int_type,Parts,Purpose,Int_supplychain,Study_goal,Study_location,Study_country,subregion,region,Design.control,Comps.type,Design.type,Study_supplychain,Stated_outcomes,out_group,Outcome,Indicators,Outcome.data_type,species_group,species_habitat,link,FullText)
    colnames(data) <- c("Article ID","Publication type","Author(s)","Publication year","Title","Journal","Target species","Desired outcome of intervention","Actions taken","Intervention group","Intervention sub-type","Parts used","Purpose","Part of the supply chain","Goal of study","Location of study","Country of study","Subregion of study","Region of study","Does the study employ a control?","What type of comparators are used?","What is the design of the study?","Where along the supply chain does this study examine?","Stated outcomes measured","Outcome categories","Outcome types","Indicators used","Data type","Type of wildlife","Species habitat","DOI link","Open Access?")
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
  
  oa2 <- reactive({
    data <- select(dataInputF(),aid,FullText)
    FT <- filter(data, FullText == "Y")
    FT <- distinct(FT)
    n <- n_distinct(FT$aid)
  })

  output$elink_us_2 <- renderText({
    as.character(us2())
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
      is_counts = matrix(nrow=length(int_group), ncol=2)
      rownames(is_counts) <- int_group
      var_type <- int_group
      for (i in var_type){
        subset <- filter(dat, int_group == i)
        is_counts[i,1] <- i
        is_counts[i,2] <- n_distinct(subset$aid)
      }
    } else if (input$show_subtypes == TRUE){
      is_counts = matrix(nrow=length(int_labels), ncol=2)
      rownames(is_counts) <- int_labels
      var_type <- int_labels
      for (i in var_type){
        subset <- filter(dat, int_label == i)
        is_counts[i,1] <- i
        is_counts[i,2] <- n_distinct(subset$aid)
      }
    }
    colnames(is_counts) <- c("var","count")
    is_counts <- as.data.frame(is_counts)
    is_counts$count <- as.numeric(as.vector(is_counts$count))
    if (input$show_subtypes == FALSE){
      is_counts$labels <- c("Enforcement/compliance", "Establish/refine laws & policies",  "Reduce demand/consumption", "Reduce threats to species", "Support livelihoods")
    } else {
      is_counts$labels <- c("Laws, regulations & codes", "Policies & regulations", "Detection", "Prosecution", "Civil action", "Substitutions", "Awareness raising", "Market-based incentives", "Disincentives for illegal behavior", "Incentives for stewardship of wildlife", "Decrease human-wildlife conflict", "Spatial areas of protection", "Regulate harvest", "Culturing of species")
    }

    plot_ly(x=is_counts$labels, y=is_counts$count ,type="bar") %>%
      layout(margin = list(b=200,r=50), xaxis=list(tickangle= 45))
  })

  output$e_out <- renderPlotly({
    dat <- as.data.frame(dataInputF())
    
    if (input$show_subtypes_o == FALSE){
      io_counts = matrix(nrow=length(out_group), ncol=2)
      rownames(io_counts) <- out_group
      var_type <- out_group
      for (i in var_type){
        subset <- filter(dat, out_group == i)
        io_counts[i,1] <- i
        io_counts[i,2] <- n_distinct(subset$aid)
      }
    } else if (input$show_subtypes_o == TRUE){
      io_counts = matrix(nrow=length(out_type), ncol=2)
      rownames(io_counts) <- out_type
      var_type <- out_type
      for (i in var_type){
        subset <- filter(dat, Outcome == i)
        io_counts[i,1] <- i
        io_counts[i,2] <- n_distinct(subset$aid)
      }
    }
    colnames(io_counts) <- c("var","count")
    io_counts <- as.data.frame(io_counts)
    io_counts$count <- as.numeric(as.vector(io_counts$count))
    if (input$show_subtypes_o == FALSE){
      io_counts$labels <- c("Behavioural","Biological","Human well-being")
    } else {
      io_counts$labels <- c("Management","Protection","Trade","Behavior change","Population","Species","Economic living standards","Material living standards","Health","Education","Social relations","Security and saffety"," Governance and empowerment","Subjective well-being","Culture & spirituality","Freedom of choice & action")
    }
    
    plot_ly(x=io_counts$labels, y=io_counts$count ,type="bar") %>%
      layout(margin = list(b=200,r=50), xaxis=list(tickangle= 45))
  })

  output$e_comp <- renderPlotly({
    dat <- as.data.frame(dataInputF())

    ic_counts = matrix(nrow=length(design_type), ncol=2)
    rownames(ic_counts) <- design_type
    var_type <- design_type
    for (i in var_type){
      subset <- filter(dat, Design.type == i)
      ic_counts[i,1] <- i
      ic_counts[i,2] <- n_distinct(subset$aid)
    }

    colnames(ic_counts) <- c("var","count")
    ic_counts <- as.data.frame(ic_counts)
    ic_counts$count <- as.numeric(as.vector(ic_counts$count))
    ic_counts$labels <- c(design_type)

    plot_ly(x=ic_counts$labels, y=ic_counts$count ,type="bar") %>%
      layout(margin = list(b=200,r=50), xaxis=list(tickangle= 45))
  })

  output$e_eco <- renderPlotly({
    dat <- as.data.frame(dataInputF())

    ib_counts = matrix(nrow=length(supply_type), ncol=2)
    rownames(ib_counts) <- supply_type
    var_type <- supply_type

    for (i in var_type){
      subset <- filter(dat, Int_supplychain == i)
      ib_counts[i,1] <- i
      ib_counts[i,2] <- n_distinct(subset$aid)
    }

    colnames(ib_counts) <- c("var","count")
    ib_counts <- as.data.frame(ib_counts)
    ib_counts$count <- as.numeric(as.vector(ib_counts$count))
    ib_counts$labels <- c("Supply","Trade","Consumer")

    plot_ly(x=ib_counts$labels, y=ib_counts$count ,type="bar") %>%
      layout(margin = list(b=200,r=50), xaxis=list(tickangle= 45))
  })
  
  output$e_use <- renderPlotly({
    dat <- as.data.frame(dataInputF())
    
    ib_counts = matrix(nrow=length(purpose_type), ncol=2)
    rownames(ib_counts) <- purpose_type
    var_type <- purpose_type
    
    for (i in var_type){
      subset <- filter(dat, Purpose == i)
      ib_counts[i,1] <- i
      ib_counts[i,2] <- n_distinct(subset$aid)
    }
    
    colnames(ib_counts) <- c("var","count")
    ib_counts <- as.data.frame(ib_counts)
    ib_counts$count <- as.numeric(as.vector(ib_counts$count))
    ib_counts$labels <- c("Food","Fuel","Medicine","Decoration","Pets","Construction","Clothing","Trophy","Other")
    
    plot_ly(x=ib_counts$labels, y=ib_counts$count ,type="bar") %>%
      layout(margin = list(b=200,r=50), xaxis=list(tickangle= 45))
  })
  
  output$e_spec <- renderPlotly({
    dat <- as.data.frame(dataInputF())
    
    spec_counts <- count(dat,species_group)
    colnames(spec_counts) <- c("labels","count")
    
    plot_ly(x=spec_counts$labels, y=spec_counts$count ,type="bar") %>%
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