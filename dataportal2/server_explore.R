elink = function(env_serv) with (env_serv, {
  data <- map_data_final
  
  dataInputB <- reactive({
    df <- data[data$Study_supplychain %in% input$supplychain,]
    df <- distinct(df)
    })
  
  dataInputC <- reactive({
    data <- as.data.frame(dataInputB())
    df <- data[data$region %in% input$region,]
    df <- distinct(df)
  })
        
  final_input <- reactive({
    if (input$expand == TRUE){
      io_counts = matrix(nrow=16, ncol=14)
      rownames(io_counts) <- out_type
      colnames(io_counts) <- int_type
      io_counts <- io_counts
    } else if (input$expand == FALSE){
      io_counts = matrix(nrow=3, ncol=5)
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
        geom_text(size=5,family="Proxima Nova")
    } else if (input$expand == TRUE){
      ggplot(DATA, aes(y=outcome,x=int,label=aid_count)) +
        geom_tile(aes(fill=aid_count),colour="black") +
        theme(axis.text.x = element_text(family="Proxima Nova",angle=45,hjust=1,size=14),axis.text.y=element_text(family="Proxima Nova",size=14),axis.title.x=element_text(family="Proxima Nova",size=20,vjust=-3),axis.title.y=element_text(family="Proxima Nova",size=20,vjust=-4)) +
        labs(x = "Types of actions",y="Types of outcomes") +
        scale_fill_gradient2(name="No. of unique articles",midpoint=(max(DATA$aid_count,na.rm=TRUE)/2),low="#c7e9c0",mid="#41ab5d",high="#00441b",na.value="white") +
        coord_fixed(ratio=5/6) +
        geom_text(size=4,family="Proxima Nova",color="gray") +
        scale_x_discrete(name="Intervention sub-types",limits=c("laws","policies","detection","prosecution","civil","substitution","awareness","market","disincentive","stewardship","conflict","spat_protect","harvest_reg","culture"),labels=c("Laws, regulations & codes","Policies & regulations", "Detection","Prosecution","Civil action","Substitutions","Awareness raising","Market-based incentives","Disincentives for illegal behavior","Incentives for stewardship of wildlife","Decrease human-wildlife conflict","Spatial areas of protection","Regulate harvest","Culturing of species"))
    }
  })
  
  output$map_data <- DT::renderDataTable({
    data <- distinct(dataInputC())
    data <- select(data,aid,Authors,Title,Journal,Species,int_group,Int_type,Desired_outcome,Actions_taken,Study_goal,Study_location,region,Study_country,Study_duration,Comps.type,Design.type,Study_supplychain,Stated_outcomes,out_group,Outcome,Indicators,Outcome.direction,Outcome_notes)
    colnames(data) <- c("Article ID","Authors","Title","Journal","Target species","Intervention type","Intervention sub-type","Desired outcome","Actions taken","Goal of study","Location of study","Region of study","Country of study","Duration of study","Comparators used","Study design","Area of supply chain studied","Stated outcomes measured","Outcome type","Outcome sub-type","Indicators used","Direction of outcome","Notes")
    data <- distinct(data)
    DT::datatable(data)
  })

  biblio_maptab <- reactive({
    data <- dataInputC()
    bib <- data %>% select(aid,Biome.,Pub_type,Authors,Title,Pub_year,DOI,Journal) %>% distinct()
    colnames(bib) <- c("Article ID", "Biome/ecoregion","Publication type","Authors","Title","Year of publication","DOI","Journal")
    bib <- bib
  })
  
  output$downloadFullData <- downloadHandler(
    filename = function() {
      paste(input$region,"_",input$supplychain,"_dataset.csv",sep="")
    },
    content = function(file) {
      write.csv(dataInputB(), file)
    }
  )
  
  output$downloadBiblio <- downloadHandler(
    filename = function() {
      paste(input$region,"_",input$supplychain,"_biblio.csv",sep="")
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
    data <- c("PLACEHOLDER")
  })
  
  oa1 <- reactive({
    data <- c("PLACEHOLDER")
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
  ## Creating reactive data table
  ##==========
  dataInputA <- reactive({
    data <- map_data_final
    if (input$eintout_int == "all"){
      sub <- data
    } else
      sub <- data[data$Int_type == input$eintout_int,]
    sub <- distinct(sub)
  })
  
  dataInputD <- reactive({
    data <- as.data.frame(dataInputA())
    if (input$eintout_out == "all"){
      sub <- data
    } else
      sub <- data[data$Outcome == input$eintout_out,]
    sub <- distinct(sub)
  })
  
  dataInputE <- reactive({
    data <- as.data.frame(dataInputD())
    if (input$eintout_region == "All"){
      sub <- data
    } else
      sub <- data[data$region == input$eintout_region,]
    sub <- distinct(sub)
  })
  
  output$e_table <- DT::renderDataTable({
    data <- distinct(dataInputE())
    data <- select(data,aid,Authors,Title,Journal,Species,int_group,Int_type,Desired_outcome,Actions_taken,Study_goal,Study_location,region,Study_country,Study_duration,Comps.type,Design.type,Study_supplychain,Stated_outcomes,out_group,Outcome,Indicators,Outcome.direction,Outcome_notes)
    colnames(data) <- c("Article ID","Authors","Title","Journal","Target species","Intervention type","Intervention sub-type","Desired outcome","Actions taken","Goal of study","Location of study","Region of study","Country of study","Duration of study","Comparators used","Study design","Area of supply chain studied","Stated outcomes measured","Outcome type","Outcome sub-type","Indicators used","Direction of outcome","Notes")
    data <- distinct(data)
    DT::datatable(data)
  })
  
  ##=========
  ## Creating reactive bibliography
  ##=========
  
  e_bibl <- reactive({
    data <- as.data.frame(dataInputE())
    data <- data %>% select(aid, Pub_type, Authors, Pub_year, Title, Journal, Int_type, Outcome, Outcome.direction, region)
    data <- distinct(data)
    colnames(data) <- c("Article ID","Publication type","Authors","Year","Title","Journal","Intervention sub-type","Outcome sub-type","Direction of outcome","Region of study")
    data <- data
    
  })
  
  output$e_bib <- DT::renderDataTable({
    data <- as.data.frame(e_bibl())
    DT::datatable(distinct(data),escape=FALSE)
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
  
  ##=========
  ## Info box summaries
  ##=========
  us2 <- reactive({
    n <- n_distinct(dataInputE()$aid)
  })
  
  output$elink_us_2 <- renderText({
    as.character(us2())
  })
  
  output$elink_ie_2 <- renderText({
    "PLACEHOLDER"
  })
  
  output$elink_oa_2 <- renderText({
    "PLACEHOLDER"
  })
  
  ##========
  ## Data summary plots
  ##========
  
  output$e_int <- renderPlotly({
    dat <- as.data.frame(dataInputE())
    dat <- dat %>% select(aid,int_group,Int_type,Study_supplychain) %>% distinct()
    
    is_counts = matrix(nrow=14, ncol=3)
    rownames(is_counts) <- int_type
    colnames(is_counts) <- c("supply","trade","consumer")
    is_counts <- is_counts
    
    for (i in int_type){
      for (j in c("supply","trade","consumer")){
        subset <- filter(dat, Study_supplychain == j, Int_type == i)
        is_counts[i,j] <- n_distinct(subset$aid)
      }
    }
    
    is_counts <- as.data.frame(is_counts)
    is_counts$Int_type <- rownames(is_counts)
    rownames(is_counts) <- NULL
    is_counts1 <- melt(is_counts,id.var="Int_type")
    is_counts3 <- assignIntGroup(is_counts1)
    is_counts2 <- assignIntLabel(is_counts3)

    is_counts2$variable <- factor(is_counts2$variable, levels=c("supply","trade","consumer"))
    is_counts4 <- assignSupLabel(is_counts2)
    is_counts4$int_group <- factor(is_counts4$int_group, levels=c("Establish/refine laws & policies","Enforcement/compliance","Reduce demand/consumption","Reduce threats to species","Support livelihoods"))
    is_counts4$int_group_new <- str_wrap(is_counts4$int_group,width=15)
    is_counts4$int_group_new <- gsub(pattern = "\n", replacement = "\n<br>", is_counts4$int_group_new)
    is_counts4$int_label_new <- str_wrap(is_counts4$int_label,width=25)
    is_counts4$int_label_new <- gsub(pattern = "\n", replacement = "\n<br>", is_counts4$int_label_new)
    
    p <- ggplot(is_counts4, aes(x=int_label_new,y=value,fill=supply_label)) + 
      geom_bar(stat="identity") +
      theme_bw() +
      theme(axis.text.x = element_text(angle=45,hjust=1,size=10),axis.title.x = element_blank(),axis.title.y = element_text(size=12,hjust=-1),axis.text.y=element_text(size=10),strip.text.x=element_text(size=10, face="bold"),strip.background=element_rect(fill="#ffffff",color="#ffffff")) +
      labs(y="NUMBER OF ARTICLES") +
      facet_grid(~int_group_new, scales = "free",space="free") +
      scale_fill_manual(breaks=c("Supply-side","Trade controls","End-market"),values=c("#66c2a5","#fc8d62","#8da0cb")) +
      guides(fill=guide_legend(title=NULL))

    m=list(l=100,
           r=25,
           b=250,
           t=100,
           pad=4
    )
    
    ggplotly(p) %>% layout(margin=m)
    
  })
  
  output$e_out <- renderPlotly({
    dat <- as.data.frame(dataInputE())
    dat <- dat %>% select(aid,Outcome,Outcome.direction) %>% distinct()
    
    is_counts = matrix(nrow=16, ncol=4)
    rownames(is_counts) <- out_type
    colnames(is_counts) <- c("Positive","Negative","Neutral","Mixed")
    is_counts <- is_counts
    
    for (i in out_type){
      for (j in c("Positive","Negative","Neutral","Mixed")){
        subset <- filter(dat, Outcome.direction == j, Outcome == i)
        is_counts[i,j] <- n_distinct(subset$aid)
      }
    }
    
    is_counts <- as.data.frame(is_counts)
    is_counts$Outcome <- rownames(is_counts)
    rownames(is_counts) <- NULL
    is_counts1 <- melt(is_counts,id.var="Outcome")
    is_counts2 <- assignOutGroup(is_counts1)
    
    is_counts2$variable <- factor(is_counts2$variable, levels=c("Positive","Negative","Neutral","Mixed"))
    is_counts2$out_group <- factor(is_counts2$out_group, levels=c("Conservation","Biological","Socio-economic"))
    is_counts2$out_type_new <- str_wrap(is_counts2$Outcome,width=25)
    is_counts2$out_type_new <- gsub(pattern = "\n", replacement = "\n<br>", is_counts2$out_type_new)
    
    p <- ggplot(is_counts2, aes(x=Outcome,y=value,fill=variable)) + 
      geom_bar(stat="identity") +
      theme_bw() +
      theme(axis.text.x = element_text(angle=45,hjust=1,size=10),axis.title.x = element_blank(),axis.title.y = element_text(size=12,vjust=-1),axis.text.y=element_text(size=10),strip.text.x=element_text(size=10, face="bold"),strip.background=element_rect(fill="#ffffff",color="#ffffff")) +
      labs(y="NUMBER OF ARTICLES") +
      facet_grid(~out_group, scales = "free",space="free_x") +
      scale_fill_manual(breaks=c("Positive","Negative","Neutral","Mixed"),values=c("#35bcad","#f6cb15","#f9f3e7","#de2b37")) +
      guides(fill=guide_legend(title=NULL))
    
    m=list(l=100,
           r=25,
           b=250,
           t=100,
           pad=4
    )
    
    ggplotly(p) %>% layout(margin=m)
  })
  
  output$e_comp <- renderPlotly({
    dat <- as.data.frame(dataInputE())
    dat <- dat %>% select(aid,Comps.type) %>% distinct()
    colnames(dat) <- c("aid","var")
    i_counts = matrix(nrow=7, ncol=2)
    rownames(i_counts) <- comp_type
    colnames(i_counts) <- c("var","count")
    
    for (i in comp_type){
      subset <- filter(dat, var == i)
      i_counts[i,1] <- i
      i_counts[i,2] <- n_distinct(subset$aid)
    }
    
    i_counts <- as.data.frame(i_counts)
    rownames(i_counts) <- NULL
    i_counts$count <- as.numeric(as.vector(i_counts$count))
    i_counts$labels <- comp_type
    
    p <- ggplot(data=i_counts, aes(x=labels,y=count)) +
      geom_bar(stat="identity",fill="turquoise") +
      theme(axis.text.x = element_text(angle=45,hjust=1,size=8),axis.title.x = element_blank(),axis.title.y = element_text(size=10),axis.text.y=element_text(size=8)) +
      ylab("Number of articles") +
      xlab("Types of comparators used") 
    
    m=list(l=75,
           b=250,
           t=50,
           r=50,
           pad=4
    )
    
    ggplotly(p) %>% layout(margin=m)
  })
  
  output$e_study <- renderPlotly({
    dat <- as.data.frame(dataInputE())
    dat <- dat %>% select(aid,Design.type) %>% distinct()
    colnames(dat) <- c("aid","var")
    i_counts = matrix(nrow=4, ncol=2)
    rownames(i_counts) <- design_type
    colnames(i_counts) <- c("var","count")
    
    for (i in design_type){
      subset <- filter(dat, var == i)
      i_counts[i,1] <- i
      i_counts[i,2] <- n_distinct(subset$aid)
    }
    
    i_counts <- as.data.frame(i_counts)
    rownames(i_counts) <- NULL
    i_counts$count <- as.numeric(as.vector(i_counts$count))
    i_counts$labels <- design_type
    
    p <- ggplot(data=i_counts, aes(x=labels,y=count)) +
      geom_bar(stat="identity",fill="turquoise") +
      theme(axis.text.x = element_text(angle=45,hjust=1,size=8),axis.title.x = element_blank(),axis.title.y = element_text(size=10),axis.text.y=element_text(size=8)) +
      ylab("Number of articles") +
      xlab("Type of study design") 
    
    m=list(l=75,
           b=250,
           t=50,
           r=50,
           pad=4
    )
    
    ggplotly(p) %>% layout(margin=m)
  })  

  output$e_country <- renderPlotly({
    dat <- as.data.frame(dataInputE())
    dat <- dat %>% select(aid,Study_country) %>% distinct()
    i_counts <- count(dat,Study_country)
    colnames(i_counts) <- c("var","count")
    i_counts <- as.data.frame(i_counts)
    rownames(i_counts) <- NULL
    i_counts$count <- as.numeric(as.vector(i_counts$count))
    i_counts$labels <- i_counts$var
    
    p <- ggplot(data=i_counts, aes(x=labels,y=count)) +
      geom_bar(stat="identity",fill="turquoise") +
      theme(axis.text.x = element_text(angle=45,hjust=1,size=10),axis.title.x = element_blank(),axis.title.y = element_text(size=12),axis.text.y=element_text(size=10)) +
      ylab("Number of articles") +
      xlab("Country of study") 
    
    m=list(l=75,
           b=250,
           t=50,
           r=50,
           pad=4
    )
    
    ggplotly(p) %>% layout(margin=m)
  })
  
  output$e_purpose <- renderPlotly({
    dat <- as.data.frame(dataInputE())
    dat <- dat %>% select(aid,Purpose) %>% distinct()
    colnames(dat) <- c("aid","var")
    i_counts = matrix(nrow=9, ncol=2)
    purpose <- c("clothing","construction","decor","food","fuel","medicine","pet","trophy","other")
    rownames(i_counts) <- c("clothing","construction","decor","food","fuel","medicine","pet","trophy","other")
    colnames(i_counts) <- c("var","count")
    
    for (i in purpose){
      subset <- filter(dat, var == i)
      i_counts[i,1] <- i
      i_counts[i,2] <- n_distinct(subset$aid)
    }
    
    i_counts <- as.data.frame(i_counts)
    rownames(i_counts) <- NULL
    i_counts$count <- as.numeric(as.vector(i_counts$count))
    i_counts$labels <- c("Clothing","Construction","Decoration","Food","Fuel","Medicine","Pets","Trophy","Other")
    
    p <- ggplot(data=i_counts, aes(x=labels,y=count)) +
      geom_bar(stat="identity",fill="turquoise") +
      theme(axis.text.x = element_text(angle=45,hjust=1,size=8),axis.title.x = element_blank(),axis.title.y = element_text(size=10),axis.text.y=element_text(size=8)) +
      ylab("Number of articles") +
      xlab("Part of supply chain studied") 
    
    m=list(l=75,
           b=250,
           t=50,
           r=50,
           pad=4
    )
    
    ggplotly(p) %>% layout(margin=m)
  })
})

eglob = function(env_serv) with (env_serv,{
  data <- map_data_final
  
  dataInputX <- reactive({
    df <- data[data$Study_supplychain %in% input$supplychain_c,]
    df <- distinct(df)
  })
  
  dataInputY <- reactive({
    data <- as.data.frame(dataInputX())
    df <- data[data$Int_type %in% input$intervention_c,]
    df <- distinct(df)
  })
  
  dataInputZ <- reactive({
    data <- as.data.frame(dataInputY())
    df <- data[data$Outcome %in% input$outcome_c,]
    df <- distinct(df)
  })
  
  country_dat <- reactive({
    dat <- as.data.frame(dataInputZ())
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
  
  p <- plot_ly(country_count, z=counts, type = 'choropleth', locations = Code, 
          text = Study_country, color = counts, colors=col, marker=list(line=l), 
          colorbar = list(title="No. of unique articles"))
  
  layout(p, geo = g)
})

country_click <- reactive({
  
  all <- read.csv("data/country_list_all.csv", head=TRUE, sep=",")
  names(all) <- c("Study_country", "Region", "Code","Subregion","Point")
   
  d <- plotly::event_data("plotly_click", source="poop")
  if (is.null(d)){
    country <- as.data.frame("none")
  } else country <- filter(all,Point == as.integer(d[2]))
  country <- as.character(country[1,1])
  return(country)
})
  
  biblio_countrytab <- reactive({
    data <- dataInputZ()
    data <- data %>% select(aid) %>% distinct()
    bib <- left_join(data,map_data_final,by="aid")
    bib <- bib %>% select(aid,Study_country,biome_group,Pub_type,Authors,Title,Pub_year,DOI,Journal,FullText) %>% distinct()
    colnames(bib) <- c("Article ID", "Publication type","Authors","Title","Year of publication","DOI","Journal","Open Access")
    bib <- bib
  })

  output$biblio_glob <- DT::renderDataTable({
    data <- as.data.frame(dataInputZ())
    click <- as.character(country_click())
    if (click != "none"){
      sub <- filter(data,Study_country == click)
      bib <- sub %>% select(aid,Study_country,Pub_type,Authors,Title,Pub_year,DOI,Journal) %>% distinct()
      colnames(bib) <- c("Article ID", "Study_country","Publication type","Authors","Title","Year of publication","DOI","Journal")
      DT::datatable(distinct(bib),escape=FALSE)
    } else if (click == "none"){
      bib <- matrix(nrow=1,ncol=8)
      colnames(bib) <- c("Article ID", "Study_country","Publication type","Authors","Title","Year of publication","DOI","Journal")
      DT::datatable(distinct(as.data.frame(bib),escape=FALSE))
    }
  })

  output$downloadFullDataC <- downloadHandler(
    filename = function() {
      paste(input$intervention_c,"_",input$outcome_c,"_",input$supplychain_c,"_dataset.csv",sep="")
    },
    content = function(file) {
      write.csv(dataInputZ(), file)
    }
  )
  
  output$downloadBiblioC <- downloadHandler(
    filename = function() {
      paste(input$intervention_c,"_",input$outcome_c,"_",input$supplychain_c,"_biblio.csv",sep="")
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