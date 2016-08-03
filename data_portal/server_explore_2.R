elink = function(env_serv) with (env_serv, {
  data <- map_data_final
  
  #observeEvent(input$showGlossary, ({
    #updateCollapse(session, "collapseGlossary", open="GLOSSARY")
  #}))
  #observeEvent(input$hideGlossary, ({
    #updateCollapse(session, "collapseGlossary", close="GLOSSARY")
  #}))
  
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
      io_counts = matrix(nrow=12, ncol=25)
      rownames(io_counts) <- out_type
      colnames(io_counts) <- int_type
      io_counts <- io_counts
    } else if (input$expand == FALSE){
      io_counts = matrix(nrow=12, ncol=10)
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
        scale_fill_gradient2(name="Number of studies",midpoint=(max(DATA$aid_count,na.rm=TRUE)/2),low="#c7e9c0",mid="#41ab5d",high="#00441b",na.value="white") +
        coord_fixed(ratio=5/6) +
        geom_text(size=5,family="Helvetica") +
        scale_x_discrete(name="IUCN conservation intervention types",limits=c("area_protect", "land_wat_mgmt", "res_mgmt", "species_mgmt", "education", "law_policy", "liv_eco_inc", "ext_cap_build", "sus_use", "other"),labels=c("Area protection", "Land/Water management", "Resource management", "Species management", "Education", "Law & Policy", "Livelihood, economic & other incentives", "External capacity building", "Sustainable use", "Other")) +
        scale_y_discrete(name="Human well-being outcomes",limits=c("other","free_choice","culture","sub_well","gov","sec_saf","soc_rel","education","health","mat_liv_std","eco_liv_std","env"),labels=c("Other","Freedom of choice/action","Cultural & spiritual","Subjective well-being","Governance & empowerment","Security & safety","Social relations","Education","Health","Material living standards","Economic living standards","Environmental"))
    } else if (input$expand == TRUE){
      ggplot(DATA, aes(y=outcome,x=int,label=aid_count)) +
        geom_tile(aes(fill=aid_count),colour="black") +
        theme(axis.text.x = element_text(family="Helvetica",angle=45,hjust=1,size=16),axis.text.y=element_text(family="Helvetica",size=16),axis.title.x=element_text(family="Helvetica",size=20,vjust=-2),axis.title.y=element_text(family="Helvetica",size=20,vjust=-2)) +
        scale_fill_gradient2(name="Number of studies",midpoint=(max(DATA$aid_count,na.rm=TRUE)/2),low="#c7e9c0",mid="#41ab5d",high="#00441b",na.value="white") +
        coord_fixed(ratio=5/6) +
        geom_text(size=5,family="Helvetica") +
        scale_x_discrete(name="IUCN conservation intervention sub-types",limits=c("area_protect", "area_mgmt", "res_mgmt", "sp_control", "restoration", "sp_mgmt", "sp_recov", "sp_reint", "ex_situ", "form_ed", "training", "aware_comm", "legis", "pol_reg", "priv_codes", "compl_enfor", "liv_alt", "sub", "market", "non_mon", "inst_civ_dev", "part_dev", "cons_fin", "sus_use", "other"),labels=c("Area protection", "Area management", "Resource management/protection", "Species control", "Restoration", "Species management", "Species recovery", "Species reintroduction", "Ex-situ conservation", "Formal education", "Training", "Awareness & Communications", "Legislation", "Policies & Regulations", "Private sector standards and codes", "Compliance & enforcement", "Enterprises & livelihood alternatives", "Substitution", "Market-based forces", "Non-monetary values", "Institutional & civil society development", "Alliance & partnership development", "Conservation finance", "Sustainable use", "Other")) +
        scale_y_discrete(name="Human well-being outcomes",limits=c("other","free_choice","culture","sub_well","gov","sec_saf","soc_rel","education","health","mat_liv_std","eco_liv_std","env"),labels=c("Other","Freedom of choice/action","Cultural & spiritual","Subjective well-being","Governance & empowerment","Security & safety","Social relations","Education","Health","Material living standards","Economic living standards","Environmental"))
    }
  })
  
  fullData_maptab <- reactive({
    data <- dataInputB()
    data <- data %>% select(aid,Biome.,region,IE) %>% distinct()
    data_i <- select(data.interv,aid,Int_type)
    data_i <- distinct(data_i)
    data_o <- select(data.outcome,aid,Outcome)
    data_o <- distinct(data_o)
    data_s <- select(data.study,aid, Study_country, Comps,Comps.type,Comps.time,Design.qual_only,Design.assigned,Design.control)
    data_s <- distinct(data_s)
    data_p <- select(data.pathways,aid,Concept_mod,Concept_mod_name)
    data_p <- distinct(data_p)
    data <- data %>% left_join(data_i,by="aid") %>% distinct() %>% left_join(data_o,by="aid") %>% distinct() %>% left_join(data_s,by="aid") %>% distinct() %>% left_join(data_p,by="aid") %>% distinct()
  })
    
  output$map_data <- DT::renderDataTable(DT::datatable({
    data <- fullData_maptab()
  }))

  biblio_maptab <- reactive({
    bib <- dataInputB()
    bib <- bib %>% distinct() %>% left_join(data.biblio,by="aid") %>% distinct() %>% select(aid,Int_type,Outcome,int_group,Study_country,region,Biome.,Pub_type, Authors, Pub_year, Title, Journal, Vol, Page_num) %>% distinct()
  })
  
  output$downloadFullData <- downloadHandler(
    filename = function() {
      paste(input$region,"_",input$biome,"_dataset.csv",sep="")
    },
    content = function(file) {
      write.csv(fullData_maptab(), file)
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
  
  us <- reactive({
    n <- n_distinct(fullData_maptab()$aid)
  })
  
  ie <- reactive({
    data <- select(fullData_maptab(),aid,IE)
    IE <- filter(data,IE == "Y")
    IE <- distinct(IE)
    n <- n_distinct(IE$aid)
  })
  
  output$elink_us <- renderInfoBox({
    infoBox(
      "Total number of unique articles:",
      div(strong(as.character(us()),style="color:#FF6633")),
      icon=icon("file-text-o")
      )
  })
  
  output$elink_ie <- renderInfoBox({
    infoBox(
      "Number of impact evaluations",
      div(strong(as.character(ie()),style="color:#FF6633")),
      icon=icon("check-square")
    )
  })
  
  output$elink_oa <- renderInfoBox({
    infoBox(
      "Number of open-access articles",
      "###",
      icon=icon("share-alt")
    )
  })
  
  intdef <- reactive({
    data <- filter(definitions,Term == input$intervention)
    def <- data$Definition
  })
  
  intex <- reactive({
    data <- filter(definitions,Term == input$intervention)
    ex <- data$Example
  })
    
  output$int_def2 <- renderText({
    paste(as.character(intdef()))
  })
  
  output$int_def3 <- renderText({
    paste(as.character(intex()))
  })
   
  outdef <- reactive({
    data <- filter(definitions,Term == input$outcome)
    def <- data$Definition
  })
  outex <- reactive({
    data <- filter(definitions,Term == input$outcome)
    ex <- data$Example
  })
  
  output$out_def2 <- renderText({
    paste(as.character(outdef()))
  })
  
  output$out_def3 <- renderText({
    paste(as.character(outex()))
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
      if (input$biome_filter == "MAR"){
        dataC <- dataC[dataC$Biome. == "M_P" | dataC$Biome. == "M_TSS" | dataC$Biome. == "M_TU" | dataC$Biome. == "M_TRU" | dataC$Biome. == "M_TRC" | dataC$Biome. == "M_TSTSS",]
      } else if (input$biome_filter == "FRW"){
        dataC <- dataC[dataC$Biome. == "FW_LL" | dataC$Biome. == "FW_LRD" | dataC$Biome. == "FW_PF" | dataC$Biome. == "FW_MF" | dataC$Biome. == "FW_TCR" | dataC$Biome. == "FW_TFRW" | dataC$Biome. == "FW_TUR" | dataC$Biome. == "FW_TSTCR" | dataC$Biome. == "FW_TSTFRW" | dataC$Biome. == "FW_TSTUR" | dataC$Biome. == "FW_XFEB" | dataC$Biome. == "FW_OI",]
      } else if (input$biome_filter == "FOR"){
        dataC <- dataC[dataC$Biome. == "T_TSTMBF" | dataC$Biome. == "T_TSTDBF" | dataC$Biome. == "T_TSTCF" | dataC$Biome. == "T_TBMF" | dataC$Biome. == "T_TCF" | dataC$Biome. == "T_BFT" | dataC$Biome. == "T_MFWS",]
      } else if (input$biome_filter == "GRS"){
        dataC <- dataC[dataC$Biome. == "T_TSTGSS" | dataC$Biome. == "T_TGSS" | dataC$Biome. == "T_FGS" | dataC$Biome. == "T_MGS",]
      } else if (input$biome_filter == "TUN"){
        dataC <- dataC[dataC$Biome. == "T_T",]
      } else if (input$biome_filter == "DES"){
        dataC <- dataC[dataC$Biome. == "T_DXS",]
      } else if (input$biome_filter == "MAN"){
        dataC <- dataC[dataC$Biome. == "T_M",]
      }
    } else if (input$intervention_c != "all" & input$outcome_c != "all" & input$biome_filter == "ALL"){
      dataC <- dataC[dataC$Int_type == input$intervention_c,]
      dataC <- dataC[dataC$outcome_c == input$outcome_c,]
    } else if (input$intervention_c != "all" & input$outcome_c == "all" & input$biome_filter != "ALL"){
      dataC <- dataC[dataC$Int_type == input$intervention_c,]
      if (input$biome_filter == "MAR"){
        dataC <- dataC[dataC$Biome. == "M_P" | dataC$Biome. == "M_TSS" | dataC$Biome. == "M_TU" | dataC$Biome. == "M_TRU" | dataC$Biome. == "M_TRC" | dataC$Biome. == "M_TSTSS",]
      } else if (input$biome_filter == "FRW"){
        dataC <- dataC[dataC$Biome. == "FW_LL" | dataC$Biome. == "FW_LRD" | dataC$Biome. == "FW_PF" | dataC$Biome. == "FW_MF" | dataC$Biome. == "FW_TCR" | dataC$Biome. == "FW_TFRW" | dataC$Biome. == "FW_TUR" | dataC$Biome. == "FW_TSTCR" | dataC$Biome. == "FW_TSTFRW" | dataC$Biome. == "FW_TSTUR" | dataC$Biome. == "FW_XFEB" | dataC$Biome. == "FW_OI",]
      } else if (input$biome_filter == "FOR"){
        dataC <- dataC[dataC$Biome. == "T_TSTMBF" | dataC$Biome. == "T_TSTDBF" | dataC$Biome. == "T_TSTCF" | dataC$Biome. == "T_TBMF" | dataC$Biome. == "T_TCF" | dataC$Biome. == "T_BFT" | dataC$Biome. == "T_MFWS",]
      } else if (input$biome_filter == "GRS"){
        dataC <- dataC[dataC$Biome. == "T_TSTGSS" | dataC$Biome. == "T_TGSS" | dataC$Biome. == "T_FGS" | dataC$Biome. == "T_MGS",]
      } else if (input$biome_filter == "TUN"){
        dataC <- dataC[dataC$Biome. == "T_T",]
      } else if (input$biome_filter == "DES"){
        dataC <- dataC[dataC$Biome. == "T_DXS",]
      } else if (input$biome_filter == "MAN"){
        dataC <- dataC[dataC$Biome. == "T_M",]
      }
    } else if (input$intervention_c == "all" & input$outcome_c != "all" & input$biome_filter != "ALL"){
      dataC <- dataC[dataC$Outcome == input$outcome_c,]
      if (input$biome_filter == "MAR"){
        dataC <- dataC[dataC$Biome. == "M_P" | dataC$Biome. == "M_TSS" | dataC$Biome. == "M_TU" | dataC$Biome. == "M_TRU" | dataC$Biome. == "M_TRC" | dataC$Biome. == "M_TSTSS",]
      } else if (input$biome_filter == "FRW"){
        dataC <- dataC[dataC$Biome. == "FW_LL" | dataC$Biome. == "FW_LRD" | dataC$Biome. == "FW_PF" | dataC$Biome. == "FW_MF" | dataC$Biome. == "FW_TCR" | dataC$Biome. == "FW_TFRW" | dataC$Biome. == "FW_TUR" | dataC$Biome. == "FW_TSTCR" | dataC$Biome. == "FW_TSTFRW" | dataC$Biome. == "FW_TSTUR" | dataC$Biome. == "FW_XFEB" | dataC$Biome. == "FW_OI",]
      } else if (input$biome_filter == "FOR"){
        dataC <- dataC[dataC$Biome. == "T_TSTMBF" | dataC$Biome. == "T_TSTDBF" | dataC$Biome. == "T_TSTCF" | dataC$Biome. == "T_TBMF" | dataC$Biome. == "T_TCF" | dataC$Biome. == "T_BFT" | dataC$Biome. == "T_MFWS",]
      } else if (input$biome_filter == "GRS"){
        dataC <- dataC[dataC$Biome. == "T_TSTGSS" | dataC$Biome. == "T_TGSS" | dataC$Biome. == "T_FGS" | dataC$Biome. == "T_MGS",]
      } else if (input$biome_filter == "TUN"){
        dataC <- dataC[dataC$Biome. == "T_T",]
      } else if (input$biome_filter == "DES"){
        dataC <- dataC[dataC$Biome. == "T_DXS",]
      } else if (input$biome_filter == "MAN"){
        dataC <- dataC[dataC$Biome. == "T_M",]
      }
    } else if (input$intervention_c != "all" & input$outcome_c != "all" & input$biome_filter != "ALL"){
      dataC <- dataC[dataC$Outcome == input$outcome_c,]
      dataC <- dataC[dataC$Int_type == input$intervention_c,]
      if (input$biome_filter == "MAR"){
        dataC <- dataC[dataC$Biome. == "M_P" | dataC$Biome. == "M_TSS" | dataC$Biome. == "M_TU" | dataC$Biome. == "M_TRU" | dataC$Biome. == "M_TRC" | dataC$Biome. == "M_TSTSS",]
      } else if (input$biome_filter == "FRW"){
        dataC <- dataC[dataC$Biome. == "FW_LL" | dataC$Biome. == "FW_LRD" | dataC$Biome. == "FW_PF" | dataC$Biome. == "FW_MF" | dataC$Biome. == "FW_TCR" | dataC$Biome. == "FW_TFRW" | dataC$Biome. == "FW_TUR" | dataC$Biome. == "FW_TSTCR" | dataC$Biome. == "FW_TSTFRW" | dataC$Biome. == "FW_TSTUR" | dataC$Biome. == "FW_XFEB" | dataC$Biome. == "FW_OI",]
      } else if (input$biome_filter == "FOR"){
        dataC <- dataC[dataC$Biome. == "T_TSTMBF" | dataC$Biome. == "T_TSTDBF" | dataC$Biome. == "T_TSTCF" | dataC$Biome. == "T_TBMF" | dataC$Biome. == "T_TCF" | dataC$Biome. == "T_BFT" | dataC$Biome. == "T_MFWS",]
      } else if (input$biome_filter == "GRS"){
        dataC <- dataC[dataC$Biome. == "T_TSTGSS" | dataC$Biome. == "T_TGSS" | dataC$Biome. == "T_FGS" | dataC$Biome. == "T_MGS",]
      } else if (input$biome_filter == "TUN"){
        dataC <- dataC[dataC$Biome. == "T_T",]
      } else if (input$biome_filter == "DES"){
        dataC <- dataC[dataC$Biome. == "T_DXS",]
      } else if (input$biome_filter == "MAN"){
        dataC <- dataC[dataC$Biome. == "T_M",]
      }
    }
    
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
  
#=========================
# Plotting static map with ggplot2
#=========================
#   map <- readShapeSpatial("data/TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp")
#   map <- fortify(map, region="ISO3")
#   
#   output$map <- renderPlot({
#     c_data <- country_dat()
#     c_data[c_data == 0] <- NA
#     c_data_na <- filter(c_data,is.na(counts))
#     c_data_2 <- filter(c_data, !is.na(counts))
#     c_data_2$counts <- as.numeric(as.vector(c_data_2$counts))
#     ggplot() + 
#       geom_map(data=c_data_2, aes(map_id=Code,fill=counts),map=map) +
#       scale_fill_gradient2(name="NUMBER OF STUDIES",low="#b2e5e5",mid="#1aa3a3",high="#0b5e56",midpoint=(max(as.numeric(as.vector(c_data_2$counts))/2)),limits=c(1,max(as.numeric(as.vector(c_data_2$counts)))),guide=guide_colorbar(barwidth=10,direction="horizontal",title.position="top",label.position="bottom")) +
#       geom_map(data=c_data_na, aes(map_id=Code),fill="white",map=map) +
#       geom_map(data=oecd, aes(map_id=CODE),fill="#bababa",map=map) +
#       expand_limits(x=map$long,y=map$lat) + 
#       xlab("Longitude") +
#       ylab("Latitude") +
#       theme(legend.key.size=unit(1,"cm"),legend.background=(element_rect(fill="black")),legend.title=element_text(colour="white",size=16,face="bold"),legend.text=element_text(colour="white",size=12,face="bold"),legend.position=c(.5,.1),panel.background=element_rect(fill="black"),panel.grid.major=element_line(color="grey50",linetype="dotted"),panel.grid.minor=element_line(color="grey50",linetype="dotted"),axis.title.x=element_text(size=14),axis.title.y=element_text(size=14),axis.text.x=element_text(size=12),axis.text.y=element_text(size=12))
#   })

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
    projection = list(type = 'Mercator'),
    showcoastlines = F,
    showcountries = T,
    countrycolor = toRGB("white"),
    coastlinecolor = toRGB("white")
  )
  
  col <- colorRampPalette(c("white", "#74c476","#41ab5d", "#238b45", "#006d2c", "#00441b")) (34)
  
  p <- plot_ly(country_count, z=COUNTS, type = 'choropleth', locations = CODE, 
          text = COUNTRY, color = COUNTS, colors=col, marker=list(line=l), 
          colorbar = list(title="No. of unique articles"))
  
  layout(p, geo = g)
})

  fullData_countrytab <- reactive({
    data <- dataInputC()
    data <- data %>% select(aid,Biome.) %>% distinct() %>% left_join(data.biblio,by="aid") %>% distinct() %>% left_join(data.interv,by="aid") %>% distinct() %>% left_join(data.outcome,by="aid") %>% distinct() %>% left_join(data.study,by="aid") %>% distinct() %>% left_join(data.pathways,by="aid") %>% distinct()
  })
  
  biblio_countrytab <- reactive({
    bib <- dataInputC()
    bib <- bib %>% distinct() %>% left_join(data.biblio,by="aid") %>% distinct() %>% select(aid,Int_type,Outcome,int_group,Study_country,region,Biome.,Pub_type, Authors, Pub_year, Title, Journal, Vol, Page_num) %>% distinct()
  })
  
  output$downloadFullDataC <- downloadHandler(
    filename = function() {
      paste(input$intervention,"_",input$outcome,"_",input$biome_filter,"_dataset.csv",sep="")
    },
    content = function(file) {
      write.csv(fullData_countrytab(), file)
    }
  )
  
  output$downloadBiblioC <- downloadHandler(
    filename = function() {
      paste(input$intervention,"_",input$outcome,"_",input$biome_filter,"_biblio.csv",sep="")
    },
    content = function(file) {
      write.csv(biblio_countrytab(), file)
    }
  )
})

eregi = function(env_serv) with (env_serv,{
  ## Setting region parameters ##
  observe({
    eregi_subreg <- if (is.null(input$eregi_region)) character(0) else{
      regions %>% 
      filter(REGION == input$eregi_region) %>%
         select(SUBREGION) %>%
         unique()
    }
      stillSelected_SR <- isolate(input$eregi_subreg[input$eregi_subreg %in% eregi_subreg])
      rownames(stillSelected_SR) <- NULL
      rownames(eregi_subreg) <- NULL
      updateSelectInput(session, "eregi_subreg", choices=c("All",as.vector(eregi_subreg$SUBREGION)),selected=stillSelected_SR)
    })

  observe({
     eregi_country <- if (is.null(input$eregi_subregion)) character(0) else{
      regions %>%
        filter(SUBREGION == input$eregi_subreg) %>%
        select(COUNTRY) %>%
        unique()
     }
      stillSelected_C <- isolate(input$eregi_country[input$eregi_country %in% eregi_country])
      rownames(stillSelected_C) <- NULL
      rownames(eregi_country) <- NULL
      updateSelectInput(session, "eregi_country",choice=c("All",as.vector(eregi_country$COUNTRY)))
      })

  ##==============
  ## Testing variables and storage (comment out when not needed)
  ##==============
  output$test1 <- renderPrint({ input$eregi_region })
  output$test2 <- renderPrint({ input$eregi_subregion })
  output$test3 <- renderPrint({ input$eregi_country })
  
  dat.bib <- dplyr::select(data.biblio,aid,Pub_type,Authors,DOI,Pub_year,Title,Journal)
  dat.bib <- distinct(dat.bib)
  
  ##REALLY SLOW NEED TO WORK ON
  
  eregiBib <- reactive({
    df <- eregiData()
    df <- dplyr::select(df,aid,region)
    df_merge <- df %>%
      distinct() %>%
      left_join(dat.bib, by="aid")
    df_bib <- df_merge %>%
      select(-region)
    df_bib <- distinct(df_bib)
    colnames(df_bib) <- c("Article ID","Publication type","Author(s)","DOI","Publication Year","Title","Journal")
    df_bib
  })
  

  output$eregi_table <- DT::renderDataTable(DT::datatable({
    data <- map_data_final
#     if (input$eregi_region != "All") {
#       data <- data[data$region == input$eregi_region,]
#     }
#     if (input$eregi_subreg != "All") {
#       data <- data[data$subregion == input$eregi_subreg,]
#     }
#     if (input$eregi_country != "All") {
#       data <- data[data$Study_country == input$eregi_country,]
#     }
#     data <- distinct(data)
    data
  }))
  
  output$eregi_bib <- DT::renderDataTable(DT::datatable({
    df_bib <- as.data.frame(eregiBib())
    }))
})
