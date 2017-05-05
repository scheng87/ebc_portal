ebiom = function()
  fluidPage(
    tags$head(
      tags$style(HTML("#ter{background-image:url('forest.jpg');background-repeat: no-repeat;width:400px;height:150px")),
      tags$style(HTML("#mar{background-image:url('marine.jpg');background-repeat: no-repeat;width:400px;height:150px")),
      tags$style(HTML("#fre{background-image:url('freshwater.jpg');background-repeat: no-repeat;width:400px;height:150px"))
      ),
    fluidRow(
      align="center",
      h3(div(strong("EXPLORE BY BIOMES & ECOREGIONS"),style="color:#006699")),
      hr(),
      br(),
      br(),
      column(4,
             actionButton("ter",label="TERRESTRIAL",style='font-size:300%;font:bold')
      ),
      column(4,
             actionButton("mar",label="MARINE",style='font-size:300%;font:bold')
      ),
      column(4,
             actionButton("fre",label="FRESHWATER",style='font-size:300%;font:bold')
      ),
      br(),
      br(),
      hr()
    ),
    fluidRow(
      align="center",
      br(),
      h4(div(em("Choose an ecosystem to display interactive summary tables, data subsets and bibliographies."))),
      br(),
      hr(),
      shinyjs::useShinyjs(),
      hidden(
        column(12,
               id="terr",
               align="center",
               h4(div(em("These datasets and graphs can be further filtered by biome, ecoregion, intervention type, and outcome type by selecting from the dropdown menus below."))),
               column(3,
                      selectInput("b_biome","Major habitat type:",
                                  c("All"="all",
                                    "Forests"="forest",
                                    "Grasslands & Savannas"="grass",
                                    "Deserts"="desert",
                                    "Tundras"="tundra",
                                    "Mangroves"="mangrove")
                      )
               ),
               column(3,
                      selectInput("b_eco","Biome",c("All eco-regions"="All"))
               ),
               column(3,
                      selectInput("b_intervention",
                                  label = "Conservation intervention type:",
                                  choices= list("All"="all","Area management"="area_mgmt","Area protection"="area_protect","Resource management"="res_mgmt","Restoration"="restoration","Species management" = "sp_mgmt","Species recovery"="sp_recov","Species control"="sp_control","Species reintroduction"="sp_reint","Ex-situ conservation" = "ex_situ","Awareness & communication"="aware_comm","Formal education"="form_ed", "Training"="training","Compliance & enforcement"="compl_enfor","Legislation" = "legis","Policies & regulations" = "pol_reg","Livelihood alternatives" = "liv_alt","Market forces" = "market","Non-monetary values" = "non_mon","Private sector standards & codes"="priv_codes","Substitution"="sub","Conservation finance"="cons_fin","Institutional & civil development"="inst_civ_dev","Partnership & alliance development"="part_dev","Sustainable use"="sus_use","Other"="other"),
                                  selected = "all")
               ),
               column(3,
                      selectInput("b_outcome",
                                  label = "Human well-being outcome:",
                                  choices = list("All"="all","Economic living standards"="eco_liv_std", "Material living standards"="mat_liv_std", "Health"="health", "Education"="education", "Social relations"="soc_rel", "Security & safety"="sec_saf", "Governance & empowerment"="gov", "Subjective well-being"="sub_well", "Culture & spirituality"="culture", "Freedom of choice/action"="free_choice", "Other"="other"),
                                  selected = "all")
               )
        )
      ),
      hidden(
        column(12,
               id="mari",
               align="center",
               h5(div(em("These datasets, graphs and maps can be further filtered by biome, ecoregion, intervention type, and outcome type by selecting from the dropdown menus below."))),
               column(4,
                      selectInput("m_eco","Ecoregion:",c("All ecoregions"="all","Polar"="M_P","Tropical Upwelling"="M_TRU","Temperate Shelfs & Seas"="M_TSS","Tropical Coral Reefs"="M_TRC","Temperate Upwelling"="M_TU","Tropical/Subtropical Shelfs & Seas"="M_TSTSS"),selected="All")
               ),
               column(4,
                      selectInput("m_intervention",
                                  label = "Conservation intervention type:",
                                  choices= list("All"="all","Area management"="area_mgmt","Area protection"="area_protect","Resource management"="res_mgmt","Restoration"="restoration","Species management" = "sp_mgmt","Species recovery"="sp_recov","Species control"="sp_control","Species reintroduction"="sp_reint","Ex-situ conservation" = "ex_situ","Awareness & communication"="aware_comm","Formal education"="form_ed", "Training"="training","Compliance & enforcement"="compl_enfor","Legislation" = "legis","Policies & regulations" = "pol_reg","Livelihood alternatives" = "liv_alt","Market forces" = "market","Non-monetary values" = "non_mon","Private sector standards & codes"="priv_codes","Substitution"="sub","Conservation finance"="cons_fin","Institutional & civil development"="inst_civ_dev","Partnership & alliance development"="part_dev","Sustainable use"="sus_use","Other"="other"),
                                  selected = "all")
               ),
               column(4,
                      selectInput("m_outcome",
                                  label = "Human well-being outcome:",
                                  choices = list("All"="all","Economic living standards"="eco_liv_std", "Material living standards"="mat_liv_std", "Health"="health", "Education"="education", "Social relations"="soc_rel", "Security & safety"="sec_saf", "Governance & empowerment"="gov", "Subjective well-being"="sub_well", "Culture & spirituality"="culture", "Freedom of choice/action"="free_choice", "Other"="other"),
                                  selected = "all")
               ),
               br()
#                p("Major habitat types developed by WWF and TNC do not correspond directly with the classification used for the marine ecoregions. Thus, currently, filtering the spatial layers by ecoregion is not yet possible. This feature is under development. However, you can still filter the country data (circle markers) by ecoregion, intervention and outcome type")
        )
      ),
      hidden(
        column(12,
               id="fres",
               align="center",
               h5(div(em("These datasets, graphs and maps can be further filtered by biome, ecoregion, intervention type, and outcome type by selecting from the dropdown menus below."))),
               column(4,
                      selectInput("f_eco","Ecoregion:",c("All ecoregions"="all","Large Lakes"="FW_LL","Large River Deltas"="FW_LRD","Polar Freshwaters"="FW_PF","Montane Freshwaters"="FW_MF","Temperate Coastal Rivers"="FW_TCR","Temperate Floodplain Rivers & Wetland Complexes"="FW_TFRWC","Temperate Upland Rivers"="FW_TUR","Tropical/Subtropical Coastal Rivers"="FW_TSTCR","Tropical/Subtropical Floodplain Rivers & Wetland Complexes"="FW_TSTFRWC","Tropical/Subtropical Upland Rivers"="TSTUR","Xeric Freshwaters & Endorheic (Closed) Basins"="FW_XFEB","Oceanic Islands"="OI"),selected="All")
               ),
               column(4,
                      selectInput("f_intervention",
                                  label = "Conservation intervention type:",
                                  choices= list("All"="all","Area management"="area_mgmt","Area protection"="area_protect","Resource management"="res_mgmt","Restoration"="restoration","Species management" = "sp_mgmt","Species recovery"="sp_recov","Species control"="sp_control","Species reintroduction"="sp_reint","Ex-situ conservation" = "ex_situ","Awareness & communication"="aware_comm","Formal education"="form_ed", "Training"="training","Compliance & enforcement"="compl_enfor","Legislation" = "legis","Policies & regulations" = "pol_reg","Livelihood alternatives" = "liv_alt","Market forces" = "market","Non-monetary values" = "non_mon","Private sector standards & codes"="priv_codes","Substitution"="sub","Conservation finance"="cons_fin","Institutional & civil development"="inst_civ_dev","Partnership & alliance development"="part_dev","Sustainable use"="sus_use","Other"="other"),
                                  selected = "all")
               ),
               column(4,
                      selectInput("f_outcome",
                                  label = "Human well-being outcome:",
                                  choices = list("All"="all","Economic living standards"="eco_liv_std", "Material living standards"="mat_liv_std", "Health"="health", "Education"="education", "Social relations"="soc_rel", "Security & safety"="sec_saf", "Governance & empowerment"="gov", "Subjective well-being"="sub_well", "Culture & spirituality"="culture", "Freedom of choice/action"="free_choice", "Other"="other"),
                                  selected = "all")
               )
        )
      ),
      hr()
    ),
    fluidRow(
      br(),
      tabsetPanel(
        type="tabs",
        tabPanel("Map",
                 fluidRow(
                   p(div(em("The map below shows the selected major habitat types/biomes/ecoregions layered with circle markers indicating how many studies are in that country respective of the chosen options above. Layers are from the WWF ecoregion datasets and grouping of biomes and marine ecoregions reflect WWF groupings."))),
                   p(div(strong("Please be patient, the data layers are quite large and may take a few moments to load."))),
                   leafletOutput("biome_map",height="600px",width="90%"),
                   hr(),
                   p("These shapefiles are used for scientific and educational purposes by the Nature and People Data Portal and the Evidence-Based Conservation SNAPP Working Group."),
                   p(div(strong("WWF Freshwater Ecoregions of the World: ")),a("http://www.feow.org",href="http://www.feow.org")),
                   p(div(em("Copyright 2008 by The Nature Conservancy and World Wildlife Fund, Inc. All Rights Reserved."))),
                   p(div(em("Citation: Abell et al. 2008"))),
                   p(div(strong("Global Ecoregions, Major Habitat Types, Biogeographical Realms and The Nature Conservancy Terrestrial Assessment Units as of December 14, 2009: ")),a("View shapefile metadata",href="http://maps.tnc.org/files/metadata/TerrEcos.xml")),
                   p(div(em("Courtesy of the Nature Conservancy and World Wildlife Fund."))),
                   p(div(strong("Marine Ecoregions of the World:")),a("View shapefile metadata",href="http://maps.tnc.org/files/metadata/MEOW.xml")),
                   p(div(em("Courtesy of the Nature Conservancy and World Wildlife Fund.")))
                 )
        ),
        tabPanel("Data Summary",
                 hr(),
                 fluidRow(
                   h3(div(strong("DATA SUMMARY"),style="color:#006699"),align="center"),
                   hr()
                 ),
                 fluidRow(
                   column(2, offset=2,
                          align="center",
                          wellPanel(
                            style="background-color:#c7eae5;",
                            icon("file-text-o","fa-2x"),
                            p(div(strong("TOTAL ARTICLES:"))),
                            h4(div(strong(textOutput("elink_us_3")),style="color:#FF6633"))
                          )
                   ),
                   column(2, offset=1,
                          align="center",
                          wellPanel(
                            style="background-color:#c7eae5;",
                            icon("file-text-o","fa-2x"),
                            p(div(strong("IMPACT EVALS:"))),
                            h4(div(strong(textOutput("elink_ie_3")),style="color:#FF6633"))
                          )
                   ),
                   column(2, offset=1,
                          align="center",
                          wellPanel(
                            style="background-color:#c7eae5;",
                            icon("file-text-o","fa-2x"),
                            p(div(strong("OPEN ACCESS:"))),
                            h4(div(strong(textOutput("elink_oa_3")),style="color:#FF6633"))
                          )
                   ),
                   hr()
                 ),
                 fluidRow(
                   column(6,
                          wellPanel(
                            h4(div(em("Type of conservation intervention")),align="center"),
                            checkboxInput("show_subtypes2",label="Show intervention sub-types",value=FALSE),
                            plotlyOutput("bio_int",height=600)
                          ),
                          br()
                   ),
                   column(6,
                          wellPanel(
                            h4(div(em("Type of human well-being outcome")),align="center"),
                            plotlyOutput("bio_out",height=600)
                          ),
                          br()
                   )
                 ),
                 fluidRow(
                   column(6,
                          wellPanel(
                            h4(div(em("Top studied countries")),align="center"),
                            selectInput("b_show_country",label="Choose number of countries",c("10"=10,"25"=25,"50"=50),selected=10),
                            plotlyOutput("bio_country",height=700)
                          ),
                          br()
                   ),
                   column(6,
                          wellPanel(
                            h4(div(em("Type of study design")),align="center"),
                            plotlyOutput("bio_study",height=600)
                          ),
                          br()
                   )
                 )
        ),
        tabPanel("Data Table",
                 hr(),
                 DT::dataTableOutput("ebiom_table")
        ),
        tabPanel('Bibliography',
                 hr(),
                 DT::dataTableOutput("ebiom_bib")
        )                   
      )
    ),
fluidRow(
  column(4,offset=4,
         align="center",
         hr(),
         actionLink("user_reg4",label="Please register to download data"),
         br(),
         uiOutput("download_opts7"),
         br(),
         uiOutput("download_opts8"),
         br()
         )
  )
)