elinkexplore = function()
  fluidPage(
    theme=shinytheme("flatly"),
    fluidRow(
      h3(div(strong("LINKAGE EXPLORER"),style="color:#006699"),align="center"),
      hr(),
      h5("This tool is designed to allow you to investigate specific linkages using an interactive heatmap."),
      h5(div(em("Click on cells within the heatmap to display included literature for that linkage. This will open a new page with links to included literature and options to download specific bibliographies."))),
      h5(div(em("Click on the intervention or outcome labels to sort the heatmap based on your selection."))),
      br(),
      htmlOutput("map_page"),
      p("This interactive map tool was created by Ian McCullough with assistance from Julien Brun, Mark Schildhauer and Lauren Walker at the National Center for Ecological Analysis and Synthesis (NCEAS).")
      )
    )

elink = function()
  fluidPage(
    theme=shinytheme("flatly"),
    tags$head(tags$style(".rightAlign{float:right;}")),
    fluidRow(
      column(3,
          wellPanel(
           h4(div(strong("FILTERING OPTIONS"),style="color:#006699")),
           radioButtons("region",
                       label="Choose region to display:",
                       choices=list("All","Africa","Asia","Europe","Latin America","Oceania"),
                       selected="All"),
           radioButtons("biome",
                       label="Choose major habitat type to display:",
                       choices=list("All"="ALL" ,"Marine"="MAR","Freshwater"="FRW","Mangroves" ="MAN","Tundra"="TUN","Forests"="FOR","Grasslands"="GRS","Deserts"="DES"), selected="ALL"),
           h5("Additional viewing options:"),
           checkboxInput("expand",label="Expand intervention groups to specific actions", value=FALSE),
           br(),
           h4(div(strong("DOWNLOAD OPTIONS"),style="color:#006699")),
           actionLink("user_reg1",label="Please register to download data"),
           hr(),
           uiOutput("download_opts1"),
           br(),
           uiOutput("download_opts2"),
           br()
           )
      ),
      column(9,
             fluidRow(
               column(3,
                      align="center",
                      wellPanel(
                        style="background-color:#c7eae5;",
                        icon("file-text-o","fa-2x"),
                        p(div(strong("TOTAL ARTICLES:"))),
                        h4(div(strong(textOutput("elink_us")),style="color:#FF6633"))
                      )
               ),
               column(3, offset=1,
                      align="center",
                      wellPanel(
                        style="background-color:#c7eae5;",
                        icon("file-text-o","fa-2x"),
                        p(div(strong("IMPACT EVALUATIONS:"))),
                        h4(div(strong(textOutput("elink_ie")),style="color:#FF6633"))
                      )
               ),
               column(3, offset=1,
                      align="center",
                      wellPanel(
                        style="background-color:#c7eae5;",
                        icon("file-text-o","fa-2x"),
                        p(div(strong("OPEN ACCESS:"))),
                        h4(div(strong(textOutput("elink_oa")),style="color:#FF6633"))
                      )
               ),
               hr()
               ),
             fluidRow(
               column(12,
                      hr(),
                      tabsetPanel(
                        type="tabs",
                        tabPanel("Evidence Map",
                                 h3("Evidence map of conservation-human well-being linkages"),
                                 hr(),
                                 h5(div(em("Numbers within the evidence map indicate how many unique articles document that nature-based intervention and human well-being outcome linkage. Filter the evidence map by desired region and biome on the left panel."))),
                                 plotOutput("heatmap",height=562,width=825)
                        ),
                        tabPanel("Data Table",
                                 br(),
                                 h4("View associated data behind the filtered evidence map."),
                                 hr(),
                                 DT::dataTableOutput("map_data")
                        )
                      )
                      )
               )
             )
    )
  )


eglob = function()
  fluidPage(
    theme = shinytheme("flatly"),
    fluidRow(
      column(3,
             wellPanel(
               h4(div(strong("FILTERING OPTIONS"),style="color:#006699")),
               p("*OECD countries are omitted from study (grey)"),
               p("No data for selected parameters indicated in white"),
               p(em("Filter map by intervention and outcome type")),
               selectInput("intervention_c",
                           label = "Conservation intervention type:",
                           choices= list("All"="all","Area management"="area_mgmt","Area protection"="area_protect","Resource management"="res_mgmt","Restoration"="restoration","Species management" = "sp_mgmt","Species recovery"="sp_recov","Species control"="sp_control","Species reintroduction"="sp_reint","Ex-situ conservation" = "ex_situ","Awareness & communication"="aware_comm","Formal education"="form_ed", "Training"="training","Compliance & enforcement"="compl_enfor","Legislation" = "legis","Policies & regulations" = "pol_reg","Livelihood alternatives" = "liv_alt","Market forces" = "market","Non-monetary values" = "non_mon","Private sector standards & codes"="priv_codes","Substitution"="sub","Conservation finance"="cons_fin","Institutional & civil development"="inst_civ_dev","Partnership & alliance development"="part_dev","Sustainable use"="sus_use","Other"="other"),
                           selected = "all"),
               selectInput("outcome_c",
                           label = "Human well-being outcome:",
                           choices = list("All"="all","Economic living standards"="eco_liv_std", "Material living standards"="mat_liv_std", "Health"="health", "Education"="education", "Social relations"="soc_rel", "Security & safety"="sec_saf", "Governance & empowerment"="gov", "Subjective well-being"="sub_well", "Culture & spirituality"="culture", "Freedom of choice/action"="free_choice", "Other"="other"),
                           selected = "all"),
               p(em("Filter map by biome type")),
               selectInput("biome_filter",
                           label= "Biome category:",
                           choices=list("All"="ALL" ,"Marine"="MAR","Freshwater"="FRW","Mangroves" ="MAN","Tundra"="TUN","Forests"="FOR","Grasslands"="GRS","Deserts"="DES"),
                           selected="ALL"),
               actionLink("user_reg2",label="Please register to download data"),
               hr(),
               uiOutput("download_opts3"),
               br(),
               uiOutput("download_opts4"),
               br()
             )
      ),
      column(9,
             br(),
             h3(div(strong("GLOBAL DISTRIBUTION OF EVIDENCE BASE"),style="color:#006699"),align="center"),
             hr(),
             plotlyOutput("map_plotly"),
             hr(),
             h4(div(em("Included literature"),style="color:#006699"),align="center"),
             h5("Click on a country on the map to display associated literature below."),
             hr(),
             DT::dataTableOutput("biblio_glob")
             )
      )
   )

eregi = function()
  fluidPage(
    theme = shinytheme("flatly"),
    fluidRow(
      h3(div(strong("REGION/COUNTRY SUMMARIES"),style="color:#006699"),align="center"),
      hr(),
      br(),
      h5(div(em("Choose a region, subregion, or country below to display summary tables, data subsets and interactive maps."))),
      hr(),
      column(3,
             selectInput("eregi_region","Region",
                         c("All regions"="All",
                           "Africa" = "Africa",
                           "Asia" = "Asia",
                           "Latin America & the Caribbean" = "Latin America",
                           "Oceania" = "Oceania",
                           "Europe" = "Europe")
             )
      ),
      column(3,
             selectInput("eregi_subreg","Subregion",c("All subregions"="All"))
             ),
      column(3,
             selectInput("eregi_country","Country", c("All countries"="All"))
             ),
      column(3,
             br(),
             actionLink("user_reg3",label="Please register to download data"),
             hr(),
             uiOutput("download_opts5"),
             br(),
             uiOutput("download_opts6"),
             br()
             )
    ),
    fluidRow(
      tabsetPanel(
        type="tabs",
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
                            h4(div(strong(textOutput("elink_us_2")),style="color:#FF6633"))
                          )
                   ),
                   column(2, offset=1,
                          align="center",
                          wellPanel(
                            style="background-color:#c7eae5;",
                            icon("file-text-o","fa-2x"),
                            p(div(strong("IMPACT EVALS:"))),
                            h4(div(strong(textOutput("elink_ie_2")),style="color:#FF6633"))
                          )
                   ),
                   column(2, offset=1,
                          align="center",
                          wellPanel(
                            style="background-color:#c7eae5;",
                            icon("file-text-o","fa-2x"),
                            p(div(strong("OPEN ACCESS:"))),
                            h4(div(strong(textOutput("elink_oa_2")),style="color:#FF6633"))
                          )
                   ),
                   hr()
                 ),
                 fluidRow(
                   column(6,
                          wellPanel(
                            h4(div(em("Type of conservation intervention")),align="center"),
                            checkboxInput("show_subtypes",label="Show intervention sub-types",value=FALSE),
                            plotlyOutput("regi_int",height=600)
                            ),
                          br()
                   ),
                   column(6,
                          wellPanel(
                            h4(div(em("Type of human well-being outcome")),align="center"),
                            plotlyOutput("regi_out",height=600)
                            ),
                          br()
                   )
                 ),
                 fluidRow(
                   column(6,
                          wellPanel(
                            h4(div(em("Major habitat type")),align="center"),
                            selectInput("show_ecoregions",label="Show ecoregions/biomes",c("All ecoregions"="ALL","Forests"="FOR","Grasslands"="GRS","Tundra"="TUN","Deserts"="DES","Mangroves"="MAN","Marine"="MAR","Freshwater"="FRW"),selected="ALL"),
                            plotlyOutput("regi_bio",height=700)
                            ),
                          br()
                   ),
                   column(6,
                          wellPanel(
                            h4(div(em("Type of study design")),align="center"),
                            plotlyOutput("regi_study",height=600)
                            ),
                          br()
                          )
                   )
                 ),
        tabPanel("Data Table",
                 hr(),
                 DT::dataTableOutput("eregi_table")
                 ),
        tabPanel('Bibliography',
                 hr(),
                 DT::dataTableOutput("eregi_bib")
                 ),
        tabPanel("Interactive Map",
                 fluidRow(
                   h5(div(em("Map will only display if a subregion or country are chosen above."))),
                   column(8,
                          br(),
                          h5(div(em("Circle markers on map indicate total number of studies in each country that document the variables chosen on the right. Toggle these markers on and off using the legend to the right."))),
                          br(),
                          plotlyOutput("regi_map")
                          ),
                   column(4,
                          wellPanel(
                            h4(div(em("Choose variable to plot:"))),
                            selectInput("eregi_map_type","Show variable group:",choices=c("Intervention types"="int_group","Intervention sub-types"="Int_type","Human well-being outcomes"="Outcome","Major habitat type"="biome_group","Ecoregions/biomes"="Biome."))
                          )
                   )
                 )
        )
        )
      )
  )

# etime = function()
#   fluidPage(
#     theme = shinytheme("flatly"),
#     fluidRow(
#       align="center",
#       h2(div(strong("NATURE AND PEOPLE OVER TIME"),style="color:#006699"),align="center"),
#       hr(),
#       h3(em("Global distribution of articles on conservation's impact on human well-being over time")),
#       plotlyOutput("timemap"),
#       sliderInput("year1","Publication Year:",min=1990,max=2015,value=1990,step=1,animate=TRUE),
#       h3(em("Distribution of research on linkages between conservation and human well-being over time")),
#       plotOutput("timematrix",height=562,width=825),
#       sliderInput("year2","Publication Year:",min=1990,max=2015,value=1990,step=1,animate=TRUE)
#       )
#     )
