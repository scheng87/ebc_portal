elink = function()
  fluidPage(
    tags$head(tags$style(".rightAlign{float:right;}")),
    fluidRow(
      column(3,
          wellPanel(
            style="background-color:##d5e2ea",
            h4(div(strong("FILTERING OPTIONS"),style="color:#0e2f44")),
            selectInput("region",
                               label="Choose region(s) to display:",
                               c("All","Africa","Asia","Europe","Latin America","Northern America","Oceania","Global","Unknown"="0"),
                               selected=c("All")
            ),
            selectInput("biome",
                               label="Choose major habitat type to display:",
                               choices=list("All"="All","Marine"="MAR","Freshwater"="FRW","Mangroves" ="MAN","Tundra"="TUN","Forests"="FOR","Grasslands"="GRS","Deserts"="DES"),
                               selected= c("All")
            ),
            selectInput("comp",
                        label="Choose a comparison type to display:",
                        choices=list("All"="All","Before/After, Control/Intervention"="BACI","Before/after"="BA","Change over time"="CT","Comparison group"="CG","Before/After, comparison group"="BACG","Comparison group, control/intervention"="CGCI","No comparator/Unknown"="None"),
                        selected="All"
                        ),
            h5("Additional viewing options:"),
            checkboxInput("expand",label="Expand intervention groups to specific actions", value=FALSE),
            br(),
            h4(div(strong("DOWNLOAD OPTIONS"),style="color:#0e2f44")),
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
                        style="background-color:#31698a;",
                        icon("file-text-o","fa-2x"),
                        p(div(strong("TOTAL ARTICLES:"),style="color:#ffffff")),
                        h4(div(strong(textOutput("elink_us")),style="color:#ff7f50"))
                      )
               ),
               column(3, offset=1,
                      align="center",
                      wellPanel(
                        style="background-color:#31698a;",
                        icon("file-text-o","fa-2x"),
                        p(div(strong("IMPACT EVALUATIONS:"),style="color:#ffffff")),
                        h4(div(strong(textOutput("elink_ie")),style="color:#ff7f50"))
                      )
               ),
               column(3, offset=1,
                      align="center",
                      wellPanel(
                        style="background-color:#31698a;",
                        icon("file-text-o","fa-2x"),
                        p(div(strong("OPEN ACCESS:"),style="color:#ffffff")),
                        h4(div(strong(textOutput("elink_oa")),style="color:#ff7f50"))
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

elinkexplore = function()
  fluidPage(
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

eintout = function()
  fluidPage(
    fluidRow(
      h3(div(strong("EXPLORE THE EVIDENCE MAP"),style="color:#006699"),align="center"),
      hr(),
      h5(div(em("Choose filters to display custom summary tables, datasets, and interactive maps.")))
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
      )
    ),
    fluidRow(
      hr(),
      column(4,
             br(),
             p("GEOGRAPHIC FILTERS",style="font-size:12pt"),
             style="background-color:#40867a",
             p("Filter data by region, subregion, and country",style="font-size:9pt"),
             fluidRow(
               column(6,
                      selectInput("eintout_region","Region",
                                  c("All regions"="All",
                                    "Africa" = "Africa",
                                    "Asia" = "Asia",
                                    "Latin America & the Caribbean" = "Latin America",
                                    "Oceania" = "Oceania",
                                    "Europe" = "Europe",
                                    "Unknown" = "Unknown",
                                    "Global" = "Global"),
                                  selected="All"
                                  )
                      ),
               column(6,
                      selectInput("eintout_subreg","Subregion",c("All subregions"="All")
                                  )
                      )
             ),
             fluidRow(
               column(6, offset=3,
                      selectInput("eintout_country","Country",c("All countries"="All")
                                  )
                      )
             )
             ),
      column(4,
             fluidRow(
               style="background-color:#59BCAB",
               column(12,
                      br(),
                      p("HABITAT/ECOREGION FILTERS",style="font-size:12pt"),
                      p("Filter data by major habitat type and/or ecoregions",style="font-size:9pt")
                      )
             ),
             fluidRow(
               style="background-color:#59BCAB",
               column(6,
                      selectInput("eintout_mht","Major Habitat Type",
                                  c("All"="All",
                                    "Marine"="MAR",
                                    "Freshwater"="FRW",
                                    "Mangroves" ="MAN",
                                    "Tundra"="TUN",
                                    "Forests"="FOR",
                                    "Grasslands"="GRS",
                                    "Deserts"="DES"),
                                  selected="All"
                                  )
                      ),
               column(6,
                      selectInput("eintout_ecoreg","Ecoregion",c("All ecoregions"="All")
                                  )
                      )
             ),
             fluidRow(
               style="background-color:#ffffff",
               column(12,
                      align="center",
                      br(),
                      actionButton("user_reg3",label="Please register to download data",style="align=center"),
                      uiOutput("download_opts5"),
                      uiOutput("download_opts6"),
                      br()
                      )
             )
             ),
      column(4,
             br(),
             style="background-color:#9fe6da",
             p("ACTION/OUTCOME FILTERS",style="font-size:12pt"),
             p("Filter by interventions and outcomes",style="font-size:9pt"),
             fluidRow(
               column(6,
                      selectInput("eintout_intgroup","Intervention group",
                                  c("All"="All","Area protection"="area_protect","Resource management"="res_mgmt","Land/Water management"="land_wat_mgmt","Species management"="species_mgmt", "Education & Awareness" = "education", "Law & Policy"="law_policy", "Livelihood, Economic, & Other Incentives"="liv_eco_inc", "External Capacity Building"="ext_cap_build", "Sustainable Use"="sus_use", "Other"="other"),
                                  selected="All"
                                  )
                      ),
               column(6,
                      selectInput("eintout_inttype","Intervention sub-type",
                                  c("All subtypes"="All")
                                  )
                      )
             ),
             fluidRow(
               column(6, offset=4,
                      selectInput("eintout_out","Outcome sub-type",
                                  c("All"="All","Economic living standards"="eco_liv_std", "Material living standards"="mat_liv_std", "Health"="health", "Education"="education", "Social relations"="soc_rel", "Security & safety"="sec_saf", "Governance & empowerment"="gov", "Subjective well-being"="sub_well", "Culture & spirituality"="culture", "Freedom of choice/action"="free_choice", "Other"="other"),
                                  selected="All"
                                  )
                      )
             )
             )
    ),
    fluidRow(
      hr(),
      tabsetPanel(
        type="tabs",
        tabPanel("Data Summary",
                 hr(),
                 fluidRow(
                   column(6,
                          wellPanel(
                            p(div(em("Type of conservation intervention")),align="center",style="font-size:9pt"),
                            checkboxInput("show_subtypes",label="Show intervention sub-types",value=FALSE),
                            plotlyOutput("e_int")
                            ),
                          br(),
                          wellPanel(
                            p(div(em("Types of outcome")),align="center",style="font-size:9pt"),
                            plotlyOutput("e_out")
                          )
                          ),
                   column(6,
                          wellPanel(
                            p(div(em("Study types")),align="center",style="font-size:9pt"),
                            plotlyOutput("e_comp")
                          ),
                          wellPanel(
                            p(div(em("Ecoregions")),align="center",style="font-size:9pt"),
                            plotlyOutput("e_eco")
                          )
                          )
                 )
        ),
        tabPanel("Data Table",
                 hr(),
                 DT::dataTableOutput("e_table")
                 ),
        tabPanel("Choropleth map",
                 fluidRow(
                   column(12,
                          br(),
                          h3(div(strong("GLOBAL DISTRIBUTION OF EVIDENCE BASE"),style="color:#006699"),align="center"),
                          hr(),
                          plotlyOutput("map_plotly")
                   )
                 )
                 ),
        tabPanel("Interactive map",
                 p("UPCOMING")
                 )
      )
    )
  )

# eglob = function()
#   fluidPage(
#     fluidRow(
#       column(12,
#              checkboxInput('showFiltering',"Show filtering options",FALSE),
#              conditionalPanel(condition='input.showFiltering',
#                               h4(div(strong("FILTERING OPTIONS"),style="color:#006699")),
#                               p("No data for selected parameters indicated in white"),
#                               column(3,
#                                      wellPanel(
#                                        p(em("Filter map by intervention sub-type")),
#                                        checkboxGroupInput("intervention_c","Intervention sub-type",
#                                                           choices=c("Laws, regulations & codes" = "laws","Policies & regulations" = "policies","Detection"="detection","Prosecution"="prosecution","Civil action"="civil","Substitution"="substitution","Awareness raising"="awareness","Market-based incentives"="market","Disincentives for illegal behavior"="disincentive","Incentives for stewardship of wildlife"="stewardship","Decrease human-wildlife conflict"="conflict","Spatial areas of protection"="spat_protect","Regulate harvest"="harvest_reg","Culturing of species"="culture"),
#                                                           selected=c("Laws, regulations & codes" = "laws","Policies & regulations" = "policies","Detection"="detection","Prosecution"="prosecution","Civil action"="civil","Substitution"="substitution","Awareness raising"="awareness","Market-based incentives"="market","Disincentives for illegal behavior"="disincentive","Incentives for stewardship of wildlife"="stewardship","Decrease human-wildlife conflict"="conflict","Spatial areas of protection"="spat_protect","Regulate harvest"="harvest_reg","Culturing of species"="culture")
#                                                           )
#                                        )
#                                      ),
#                               column(3,
#                                      wellPanel(
#                                        p(em("Filter map by outcome sub-type")),
#                                        checkboxGroupInput("outcome_c","Outcome sub-type", 
#                                                           choices=c("Management","Protection","Trade","Behavior change","Population","Species","Economic living standards","Material living standards","Health","Education","Social relations","Security and safety","Governance and empowerment","Subjective well-being","Culture & spirituality","Freedom of choice & action"),
#                                                           selected=c("Management","Protection","Trade","Behavior change","Population","Species","Economic living standards","Material living standards","Health","Education","Social relations","Security and safety","Governance and empowerment","Subjective well-being","Culture & spirituality","Freedom of choice & action")
#                                                           )
#                                        )
#                                      ),
#                               column(3,
#                                      wellPanel(
#                                         p(em("Filter map by part of the supply chain studied")),
#                                         checkboxGroupInput("supplychain_c",
#                                                            label="Choose part(s) of supply chain being studied:",
#                                                            choices = c("Supply-side" = "supply", "Trade controls" = "trade","End-market" = "consumer"),
#                                                            selected = c("supply","trade","consumer")
#                                         )
#                                        )
#                                      ),
#                               column(3,
#                                      wellPanel(
#                                        actionLink("user_reg2",label="Please register to download data"),
#                                        hr(),
#                                        uiOutput("download_opts3"),
#                                        br(),
#                                        uiOutput("download_opts4"),
#                                        br()
#                                        )
#                                      )
#                                 )
#                
#       )
#     ),
#     fluidRow(
#       column(12,
#              br(),
#              h3(div(strong("GLOBAL DISTRIBUTION OF EVIDENCE BASE"),style="color:#006699"),align="center"),
#              hr(),
#              plotlyOutput("map_plotly"),
#              hr(),
#              h4(div(em("Included literature"),style="color:#006699"),align="center"),
#              h5("Click on a country on the map to display associated literature below."),
#              hr(),
#              DT::dataTableOutput("biblio_glob")
#              )
#       )
#     )