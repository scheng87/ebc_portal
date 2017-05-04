elink = function()
  fluidPage(
    tags$head(tags$style(".rightAlign{float:right;}")),
    fluidRow(
      column(3,
          wellPanel(
            style="background-color:##d5e2ea",
            h4(div(strong("FILTERING OPTIONS"),style="color:#0e2f44")),
            checkboxGroupInput("region",
                               label="Choose region(s) to display:",
                               c("Africa","Asia","Europe","Latin America","Northern America","Oceania","Global","Unknown"),
                               selected=c("Africa","Asia","Europe","Latin America","Northern America","Oceania","Global","Unknown")
            ),
            checkboxGroupInput("supplychain",
                               label="Choose part(s) of supply chain being studied:",
                               choices = c("Supply-side" = "supply", "Trade controls" = "trade","End-market" = "consumer"),
                               selected = c("supply","trade","consumer")
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

eintout = function()
  fluidPage(
    fluidRow(
      h3(div(strong("EXPLORE THE EVIDENCE MAP"),style="color:#006699"),align="center"),
      hr(),
      br(),
      h5(div(em("Choose a intervention, outcome and/or region below to display summary tables, data subsets and interactive maps."))),
      hr(),
      column(3,
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
      column(3,
             selectInput("eintout_int","Intervention sub-type",
                         c("All interventions"="all","Laws, regulations & codes" = "laws","Policies & regulations" = "policies","Detection"="detection","Prosecution"="prosecution","Civil action"="civil","Substitution"="substitution","Awareness raising"="awareness","Market-based incentives"="market","Disincentives for illegal behavior"="disincentive","Incentives for stewardship of wildlife"="stewardship","Decrease human-wildlife conflict"="conflict","Spatial areas of protection"="spat_protect","Regulate harvest"="harvest_reg","Culturing of species"="culture"
                           )
                         )
             ),
      column(3,
             selectInput("eintout_out","Outcome sub-type", 
                         c("All outcomes"="all","Management","Protection","Trade","Behavior change","Population","Species","Economic living standards","Material living standards","Health","Education","Social relations","Security and safety","Governance and empowerment","Subjective well-being","Culture & spirituality","Freedom of choice & action"
                           )
                         )
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
                   column(12,
                          wellPanel(
                            h4(div(em("Type of conservation intervention")),align="center"),                          
                            plotlyOutput("e_int",height=600)
                            ),
                          br()
                   )
                 ),
                 fluidRow(
                   column(12,
                          wellPanel(
                            h4(div(em("Types of outcome")),align="center"),
                            plotlyOutput("e_out",height=600)
                          ),
                          br()
                   )
                  ),
                 fluidRow(
                   column(6,
                          wellPanel(
                            h4(div(em("Comparators used")),align="center"),
                            plotlyOutput("e_comp",height=600)
                            ),
                          br()
                   ),
                   column(6,
                          wellPanel(
                            h4(div(em("Type of study design")),align="center"),
                            plotlyOutput("e_study",height=600)
                            ),
                          br()
                          )
                   ),
                 fluidRow(
                   column(6,
                          wellPanel(
                            h4(div(em("Countries studied")),align="center"),
                            plotlyOutput("e_country",height=600)
                          ),
                          br()
                   ),
                   column(6,
                          wellPanel(
                            h4(div(em("Use of target species")),align="center"),
                            plotlyOutput("e_purpose",height=600)
                          ),
                          br()
                   )
                 )
                 ),
        tabPanel("Data Table",
                 hr(),
                 DT::dataTableOutput("e_table")
                 )
#         tabPanel("Interactive Map",
#                  fluidRow(
#                    h5(div(em("Map will only display if a subregion or country are chosen above."))),
#                    column(8,
#                           br(),
#                           h5(div(em("Circle markers on map indicate total number of studies in each country that document the variables chosen on the right. Toggle these markers on and off using the legend to the right."))),
#                           br(),
#                           plotlyOutput("regi_map")
#                           ),
#                    column(4,
#                           wellPanel(
#                             h4(div(em("Choose variable to plot:"))),
#                             selectInput("eregi_map_type","Show variable group:",choices=c("Intervention types"="int_group","Intervention sub-types"="Int_type","Human well-being outcomes"="Outcome","Major habitat type"="biome_group","Ecoregions/biomes"="Biome."))
#                           )
#                    )
#                  )
#         )
        )
      )
  )

eglob = function()
  fluidPage(
    fluidRow(
      column(12,
             checkboxInput('showFiltering',"Show filtering options",FALSE),
             conditionalPanel(condition='input.showFiltering',
                              h4(div(strong("FILTERING OPTIONS"),style="color:#006699")),
                              p("No data for selected parameters indicated in white"),
                              column(3,
                                     wellPanel(
                                       p(em("Filter map by intervention sub-type")),
                                       checkboxGroupInput("intervention_c","Intervention sub-type",
                                                          choices=c("Laws, regulations & codes" = "laws","Policies & regulations" = "policies","Detection"="detection","Prosecution"="prosecution","Civil action"="civil","Substitution"="substitution","Awareness raising"="awareness","Market-based incentives"="market","Disincentives for illegal behavior"="disincentive","Incentives for stewardship of wildlife"="stewardship","Decrease human-wildlife conflict"="conflict","Spatial areas of protection"="spat_protect","Regulate harvest"="harvest_reg","Culturing of species"="culture"),
                                                          selected=c("Laws, regulations & codes" = "laws","Policies & regulations" = "policies","Detection"="detection","Prosecution"="prosecution","Civil action"="civil","Substitution"="substitution","Awareness raising"="awareness","Market-based incentives"="market","Disincentives for illegal behavior"="disincentive","Incentives for stewardship of wildlife"="stewardship","Decrease human-wildlife conflict"="conflict","Spatial areas of protection"="spat_protect","Regulate harvest"="harvest_reg","Culturing of species"="culture")
                                                          )
                                       )
                                     ),
                              column(3,
                                     wellPanel(
                                       p(em("Filter map by outcome sub-type")),
                                       checkboxGroupInput("outcome_c","Outcome sub-type", 
                                                          choices=c("Management","Protection","Trade","Behavior change","Population","Species","Economic living standards","Material living standards","Health","Education","Social relations","Security and safety","Governance and empowerment","Subjective well-being","Culture & spirituality","Freedom of choice & action"),
                                                          selected=c("Management","Protection","Trade","Behavior change","Population","Species","Economic living standards","Material living standards","Health","Education","Social relations","Security and safety","Governance and empowerment","Subjective well-being","Culture & spirituality","Freedom of choice & action")
                                                          )
                                       )
                                     ),
                              column(3,
                                     wellPanel(
                                        p(em("Filter map by part of the supply chain studied")),
                                        checkboxGroupInput("supplychain_c",
                                                           label="Choose part(s) of supply chain being studied:",
                                                           choices = c("Supply-side" = "supply", "Trade controls" = "trade","End-market" = "consumer"),
                                                           selected = c("supply","trade","consumer")
                                        )
                                       )
                                     ),
                              column(3,
                                     wellPanel(
                                       actionLink("user_reg2",label="Please register to download data"),
                                       hr(),
                                       uiOutput("download_opts3"),
                                       br(),
                                       uiOutput("download_opts4"),
                                       br()
                                       )
                                     )
                                )
               
      )
    ),
    fluidRow(
      column(12,
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
#
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
