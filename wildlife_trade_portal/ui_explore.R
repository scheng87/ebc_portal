elink = function()
  fluidPage(
    tags$head(tags$style(".rightAlign{float:right;}")),
    fluidRow(
      column(3,
          wellPanel(
            style="background-color:##d5e2ea",
            h4(div(strong("FILTERING OPTIONS"),style="color:#0e2f44")),
            selectInput("region1",
                               label="Choose region(s) to display:",
                        c("All","Africa","Asia","Europe","Latin America","Northern America","Oceania","Global","Unknown"),
                        selected=c("All")
            ),
            selectInput("supplychain",
                        label="Choose part(s) of supply chain being studied:",
                        choices=list("All"="All","Supply-side" = "supply", "Trade controls" = "trade","End-market" = "consumer"),
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
               column(3, offset=2,
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
      h5(div(em("Choose filters to display custom summary tables, datasets, and interactive maps.")))
    ),
    fluidRow(
      column(3, offset=2,
             align="center",
             wellPanel(
               style="background-color:#c7eae5;",
               icon("file-text-o","fa-2x"),
               p(div(strong("TOTAL ARTICLES:"))),
               h4(div(strong(textOutput("elink_us_2")),style="color:#FF6633"))
             )
      ),
      column(3, offset=1,
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
               column(6, offset=3,
                      selectInput("eintout_mht","Species habitat type",
                                  c("All"="All",
                                    "Freshwater"="Freshwater",
                                    "Marine"="Marine",
                                    "Terrestrial"="Terrestrial",
                                    "Unknown"="Unknown"),
                                  selected="All"
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
                                  c("All"="All","Enforcement/compliance"="Enforcement/compliance","Establish/refine laws & policies"="Establish/refine laws & policies","Reduce demand/consumption"="Reduce demand/consumption","Reduce threats to species"="Reduce threats to species", "Support livelihoods" = "Support livelihoods"),
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
               column(6,
                      selectInput("eintout_outgroup","Outcome group",
                                  c("All"="All","Behavioural"="Behavioural","Biological"="Biological","Human well-being"="Human well-being"),
                                  selected="All"
                                  )
                      ),
               column(6,
                      selectInput("eintout_outtype","Outcome sub-type",
                                  c("All subtypes="="All")
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
                            checkboxInput("show_subtypes_o",label="Show outcome sub-types",value=FALSE),
                            plotlyOutput("e_out")
                          )
                          ),
                   column(6,
                          wellPanel(
                            p(div(em("Study designs")),align="center",style="font-size:9pt"),
                            plotlyOutput("e_comp")
                          ),
                          wellPanel(
                            p(div(em("Supply chain action")),align="center",style="font-size:9pt"),
                            plotlyOutput("e_eco")
                          )
                          )
                 ),
                 fluidRow(
                   column(6,
                          wellPanel(
                            p(div(em("Species targets")),align="center",style="font-size:9pt"),
                            plotlyOutput("e_spec")
                          )
                          ),
                   column(6,
                          wellPanel(
                            p(div(em("Uses")),align="center",style="font-size:9pt"),
                            plotlyOutput("e_use")
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