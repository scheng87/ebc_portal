##Interactive webportal for international wildlife trade evidence map

##ui

#ui for beta tool
source("sources.R")
source("ui_pages.R")
source("ui_about.R")
source("ui_explore.R")
#source("ui_biome.R")

shinyUI(
  fluidPage(
    theme="bootstrap.css",
    tags$head(tags$style(HTML("
                              .navbar .navbar-nav {float: right}
                              .navbar .navbar-header {float: right}
                              .navbar .navbar-nav {background-color: #44CDB5}
                              .navbar .navbar-header {background-color: #44CDB5}
                              .navbar {background-color: #44CDB5}
                              .navbar .navbar-default {background-color: #44CDB5}
                              .navbar-collapse {background-color: #994C30}
                              div.hcon {
                                    background-color: rgba(0, 0, 0, 0.5);
                                    width: 675px;
                                    padding: 10px 20px 10px 20px;}
                              div.hcon > h1 {
                                    font-weight: 200;
                                    color: white;
                                    margin-bottom: 0px;
                                    padding-top: 0px}
                              "))
    ),
    tags$head(tags$style(".banner_portal{height:329px;background-image: url('header2_test.png');background-repeat: repeat-y}")),
    fluidRow(
      column(12,
             div(class="banner_portal",style="padding:20px 20px 20px 20px",
                 column(8,
                        align="left",
                        br(),
                        br(),
                        br(),
                        br(),
                        div(class="hcon",h1(div(strong("EVIDENCE FOR NATURE AND PEOPLE"))),p(div(em("DATA PORTAL"),style="color:#F9CC0F;font-size:30px")))
                 ),
                 column(4,
                        align="right",
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        a(img(src="snap-acronym-color-white.png",height=75),href="http://www.snappartnership.net/"),
                        br()
                 )
             )
      )
    ),
    navbarPage("",
               id="mainpage",
               tabPanel("HOME",
                        startp()
               ),
               navbarMenu("ABOUT",
                          tabPanel("The Method",
                                   ameth()
                          ),
                          tabPanel("The Data & Tool",
                                   adata()
                          ),
                          tabPanel("The Group",
                                   agrou()
                                   )
               ),
               navbarMenu("EXPLORE BY:",
                          tabPanel("Evidence Map",
                                   elink()
                                   ),
                          tabPanel("Linkage Explorer",
                                   elinkexplore()
                                   ),
                          tabPanel("Dashboard",
                                   eintout()
                                   )
               ),
               tabPanel("NEWS",
                        newsp()
                        ),
               navbarMenu("RESEARCH",
                          tabPanel("Ongoing syntheses/maps",
                                   researchp()
                          )
               ),
               tabPanel("CONTACT",
                        contactp()
               ),
               tabPanel("USER REGISTRATION",
                        value="regist",
                        fluidRow(
                          column(6, offset="3",
                                 align="center",
                                 h3(div(strong("USER REGISTRATION"),style="color:#006699")),
                                 p("In order to download datasets from this portal, we ask that users complete a registration agreeing to appropriately cite the source of the data as:"),
                                 p(a("McKinnon MC, Cheng SH, Dupre S, Edmond J, Garside R, Glew L, Holland MB, Levine E, Masuda YJ, Miller DC, Oliveria I, Revenaz J, Roe D, Shamer S, Wilkie D, Wongbusarakum S, Woodhouse E. (2016) What are the effects of nature conservation on human well-being? A systematic map of empirical evidence from developing countries. Environmental Evidence 5:8.",href="https://environmentalevidencejournal.biomedcentral.com/articles/10.1186/s13750-016-0058-7")),
                                 p("The Creative Commons Attribution 4.0 International License (",a("https://creativecommons.org/licenses/by/4.0/",href="https://creativecommons.org/licenses/by/4.0/"),") applies to the article and the data made available in this article and displayed in this data portal."),
                                 hr(),
                                 textInput("name", "Name", ""),
                                 textInput("email","Email",""),
                                 textInput("organization","Organization",""),
                                 checkboxInput("agree", "I agree to appropriately cite the source of the data being downloaded if it will be used in any publications.", FALSE),
                                 actionButton("submit", "Submit"),
                                 br(),
                                 hr(),
                                 textOutput("thankyou"),
                                 hr()
                          )
                        )
               )
    ),
    fluidPage(
      shinyjs::useShinyjs(),
      fluidRow(
        br(),
        actionButton("toggle_lu", label="Show/hide glossary"),
        br(),
        hr(),
        hidden(
          column(12,
                 id="gloss",
                 h4(div(strong("GLOSSARY")),align="center"),
                 p(em("Look up definitions for intervention, outcome, biome, and study design types used to categorize this evidence map. Look up abbreviations used to code different variables in the data table. (See ",span(strong("ABOUT"),style="color:#006699")),"for more information)",align="center"),
                 column(3,
                        textInput("code",label=h4(div(strong("CODES"),style="color:#006699"),value="")),
                        p("Type in code abbreviations from data tables to look up code values. E.g. 'sp_mgmt' can be typed in to return it's value, 'species management.'"),
                        hr(),
                        verbatimTextOutput("code_out")
                 ),
                 column(9,
                        fluidRow(
                          column(6,
                                 wellPanel(
                                   h4(div(strong("Intervention types"),style="color:#006699"),align="center"),
                                   p("We use the ", a("IUCN-Conservation Measures Partnership standardized typology",href="http://cmp-openstandards.org/using-os/tools/actions-taxonomy/")," for conservation actions/intervention types."),
                                   selectInput("intervention_def1",
                                               label = "Intervention type:",
                                               choices= list("Area protection", "Resource protection/management","Land/Water management",  "Species management", "Education", "Law & Policy", "Livelihood, economic & other incentives", "External capacity building")
                                   ),
                                   p(div(strong("Definition:"))),
                                   textOutput("int_def21"),
                                   p(div(strong("Included Actions:"))),
                                   textOutput("int_def31")
                                 )
                          ),
                          column(6,
                                 wellPanel(
                                   h4(div(strong("Human well-being outcomes"),style="color:#006699"),align="center"),
                                   p("The outcome categories were synthesized from a number of sources including ",a("Leisher et al. 2013",href="http://www.mdpi.com/2071-1050/5/3/997"),", ",a("Wongbusarakum et al. 2014",href="http://www.conservationgateway.org/ConservationPractices/PeopleConservation/SocialScience/Pages/strengthening-social-impacts.aspx"),", and Masuda et al. 2015"),
                                   selectInput("outcome_def1",
                                               label = "Human well-being outcome:",
                                               choices = list("Material living standards", "Economic living standards", "Health", "Education", "Social relations", "Security & safety", "Governance & empowerment", "Subjective well-being", "Cultural & Spiritual", "Freedom of choice/action")
                                   ),
                                   p(div(strong("Definition:"))),
                                   textOutput("out_def21"),
                                   p(div(strong("Examples:"))),
                                   textOutput("out_def31")
                                 )
                          )
                        ),
                        fluidRow(
                          column(6,
                                 wellPanel(
                                   h4(div(strong("Major habitat type, biome and ecoregion types"),style="color:#006699"),align="center"),
                                   p("We used the World Wildlife Fund ecoregion ",a("definitions",href="http://www.worldwildlife.org/biomes")),
                                   selectInput("biome_def1",
                                               label = "Major habitat types",
                                               choices = list("Marine", "Freshwaters", "Forests", "Grasslands", "Tundra","Deserts","Mangroves")
                                   ),
                                   p(div(strong("Definition:"))),
                                   textOutput("bio_def21"),
                                   p(div(strong("Examples:"))),
                                   textOutput("bio_def31")
                                 )
                          ),
                          column(6,
                                 wellPanel(
                                   h4(div(strong("Study design types"),style="color:#006699"),align="center"),
                                   p("Study design categories were adapted from ",a("Margoluis et al. 2009",href="http://onlinelibrary.wiley.com/doi/10.1002/ev.298/abstract"),"."),
                                   selectInput("study_def1",
                                               label = "Study design types",
                                               choices = study_labels
                                   ),
                                   p(div(strong("Definition:"))),
                                   textOutput("study_def21"),
                                   p(div(strong("Examples:"))),
                                   textOutput("study_def31")
                                 )
                          )
                        )
                 )
          )
        )

      )
    )
    )
)