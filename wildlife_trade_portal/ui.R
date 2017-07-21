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
                              .navbar .navbar-nav {background-color: #ff7f50}
                              .navbar .navbar-header {background-color: #ff7f50}
                              .navbar {background-color: #ff7f50}
                              .navbar .navbar-default {background-color: #ff7f50}
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
    tags$head(tags$style(".banner_portal{height:329px;background-image: url('Ngorongoro_Spitzmaulnashorn_edit1.jpg');background-repeat: no-repeat}")),
    fluidRow(
      column(12,
             div(class="banner_portal",style="padding:20px 20px 20px 20px",
                 column(8,
                        align="left",
                        br(),
                        br(),
                        br(),
                        br(),
                        div(class="hcon",h1(div(strong("INTERNATIONAL WILDLIFE TRADE"))),p(div(em("EVIDENCE PORTAL"),style="color:#fa8072;font-size:30px")))
                 ),
                 column(4,
                        align="right",
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        a(img(src="ci_logo.png",height=75),href="http://www.conservation.org/"),
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
                          )
               ),
               navbarMenu("EXPLORE BY:",
                          tabPanel("Evidence Map",
                                   elink()
                                   ),
                          tabPanel("Dashboard",
                                   eintout()
                                   )
               ),
               tabPanel("NEWS",
                        newsp()
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
                                 p(a("Cheng SH, Robinson JE, Cox N, Biggs D, Olsson A, Mascia M, McKinnon MC. (2017) Mapping the evidence: effectiveness of international wildlife trade practices and policies. Conservation International Working Paper Series: 1.",href="https://www.conservation.org/publications/")),
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
                                   p("Categories and subcategories of programs and policies targeted at regulating internation wildlife trade with framework derived from a synthesis of wildlife trade frameworks (Cooney et al. 2015, Biggs et al. 2016) and the IUCN-CMP Conservation Actions Classifcation (version 2.0)"),
                                   selectInput("intervention_def1",
                                               label = "Intervention type:",
                                               choices= list("Establish laws, regulations and codes", "Define/refine policies and guidelines for implementation","Detection", "Prosecution", "Civil action", "Promote substitutions", "Awareness raising and building", "Market-based incentives", "Strengthen disincentives for illegal behavior", "Increase incentives for stewardship of wildlife", "Decrease human-wildlife conflict", "Establish spatial areas of protection", "Regulate harvest of species of concern","Culturing of species to reduce pressure on wild species")
                                   ),
                                   p(div(strong("Definition:"))),
                                   textOutput("int_def21"),
                                   p(div(strong("Included Actions:"))),
                                   textOutput("int_def31")
                                 )
                          ),
                          column(6,
                                 wellPanel(
                                   h4(div(strong("Outcomes "),style="color:#006699"),align="center"),
                                   p("This evidence map examines impacts on behavioural (changes in conservation behaviour), biological (changes in populations or species of concern), and human well-being outcomes (changes in social and economic outcomes for human populations)."),
                                   selectInput("outcome_def1",
                                               label = "Outcome type:",
                                               choices = list("Management (behavioural)"="Management","Protection (behavioural)" ="Protection","Trade (behavioural)"="Trade","Behaviour change (behavioural)"="Behaviour change","Population (biological)"="Population","Species (biological)"="Species","Economic living standards (well-being)"="Economic living standards","Material living standards (well-being)"="Material living standards","Health (well-being)"="Health","Education (well-being)"="Education","Social relations (well-being)"="Social relations","Security and safety (well-being)"="Security and safety", "Governance and empowerment (well-being)"="Governance and empowerment","Subjective well-being (well-being)"="Subjective well-being", "Culture & spirituality (well-being)"="Culture & spirituality","Freedom of choice & action (well-being)"="Freedom of choice & action")
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
                                               choices = c("Experimental","Quasi-experimental","Non-experimental","Non-systematic review","Systematic review")
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