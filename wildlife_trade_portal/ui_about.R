ameth = function()
  fluidPage(
    fluidRow(
      column(8,
             h2(div(strong("ABOUT THE METHOD"),style="color:#006699"),align="center"),
             br(),
             h3(div(strong("Systematic mapping")),align="center"),
             hr(),
             p("Systematic maps and reviews are transparent, robust, and repeatable methods used to identify and synthesize relevant evidence within a policy-relevant framework. These maps and reviews (SMs and SRs) aim to synthesize large bodies of evidence while minimizing bias in order to provide reliable and comprehensive information for examining impacts and informing decision making. SMs and SRs are widely conducted and used within the medical and public health sector, providing critical syntheses informing best practices and treatment efficacy (Cochrane Collaboration). Systematic maps are particularly useful for assessing the current state of the evidence base, characterizing the research that has been conducted and where key knowledge gaps exist. Both SMs and SRs are increasingly being applied to conservation, environmental management, and development fields (Pullin & Stewart 2006)."),
             br(),
             p("In systematic mapping, the searching and inclusion processes are conducted with the same comprehensive method as for a full review, but the process does not extend to critical appraisal or data synthesis. Data are however extracted from included studies in order to describe important aspects of the studies using a standard template and defined keywords and coding. This approach is designed to capture information on generic variables, such as the country in which a study took place, the population focus, study design and the intervention being assessed. This standard and well-defined set of keywords and codes is essential whenever classifying and characterising studies in order for reviewers to pull out key aspects of each study in a systematic way. Once the research has been mapped in this way it is then possible to identify pools of research which may be used to identify more narrowly defined review questions."),
             p("Source: ",a("CIFOR Guidance on systematic maps",href="http://www.cifor.org/fileadmin/subsites/ebf/pubs/Guidance_Systematic_Maps.pdf")),
             br(),
             p("You can read more about our systematic mapping process and criteria in our working paper (",a("Cheng et al. 2017",href="http://www.conservation.org/publications"),").")
      ),
      column(4,
             h3(div(strong("RESOURCES")),align="center"),
             wellPanel(
               h4(div(strong("Useful links"))),
               div(tags$ul(
                 tags$li(a("Collaboration for Environmental Evidence",href="http://www.environmentalevidence.org")),
                 tags$li(a("Social Care Institute for Excellence (SCIE) systematic map guidelines and examples:",href="http://www.scie.org.uk/research/maps.asp")),
                 tags$li(a("International Initiative for Impact Evaluation (3ie) evidence gap maps",href="http://www.3ieimpact.org")),
                 tags$li(a("Cochrane Collaboration",href="http://www.cochrane.org"))
               )),
               h4(div(strong("Literature"))),
               div(tags$ul(
                 tags$li(a("Haddaway NR, Bernes C, Honsson B, Hedlund K. 2016. The benefits of systematic mapping to evidence-based environmental management. ",em("Ambio."),"1-8",href="http://link.springer.com/article/10.1007%2Fs13280-016-0773-x")),
                 tags$li(a("Pullin AS, Stewart GB. 2006. Guidelines for systematic review in conservation and environmental management. ",em("Conservation Biology."),"20:1647-1656",href="http://onlinelibrary.wiley.com/doi/10.1111/j.1523-1739.2006.00485.x/abstract")),
                 tags$li(a("McKinnon MC, Cheng SH, Garside R, Masuda YJ, Miller DC. 2015. Sustainability: Map the Evidence. ",em("Nature."),"528:185-187",href="http://www.nature.com/news/sustainability-map-the-evidence-1.18962"))
               ))
             )
      )
    )
  )

adata = function()
  fluidPage(
    fluidRow(
      column(2
      ),
      column(8,
             h2(div(strong("ABOUT THE DATA & TOOL"),style="color:#006699"),align="center"),
             br(),
             tabsetPanel(
               type="tabs",
               tabPanel("The Data",
                        h3(div(strong("How the data was compiled")),align="center"),
                        hr(),
                        p("The data in this portal derives from a systematic map of evidence that measures the effectiveness of international wildlife trade actions on behavioral, biological and human well-being outcomes."),
                        p("For more information on specific inclusion and exclusion criteria and parameters of this search, please refer to working paper (", a("Cheng et al. 2017",href="http://www.theoatmeal.com"), ")."),
                        p("For metadata on data columns, please refer to the codebook from Cheng et al. 2017 available ",a("here",href="http://www.theoatmeal.com"),"."),
                        p("For more information about the process of conducting systematic maps, please see the ", span(strong("METHOD")), "tab and detailed guidelines at the Collaboration for Environmental Evidence's ",a("website",href="http://environmentalevidence.org/wp-content/uploads/2014/06/Review-guidelines-version-4.2-final.pdf"))
               ),
               tabPanel("The Tool",
                        br(),
                        h3(div(strong("HOW TO USE THIS TOOL"),style="color:#006699"),align="center"),
                        hr(),
                        p("This is an open access online knowledge management tool designed to help users explore the existing evidence on linkages between nature and people. The evidence collated in this tool is designed to help conservation and development policymakers, practitioners, and researchers identify relevant information for decision-making. To date, there are ~42 peer-reviewed articles, unpublished reports, and theses in the international wildlife trade evidence base that were identified and included using a systematic mapping method."),
                        br(),
                        h4(div(strong("FAQs"),style="color:#006699"),align="center"),
                        br(),
                        h5(div(strong("I am looking for a specific action or outcome, how do I find out what category that falls into?"))),
                        p("The glossary is the best place to figure out what types of actions or outcomes fall into which categories. Since we used either standardized typologies or synthesized frameworks, the definitions stem from those references. Below the definition, you find examples of what types of actions are included. The glossary can be found at the bottom of every page. Click the 'Show/Hide glossary' button to display the panel. You can also find the glossary in the ABOUT section, and then navigating to GLOSSARY."),
                        br(),
                        h5(div(strong("Where do I filter data and explore it? Why are there so many different options?"))),
                        p("This site is constantly in development and we are refining it to make searching for and exploring data as functional and intuitive as possible. As well, while Shiny is a fantastic method for visualizing data, it can be difficult to incorporate multiple elements that it is not designed for. Hence, there are different tabs for exploring data. We encourage you to watch the how-to video on how to explore, filter, and download data."),
                        p("In general, options for exploration are located in the ",span(strong("EXPLORE"),style="color:#006666")," section. In the ",span(strong("EVIDENCE MAP"),style="color:#006666")," tab, you will find an interactive evidence map that highlights the number of articles that document a specific linkage between a type of intervention and a type of outcome. You can filter this map by region, major habitat type, and study type. You can also view the data table behind this map by clicking on the ",span(strong("DATA TABLE"),style="color:#006666")," tab."),
                        p("The ",span(strong("LINKAGE EXPLORER"),style="color:#006666")," affords a different interaction with the map, allowing you dive into a specific linkage and view a bibliography of included articles."),
                        p("The ",span(strong("DASHBOARD"),style="color:#006666")," tab allows for filtering by geographic region, habitat/ecoregion type, intervention type, and outcome type. Here you can view summaries of different variables, explore the full data, and visualize spatially on both choropleth and interactive maps."),
                        br(),
                        h5(div(strong("Are there other efforts to expand from this evidence map or on other topics?"))),
                        p("You can read more about ongoing work to build this evidence base and conduct in-depth syntheses on some of these linkages in the",span(strong("RESEARCH"),style="color:#006666"), "tab"),
                        br(),
                        div(htmlOutput("howto2"),align="center"),
                        br(),
                        h4(div(strong("FEEDBACK"))),
                        p("We are constantly developing and honing features in order to tailor this tool for our end-users. Any feedback would be very much appreciated on functionality and utility. Please see the ",span(strong("CONTACT"),style="color:#006666")," tab to fill out a feedback form or you can directly email the postdoctoral fellow, Samantha Cheng at ",a("cheng@nceas.ucsb.edu",href="cheng@nceas.ucsb.edu"),"."),
                        p("For any questions or comments on the wildlife trade evidence sub-portal, please contact the project leads, ",a("Samantha Cheng",href="cheng@nceas.ucsb.edu"), " and ", a("Michael Mascia",href="mmascia@conservation.org")),
                        br(),
                        h4(div(strong("ACKNOWLEDGMENTS"))),
                        p("The wildlife trade evidence map was funded by the Betty and Gordon Moore Foundation in a grant to Conservation International. This project benefitted from the collective expertise of wildlife experts from Conservation International and evidence synthesis experts from the Evidence-Based Conservation Working Group (part of the ",a("Science for Nature and People Partnership",href="http://snappartnership.net")," consisting of , ",a("the National Center for Ecological Analysis and Synthesis (NCEAS)",href="https://www.nceas.ucsb.edu/"),", ",a("The Nature Conservancy",href="http://www.nature.org"),", and the ",a("Wildlife Conservation Society",href="http://www.wcs.org"),". This sub-portal for the wildlife trade evidence map is part of the ",a("Evidence for Nature and People Data Portal", href="http://www.natureandpeopleevidence.org"),". This larger portal is grateful for technical expertise and computing support from the ",a("scientific computing team at NCEAS",href="https://www.nceas.ucsb.edu/ecoinfo#team")," (Julien Brun, Nick Outin, Ian McCullough, Sarah McCutcheon, Justin Kroes, Mark Schildhauer, & Lauren Walker). This tool was developed and piloted with a number of potential end-users including scientists, researchers, and practitioners from multiple academic, conservation, and development institutions and agencies.")
               ),
               tabPanel("Glossary",
                        fluidRow(
                          br(),
                          p(em("Look up definitions for intervention, outcome, biome, and study design types used to categorize this evidence map. (See ",span(strong("ABOUT"),style="color:#006699")),"for more information)"),
                          hr()
                        ),
                        fluidRow(
                          column(6,
                                 wellPanel(
                                   h4(div(strong("Intervention types"),style="color:#006699"),align="center"),
                                   p("This framework of programs and policies is derived from a synthesis of wildlife trade frameworks (",a("Cooney et al. 2015",href="https://portals.iucn.org/library/sites/library/files/documents/2015-014.pdf")," and ",a("Biggs et al. 2017",href="http://onlinelibrary.wiley.com/doi/10.1111/cobi.12796/full"),") and the ",a("IUCN-Conservation Measures Partnership standardized typology",href="http://cmp-openstandards.org/using-os/tools/actions-taxonomy/")," for conservation actions/intervention types."),
                                   selectInput("intervention_def",
                                               label = "Intervention type:",
                                               choices= list("Establish laws, regulations and codes","Define/refine policies and guidelines for implementation","Detection","Prosecution","Civil action","Promote substitutions","Awareness raising and building","Market-based incentives","Strengthen disincentives for illegal behavior","Increase incentives for stewardship of wildlife","Decrease human-wildlife conflict","Establish spatial areas of protection","Regulate harvest of species of concern","Culturing of species to reduce pressure on wild species")
                                   ),
                                   p(div(strong("Definition:"))),
                                   textOutput("int_def2"),
                                   p(div(strong("Included Actions:"))),
                                   textOutput("int_def3")
                                 )
                          ),
                          column(6,
                                 wellPanel(
                                   h4(div(strong("Outcome types"),style="color:#006699"),align="center"),
                                   p("Behavioural, biological, and human well-being outcomes are considered"),
                                   selectInput("outcome_def",
                                               label = "Outcome type:",
                                               choices = list("Management","Protection","Trade","Behaviour change","Population","Species","Material living standards", "Economic living standards", "Health", "Education", "Social relations", "Security & safety", "Governance & empowerment", "Subjective well-being", "Cultural & Spiritual", "Freedom of choice/action")
                                   ),
                                   p(div(strong("Definition:"))),
                                   textOutput("out_def2"),
                                   p(div(strong("Examples:"))),
                                   textOutput("out_def3")
                                 )
                          )
                        ),
                        fluidRow(
                          column(6,
                                 wellPanel(
                                   h4(div(strong("Study design types"),style="color:#006699"),align="center"),
                                   p("Study design categories were adapted from ",a("Margoluis et al. 2009",href="http://onlinelibrary.wiley.com/doi/10.1002/ev.298/abstract"),"."),
                                   selectInput("study_def",
                                               label = "Study design types",
                                               choices = study_labels
                                   ),
                                   p(div(strong("Definition:"))),
                                   textOutput("study_def2"),
                                   p(div(strong("Examples:"))),
                                   textOutput("study_def3")
                                 )
                          )
                        )
               )
             )
      ),
      column(2
      ),
      br()
    )
  )



