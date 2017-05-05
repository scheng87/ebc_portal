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
             p("You can read more about our systematic mapping process and criteria in our protocol in Environmental Evidence (",a("Bottrill et al. 2014",href="http://environmentalevidencejournal.biomedcentral.com/articles/10.1186/2047-2382-3-16"),").")
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
                        p("The data in this portal was generated using a peer-reviewed systematic map protocol (",a("Bottrill et al. 2014",href="http://environmentalevidencejournal.biomedcentral.com/articles/10.1186/2047-2382-3-16"),") and documented in ",a("McKinnon et al. 2016",href="https://environmentalevidencejournal.biomedcentral.com/articles/10.1186/s13750-016-0058-7"),". This systematic map consists of articles that measure some impact of a nature-based conservation intervention (as defined by the IUCN-Conservation Measures Partnership (IUCN-CMP) Unified Classification of Conservation Actions) on aspects of human well-being. This map is limited to studies that examined these impacts in non-OECD countries."),
                        p("For more information on specific inclusion and exclusion criteria and parameters of this search, please refer to the systematic map protocol in Environmental Evidence (", a("Bottrill et al. 2014",href="http://environmentalevidencejournal.biomedcentral.com/articles/10.1186/2047-2382-3-16"), ")."),
                        p("For metadata on data columns, please refer to the codebook from McKinnon et al. 2014 available ",a("here",href="https://static-content.springer.com/esm/art%3A10.1186%2Fs13750-016-0058-7/MediaObjects/13750_2016_58_MOESM2_ESM.xlsx"),"."),
                        p("For more details on specific categorizations of intervention type, human well-being outcome, major habitat types, ecoregion/biome types, and study design types, please see the ",span(strong("GLOSSARY"))," tab."),
                        p("For more information about the process of conducting systematic maps, please see the ", span(strong("METHOD")), "tab and detailed guidelines at the Collaboration for Environmental Evidence's ",a("website",href="http://environmentalevidence.org/wp-content/uploads/2014/06/Review-guidelines-version-4.2-final.pdf"))
               ),
               tabPanel("The Tool",
                        br(),
                        h3(div(strong("HOW TO USE THIS TOOL"),style="color:#006699"),align="center"),
                        hr(),
                        p("This is an open access online knowledge management tool designed to help users explore the existing evidence on linkages between nature and people. The evidence collated in this tool is designed to help conservation and development policymakers, practitioners, and researchers identify relevant information for decision-making. To date, there are ~1,000 peer-reviewed articles, unpublished reports, and theses in the evidence base that were identified and included using a systematic mapping method. You can read more about the motivation behind this project, the data and methodology, and this group in the ", span(strong("ABOUT"),style="color:#006666"), "tab above."),
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
                        p("For any questions or comments on the SNAPP Evidence-Based Conservation Group, please contact the project leads, ",a("Madeleine McKinnon",href="mmckinnon@conservation.org"), " and ", a("David Wilkie",href="dwilkie@wcs.org"),". For any questions or comments on this tool, please contact the postdoctoral fellow, ",a("Samantha Cheng",href="cheng@nceas.ucsb.edu"),"."),
                        br(),
                        h4(div(strong("ACKNOWLEDGMENTS"))),
                        p("The concept for the Evidence for Nature and People Data Portal was conceived by the Evidence-Based Conservation Working Group. This portal is supported by the ",a("Science for Nature and People Partnership",href="http://snappartnership.net")," consisting of , ",a("the National Center for Ecological Analysis and Synthesis (NCEAS)",href="https://www.nceas.ucsb.edu/"),", ",a("The Nature Conservancy",href="http://www.nature.org"),", and the ",a("Wildlife Conservation Society",href="http://www.wcs.org"),". Without the technical expertise and computing support from the ",a("scientific computing team at NCEAS",href="https://www.nceas.ucsb.edu/ecoinfo#team")," (Julien Brun, Nick Outin, Ian McCullough, Sarah McCutcheon, Justin Kroes, Mark Schildhauer, & Lauren Walker) this tool would not have been possible. This tool was developed and piloted with a number of potential end-users including scientists, researchers, and practitioners from multiple academic, conservation, and development institutions and agencies.")
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
                                   p("We use the ", a("IUCN-Conservation Measures Partnership standardized typology",href="http://cmp-openstandards.org/using-os/tools/actions-taxonomy/")," for conservation actions/intervention types."),
                                   selectInput("intervention_def",
                                               label = "Intervention type:",
                                               choices= list("Area protection", "Resource protection/management","Land/Water management",  "Species management", "Education", "Law & Policy", "Livelihood, economic & other incentives", "External capacity building")
                                   ),
                                   p(div(strong("Definition:"))),
                                   textOutput("int_def2"),
                                   p(div(strong("Included Actions:"))),
                                   textOutput("int_def3")
                                 )
                          ),
                          column(6,
                                 wellPanel(
                                   h4(div(strong("Human well-being outcomes"),style="color:#006699"),align="center"),
                                   p("The outcome categories were synthesized from a number of sources including ",a("Leisher et al. 2013",href="http://www.mdpi.com/2071-1050/5/3/997"),", ",a("Wongbusarakum et al. 2014",href="http://www.conservationgateway.org/ConservationPractices/PeopleConservation/SocialScience/Pages/strengthening-social-impacts.aspx"),", and Masuda et al. 2015"),
                                   selectInput("outcome_def",
                                               label = "Human well-being outcome:",
                                               choices = list("Material living standards", "Economic living standards", "Health", "Education", "Social relations", "Security & safety", "Governance & empowerment", "Subjective well-being", "Cultural & Spiritual", "Freedom of choice/action")
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
                                   h4(div(strong("Major habitat type, biome and ecoregion types"),style="color:#006699"),align="center"),
                                   p("We used the World Wildlife Fund ecoregion ",a("definitions",href="http://www.worldwildlife.org/biomes")),
                                   selectInput("biome_def",
                                               label = "Major habitat types",
                                               choices = list("Marine", "Freshwaters", "Forests", "Grasslands", "Tundra","Deserts","Mangroves")
                                   ),
                                   p(div(strong("Definition:"))),
                                   textOutput("bio_def2"),
                                   p(div(strong("Examples:"))),
                                   textOutput("bio_def3")
                                 )
                          ),
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

agrou = function()
  fluidPage(
    fluidRow(
      column(6,
             h2(div(strong("ABOUT THE GROUP"),style="color:#006699"),align="center"),
             hr(),
             p("We are the Science for Nature and People Partnership (SNAPP) Evidence-Based Conservation working group. We are an interdisciplinary group of scientists and practitioners from a diverse set of academic institutions, conservation and development organizations, and government agencies. Our aim is to document the state of current evidence on the impact of conservation on human well-being and provide tools and syntheses to improve evidence-based decision making in conservation. You can read more about our group and research at the ",a("SNAPP website",href="http://snappartnership.net/groups/evidence-based-conservation/"),"."),
             img(src="SNAP.jpg",height=400,width=600)
      ),
      column(6,
             h3(div(strong("WORKING GROUP MEMBERS"),style="color:#006699"),align="center"),
             hr(),
             br(),
             column(4,
                    p(a("Madeleine McKinnon (Lead)",href="mailto:mmckinnon@conservation.org")),
                    p(em("Conservation International")),
                    br(),
                    p(a("David Wilkie (Co-Lead)",href="mailto:dwilkie@wcs.org")),
                    p(em("Wildlife Conservation Society")),
                    br(),
                    p(a("Samantha Cheng (Postdoctoral Fellow)",href="mailto:cheng@nceas.ucsb.edu")),
                    p(em("National Center for Ecological Analysis & Synthesis (NCEAS)")),
                    br(),
                    p(a("Sofia Ahlroth",href="mailto:sahlroth@worldbank.org")),
                    p(em("The World Bank")),
                    br(),
                    p(a("Glenn Althor",href="mailto:g.althor@uq.edu.au")),
                    p(em("University of Queensland")),
                    br(),
                    p(a("Rebecca Butterfield",href="mailto:rbutterfield@usaid.gov")),
                    p(em("USAID")),
                    br(),
                    p(a("Chris Cooper",href="mailto:christopher.cooper@exeter.ac.uk")),
                    p(em("University of Exeter")),
                    br(),
                    p(a("Peter Darche",href="mailto:pete@datakind.org")),
                    p(em("DataKind")),
                    br(),
                    p(a("David Gill",hred="mailto:dgill@conservation.org")),
                    p(em("Conservation International"))
             ),
             column(4,
                    p(a("Lynn Dicks",href="mailto:lvd22@cam.ac.uk")),
                    p(em("University of Cambridge")),
                    br(),
                    p(a("Louise Glew",href="mailto:louise.glew@wwfus.org")),
                    p(em("World Wildlife Fund-US")),
                    br(),
                    p(a("Ruth Garside",href="mailto:r.garside@exeter.ac.uk")),
                    p(em("University of Exeter")),
                    br(),
                    p(a("Valerie Hickey",href="mailto:vhickey@worldbank.org")),
                    p(em("The World Bank")),
                    br(),
                    p(a("Kelly Jones",href="mailto:kelly.jones@colostate.edu")),
                    p(em("Colorado State University")),
                    br(),
                    p(a("Maggie Holland",href="mailto:mholland@umbc.edu")),
                    p(em("University of Maryland – Baltimore County")),
                    br(),
                    p(a("Eliot Levine",href="mailto:elevine@mercycorps.org")),
                    p(em("Mercy Corps")),
                    br(),
                    p(a("Yuta Masuda",href="mailto:ymasuda@tnc.org")),
                    p(em("The Nature Conservancy")),
                    br(),
                    p(a("Alison Bethel", href="mailto:A.Bethel@exeter.ac.uk")),
                    p(em("University of Exeter")),
                    br()
             ),
             column(4,
                    p(a("Daniel Miller",href="mailto:dcmiller@illinois.edu")),
                    p(em("University of Illinois")),
                    br(),
                    p(a("Andrew Pullin",href="mailto:a.s.pullin@bangor.ac.uk")),
                    p(em("University of Bangor, Wales")),
                    br(),
                    p(a("Dilys Roe",href="mailto:dilys.roe@iied.org")),
                    p(em("International Institute for Environment & Development")),
                    br(),
                    p(a("Birte Snilstveit",href="mailto:bsnilstveit@3ieimpact.org")),
                    p(em("International Impact Initiative (3ie)")),
                    br(),
                    p(a("Bill Sutherland",href="mailto:w.sutherland@zoo.cam.ac.uk")),
                    p(em("University of Cambridge")),
                    br(),
                    p(a("Emily Woodhouse",href="mailto:e.woodhouse@ucl.ac.uk")),
                    p(em("University College London and Imperial College")),
                    br(),
                    p(a("Supin Wongbusarakum",href="mailto:supin.wongbusarakum@noaa.gov")),
                    p(em("National Oceanic and Atmospheric Administration")),
                    br(),
                    p(a("Caitlin Augustin", href="mailto:caitlin.augustin@datakind.org")),
                    p(em("DataKind")),
                    br()
             ),
             br()
      )
    )
  )



