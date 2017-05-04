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
                        p("The data in this portal derives from a systematic map of evidence that measures the effectiveness of international wildlife trade actions on conservation, biological and socio-economic outcomes."),
                        p("For more information on specific inclusion and exclusion criteria and parameters of this search, please refer to working paper (", a("Cheng et al. 2017",href="http://www.theoatmeal.com"), ")."),
                        p("For metadata on data columns, please refer to the codebook from Cheng et al. 2017 available ",a("here",href="http://www.theoatmeal.com"),"."),
                        p("For more information about the process of conducting systematic maps, please see the ", span(strong("METHOD")), "tab and detailed guidelines at the Collaboration for Environmental Evidence's ",a("website",href="http://environmentalevidence.org/wp-content/uploads/2014/06/Review-guidelines-version-4.2-final.pdf"))
               ),
               tabPanel("The Tool",
                        h3(div(strong("How to use this tool")),align="center"),
                        hr(),
                        p("This is an open access online knowledge management tool designed to help users explore the existing evidence on the effectiveness of international wildlife trade actions. The evidence collated in this tool is designed to help conservation and development policymakers, practitioners, and researchers identify relevant information for decision-making. To date, there are 42 peer-reviewed articles, unpublished reports, and theses in the evidence base that were identified and included using a systematic mapping method. This systematic map was commissioned by ",a("Conservation International",href="http://www.conservation.org"),"."),
                        p("Click ", span(strong("EXPLORE"),style="color:#006666"), "to filter and visualize patterns in the evidence base and download subsets of the data and relevant bibliographies. Most of the generated plots on this page are made using Plotly, therefore you can use the controls in the upper right corner of each graph to toggle and adjust the image, as well as download them. Any data, graphs, or summaries downloaded from this tool must be cited as ",a("Cheng et al. 2017",href="http://www.theoatmeal.com"),"."),
                        br(),
                        h4(div(strong("Short video tutorial on using the EBC data portal")),align="center"),
                        div(htmlOutput("howto2"),align="center"),
                        br(),
                        p("This tool was built using the R Shiny platform from RStudio (",a("RStudio 2015",href="http://shiny.rstudio.com"),") and the Plotly (",a("https://plot.ly/r/",href="https://plot.ly/r/"),") and Leaflet (",a("http://leafletjs.com/",href="http://leafletjs.com/"),") APIs for R. The source code for this tool will be open access and made available on Github in following months."),
                        br(),
                        h4(div(strong("FEEDBACK"))),
                        p("As this tool is designed to help users find and explore usable data, feedback would be very much appreciated on functionality and utility. Please see the ",span(strong("CONTACT"),style="color:#006666")," tab to fill out a feedback form or you can directly email Samantha Cheng at ",a("cheng@nceas.ucsb.edu",href="cheng@nceas.ucsb.edu"),"."),
                        p("For any questions or comments, please contact the project leads, ",a("Samantha Cheng",href="cheng@nceas.ucsb.edu"), " and ", a("Michael Mascia",href="mmascia@conservation.org"),"."),
                        br(),
                        h4(div(strong("ACKNOWLEDGMENTS"))),
                        p("This work is supported by ", a("Conservation International",href="http://www.conservation.org")," and ",a("the Betty and Gordon Moore Foundation",href="http://www.moore.org"),". This portal is supported by them and in part by the ",a("Science for Nature and People Partnership Evidence Based Conservation Working Group",href="http://snappartnership.net/groups/evidence-based-conservation/")," consisting of , ",a("the National Center for Ecological Analysis and Synthesis (NCEAS)",href="https://www.nceas.ucsb.edu/"),", ",a("The Nature Conservancy",href="http://www.nature.org"),", and the ",a("Wildlife Conservation Society",href="http://www.wcs.org"),". Without the technical expertise and computing support from the ",a("scientific computing team at NCEAS",href="https://www.nceas.ucsb.edu/ecoinfo#team")," (Julien Brun, Nick Outin, Ian McCullough, Sarah McCutcheon, Justin Kroes, Mark Schildhauer, & Lauren Walker) this tool would not have been possible. This tool was developed and piloted with a number of potential end-users including scientists, researchers, and practitioners from multiple academic, conservation, and development institutions and agencies.")
               )
             )
             ),
      column(2
             ),
      br()
      )
    )

# agrou = function()
#   fluidPage(
#     fluidRow(
#       column(6,
#              h2(div(strong("ABOUT THE GROUP"),style="color:#006699"),align="center"),
#              hr(),
#              p("We are the Science for Nature and People Partnership (SNAPP) Evidence-Based Conservation working group. We are an interdisciplinary group of scientists and practitioners from a diverse set of academic institutions, conservation and development organizations, and government agencies. Our aim is to document the state of current evidence on the impact of conservation on human well-being and provide tools and syntheses to improve evidence-based decision making in conservation. You can read more about our group and research at the ",a("SNAPP website",href="http://snappartnership.net/groups/evidence-based-conservation/"),"."),
#              img(src="SNAP.jpg",height=400,width=600)
#              ),
#       column(6,
#              h3(div(strong("WORKING GROUP MEMBERS"),style="color:#006699"),align="center"),
#              hr(),
#              br(),
#              column(4,
#                     p(a("Madeleine McKinnon (Lead)",href="mailto:mmckinnon@conservation.org")),
#                     p(em("Conservation International")),
#                     br(),
#                     p(a("David Wilkie (Co-Lead)",href="mailto:dwilkie@wcs.org")),
#                     p(em("Wildlife Conservation Society")),
#                     br(),
#                     p(a("Samantha Cheng (Postdoctoral Fellow)",href="mailto:cheng@nceas.ucsb.edu")),
#                     p(em("National Center for Ecological Analysis & Synthesis (NCEAS)")),
#                     br(),
#                     p(a("Sofia Ahlroth",href="mailto:sahlroth@worldbank.org")),
#                     p(em("The World Bank")),
#                     br(),
#                     p(a("Glenn Althor",href="mailto:g.althor@uq.edu.au")),
#                     p(em("University of Queensland")),
#                     br(),
#                     p(a("Rebecca Butterfield",href="mailto:rbutterfield@usaid.gov")),
#                     p(em("USAID")),
#                     br(),
#                     p(a("Chris Cooper",href="mailto:christopher.cooper@exeter.ac.uk")),
#                     p(em("University of Exeter")),
#                     br(),
#                     p(a("Peter Darche",href="mailto:pete@datakind.org")),
#                     p(em("DataKind"))
#              ),
#              column(4,
#                     p(a("Lynn Dicks",href="mailto:lvd22@cam.ac.uk")),
#                     p(em("University of Cambridge")),
#                     br(),
#                     p(a("Louise Glew",href="mailto:louise.glew@wwfus.org")),
#                     p(em("World Wildlife Fund-US")),
#                     br(),
#                     p(a("Ruth Garside",href="mailto:r.garside@exeter.ac.uk")),
#                     p(em("University of Exeter")),
#                     br(),
#                     p(a("Valerie Hickey",href="mailto:vhickey@worldbank.org")),
#                     p(em("The World Bank")),
#                     br(),
#                     p(a("Kelly Jones",href="mailto:kelly.jones@colostate.edu")),
#                     p(em("Colorado State University")),
#                     br(),
#                     p(a("Maggie Holland",href="mailto:mholland@umbc.edu")),
#                     p(em("University of Maryland – Baltimore County")),
#                     br(),
#                     p(a("Eliot Levine",href="mailto:elevine@mercycorps.org")),
#                     p(em("Mercy Corps")),
#                     br(),
#                     p(a("Yuta Masuda",href="mailto:ymasuda@tnc.org")),
#                     p(em("The Nature Conservancy")),
#                     br()
#              ),
#              column(4,
#                     p(a("Daniel Miller",href="mailto:dcmiller@illinois.edu")),
#                     p(em("University of Illinois")),
#                     br(),
#                     p(a("Andrew Pullin",href="mailto:a.s.pullin@bangor.ac.uk")),
#                     p(em("University of Bangor, Wales")),
#                     br(),
#                     p(a("Dilys Roe",href="mailto:dilys.roe@iied.org")),
#                     p(em("International Institute for Environment & Development")),
#                     br(),
#                     p(a("Birte Snilstveit",href="mailto:bsnilstveit@3ieimpact.org")),
#                     p(em("International Impact Initiative (3ie)")),
#                     br(),
#                     p(a("Bill Sutherland",href="mailto:w.sutherland@zoo.cam.ac.uk")),
#                     p(em("University of Cambridge")),
#                     br(),
#                     p(a("Emily Woodhouse",href="mailto:e.woodhouse@ucl.ac.uk")),
#                     p(em("University College London and Imperial College")),
#                     br(),
#                     p(a("Supin Wongbusarakum",href="mailto:supin.wongbusarakum@noaa.gov")),
#                     p(em("National Oceanic and Atmospheric Administration")) 
#              ),
#              br()
#              )
#       )
#     )



