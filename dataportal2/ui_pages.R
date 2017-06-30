startp = function() 
fluidPage(
  tags$style(HTML("
                  body {
                        background-color: #ffffff;
                  }
                  .intro-divider {
                        height: 6px;
                        background: transparent linear-gradient(to right, #0e2f44 0%, #66cccc 100%) repeat scroll 0% 0%;
                        margin-top: 20px;
                        margin-bottom: 20px;
}
                  ")),
  fluidRow(
    br(),
    br(),
    column(4,
           div(img(src="fishingVillage_circle.png",width="70%",title="Local village fisherman work to catch barely enough fish to make a living selling to the local market in the village of Katumbi on Lake Tanganyika in Tanzania. Credit: ©Ami Vitale"),align="center"),
           hr(),
           wellPanel(
             width=NULL,
             h3(div(strong("ABOUT"),style="color:#006699")),
             p("Welcome to the Evidence for Nature and People Data Portal! This portal features data from a systematic map on the impacts of conservation on human well-being in non-OECD nations (Bottrill et al. 2014, McKinnon et al. 2015, 2016) drawn from 1,038 peer-reviewed and grey literature. Here, you can access the knowledge management tool and various analytic dashboards for slicing the data. As we progress in our different initiatives (see ", span(strong("RESEARCH"),style="color:#006666"), "section), we will update this dashboard with beta versions of support tools and other analytics.")
           )
    ),
    column(4,
           div(img(src="waterTesting_circle.png",width="70%", title="Reserve volunteers take samples of creek water for preservation research in the Santa Rosa Plateau Ecological Reserve, an unspoiled wilderness refuge within the Santa Ana Mountains, southern California. Credit: ©Stephen Francis"),align="center"),
           hr(),
           wellPanel(
             width=NULL,
             h3(div(strong("NEWS"),style="color:#009999")),
             h5(div(em("May 5, 2017"),style="color:#FF6633")),
             p("Thanks to feedback from our users, we've refined how you can play with the data on the ",span(strong("EXPLORE"),style="color:#006666")," page."),
             h5(div(em("May 1, 2017"),style="color:#FF6633")),
             p("New affiliated systematic map protocol on the contribution of forests to poverty alleviation out now! ",a("Read here",href="https://environmentalevidencejournal.biomedcentral.com/articles/10.1186/s13750-017-0088-9"))
           )
    ),
    column(4,
           div(img(src="measuringTree_circle.png",width="70%", title="Carbon monitoring in forest one of the land use types that WAC (ICRAF (now World Agroforestry Centre or WAC) carbon measurement project) is using to determine the carbon load of the entire Berau District. This carbon monitoring project is the “ground-truthing” portion of the REDD project. The object of the carbon measurement project is to determine the carbon load of the 17 different land use types (teak plantation, palm oil and forest – all photographed, also coffee, cocoa, forest concessions (logged), industry forests (planted), etc…). Using GIS they select locations in Berau to represent the different land use types. They then select the site and by throwing a stick locate one corner of the 200 meter transect. After setting it up with a rope, they locate random areas around the transect and using 20 cm transect squares they determine the “wet” weight of the top 0-10 cm, 10-20 cm and 20-30 cm. They weigh the samples, then put them in a bag to take back to the lab. At the lab, they dry the material and weigh the “dry” weight. They will use the carbon load data, together with GPS data of land use types throughout the Berau District to determine the carbon load of the District and the changes over time. This data will be used by the Conservancy and partners in developing the REDD approach in the Berau District. The team includes; Harti Ningsih (only woman-- with scarf around head), Heri Surriyanto (baseball cap and brown T-shirt) Ekowidya Daryono (brown long sleeves) Dayat (white T-shirt) and Yosua Naibaho (glasses and black cap). Credit: ©Bridget Besaw"),align="center"),
           hr(),
           wellPanel(
             width=NULL,
             h3(div(strong("ONGOING"),style="color:#00CC99")),
             h5(div(em("Systematic map on impact of forestry on poverty"),style="color:#FF6633")),
             p(strong("Lead(s):")," World Bank Program on Forestry and Poverty (PROFOR), Conservation International, University of Illinois"),
             p(strong("Projected timeline:")," Present-September 2017"),
             h5(div(em("Systematic map on wildlife trade"),style="color:#FF6633")),
             p(strong("Lead(s):")," Conservation International"),
             p(strong("Status:")," Finished, in press")
           )
    )
  ),
  fluidRow(
    column(8, offset=2,
           br(),
           h3(div(strong("HOW TO USE"),style="color:#006699"),align="center"),
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
           div(htmlOutput("howto"),align="center"),
           br(),
           h4(div(strong("FEEDBACK"))),
           p("We are constantly developing and honing features in order to tailor this tool for our end-users. Any feedback would be very much appreciated on functionality and utility. Please see the ",span(strong("CONTACT"),style="color:#006666")," tab to fill out a feedback form or you can directly email the postdoctoral fellow, Samantha Cheng at ",a("cheng@nceas.ucsb.edu",href="cheng@nceas.ucsb.edu"),"."),
           p("For any questions or comments on the SNAPP Evidence-Based Conservation Group, please contact the project leads, ",a("Madeleine McKinnon",href="mmckinnon@conservation.org"), " and ", a("David Wilkie",href="dwilkie@wcs.org"),". For any questions or comments on this tool, please contact the postdoctoral fellow, ",a("Samantha Cheng",href="cheng@nceas.ucsb.edu"),".")
    )
  )
)

contactp=function()
fluidPage(
  fluidRow(
    column(3
           ),
    column(6,
           h3(div(strong("CONTACT US"),style="color:#006699"),align="center"),
           br(),
           p("For more information on this SNAPP working group, please visit our ",a("website",href="http://snappartnership.net/groups/evidence-based-conservation/",".")),
           p("If you are interested in submitting evidence relevant to this topic, please contact ",a("Madeleine McKinnon",href="mmckinnon@conservation.org")," and ",a("Samantha Cheng",href="cheng@nceas.ucsb.edu"),"."),
           br()
           ),
    column(3
           )
    ),
  fluidRow(
    align="center",
    h3(div(strong("FEEDBACK"),style="color:#006699")),
    tags$iframe(src="https://docs.google.com/forms/d/1fjqt6Zb1igR3UxrMcd4SbBmceZWocPAReT-REgOKYC0/viewform?embedded=true",width="760",height="1100",scrolling="no")
    )
  )

newsp=function()
  fluidPage(
    fluidRow(
      column(1
      ),
      column(10,
             fluidRow(
               h3(div(strong("RECENT NEWS"),style="color:#006699"),align="center"),
               hr(),
               br()
             ),
             fluidRow(
               column(4,
                      wellPanel(
                        style="background-color:#eef8f7;",
                        img(src="news1.jpg",width="100%"),
                        h4(div(strong("Systematic map published in Environmental Evidence"))),
                        p(div(em("May 2, 2016"),style="color:#FF6633")),
                        p("Madeleine McKinnon and colleagues detail insights and patterns in the evidence base on the impact of conservation on huan well-being, along with the full dataset and bibliography. The data from this article forms the basis of this data portal."),
                        p(strong(a("Read more",href="https://environmentalevidencejournal.biomedcentral.com/articles/10.1186/s13750-016-0058-7")))
                      )
               ),
               column(4,
                      wellPanel(
                        style="background-color:#c7eae5;",
                        img(src="news2.jpg",width="100%"),
                        h4(div(strong("Conservation International/DataKind team featured on DataKind blog"))),
                        p(div(em("April 15, 2016"),style="color:#FF6633")),
                        p("DataCorps Team and their work on automating the search, screening and data extraction process in systematic mapping and reviewing for conservation decision making."),
                        p(strong(a("Read more",href="http://www.datakind.org/blog/protecting-the-planet-to-help-communities-thrive")))
                      )
               ),
               column(4,
                      wellPanel(
                        style="background-color:#80cdc1;",
                        img(src="news3.jpg",width="100%"),
                        h4(div(strong("SNAPP team has a productive working group meeting in Santa Barbara"))),
                        p(div(em("February 28, 2016"),style="color:#FF6633")),
                        p("SNAPP working group has their first meeting in Santa Barbara to set the agenda for the next two years.")
                      )
               )
             )
      ),
      column(1
      )
    )
  )

researchp=function()
  fluidPage(
    fluidRow(
      column(1
      ),
      column(10,
             fluidRow(
               h3(div(strong("ONGOING RESEARCH, MAPS, REVIEWS"),style="color:#006699"),align="center"),
               hr(),
               column(2
               ),
               column(8,
                      p("If you are interested in learning more about these ongoing projects, see associated links and/or contact the respective project lead.",align="center")
               ),
               column(2
               ),
               br()
             ),
             fluidRow(
               column(4,
                      wellPanel(
                        h4(div(strong("SYSTEMATIC MAP:"),style="color:#009999")),
                        h4(div(em("Forests' contribution to poverty alleviation"),style="color:#00CC99")),
                        p(strong("Partners:"),"World Bank-Program on Forests (PROFOR), Conservation International, University of Illinois"),
                        p(strong("Project lead(s):"), "Sofia Ahlroth (World Bank), Stefanie Sieber (World Bank)"),
                        p(strong("Project team:"),"Samantha Cheng (NCEAS), Kavita MacLeod"),
                        p(strong("Contact: "),a("Sofia Ahlroth",href="mailto:sahlroth@worldbank.org")," or ",a("Samantha Cheng",href="mailto:cheng@nceas.ucsb.edu")),
                        p(strong("Status:"),"Ongoing"),
                        p(strong("Links:"),a("Systematic map protocol",href="https://environmentalevidencejournal.biomedcentral.com/articles/10.1186/s13750-017-0088-9")),
                        a("World Bank Program on Forests (PROFOR) Partnership",href="http://www.profor.info/")
                      )
               ),
               column(4,
                      wellPanel(
                        h4(div(strong("SYSTEMATIC MAP:"),style="color:#009999")),
                        h4(div(em("Effectiveness of wildlife trade policies and programs"),style="color:#00CC99")),
                        p(strong("Partners:"),"Conservation International (CI), International Union for Conservation of Nature (IUCN)"),
                        p(strong("Project lead:"), "Neil Cox (CI/IUCN), Madeleine McKinnon (CI), Samantha Cheng (NCEAS)"),
                        p(strong("Project team:"),", Annette Olsen (CI), Duan Biggs (University of Queensland, Janine Robinson (University of Kent), Michael Mascia (CI)"),
                        p(strong("Contact:"),a("Michael Mascia",href="mailto:mmascia@conservation.org")," or ",a("Samantha Cheng",href="mailto:cheng@nceas.ucsb.edu")),
                        p(strong("Status:"),"Finished"),
                        p(strong("Links:")),
                        p(em("Working paper in press"))
                      )
               ),
               column(4,
                      wellPanel(
                        h4(div(strong("SYSTEMATIC REVIEW:"),style="color:#009999")),
                        h4(div(em("Role of social equity on human well-being outcomes in conservation"),style="color:#00CC99")),
                        p(strong("Partners:"),"University of Queensland, Conservation International, National Center for Ecological Analysis & Synthesis"),
                        p(strong("Project lead:"), "Glenn Althor (University of Queensland)"),
                        p(strong("Project team:"),"Carissa Klein (Univ. Queensland), James Watson (Univ. Queensland), Samantha Cheng (NCEAS), Madeleine McKinnon (Conservation International)"),
                        p(strong("Contact:"),a("Glenn Althor",href="mailto:g.althor@uq.edu.au")),
                        p(strong("Status:"),"Ongoing"),
                        p(strong("Links:")),
                        p(a("Systematic review protocol",href="https://environmentalevidencejournal.biomedcentral.com/articles/10.1186/s13750-016-0078-3"))
                      )
               )
             ),
             fluidRow(
               column(4,
                      wellPanel(
                        h4(div(strong("DATA SCIENCE"),style="color:#009999")),
                        h4(div(em("Automating searching, identifying, and filtering relevant evidence"),style="color:#00CC99")),
                        p(strong("Partners:"),"DataKind, Conservation International"),
                        p(strong("Project lead:"), "Madeleine McKinnon (Conservation International), Samantha Cheng (NCEAS), Peter Darche (DataKind), JeanCarlo Bonilla (DataKind)"),
                        p(strong("Project team:"),"Caitlin Augustin (Kaplan), Bob Minnich (Columbia University), Burton DeWilde (Chartbeat), Sam Anzaroot (Dataminr), Ray Shah"),
                        p(strong("Contact:"),a("Caitlin Augustin",href="mailto:c.augustin@umiami.edu")," or ",a("Samantha Cheng",href="mailto:cheng@nceas.ucsb.edu")),
                        p(strong("Status:"),"Ongoing"),
                        p(strong("Links:")),
                        a("Datakind Blog",href="http://www.datakind.org/blog/protecting-the-planet-to-help-communities-thrive"),
                        p("Machine learning enabled evidence synthesis tool: ",a("Colandr app",href="http://www.colandrapp.org"))
                      )
               ),
               column(4,
                      wellPanel(
                        h4(div(strong("SYSTEMATIC MAP"),style="color:#009999")),
                        h4(div(em("Evidence for impact of ecosystem services on human health"),style="color:#00CC99")),
                        p(strong("Partners:"),"University of Exeter, University of Illinous, NCEAS"),
                        p(strong("Project lead:"), "Ruth Garside (Conservation International), Daniel Miller (University of Illinois), Samantha Cheng (NCEAS)"),
                        p(strong("Project team:")),
                        p(strong("Contact:")),
                        p(strong("Status:"),"Prospective"),
                        p(strong("Links:")),
                        p(em("Forthcoming"))
                      )
               ),
               column(4,
                      wellPanel(
                        h4(div(strong("SYSTEMATIC MAP"),style="color:#009999")),
                        h4(div(em("Impact of marine ecosystem-based management on ecosystem and human well-being"),style="color:#00CC99")),
                        p(strong("Partners:"),"NCEAS, Conservation International, World Wildlife Fund (WWF), National Oceanic & Atmospheric Administration (NOAA)"),
                        p(strong("Project lead:"), "Samantha Cheng (NCEAS)"),
                        p(strong("Project team:"),"Supin Wongbusarakum (NOAA), Louise Glew (WWF), Madeleine McKinnon (CI)"),
                        p(strong("Contact:")),
                        p(strong("Status:"),"Prospective"),
                        p(strong("Links:")),
                        p(em("Forthcoming"))
                      )
               )
             ),
             fluidRow(
               column(4,
                      wellPanel(
                        h4(div(strong("SYSTEMATIC MAP & REVIEWS"),style="color:#009999")),
                        h4(div(em("Evidence on community-based natural resource management (CBNRM) impacts"),style="color:#00CC99")),
                        p(strong("Partners:"),"Conservation International, World Wildlife Fund (WWF), Wildlife Conservation Society (WCS)"),
                        p(strong("Project lead:"), "Madeleine McKinnon (CI), Michael Mascia (CI)"),
                        p(strong("Project team:"),"David Wilkie (WCS), Louise Glew (WWF), Samantha Cheng (NCEAS)"),
                        p(strong("Contact:")),
                        p(strong("Status:"),"Ongoing/Prospective"),
                        p(strong("Links:")),
                        p(em("Forthcoming"))
                      )
               ),
               column(4
               ),
               column(4
               )
             )
      ),
      column(1
      )
    )
  )
