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
    column(8, offset=2,
           br(),
           p(div("Welcome to the data portal for International Wildlife Trade (a sub-portal of the ", a("Evidence for Nature and People Data Portal",href="http://www.natureandpeopleevidence.org"),". This sub-portal features data from a systematic map on the effectiveness of international wildlife trade practices and policies drawn from 42 peer-reviewed and grey literature sources (", a("Cheng et al. 2017",href="http://www.conservation.org/publications/"),"). Here, you can access the knowledge management tool and various analytic dashboards for filtering and visualizing the data.", style="font-size:20px;color:#0e2f44;"),align="left"),
           div(class = "intro-divider"),
           br(),
           h3(div(strong("HOW TO USE"),style="color:#006699"),align="center"),
           hr(),
           p("This is an open access online knowledge management tool designed to help users explore the existing evidence on linkages between nature and people. The evidence collated in this tool is designed to help conservation and development policymakers, practitioners, and researchers identify relevant information for decision-making. You can read more about the motivation behind this project, and the data and methodology, ", span(strong("ABOUT"),style="color:#006666"), "tab above."),
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
           div(htmlOutput("howto"),align="center"),
           br(),
           h4(div(strong("FEEDBACK"))),
           p("We are constantly developing and honing features in order to tailor this tool for our end-users. Any feedback would be very much appreciated on functionality and utility. Please see the ",span(strong("CONTACT"),style="color:#006666")," tab to fill out a feedback form or you can directly email Samantha Cheng at ",a("cheng@nceas.ucsb.edu",href="cheng@nceas.ucsb.edu"),"."),
           p("For any questions or comments on the wildlife trade evidence map, please contact the project leads, ",a("Samantha Cheng",href="cheng@nceas.ucsb.edu"), " and ", a("Michael Mascia",href="mmascia@conservation.org"))
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
           p("For more information on Conservation International, please visit our ",a("website",href="http://www.conservation.org/",".")),
           p("If you are interested in submitting evidence relevant to this topic, please contact ",a("Samantha Cheng",href="cheng@nceas.ucsb.edu")," and ",a("Michael Mascia",href="mmascia@conservation.org"),"."),
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
                        h4(div(strong("Evidence map published as first in Conservation International Working Paper Series"))),
                        p(div(em("June XX, 2017"),style="color:#FF6633")),
                        p("Samantha Cheng, Michael Mascia, and colleagues detail insights and patterns in the evidence base on the effectiveness of international wildlife trade practices and policies, along with the full dataset and bibliography. The data from this article forms the basis of this sub-portal."),
                        p(strong(a("Read more",href="https://www.conservation.org/publications/")))
                      )
               )
             )
      )
    )
  )
