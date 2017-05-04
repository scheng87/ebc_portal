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
    column(10, offset=1,
           br(),
           p(div("Welcome to the data portal for International Wildlife Trade. This portal features data from a systematic map on the effectiveness of international wildlife trade practices and policies drawn from 42 peer-reviewed and grey literature sources (Cheng et al. 2017). Here, you can access the knowledge management tool and various analytic dashboards for filtering and visualizing the data.", style="font-size:20px;color:#0e2f44;"),align="left"),
           div(class = "intro-divider"),
           p("This is an open access online knowledge management tool designed to help users explore the existing evidence on the effectiveness of international wildlife trade practices and policies for achieving conservation, biological and socio-economic outcomes. The evidence collated in this tool is designed to help you identify relevant information for decision-making. To date, there are 42 peer-reviewed articles, unpublished reports, and theses in the evidence base that were identified and included using a systematic mapping method. You can read more about how this data was collected and synthesized in the ", span(strong("ABOUT"),style="color:#ff7f50"), "tab above.",style="color:#0e2f44"),
           br(),
           p("Click ", span(strong("EXPLORE"),style="color:#ff7f50"), "to filter and visualize patterns in the evidence base. Graphs and figures can be downloaded as .png files and tables (data and bibliographies) can be downloaded as .csv files. Information on how to find and explore and download data is available in the video below. Any data, graphs, or summaries downloaded from this tool must be cited as ", span("Cheng et al. 2017",style="color:#ff7f50"), ". Please register as a user to download data.",style="color:#0e2f44"),
           br(),
           p("The ",span(strong("GLOSSARY"), style="color:#ff7f50")," at the bottom of the pages allows you to look up definitions of different terms and look up abbreviations used in the data tables.",style="color:#0e2f44"),
           column(5,
                  br(),
                  br(),
                  br(),
                  br(),
                  wellPanel(
                    style="background-color:#0e2f44",
                    br(),
                    p("We are constantly developing and honing features in order to tailor this tool for our end-users. Any feedback would be very much appreciated on functionality and utility. Please see the ",span(strong("CONTACT"),style="color:#ff7f50")," tab to fill out a feedback form or you can directly email Samantha Cheng at ",a("cheng@nceas.ucsb.edu",href="cheng@nceas.ucsb.edu"),".",style="color:#ffffff"),
                    br()
                  )
                  ),
           column(7,
                  h4(div(strong("HOW TO USE"),style="color:#0e2f44"),align="right"),
                  div(htmlOutput("howto"),align="right")
                  )
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

