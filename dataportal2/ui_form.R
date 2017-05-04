formp = function()
  fluidPage(
  fluidRow(
    align="center",
    br(),
    h3(div(strong("USER REGISTRATION"),style="color:#006666")),
    hr(),
    br()
  ),
  fluidRow(
    column(3
    ),
    column(6,
           align="center",
           wellPanel(
             textInput("name", "Name", ""),
             textInput("email","Email",""),
             textInput("organization","Organization",""),
             checkboxInput("agree", "I agree to appropriately cite the source of the data being downloaded if it will be used in any publications.", FALSE),
             actionButton("submit", "Submit")
           )
    ),
    column(3
           )
    )
  )