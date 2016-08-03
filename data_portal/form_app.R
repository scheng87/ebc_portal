library(shiny)
library(googlesheets)

# Define the fields we want to save from the form
fields <- c("name", "email","organization", "agree")

# Define functions
table <- "portal_registration"

saveData <- function(data) {
  # Grab the Google Sheet
  sheet <- gs_title(table)
  # Add the data as a new row
  gs_add_row(sheet, input = data)
}

loadData <- function() {
  # Grab the Google Sheet
  sheet <- gs_title(table)
  # Read the data
  gs_read_csv(sheet)
}

# Shiny app with 3 fields that the user can submit data for
shinyApp(
  ui = fluidPage(
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
  ),
  server = function(input, output, session) {
    
    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
      data <- sapply(fields, function(x) input[[x]])
      data
    })
    
    # When the Submit button is clicked, save the form data
    observeEvent(input$submit, {
      saveData(formData())
    })
    
    # Show the previous responses
    # (update with current response when Submit is clicked)
    output$responses <- DT::renderDataTable({
      input$submit
      loadData()
    })     
  }
)