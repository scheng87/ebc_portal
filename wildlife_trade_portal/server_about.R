adata = function(env_serv) with (env_serv, {
  intdef <- reactive({
    data <- filter(definitions,Term == input$intervention_def)
    def <- data$Definition
  })
  
  intex <- reactive({
    data <- filter(definitions,Term == input$intervention_def)
    ex <- data$Example
  })
  
  output$int_def2 <- renderText({
    paste(as.character(intdef()))
  })
  
  output$int_def3 <- renderText({
    paste(as.character(intex()))
  })
  
  outdef <- reactive({
    data <- filter(definitions,Term == input$outcome_def)
    def <- data$Definition
  })
  outex <- reactive({
    data <- filter(definitions,Term == input$outcome_def)
    ex <- data$Example
  })
  
  output$out_def2 <- renderText({
    paste(as.character(outdef()))
  })
  
  output$out_def3 <- renderText({
    paste(as.character(outex()))
  })
  
  studydef <- reactive({
    data <- filter(definitions,Term == input$study_def)
    def <- data$Definition
  })
  studyex <- reactive({
    data <- filter(definitions,Term == input$study_def1)
    ex <- data$Example
  })
  
  output$study_def2 <- renderText({
    paste(as.character(studydef()))
  })
  
  output$study_def3 <- renderText({
    paste(as.character(studyex()))
  })
  
  video2 <- ("https://www.youtube.com/embed/MscOjSQMovs")
  output$howto2 <- renderUI({
    my_test <- tags$iframe(src=video, height=360, width=640,scrolling="no",allowfullscreen="yes")
    print(my_test)
    my_test
  })
})

