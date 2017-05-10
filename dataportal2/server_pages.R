startp = function(env_serv) with (env_serv, {
  
  output$carousel <- renderUI({
    my_test <- tags$iframe(src="carousel.html",height="533",width="800")
    print(my_test)
    my_test
  })
  
  video <- ("https://www.youtube.com/embed/MscOjSQMovs")
  output$howto <- renderUI({
    my_test <- tags$iframe(src=video, height=360, width=640,scrolling="no",allowfullscreen="yes")
    print(my_test)
    my_test
  })
})

# contactp = function(env_serv) with (env_serv, {
#   
#   form <- ("https://docs.google.com/forms/d/1fjqt6Zb1igR3UxrMcd4SbBmceZWocPAReT-REgOKYC0/viewform")
#   
#   output$feedback <- renderUI({
#     my_test <- tags$iframe(src=form, height="760", width="500",scrolling="no")
#     print(my_test)
#     my_test
#   })
# })