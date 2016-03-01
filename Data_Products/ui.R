library(shiny)

shinyUI(pageWithSidebar(
    headerPanel("How Old Am I?"),
    sidebarPanel(
        dateInput("date", "Birth Date:"),
        h3('Documentation:'),
        h4('This app is quite simple to use. Input your birthdate by either 
           typing it in yyyy-mm-dd form or by using the selection box. Your 
           results will be displayed to your right.')
    ),
    mainPanel(
        h3('Age Calculation'),
        h4('You entered'),
        verbatimTextOutput("inputDate"),
        h4('Your age in years is:'),
        verbatimTextOutput("ageYears"),
        h4('Your age in days is:'),
        verbatimTextOutput("ageDays")
    )
))