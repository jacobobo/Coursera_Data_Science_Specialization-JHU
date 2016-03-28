library(shiny)

source('helper.R')
loadData()

shinyServer(function(input, output) {
    cleantext <- reactive({CreateSearch(input$userInput)})
    output$cleanedInput <- renderText({cleantext()})
    
    top5 <- reactive({FindNextWord(cleantext())})
 
    output$top5 <- renderTable({top5()})
})