ageInYears <- function(date) {
    (as.numeric(difftime(Sys.Date(),date)))/365
}

ageInDays <- function(date) {
    as.numeric(difftime(Sys.Date(),date))
}

shinyServer(
    function(input, output) {
        output$inputDate <- renderPrint({input$date})
        output$ageYears <-  renderPrint({ageInYears(input$date)})
        output$ageDays <- renderPrint({ageInDays(input$date)})
    }
)