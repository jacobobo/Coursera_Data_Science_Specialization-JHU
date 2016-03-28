library(shiny)

shinyUI(fluidPage(
    
    titlePanel("Next Word Predictor"),
    
    sidebarLayout(
        sidebarPanel(
            textInput("userInput", "Enter Your Starting Text Here:","At the end of the"),
            
            helpText("This application will attempt to predict the next word of the phrase based on what you type here."),
            
            submitButton("Predict")
        ),
    
        mainPanel(
            h4("Based on the last few words you input:"),
            textOutput("cleanedInput"),
            br(),
            h3("The Top Ranked Predictions for the next word in the phrase is:"),
            tableOutput("top5")
        )    
        
    )
))