# ui.R

shinyUI(fluidPage(
  titlePanel("Data Science Capstone: Word Prediction"),
  
  fluidRow(
    column(12,
           br(),
           h4("This Shiny App predicts the next word as you type it."),
           br(),
           h4("Usage: Type a phrase in the box below."),
           br(),
           h4("You will see the typed phrase along with suggested word"),
           br(),
           br()
    )
  ),
  
  fluidRow(
    column(6,
           textInput("input_str", 
                     label = "Enter your text here:", 
                     value = " "
           )             
    )    
  ),
  
  fluidRow(
    column(12,
           br(),
           br(),
           br(),
           br(),
           h4("You entered:", style = "color:red;"), 
           verbatimTextOutput("text1")             
    )
  ),
  
  fluidRow(
    column(12,
           br(),
           br(),
           h4("Predicted next word:", style = "color:blue"), 
           verbatimTextOutput("text2")            
    )
  )
))