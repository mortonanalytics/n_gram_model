library(data.table)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("nextWord"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        h1("Enter your text!"),
        h5("Let us predict your next word! Enter any word or text
           in the box to the right. If you have nothing entered,
           it will tell you so. After you enter you text, see what
           the model predicts. Enjoy!")
         ),
      
      # Show a plot of the generated distribution
      mainPanel(
         textInput("text", "Your text here:"),
         textOutput("nextWord")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  source("final_model.R")
   
  output$nextWord <- renderText({
      x <- input$text
      word <- ifelse(nchar(x) > 0, select.word(x), "You haven't entered any text!")
      # word.alt <- select.word.stop(x)
      # ifelse(!is.na(word), word, ifelse(!is.na(word.alt), word.alt, " "))
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)
