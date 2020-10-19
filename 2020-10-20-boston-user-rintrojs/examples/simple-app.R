################################################################################
#' This application showcases rintrojs for a basic shiny app
#' 
#' First a tibble/data.frame `step_df` is created that contains the information
#' for the intro tooltips.
#' Then the app's UI and server function are defined, lastly the app is run.
#' 
#' Author: David Zimmermann
#' For questions, don't hesitate to get into contact with me, you can find my 
#' contact here https://davzim.github.io/
################################################################################

library(shiny)
library(rintrojs)

# create the information for the tour
step_df <- dplyr::tribble(
  ~element, ~intro,
  NA, "<h4>Welcome to this simple shiny application</h4><br><strong>You can even write html</strong>, try writing <code>&lt;strong&gt;Hello World&lt;/strong&gt;</code>!<br>Element: <code>NA</code>",
  "#start_tour", "This button starts this tour.<br>Element: <code>#start_tour</code>",
  "#plot", "Here you find a graphic output, in this case a simulated price series.<br>Element: <code>#plot</code>",
  "#no-exist", "This is an intro to a non-existing item!<br>Element: <code>#no-exist</code>",
  NA, "This is the last element.<br>Element: <code>NA</code>"
)

# create the shiny UI consisting of a button and a plot
ui <- fluidPage(
  # include the introJS javascript and css files
  introjsUI(),
  
  actionButton("start_tour", "Start the Tour"),
  hr(),
  plotOutput("plot")
)

# shiny server function
server <- function(input, output, session) {
  output$plot <- renderPlot(plot(1:100, cumsum(rnorm(100)), type = "l"))
  
  # on the click of the `start_tour` button, start the introjs tour
  observeEvent(input$start_tour, {
    introjs(session, options = list(steps = step_df))
  })
}

# start the shiny app
shinyApp(ui, server)
