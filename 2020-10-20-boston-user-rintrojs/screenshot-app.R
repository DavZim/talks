library(shiny)
library(shinydashboard)
library(rintrojs)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    introjsUI(),
    actionButton("btn", "Start Tour"),
    hr(),
    valueBox(div("42%", id = "target"), subtitle = "Current Success Ratio", icon = icon("dashboard"), color = "green")
  )
)

steps_df <- dplyr::tribble(
  ~element, ~intro,
  "#target", "<h4>Success Rate</h4><br>The value shows the <strong>current success ratio</strong> in percent.<br><br>The value is defined as the total number of successful cases divided by the total amount of active cases.<br><br>Currently, the value is 42%."
)

server <- function(input, output, session) {
  introjs(session, options = list(steps = steps_df))
}
shinyApp(ui, server)

