################################################################################
#' This shiny application showcases rintrojs for different widgets and 
#' extensions of shiny:
#' 
#' - shinydashboard (sidebar, box, valueBox)
#' - DT (table, table contents, columns)
#' - leaflet (map, map widgets & control)
#' 
#' The app is structured to use shiny modules. If you are not familiar with them,
#' have a look at this introduction: 
#'  - https://shiny.rstudio.com/articles/modules.html
#' 
#' The app is structured in the following way:
#' First the modules are created - one for each widget, containing the tooltips
#' information, then the app's UI and server functions are defined, lastly, 
#' the app is run.
#' 
#' Author: David Zimmermann
#' For questions, don't hesitate to get into contact with me, you can find my 
#' contact here https://davzim.github.io/
################################################################################

library(shiny)
library(rintrojs)
library(shinydashboard)
library(DT)
library(leaflet)
library(leaflet.extras)

##### Shiny Modules ######

# For each element, create a steps_* dataset and a *_ui and *_server module function
# 1) Shiny Dashboard
# 2) DT
# 3) Leaflet

####

# 1) Shiny Dashboard ----

steps_shinydashboard <- dplyr::tribble(
  ~element, ~intro,
  NA, "<h4>This is the <code>Shinydashboard</code> Tour</h4><br>Element: <code>NA</code>",
  ".sidebar-menu", "This highlights the Sidebar menu.<br>Element: <code>.sidebar-menu</code>",
  ".active", "This is the current selection.<br>Element: <code>.active</code>",
  # "#shiny-tab-dt", "Highlighting a single item works so-so.",
  ".sidebar-toggle", "The sidebar can be toggled here <br>Element: <code>.sidebar-toggle</code>",
  "#boxid", "A box from <code>shinydashboard</code> wrapped in <code>div(id = \"boxid\", box(...))</code>. Note that not necessarily all the box is highlighted!<br>Element: <code>#boxid</code>",
  ".small-box", "A value box from <code>shinydashboard</code>.<br>Element: <code>small-box</code>",
  "#value", "The value of a value box. The code is written as: <code>valueBox(value = div(100, id = \"value\"))</code><br>Element: <code>#value</code>",
  "#subtitle", "The title of the value box - code written as <code>valueBox(subtitle = div(\"One Hundred\", id = \"subtitle\"))</code><br>Element: <code>#subtitle</code>",
  NA, "The last element<br>Element: <code>NA</code>"
)

dashboard_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "shinydashboard",
          fluidRow(
            box(
              actionButton(ns("tour_shinydashboard"), HTML("Start <code>shinydashboard</code> Tour")),
              hr(),
              div(id = "boxid", 
                  box(
                    title = "A Box", width = 12, solidHeader = TRUE, status = "success",
                    valueBox(
                      value = div(100, id = "value"), 
                      subtitle = div("One Hundred", id = "subtitle"),
                      icon = icon("dashboard"), 
                      color = "green"
                    )
                  )
              )
            )
          )
  )
}

dashboard_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$tour_shinydashboard, {
        introjs(session, options = list(steps = steps_shinydashboard))
      })
    }
  )
}


# 2) DT ----

steps_dt <- dplyr::tribble(
  ~element, ~intro,
  NA, "<h4>This is the <code>DT</code> Tour!</h4>",
  # note that the prefix 'dt1-' is a result of using modules, without modules,
  # the element reference would be just #table as this is the id given in the code
  "#dt1-table", "This is the whole DT table.<br>Element: <code>#table</code>",
  ".dataTables_length", "This element allows you to set how many items are displayed in the table.<br>Element: <code>.dataTables_length</code>",
  ".dataTables_filter", "Here you can filter the data and search for elements.<br>Element: <code>.dataTables_filter</code>",
  ".dataTable", "This is where the actual data is displayed <br>Element: <code>.dataTable</code>",
  "#dt_name", "Using a container for the header of the <code>DT</code>, allows you to set ids!<br>Element: <code>#dt_name</code>...",
  "#dt_mpg", "<code>mpg</code> stands for miles per gallon.<br>Element: <code>#dt_mpg</code>",
  "#dt_cyl", "<code>cyl</code> refers to the cylinders of the car.<br>Element: <code>#dt_cyl</code>",
  "#dt_disp", "<code>disp</code> stands for the displacement of the car in cubin inch.<br>Element: <code>#dt_disp</code>",
  ".dataTables_info", "General pagination information is displayed here.<br>Element: <code>.dataTables_info</code>",
  ".dataTables_paginate", "The data is split into multiple pages, this part allows you to navigate the pages.<br>Element: <code>.dataTables_paginate</code>",
  ".previous", "Navigate to the previous page.<br>Element: <code>.previous</code>",
  ".current", "Navigate to a specific page, currently we are on this page.<br>Element: <code>.current</code>",
  ".next", "Navigate to the next page.<br>Element: <code>.next</code>",
  NA, "These are the basics elements of a <code>DT</code> table."
)

dt_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "dt",
          fluidRow(
            box(
              actionButton(ns("tour_dt"), HTML("Start <code>DT</code> Tour")),
              hr(),
              h5(code("mtcars"), "in a", code("DT::dataTable")),
              DT::dataTableOutput(ns("table"), width = "100%"),
              width = 12
            )
          )
  )
}

dt_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$table <- renderDataTable({
        
        # see also: https://rstudio.github.io/DT/ # 2.6 Custom Table Container
        sketch <- withTags(table(
          class = "display",
          thead(
            tr(
              th("Name", id = "dt_name"),
              th("Miles per Gallon", id = "dt_mpg"),
              th("Cylinders", id = "dt_cyl"),
              th("Displacement", id = "dt_disp")
            )
          )
        ))
        DT::datatable(mtcars[, 1:3], container = sketch)
        
      })
      
      observeEvent(input$tour_dt, {
        introjs(session, options = list(steps = steps_dt))
      })
    }
  )
}


# 3) Leaflet ----

steps_leaflet <- dplyr::tribble(
  ~element, ~intro,
  NA, "<h4>This is the <code>leaflet</code> Tour</h4>",
  # note that the prefix 'leaflet1-' is a result of using modules, without modules,
  # the element reference would be just #map as this is the id given in the code
  "#leaflet1-map", "This is the whole <code>leaflet</code> map.<br>Element: <code>#map</code>",
  ".leaflet-control-zoom", "Here you can control the zoom level of the app.<br>Element: <code>.leaflet-control-zoom</code>",
  ".leaflet-control-zoom-in", "Press <code>+</code> to zoom into to the map.<br>Element: <code>.leaflet-control-zoom-in</code>",
  ".leaflet-control-zoom-out", "Press <code>-</code> to zoom out.<br>Element: <code>.leaflet-control-zoom-out</code>",
  ".leaflet-control-fullscreen", "Press for full-screen map view, enabled by the <code>leaflet.extras</code> <a href='https://github.com/bhaskarvk/leaflet.extras'>package</a>.<br>Element: <code>.leaflet-control-fullscreen</code>",
  ".info", "The legend of the map.<br>Element: <code>.info</code>",
  ".leaflet-control-minimap", "A minimap showing the current position of the map, enabled by the <code>leaflet.extras</code> <a href='https://github.com/bhaskarvk/leaflet.extras'>package</a>.<br>Element: <code>.leaflet-control-minimap</code>",
  ".leaflet-control-attribution", "The attribution and license of the map.<br>Element: <code>.leaflet-control-attribution</code>",
  NA, "Thats it - thats all.<br>Element: <code>NA</code>"
)

leaflet_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "leaflet",
          fluidRow(
            box(
              actionButton(ns("tour_leaflet"), HTML("Start <code>leaflet</code> Tour")),
              hr(),
              leafletOutput(ns("map"), width = "100%"),
              width = 12
            )
          )
  )
}

leaflet_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$map <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          addMarkers(lng = 174.768, lat = -36.852, popup = "The birthplace of R") %>% 
          addMiniMap() %>% 
          addFullscreenControl() %>%
          addLegend("bottomright", colors = "blue", 
                    labels = "The birthplace of R",
                    title = "Markers", opacity = 1)
      })
      
      observeEvent(input$tour_leaflet, {
        introjs(session, options = list(steps = steps_leaflet))
      })
    }
  )
}




############ Shiny Functions ######

# The UI of the shinydashboard
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Shinydashboard", tabName = "shinydashboard", icon = icon("dashboard")),
      menuItem("DT", tabName = "dt", icon = icon("table")),
      menuItem("Leaflet", tabName = "leaflet", icon = icon("map"))
    )
  ),
  dashboardBody(
    introjsUI(),
    # set the tooltip width 
    tags$style(HTML(".introjs-tooltip {max-width: 100%;min-width: 400px;}")),
    
    tabItems(
      dashboard_ui("shinydashboard1"),
      dt_ui("dt1"),
      leaflet_ui("leaflet1")
    )
  )
)

# The shiny server function
server <- function(input, output, session) {
  dashboard_server("shinydashboard1")
  dt_server("dt1")
  leaflet_server("leaflet1")
}

# start the shiny app
shinyApp(ui, server)
