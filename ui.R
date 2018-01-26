library(shinydashboard)
library(shiny)
library(leaflet)


dashboardPage(
  dashboardHeader(
    title = "Gapminder dashboard"),
  dashboardSidebar(
    h2("Filter Options"),
    uiOutput("contientChecks"),
    uiOutput("filterCountry"),
    br(),
    uiOutput("dateRange")
  ),
  dashboardBody(
    includeCSS("www/customStyle.css"),
    tabsetPanel( 
      tabPanel("Plot",
        leafletOutput("leafletWorldMap"),
        br(),
        br(),
        plotOutput("gapminderPlot"),
        br(),
        radioButtons(inputId = "axis",
                    label ="X-axis Scale",
                    choices = c("Linear","Log10"),
                    selected = "Log10",
                    inline=TRUE),
        checkboxInput(inputId = "showPath",
                     label ="Show Path?",
                     value = FALSE)
      ),
      tabPanel("Table",
               DT::dataTableOutput("table")
      ),
      id = "tabsetPanel1"
    )
  )
)