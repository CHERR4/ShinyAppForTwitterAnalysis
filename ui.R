library(shiny)
library(DT)
# Define UI for app ----
ui <- fluidPage(
  titlePanel("Twitter app"),

  sidebarLayout(
    sidebarPanel(
        textInput("text", h3("Word search"), 
            value = "CambioClimatico"),
        actionButton("submit", label="submit"),
        selectInput("topDevices", h3("Top devices"), 
            choices = list("Top 5" = 5, "Top 10" = 10,
                                      "All" = 100), selected = 5)
    )       
    ,
    mainPanel(
      fluidRow(
        column(5, 
        DT::dataTableOutput("table"),
        ),
        column(5, 
        plotOutput("topDevices")
        )
      ),
      fluidRow(
        column(5, 
        DT::dataTableOutput("users"),
        ),
        column(5,
        plotOutput("topWords"))
      )
    )
  )
)