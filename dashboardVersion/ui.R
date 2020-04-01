library(shiny)
library(shinydashboard)
library(shinyWidgets)


ui <- dashboardPage(
  dashboardHeader(title = "Twitter app"),
  dashboardSidebar(
    searchInput(inputId = "search", label = "Search..."),
    menuItem("Theme", tabName = "theme", icon = icon("dashboard")),
    menuItem("User", tabName = "user", icon = icon("user"))
  ),
  dashboardBody(
  sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                    label = "Search..."),
  tabItems(
      # First tab content
      tabItem(tabName = "theme",
        fluidRow(
          tabBox(title= "Tweets", id="tweets",
          tabPanel("All", 
          DT::dataTableOutput("all.tweets")),
          tabPanel("Top", 
          DT::dataTableOutput("top.tweets"))),
          tabBox(title= "Users", id="users",
          tabPanel("Most", 
          DT::dataTableOutput("most.tweeters")),
          tabPanel("Top", 
          DT::dataTableOutput("top.tweeters")))
        ),
        fluidRow(
          box(
            title = "Top words", id="words", status="warning", solidHeader = TRUE,
            plotOutput("top.words")
          ),
          box(
            title = "Sentiments", id="sentiments.box", status="warning", solidHeader = TRUE,
            plotOutput("sentiments")
          )
        )
      ),
      # Second tab content
      tabItem(tabName = "widgets",
        h2("Widgets tab content")
      )
    )
  )
)
