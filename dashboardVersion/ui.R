library(shiny)
library(shinydashboard)
library(shinyWidgets)


ui <- dashboardPage(
  dashboardHeader(title = "Twitter app"),
  dashboardSidebar(
    menuItem("Theme", tabName = "theme", icon = icon("dashboard")),
    menuItem("User", tabName = "user", icon = icon("user"))
  ),
  dashboardBody(
    column(12, 
      fluidRow(textInput("text", h3("Word search"), 
        value = "CambioClimatico"),
      actionButton("submit", label="submit"))),
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
          tabBox(title = "Words", id="words",
          tabPanel("Wordcloud",
          plotOutput("wordcloud")),
          tabPanel("Top words",
          plotOutput("top.words"))
          ),
          tabBox(title = "Sentiments", id="sentiments",
          tabPanel("Sentiments by tweet",
          plotOutput("sentiments.by.tweet")))
        )
      ),
      # Second tab content
      tabItem(tabName = "widgets",
        h2("Widgets tab content")
      )
    )
  )
)
