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
  tabItems(
      # First tab content
      tabItem(tabName = "theme",
        column(12, 
          fluidRow(textInput("text", h3("Word search"), 
          value = "CambioClimatico"),
      actionButton("theme", label="submit"))),
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
      tabItem(tabName = "user",
        column(12, 
            fluidRow(textInput("user", h3("User search"), 
            value = "joserraCasero"),
        actionButton("user.submit", label="submit"))),
        fluidRow(
          tabBox(title= "Tweets", id="tweets",
          tabPanel("All", 
          DT::dataTableOutput("all.tweets.user")),
          tabPanel("Top", 
          DT::dataTableOutput("top.tweets.user"))),
          tabBox(title="Users", id="users",
          tabPanel("Followers", 
          DT::dataTableOutput("followers")),
          tabPanel("Followed", 
          DT::dataTableOutput("followed")),
          tabPanel("Dont follow you", 
          DT::dataTableOutput("dont.follow.you")
          ),
          tabPanel("You dont follow",
          DT::dataTableOutput("you.dont.follow")
          ),
          tabPanel("Mutuals",
          DT::dataTableOutput("mutuals")
          )
          )
        ),
        fluidRow(
          tabBox(title = "Words", id="words",
          tabPanel("Top words",
          plotOutput("top.words.user"))
          ), 
          tabBox(title = "Activity", id="activity",
          tabPanel("Frequency",
          plotOutput("tweets.per.week"))
          )
          #tabBox(title = "Sentiments", id="sentiments",
          #tabPanel("Sentiments by tweet",
          #plotOutput("sentiments.by.tweet"))
          
        )
      )
    )
  )
)
