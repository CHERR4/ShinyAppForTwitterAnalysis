library(shiny)
library(shinydashboard)
library(shinyWidgets)


ui <- dashboardPage(
  dashboardHeader(title = "Twitter app"),
  dashboardSidebar(
    menuItem("Topic", tabName = "topic", icon = icon("dashboard")),
    menuItem("User", tabName = "user", icon = icon("user"))
  ),
  dashboardBody(
  tabItems(
      # First tab content
      tabItem(tabName = "topic",
        column(12, 
          fluidRow(textInput("text", h3("Word search"), 
          value = "CambioClimatico"),
      actionButton("topic", label="submit"))),
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
        ),
        fluidRow(
          tabBox(title = "Topic", id="topic",
          tabPanel("Number of topics",
          plotOutput("number.of.topics"))
          )
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
          ),          
          tabPanel("No active",
          DT::dataTableOutput("no.active.following")
          )
          )
        ),
        fluidRow(
          tabBox(title = "Words", id="words",
          tabPanel("Top words",
          plotOutput("top.words.user")),
          tabPanel("Correlated words",
          plotOutput("top.correlated.word")),
          tabPanel("Correlated liked words",
          plotOutput("top.correlated.liked.word"))
          ), 
          tabBox(title = "Activity", id="activity",
          tabPanel("Week",
          plotOutput("tweets.per.week")),
          tabPanel("Hour",
          plotOutput("tweets.per.hour"))
          )
          #tabBox(title = "Sentiments", id="sentiments",
          #tabPanel("Sentiments by tweet",
          #plotOutput("sentiments.by.tweet"))
          
        )
      )
    )
  )
)
