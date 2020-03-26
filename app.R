library(shiny)
# Here you have to write the code that will be only executed
# in the start of the app
library("twitteR")
library("tm")
library(wordcloud)
library(wordcloud2)
library("tidyverse")
library(DT)

# Run app ----
shinyApp(ui = ui, server = server)
runApp("RShinyApp", display.mode = "showcase")