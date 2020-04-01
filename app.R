library(shiny)

# Run app ----
shinyApp(ui = test.ui, server = server)
runApp("RShinyApp/ShinyAppForTwitterAnalysis", display.mode = "showcase")