library(shiny)
shinyApp(
  ui =
    fluidPage(
    tags$head(
      tags$script(
        'Shiny.addCustomMessageHandler("testmessage", function(message) {
          alert("The variable " + message.var + " = " + message.n);
        });'
      )
    ),
    textInput("var_name", "Variable Name:", value = "'TEST_123'"),
    sliderInput("slider", "Slider:", min = 1, max = 20, value = 15),
  ),
  server = function(input, output, session) {
    observe({
      session$sendCustomMessage(
        type = "testmessage",
        message = list(var = input$var_name, n = input$slider)
      )
    })
  }
)
