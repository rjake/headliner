library(shiny)
library(shinyWidgets)

refocus <-
  "Shiny.addCustomMessageHandler('refocus', function(NULL) {
    var text = document.getElementById('phrase');
    text.select();
    text.setSelectionRange(3, 4);
  });"


ui <- fluidPage(
  tags$head(
    tags$script(refocus)#,tags$style(HTML(my_css))
  ),
  textAreaInput("phrase", NULL, value = "this is a test"),
  uiOutput(outputId = "components")
)


server <- function(input, output, session) {
  output$components <- renderUI({
    radioGroupButtons(
      inputId = "component",
      label = NULL,
      choices = letters[1:5],
      selected = ""
    )
  })

  observeEvent(input$component, {
    session$sendCustomMessage(type = "refocus", message = list(NULL))
  })
}


runGadget(
  app = shinyApp(ui, server),
  server = NULL,
  port = getOption("shiny.port"),
  viewer =
    paneViewer(minHeight = "maximize"),
  #dialogViewer(dialogName = "test", width = 800, height = 1700),
  stopOnCancel = TRUE
)
