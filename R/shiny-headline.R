# TOD):
# [ ] replace cursor area https://stackoverflow.com/questions/50430219/how-to-get-the-cursor-position-in-a-shiny-textareainput
#
# [ ]
#
# [ ]
#
# [ ]
#
options(browser = "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")

dummy_input <- list(
  comp = "3.5",
  ref = "2.7",
  orig_values = "{c} vs. {r}",
  phrase = "",
  component = "delta"
)

library(shiny)
library(shinyWidgets)
library(headliner)
library(glue)

my_css <-
  ".btn-group, .btn-group-vertical {
    column-count: 5;
  }

  .btn-group-toggle {
  width:200px;
  }

  .radiobtn {
    width:200px;
  }"


ui <-
  fluidPage(
    tags$head(tags$style(HTML(my_css))),
    radioGroupButtons(
      inputId = "somevalue1",
      label = NULL,
      choices =
        setNames(
          1:20,
          rep(c("xs", "medium", "very long", "a whole lotta text"), 5)
        )#, direction = "vertical"
    )
  )

ui <- {fluidPage(
  tags$head(tags$style(HTML(my_css))),
  fixedRow(
    column(3, textInput("comp", "New", 3.5)),
    column(3, textInput("ref", "Old", 2.7)),
    column(
      6,
      textInput(
        inputId = "orig_values",
        label = '{orig_values} default: "{c} vs. {r}"',
        value = "{c} vs. {r}")
    )
  ),
  textAreaInput(
    inputId = "phrase",
    label = "phrase",
    value = "{trend} of {delta} ({orig_values})",
    width = "100%"
  ),
  verbatimTextOutput(outputId = "headline"),
  uiOutput(outputId = "components")
)}


server <- function(input, output, session) {
  output$components <- renderUI({
    res <-
      compare_values(
        x = as.numeric(c(input$comp, input$ref)),
        orig_values = input$orig_values
      )
    # names(res)
    choices <-
      res[c(
        "article_delta", "article_delta_p", "article_trend",
        "delta",         "delta_p",         "trend",
        "raw_delta",     "raw_delta_p",     "sign",
        "comp_value",    "ref_value",       "orig_values"
      )]

    button_options <-
      setNames(
        object = glue("{[names(choices)]}", .open = "[", .close = "]"),
        nm =
          glue(
            '[choices] <br>
            <p style="font-size:10px;color:dimgrey;">
            {[names(choices)]}
            </p>',
            .open = "[",
            .close = "]"
          )
      )

    radioGroupButtons(
      inputId = "component",
      label = NULL,
      choices = button_options,
      selected = ""
    )
  })

  output$headline <- renderPrint({
    headline(
      list(as.numeric(input$comp), as.numeric(input$ref)),
      headline = input$phrase,
      orig_values = input$orig_values
    )
  })

  observeEvent(input$component, {
    updateTextAreaInput(
      session = session,
      inputId = "phrase",
      # label = "phrase",
      value = paste0(input$phrase, input$component)
    )
  })
}

runApp(
  list(ui = ui, server = server),
  host = "127.0.0.1",
  port = 3482,
  launch.browser = TRUE
)
