# TODO:
# [ ] replace cursor area https://stackoverflow.com/questions/50430219/how-to-get-the-cursor-position-in-a-shiny-textareainput
#
# [ ] Copy phrase to clipboard
# [ ] Copy code to clipboard
# [ ] add headline() settings
# [ ] custom phrases
#
# [ ] Place cursor at the end of inserted text
# [x] Reset buttons  ended up dusing 'Unselect' button--http://jsfiddle.net/2mu8pj4t/3/
# from Maya:
#    - https://js4shiny.com/schedule/
#    - https://github.com/gadenbuie/js4shiny/blob/examples/inst/examples/shiny/shiny-typing/shiny-typing-02/app.R
#    - https://shiny.rstudio.com/articles/js-send-message.html
#    - https://book.javascript-for-r.com/shiny-input.html
#    - https://github.com/MayaGans/shinylearnrdemo

dummy_input <- # for shinyobject debugging
  list(
    comp = "27",
    ref = "35",
    orig_values = "{c} vs. {r}",
    phrase = "",
    component = "{delta_p}"
  )

library(shiny)
library(shinyWidgets)
library(headliner)
library(glue)
library(miniUI)

create_headline <- function(new = 40, old = 50) {
  # shinyjs::useShinyjs()
  resource_path <- "inst/gadgets/create-headline"
  # system.file("gadgets", "create-headline", package = "headliner")

  get_replacement_location <-
    "Shiny.setInputValue(
      'selected_text', [
        document.getElementById('phrase').selectionStart,
        document.getElementById('phrase').selectionEnd
      ]
    );"

  default_phrase <- "{trend} of {delta} ({orig_values})"
  selected_text <- rep(nchar(default_phrase), 2)

  ui <- {miniUI::miniPage(
    shiny::tags$head(
      tags$script(get_replacement_location),
      # shiny::includeScript(file.path(resource_path, "js", "create-headline.js")),
      shiny::includeCSS(file.path(resource_path, "css", "create-headline.css")),
    ),
    miniUI::gadgetTitleBar("Create Headline"),
    miniUI::miniContentPanel(
      padding = 0,
      fillCol(
        flex = c(1, 2, 1, 3),
        fillRow(
          flex = c(1,1,3),
          textInput("comp", "New", new),
          textInput("ref", "Old", old),
          textInput(
            inputId = "orig_values",
            label = '{orig_values} default: "{c} vs. {r}"',
            value = "{c} vs. {r}"
          )
        ),
        textAreaInput(
          inputId = "phrase",
          label = 'Phrase: default: "{trend} of {delta} ({orig_values})"',
          value = default_phrase
        ) %>%
          tagAppendAttributes(
            style = 'width: 100%; height:100%;',
            onmouseup = get_replacement_location,
            onkeyup =   get_replacement_location
          ),
        verbatimTextOutput(outputId = "headline"),
        uiOutput(outputId = "components")
      )
    )
  )}

  # server ----
  server <- function(input, output, session) {
    print(selected_text)

    output$components <- renderUI({
      res <-
        compare_values(
          x = as.numeric(c(input$comp, input$ref)),
          orig_values = input$orig_values
        )

      res$delta_p <- paste0(res$delta_p, "%")
      res$raw_delta_p <- paste0(res$raw_delta_p, "%")

      choices <-
        res[c(
          "article_delta", "delta", "raw_delta", "comp_value",
          "article_delta_p", "delta_p", "raw_delta_p", "ref_value",
          "article_trend", "trend", "sign", "orig_values"
        )]

      button_options <-
        setNames(
          object = c(
            glue("{[names(choices)]}", .open = "[", .close = "]"),
            "Clear"
          ),
          nm =c(
            glue(
              '<big>[choices]</big>
              <br> <small>{[names(choices)]}</small>',
              .open = "[",
              .close = "]"
            ),
            "Unselect"
          )
        )

      radioGroupButtons(
        inputId = "component",
        label = NULL,
        choices = button_options,
        size = "sm",
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

    observeEvent(input$selected_text, {
      selected_text <<- input$selected_text
      print(selected_text)
      print(input$component)
    })

    observeEvent(input$component, {
      if (input$component == "Clear") {
        return(TRUE)
      }
      print(selected_text)
      loc_start <- selected_text[1]
      loc_end <- selected_text[2]
      print(c(loc_start, loc_end))

      if (loc_start == loc_end) loc_end <- selected_text[2] + 1

      if (loc_start == nchar(input$phrase)) {
        end_phrase <- ""
      } else {
        end_phrase <- substr(input$phrase, loc_end, nchar(input$phrase))
      }

      selected_component <- input$component

      if (str_detect(selected_component, "^.(raw_)?delta_p")) {
        selected_component <- paste0(input$component, "%")
      }

      new_phrase <-
        paste0(
          substr(input$phrase, 0, loc_start),
          selected_component,
          end_phrase
        )

      updateTextInput(
        session = session,
        inputId = "phrase",
        value = new_phrase
      )
    })
  }

  runGadget(
    app = shinyApp(ui, server),
    server = NULL,
    port = getOption("shiny.port"),
    viewer =
      paneViewer(minHeight = "maximize"),
    # dialogViewer(dialogName = "test", width = 800, height = 1700),
    stopOnCancel = TRUE
  )
}

create_headline()
# runApp ----
# only seems to render correctly when shown in browser
# runApp(
#   list(ui = ui, server = server),
#   host = "127.0.0.1",
#   port = 3482,
#   launch.browser = TRUE
# )

# app <- shinyApp(ui, server)#, options = list(launch.browser = FALSE))
