# TODO:
# [ ] replace cursor area https://stackoverflow.com/questions/50430219/how-to-get-the-cursor-position-in-a-shiny-textareainput
#
# [x] Copy phrase to clipboard
# [x] Copy code to clipboard
# [x] add headline() settings
# [ ] only add
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
    x = "40.123",
    y = "50.456",
    orig_values = "{x} vs. {y}",
    phrase = "{trend} of {delta} ({orig_values})",
    component = "{delta_p}",
    n_decimal = 2,
    multiplier = "1"
  )

options(shiny.suppressMissingContextError = TRUE)


library(shiny)
library(shinyWidgets)
library(headliner)
library(tidyverse)
library(glue)
library(miniUI)

create_headline <- function(x = 11, y = 19) {
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

  ui <- {
    miniUI::miniPage(
      shiny::tags$head(
        tags$script(get_replacement_location),
        # shiny::includeScript(file.path(resource_path, "js", "create-headline.js")),
        shiny::includeCSS(file.path(resource_path, "css", "create-headline.css")),
      ),
      miniUI::gadgetTitleBar("Create Headline"),
      miniUI::miniContentPanel(
        padding = 0,
        fillCol(
          flex = c(1, 2, 1, 3, 2),
          fillRow(
            flex = c(1,1,1,3),
            dropdownButton(
              tags$h4("Advanced Options"),
              tags$b("# of Decimal Places"),
              tags$p("Adjust value to limit the number of decimal places in the returned values"),
              shinyWidgets::sliderTextInput(
                inputId = "n_decimal",
                label = NULL,
                choices = 0:5, selected = 1
              ),
              tags$b("Multiplier"),
              tags$p("Do input values require a multiplier? (ex: 0.75 x 100 = 75)"),
              radioGroupButtons(
                inputId = "multiplier",
                label = NULL,
                choices = c(1, 100)
              ),
              icon = icon("gear", verify_fa = FALSE),
              tooltip =
                tooltipOptions(title = "Click to see advanced options!", placement = "bottom")
            ),
            textInput("x", "X", x),
            textInput("y", "Y", y),
            textInput(
              inputId = "orig_values",
              label = '{orig_values} default: "{x} vs. {y}"',
              value = "{x} vs. {y}"
            )
          ),
          fillRow(
            flex = c(7, 1),
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
            actionButton("copy_phrase", "Copy")
          ),
          verbatimTextOutput(outputId = "headline"),
          uiOutput(outputId = "components"),
          fillRow(
            flex = c(7, 1),
            verbatimTextOutput(outputId = "view_code"),
            actionButton("copy_code", "Copy")
          )
        )
      )
    )
  }

  # server ----
  server <- function(input, output, session) {
    #print(selected_text)
    #reactive(print(input$x))

    get_call <- reactive({
      substitute_headline <- function(){
        x <- as.numeric(input$x)
        y <- as.numeric(input$y)
        phrase <- input$phrase
        multiplier <- as.integer(input$multiplier)
        n_decimal <- input$n_decimal
        orig_values <- input$orig_values

        use_call <-
          substitute(
            headline(
              x = x,
              y = y,
              headline = phrase,
              orig_values = orig_values,
              multiplier = multiplier,
              n_decimal = n_decimal
            )
          )
        use_call
      }

      substitute_headline()

    })

    call_as_string <- function(expr) {
      rlang::expr_text(expr) %>%
        str_replace_all("^w+\\(|, ", ",\n") %>%
        str_replace_all("(.)\\)$", "\\1\n\\)") %>%
        styler::style_text()
    }

    output$components <- renderUI({
      # print(input$n_decimal)
      # print(input$multiplier)

      res <-
        compare_values(
          x = as.numeric(input$x),
          y = as.numeric(input$y),
          orig_values = input$orig_values,
          n_decimal = input$n_decimal,
          multiplier = as.integer(input$multiplier)
        )

      # print(res$comp_value)
      # print(res$article_delta_p)

      # res$delta_p <- paste0(res$delta_p, "%")
      # res$raw_delta_p <- paste0(res$raw_delta_p, "%")

      choices <-
        res[c(
          "article_delta",       "delta",       "x",
          "article_delta_p",     "delta_p",     "y",
          "article_raw_delta",   "raw_delta",
          "article_raw_delta_p", "raw_delta_p",
          "trend",               "sign",        "orig_values"
        )]

      button_options <- local({
        button_values <-
          glue("{[names(choices)]}", .open = "[", .close = "]")

        button_names <-
          glue(
            "<big>[choices]</big><br><small>{[names(choices)]}</small>",
            .open = "[", .close = "]"
          )

        delta_p <- which(str_detect(button_values, "^.(raw_)?delta_p"))

        if (input$multiplier == "100") {
          button_names[delta_p] <-
            str_replace(button_names[delta_p], "(\\d)(\\<)", "\\1 (%)\\2")
        }

        setNames(
          object = c(button_values, "Clear"),
          nm = c(button_names, "Unselect")
        )
      })


      radioGroupButtons(
        inputId = "component",
        label = NULL,
        choices = button_options,
        size = "sm",
        selected = ""
      )
    })

    output$headline <- renderPrint({
      x <- get_call()
      eval(x)
    })

    output$view_code <- renderPrint({
      call_as_string(get_call()) %>%
      styler:::print.vertical(colored = FALSE) # turn off warning
    })

    observeEvent(input$copy_phrase, {
      glue('"{input$phrase}"') %>%
        clipr::write_clip()
    })


    observeEvent(input$copy_code, {
      call_as_string(get_call()) %>%
        clipr::write_clip()
    })

    observeEvent(input$selected_text, {
      selected_text <<- input$selected_text
      #print(selected_text)
      #print(input$component)
    })

    observeEvent(input$component, {
      if (input$component == "Clear") {
        return(TRUE)
      }
      #print(selected_text)
      loc_start <- selected_text[1]
      loc_end <- selected_text[2]
      #print(c(loc_start, loc_end))

      if (loc_start == loc_end) loc_end <- selected_text[2] + 1

      if (loc_start == nchar(input$phrase)) {
        end_phrase <- ""
      } else {
        end_phrase <- substr(input$phrase, loc_end, nchar(input$phrase))
      }

      selected_component <- input$component

      if (str_detect(selected_component, "^.(raw_)?delta_p") &
          input$multiplier == 100
          ) {
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
