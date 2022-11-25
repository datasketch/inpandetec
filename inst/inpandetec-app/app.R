library(inpandetec)
library(shiny)
library(parmesan)

ui <-
  fluidPage(
    tags$head(
      tags$link(rel="stylesheet", type="text/css", href="custom.css")
    ),
    div(class = "layout-container",
        div(class = "layout-panels",
            div(class = "app-container",
                div(class = "panel top-malibu",
                    div (class = "panel-body",
                         uiOutput("controls")
                    )),
                div(class = "panel",
                    div (class = "panel-body",
                         div(style="flex-grow: 1; min-width: 600px;",
                             div(class = "head-viz",
                                 div(style = "display:flex;gap:20px;margin-bottom: 20px;align-items: flex-end;",
                                     "VISUALIZACIÓN",
                                     "viz"),
                                 "descarga"),
                             div(class = "viz-nucleo",
                                 verbatimTextOutput("aver")
                             )
                         )
                    )),
                div(class = "panel",
                    div (class = "panel-body",
                         div(style="flex-grow: 1; min-width: 320px;",
                             "info extra")
                    )
                )
            )
        )
    )
  )


server <-
  function(input, output, session) {

    pickerOpts <- reactive({
      list(
        `actions-box` = TRUE,
        `deselect-all-text` = "Ninguno",
        `select-all-text` = "Todos",
        title = "Todos"
      )
    })

    types <- reactive({
      sort(unique(data_to_app$`Tipo de violencia experimentada`))
    })

    defaultType <- reactive({
      req(types())
      types()[1]
    })

    freq <- reactive({
      sort(unique(data_to_app$Frecuencia))
    })


    countries <- reactive({
      unique(data_to_app$País)
    })


    genero <- reactive({
      unique(data_to_app$`Identidad de género`)
    })

    orientation <- reactive({
      unique(data_to_app$`Orientación sexual`)
    })

    min_age <- reactive({
      min(data_to_app$Edad, na.rm = T)
    })

    max_age <- reactive({
      max(data_to_app$Edad, na.rm = T)
    })

    range_age  <- reactive({
      req(min_age())
      req(max_age())
      c(min_age(), max_age())
    })


    # Renderizar inputs con parmesan ------------------------------------------

    parmesan <- parmesan_load()
    parmesan_input <- parmesan_watch(input, parmesan)

    output_parmesan("controls",
                    input = input, output = output, session = session,
                    env = environment())


    d_filter <- reactive({
      req(reactiveValuesToList(input))
      ls <- reactiveValuesToList(input)[c("typeId", "freqId", "countryId", "generoId", "orienId", "ageId")]
      names(ls) <- c("Tipo de violencia experimentada", "Frecuencia", "País", "Identidad de género", "Orientación sexual", "Edad")
      inpandetec::data_filter(data_to_app, ls)
    })


    output$aver <- renderPrint({
      d_filter()
    })

}


shinyApp(ui, server)



