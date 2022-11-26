library(inpandetec)
library(shiny)
library(parmesan)
library(hgchmagic)
library(lfltmagic)

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
                                     uiOutput("viz_icons")),
                                 "descarga"),
                             div(class = "viz-nucleo",
                                 uiOutput("viz_ui")
                             )
                         )
                    )),
                div(class = "panel",
                    div (class = "panel-body",
                         div(style = "display:block;min-width: 350px;",
                                 uiOutput("click_ui")
                         )
                    )
                )
            )
        )
    )
  )


server <-
  function(input, output, session) {


    # Renderizar graficos ------------------------------------------

    actual_but <- reactiveValues(active = NULL)

    observe({
      if (is.null(input$viz_selection)) return()
      viz_rec <- c("map", "bar", "treemap")
      if (input$viz_selection %in% viz_rec) {
        actual_but$active <- input$viz_selection
      } else {
        actual_but$active <- viz_rec[1]
      }
    })

    # print viz
    output$viz_icons <- renderUI({
      possible_viz <- c("map", "bar", "treemap")

      suppressWarnings(
        shinyinvoer::buttonImageInput("viz_selection",
                                      " ",
                                      images = possible_viz,
                                      tooltips = c("Mapa", "Barras", "Treemap"),
                                      path = "img/viz_icons/",
                                      active = actual_but$active,
                                      imageStyle = list(shadow = TRUE,
                                                        borderColor = "#ffffff",
                                                        padding = "3px")
        )
      )
    })

    # opciones para parmesan ------------------------------------------

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


    # Filtrar datos ------------------------------------------


    d_filter <- reactive({
      ls <- parmesan_input()
      # req(reactiveValuesToList(input))
      ls <- ls[c("typeId", "freqId", "countryId", "generoId", "orienId", "ageId")]
      names(ls) <- c("Tipo de violencia experimentada", "Frecuencia", "País", "Identidad de género", "Orientación sexual", "Edad")
      inpandetec::data_filter(data_to_app, ls)
    })

    # Seleccionar datos ------------------------------------------

    d_viz <- reactive({

      req(actual_but$active)
      req(d_filter())
      df <- d_filter()
      v <- NULL
      if (actual_but$active == "map") {
        v <- c("País")
      } else {
        v <- "Frecuencia"
        if (!is.null(input$freqId)) {
          if (length(unique(input$freqId)) > 1) {
            if (length(unique(input$typeId)) > 1) v <- c( "Tipo de violencia experimentada", v)
          } else {
            v <- "Tipo de violencia experimentada"
            if (length(unique(input$typeId)) == 1) v <- "País"  #comparacion por pais
            if (length(unique(input$countryId)) == 1 & length(unique(input$typeId)) == 1) v <- "Identidad de género"  #comparacion por pais
          }
        }
      }
      inpandetec::var_selection(df, v) |>
        inpandetec::var_aggregation(`Total respuestas` = dplyr::n())
    })

    # Visualizar datos ------------------------------------------

    viz_opts <- reactive({
      req(d_viz())
      req(actual_but$active)

      viz <- NULL
      tv <- NULL
      if (actual_but$active == "map") {
        viz <- "lfltmagic::lflt_choropleth_GnmNum"
      } else {
        tv <- "CatNum"
        if (ncol(d_viz()) == 3) tv <- "CatCatNum"
        viz <-  paste0("hgchmagic::","hgch_", actual_but$active, "_", tv)
      }
      if (!is.null(tv)) {
        myFunc <- paste0("function(event) {Shiny.onInputChange('", 'hcClicked', "', {id:event.point.name, timestamp: new Date().getTime()});}")
        if (tv == "CatCatNum") {
          myFunc <- paste0("function(event) {Shiny.onInputChange('", 'hcClicked', "', {cat:this.name, id:event.point.category, timestamp: new Date().getTime()});}")
          if (actual_but$active == "treemap")  myFunc <- paste0("function(event) {Shiny.onInputChange('", 'hcClicked', "', {cat:event.point.node, id:event.point.name, timestamp: new Date().getTime()});}")
        }
      }

      l <- list(
        opts = list(
          data = d_viz(),
          title_size = 15,
          orientation = "hor",
          ver_title = " ",
          hor_title = " ",
          text_family = "Fira Sans",
          title_family = "Fira Sans",
          label_wrap_legend = 100,
          label_wrap = 40,
          background_color = "#ffffff",
          axis_line_y_size = 1,
          axis_line_color = "#dbd9d9",
          grid_y_color = "#dbd9d9",
          grid_x_color = "#fafafa",
          cursor = "pointer",
          map_name = "latam_countries",
          map_tiles = "OpenStreetMap",
          legend_position = "bottomleft",
          border_weight = 0.3,
          map_min_zoom = 5,
          map_max_zoom = 15
        ),
        type = viz
      )

      if (actual_but$active == "map") {
        l$opts$na_color <- "transparent"
      } else {
        l$opts$clickFunction = htmlwidgets::JS(myFunc)
      }

      if (actual_but$active == "treemap") {
        l$opts$dataLabels_align <- "middle"
        l$opts$dataLabels_inside <- TRUE
        l$opts$dataLabels_show <- TRUE
        l$opts$legend_show <- FALSE
      }

      l
    })



    viz_render <- reactive({
      req(viz_opts())

      suppressWarnings(
        do.call(eval(parse(text=viz_opts()$type)),
                viz_opts()$opts
        ))
    })


    output$hgch_viz <- highcharter::renderHighchart({
      req(actual_but$active)
      if (actual_but$active == "map") return()
      req(viz_render())
      viz_render()
    })

    output$lflt_viz <- leaflet::renderLeaflet({
      print(actual_but$active)
      if (actual_but$active != "map") return()
      req(viz_render())
      viz_render() |>
        leaflet::setView(lng = -86.5, lat = 11.5, 5.5)
    })


    output$viz_ui <- renderUI({
      req(actual_but$active)
      if (is.null(d_viz())) return()
      if (nrow(d_viz()) == 0) return()
      if (actual_but$active == "map") {
        leaflet::leafletOutput("lflt_viz", height = 650)
      } else {
        highcharter::highchartOutput("hgch_viz", height = 650)
      }
    })


    id_click_viz <- reactiveValues(id = NULL, cat = NULL)

    observeEvent(input$lflt_viz_shape_click, {
      id_click_viz$id <- input$lflt_viz_shape_click$id
    })


    observeEvent(input$hcClicked, {
      if (is.null(d_viz())) return()
      df <- d_viz()
      viz <- actual_but$active
      if (ncol(df) == 2) {
        id_click_viz$id <- input$hcClicked$id #frecuencia
      } else {
        if (viz == "bar") {
          id_click_viz$id <- input$hcClicked$id #frecuencia
          id_click_viz$cat <- input$hcClicked$cat # tipo
        } else if (viz == "treemap") {
          id_click_viz$id <- input$hcClicked$cat$name
          id_click_viz$cat <- input$hcClicked$cat$parent
        } else {
          return()
        }
      }

    })

    click_filter <- reactive({
      if (is.null(id_click_viz$id)) return()
      dv <- d_viz()
      df <- d_filter()
      dc <- NULL
      if ("País" %in% names(dv)) {
        dc <- df |> dplyr::filter(País %in% id_click_viz$id)
      }
      if ("Frecuencia" %in% names(dv)) {
        dc <- df |> dplyr::filter(Frecuencia %in% id_click_viz$id)
      }
      if ("Tipo de violencia experimentada" %in% names(dv)) {
        if (!is.null(id_click_viz$cat)) {
          dc <- dc |> dplyr::filter(`Tipo de violencia experimentada` %in% id_click_viz$cat)
        } else {
          dc <- df |> dplyr::filter(`Tipo de violencia experimentada` %in% id_click_viz$id)
        }
      }
      if ("Identidad de género" %in% names(dv)) {
        dc <- df |> dplyr::filter(`Identidad de género` %in% id_click_viz$id)
      }
      if (nrow(dc) == 0) dc <- NULL
      dc

    })


    output$viz_click <- highcharter::renderHighchart({
      if (is.null(id_click_viz$id)) return()
      if (is.null(click_filter())) return()
      dc <-  inpandetec::var_selection(click_filter(), `Ejemplos de violencia experimentada`) |>
        inpandetec::var_aggregation(`Total respuestas` = dplyr::n())
      suppressWarnings(
        hgchmagic::hgch_pie_CatNum(dc, legend_show = FALSE, color_by = names(dc)[1])
      )
    })

    output$click_ui <- renderUI({
      if (is.null(id_click_viz$id)) return(HTML("<div class = 'click'><img src='img/click/click.svg' style='width: 50px; display:block;margin-left: 40%;'/>"))
      if (is.null(click_filter())) return(HTML("<div class = 'click'><img src='img/click/click.svg' style='width: 50px; display:block;margin-left: 40%;'/>"))
      highcharter::highchartOutput("viz_click", width = 300)
    })


  }


shinyApp(ui, server)



