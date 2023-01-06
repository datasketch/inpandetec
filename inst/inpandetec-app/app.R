webshot::install_phantomjs()
library(inpandetec)
library(shiny)
library(shinyWidgets)
library(parmesan)
library(hgchmagic)
library(lfltmagic)
library(dsmodules)

ui <-
  fluidPage(
    tags$head(
      tags$link(rel="stylesheet", type="text/css", href="custom.css"),
      includeScript("www/iframeSizer.contentWindow.min.js"),
    ),
    div(class = "layout-container",
        div(class = "layout-panels",
            div(class = "app-container",
                div(class = "panel top-malibu",
                    div (class = "panel-body",
                         div(class = "panel-title", style = "padding: 18px 0px 8px 0px;", "Filtros"),
                         uiOutput("controls")
                    )),
                div(class = "panel",
                    div (class = "panel-body",
                         div(style="flex-grow: 1; min-width: 750px;",
                             div(class = "head-viz",
                                 div(style = "display:flex;gap:20px;margin-bottom: 20px;align-items: flex-end;",
                                     div(class = "panel-title" ,"Visualización"),
                                     uiOutput("viz_icons")),
                                 uiOutput("descargas")),
                             div(class = "viz-nucleo",
                                 uiOutput("viz_ui")
                             )
                         )
                    )),
                div(class = "panel",
                    div (class = "panel-body",
                         div(class = "panel-title", style = "padding: 18px 0px 8px 0px;" ,"Detalle"),
                         div(style = "display:block;max-width: 300px;text-align: center;",
                             uiOutput("click_ui"),
                             div(style = "margin-top:3%;",
                                 actionButton("modal_info", "Testimonios", icon = icon("user"))
                             ),
                             highchartOutput("click_treemap")
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
      viz_rec <- c("map", "bar", "treemap", "table")
      if (input$viz_selection %in% viz_rec) {
        actual_but$active <- input$viz_selection
      } else {
        actual_but$active <- viz_rec[1]
      }
    })

    # print viz
    output$viz_icons <- renderUI({
      possible_viz <- c("map", "bar", "treemap", "table")

      suppressWarnings(
        shinyinvoer::buttonImageInput("viz_selection",
                                      " ",
                                      images = possible_viz,
                                      tooltips = c("Mapa", "Barras", "Treemap", "Tabla"),
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
      sort(unique(data_to_app$`Tipo de violencia experimentada`)) |>
        setdiff("Sin respuesta")
    })

    defaultType <- reactive({
      req(types())
      tp <- types()[1]
      if (!is.null(input$countryId)) tp <- NULL
      tp
    })

    freq <- reactive({
      sort(unique(data_to_app$Frecuencia)) |>
        setdiff("Sin respuesta")
    })

    freq_sel <- reactive({
      if (is.null(input$countryId)) return()
      freq()[1]
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

    etnia <- reactive({
      unique(data_to_app$`Identificación étnica`)
    })

    discapacidad <- reactive({
      unique(data_to_app$`Personas con discapacidad`)
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
      ls <- ls[c("typeId", "freqId", "countryId", "generoId", "orienId",
                 "etniaId" ,"discId", "ageId")]
      names(ls) <- c("Tipo de violencia experimentada", "Frecuencia", "País",
                     "Identidad de género", "Orientación sexual",
                     "Identificación étnica", "Personas con discapacidad", "Edad")
      inpandetec::data_filter(data_to_app, ls)
    })

    # Seleccionar datos ------------------------------------------

    d_viz <- reactive({

      req(actual_but$active)
      req(d_filter())
      df <- d_filter()
      if (nrow(df) == 0) return()
      v <- NULL
      if (actual_but$active == "map") {
        v <- c("País")
      } else {
        v <- "Frecuencia"
        if (!is.null(input$freqId)) {
          if (length(unique(input$freqId)) > 1) {
            if (length(unique(input$typeId)) > 1 | is.null(input$typeId)) v <- c( "Tipo de violencia experimentada", v)
          } else {
            v <- "Tipo de violencia experimentada"
            if (length(unique(input$typeId)) == 1) v <- "País"  #comparacion por pais
            if (length(unique(input$countryId)) == 1 & length(unique(input$typeId)) == 1) v <- "Tipo de violencia experimentada"  #comparacion por pais
          }
        }
      }
      df <-
        inpandetec::var_selection(df, v) |>
        inpandetec::var_aggregation(`Total respuestas` = dplyr::n())

      if (ncol(df) == 3) {
        available_colors <- c("#4BAEE1", "#EA524E", "#50C8AC", "#F4E62F", "#FF8000", "#5151F2", "#F7DBCB", "#F8A557", "#AEF0F9", "#908AFF", "#F4B3BE", "#dddddd") #
        dic_col <- data.frame(tipo = unique(data_to_app$`Tipo de violencia experimentada`), ...colors = available_colors)
        df <- df |> dplyr::left_join(dic_col, by = c("Tipo de violencia experimentada" = "tipo"))
      }

      df
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
        if (ncol(d_viz()) == 4) tv <- "CatCatNum"
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
          text_family = "Mulish",
          title_family = "Mulish",
          label_wrap_legend = 100,
          label_wrap = 40,
          background_color = "#ffffff",
          axis_line_y_size = 1,
          axis_line_color = "#dbd9d9",
          grid_y_color = "#dbd9d9",
          grid_x_color = "#fafafa",
          title_family = "Lato",
          text_family  = "Lato",
          cursor = "pointer",
          map_name = "latamcaribbean_countries",#"latam_countries",
          background_color = "#ffffff",
          #map_tiles = "OpenStreetMap",
          legend_position = "bottomleft",
          border_weight = 0.3,
          map_min_zoom = 5,
          map_max_zoom = 15
        ),
        type = viz
      )

      if (actual_but$active == "map") {
        l$opts$na_color <- "transparent"
        l$opts$palette_colors <- rev(c("#EA524E", "#F16E54", "#F68660", "#F08D45", "#F8A557", "#FDBD6B", "#FDD783"))
      } else {
        l$opts$clickFunction <- htmlwidgets::JS(myFunc)
        # l$opts$palette_colors <- c("#4BAEE1", "#EA524E ", "#50C8AC", "#F4E62F", "#FF8000", "#5151F2", "#F7DBCB")
        if (ncol(d_viz()) == 2) {
          l$opts$palette_colors <- c("#4BAEE1")
          # l$opts$color_by <- names(d_viz())[1]
        }
        # if (ncol(d_viz()) == 4) {
        #   l$opts$color_by <- names(d_viz())[2]
        # }
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


      v <-  suppressWarnings(do.call(eval(parse(text=viz_opts()$type)),
                                     viz_opts()$opts))

      if (actual_but$active == "map") {
        v <- v  |>
          leaflet::setView(lng = -79.5, lat = 17.5, 5.2) #-86.5   11.5
      }
      v

    })


    output$hgch_viz <- highcharter::renderHighchart({
      req(actual_but$active)
      if (actual_but$active %in% c("map", "table")) return()
      req(viz_render())
      viz_render()
    })

    output$lflt_viz <- leaflet::renderLeaflet({
      print(actual_but$active)
      if (actual_but$active != "map") return()
      req(viz_render())
      viz_render()
    })

    output$dt_viz <- DT::renderDataTable({
      req(actual_but$active)
      if (actual_but$active != "table") return()
      req(d_filter())
      df <- d_filter()
      dtable <- DT::datatable(df,
                              rownames = F,
                              selection = 'none',
                              options = list(
                                scrollX = T,
                                fixedColumns = TRUE,
                                fixedHeader = TRUE,
                                scrollY = "500px"
                              ))

      dtable
    })


    output$viz_ui <- renderUI({
      req(actual_but$active)
      if (is.null(d_viz())) return("Sin datos para los filtros seleccionados")
      if (nrow(d_viz()) == 0) return("Sin datos para los filtros seleccionados")
      if (actual_but$active == "map") {
        leaflet::leafletOutput("lflt_viz", height = 650)
      } else if (actual_but$active == "table") {
        DT::dataTableOutput("dt_viz", width = "900px")
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
      if (nrow(dv) == 0) return()
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
          dc <- dc |>
            dplyr::filter(`Tipo de violencia experimentada` %in% gsub("<br/>", " ",id_click_viz$cat))
        } else {
          dc <- df |>
            dplyr::filter(`Tipo de violencia experimentada` %in% gsub("<br/>", " ",id_click_viz$id))
        }
      }
      if ("Identidad de género" %in% names(dv)) {
        dc <- df |> dplyr::filter(`Identidad de género` %in% id_click_viz$id)
      }
      if (nrow(dc) == 0) dc <- NULL
      dc

    })


    output$viz_click <- highcharter::renderHighchart({
      tryCatch({
        if (is.null(id_click_viz$id)) return()
        if (is.null(click_filter())) return()
        if (nrow(click_filter()) == 0) return()

        dc <- click_filter() |> tidyr::separate_rows(`Ejemplos de violencia experimentada`, sep = "-")
        if (length(unique(dc$`Ejemplos de violencia experimentada`)) > 11) {
          tx <- "Tipos de violencias experimentadas"
          dv <- d_viz()
          if ("País" %in% names(dv)) {
            tx <- paste0(tx, " en ", id_click_viz$id)
          }
          if ("Frecuencia" %in% names(dv)) {
            tx <-  paste0(tx, "<br/>(frecuencia: ", id_click_viz$id, ")")
          }
          if ("Tipo de violencia experimentada" %in% names(dv)) {
            if (!is.null(id_click_viz$cat)) {
              tx <-  paste0(tx, "<br/>(Tipo de violencia experimentada: ", id_click_viz$cat, ")")
            } else {
              tx <-  paste0(tx, "<br/>(Tipo de violencia experimentada: ", id_click_viz$id, ")")
            }
          }
          if ("Identidad de género" %in% names(dv)) {
            tx <-  paste0(tx, "<br/>(Identidad de género ", id_click_viz$id, ")")
          }
          dc <- click_filter() |> dplyr::select(`Tipo de violencia experimentada`)
          dc <-  inpandetec::var_selection(dc, `Tipo de violencia experimentada`) |>
            inpandetec::var_aggregation(`Total respuestas` = dplyr::n())
          available_colors <- c("#4BAEE1", "#EA524E", "#50C8AC", "#F4E62F", "#FF8000", "#5151F2", "#F7DBCB", "#F8A557", "#AEF0F9", "#908AFF", "#F4B3BE")
          dic_col <- data.frame(ejemplo = unique(dc$`Tipo de violencia experimentada`), ...colors = available_colors[1:(length(unique(dc$`Tipo de violencia experimentada`)))])
          dc <- dc |> dplyr::left_join(dic_col, by = c("Tipo de violencia experimentada" = "ejemplo"))
          myFunc <- paste0("function(event) {Shiny.onInputChange('", 'hcClickedTree', "', {id:event.point.name, timestamp: new Date().getTime()});}")

          viz <-
            suppressWarnings(
              hgchmagic::hgch_treemap_CatNum(dc,
                                         background_color = "#ffffff",
                                         legend_show = FALSE,
                                         #palette_colors = c("#5151f2", "#00afff ", "#4ad3ac", "#ffd150", "#ffe0bb", "#f26330", "#163875"),
                                         color_by = names(dc)[1],
                                         caption = "Da click para ver ejemplos según tipo de violencia",
                                         title = tx,
                                         plot_margin_bottom  = 50,
                                         title_size = 13,
                                         title_family = "Raleway",
                                         text_family  = "Mulish",
                                         cursor = "pointer",
                                         clickFunction = htmlwidgets::JS(myFunc),
                                         title_align = "center") |>
                hc_tooltip(style = list(
                  width = "150px"))
            )
        } else {
          tv <- paste0(unique(dc$`Tipo de violencia experimentada`), collapse = " - ")
            dc <-  inpandetec::var_selection(dc, `Ejemplos de violencia experimentada`) |>
              inpandetec::var_aggregation(`Total respuestas` = dplyr::n())
            available_colors <- c("#4BAEE1", "#EA524E", "#50C8AC", "#F4E62F", "#FF8000", "#5151F2", "#F7DBCB", "#F8A557", "#AEF0F9", "#908AFF", "#F4B3BE")
            dic_col <- data.frame(ejemplo = unique(dc$`Ejemplos de violencia experimentada`), ...colors = available_colors[1:(length(unique(dc$`Ejemplos de violencia experimentada`)))])
            dc <- dc |> dplyr::left_join(dic_col, by = c("Ejemplos de violencia experimentada" = "ejemplo"))
            tx <- "Ejemplos de violencias experimentadas"
            dv <- d_viz()
            if ("País" %in% names(dv)) {
              tx <- paste0(tx, " en ", id_click_viz$id)
              tx <- paste0(tx, " por ", tv)
            }
            if ("Frecuencia" %in% names(dv)) {
              tx <-  paste0(tx, "<br/>(frecuencia: ", id_click_viz$id, ")")
              tx <- paste0(tx, " por ", tv)
            }
            if ("Tipo de violencia experimentada" %in% names(dv)) {
              if (!is.null(id_click_viz$cat)) {
                tx <-  paste0(tx, "<br/>(Tipo de violencia experimentada: ", id_click_viz$cat, ")")
              } else {
                tx <-  paste0(tx, "<br/>(Tipo de violencia experimentada: ", id_click_viz$id, ")")
              }
            }
            if ("Identidad de género" %in% names(dv)) {
              tx <-  paste0(tx, "<br/>(Identidad de género ", id_click_viz$id, ")")
              tx <- paste0(tx, " por ", tv)
            }

            viz <-
              suppressWarnings(
                hgchmagic::hgch_pie_CatNum(dc,
                                           background_color = "#ffffff",
                                           legend_show = FALSE,
                                           #palette_colors = c("#5151f2", "#00afff ", "#4ad3ac", "#ffd150", "#ffe0bb", "#f26330", "#163875"),
                                           color_by = names(dc)[1],
                                           title = tx,
                                           title_size = 13,
                                           title_family = "Raleway",
                                           text_family  = "Mulish",
                                           title_align = "center") |>
                  hc_tooltip(style = list(
                    width = "150px"))
              )
          }
        viz
      },
      error = function(e){
        return()
      })
    })

    output$click_ui <- renderUI({
      tx <- HTML("<div class = 'click'><img src='img/click/click.svg' style='width: 50px; display:block;margin-left: 40%;'/><br/>Da clic <b>sobre la visualización</b> para ver más información.")
      if (is.null(id_click_viz$id)) return(tx)
      if (is.null(click_filter())) return(tx)
      if (nrow(click_filter()) == 0) return("No hay información para los filtros seleccionados")
      highcharter::highchartOutput("viz_click", width = 300)
    })


    output$click_treemap <- renderHighchart({
      if (is.null(click_filter())) return()
      if (is.null(input$hcClickedTree)) return()

      dc <-  click_filter() |> tidyr::separate_rows(`Ejemplos de violencia experimentada`, sep = "-")
      if (length(unique(dc$`Ejemplos de violencia experimentada`)) < 11 ) return()
      dc <- dc |> dplyr::filter(`Tipo de violencia experimentada` %in% gsub("<br/>", " ",input$hcClickedTree))
      tv <- unique(dc$`Tipo de violencia experimentada`)
      dc <-  inpandetec::var_selection(dc, `Ejemplos de violencia experimentada`) |>
         inpandetec::var_aggregation(`Total respuestas` = dplyr::n())
      available_colors <- c("#4BAEE1", "#EA524E", "#50C8AC", "#F4E62F", "#FF8000", "#5151F2", "#F7DBCB", "#F8A557", "#AEF0F9", "#908AFF", "#F4B3BE")
      dic_col <- data.frame(ejemplo = unique(dc$`Ejemplos de violencia experimentada`), ...colors = available_colors[1:(length(unique(dc$`Ejemplos de violencia experimentada`)))])
      dc <- dc |> dplyr::left_join(dic_col, by = c("Ejemplos de violencia experimentada" = "ejemplo"))
      tx <- paste0("Ejemplos de violencias experimentadas por ", tv)
      hgch_pie_CatNum(data = dc,
                      background_color = "#ffffff",
                      legend_show = FALSE,
                      #palette_colors = c("#5151f2", "#00afff ", "#4ad3ac", "#ffd150", "#ffe0bb", "#f26330", "#163875"),
                      color_by = names(dc)[1],
                      title = tx,
                      title_size = 13,
                      title_family = "Raleway",
                      text_family  = "Mulish",
                      title_align = "center")
    })


    info_modal <- reactive({
      req(d_filter())
      ls <- list("Tipo de violencia" = unique(d_filter()$`Tipo de violencia experimentada`))
      dm <- inpandetec::data_filter(data_modal, ls)
      HTML(
        paste0(
          purrr::map(unique(dm$`Tipo de violencia`), function(x) {
            df <- dm |> dplyr::filter(`Tipo de violencia` %in% x)
            HTML(
              paste0(
                div(class = "modal-type",
                    x),
                div(class = "modal-text",
                    HTML( paste0(df$Testimonio, collapse = "</br></br>"))
                )
              )
            )

          }), collapse = "</br>" )
      )

    })

    observeEvent(input$modal_info, {
      showModal(modalDialog(
        title = "Testimonios",
        info_modal(),
        easyClose = TRUE,
        footer = NULL
      ))
    })



    output$descargas <- renderUI({
      if (is.null(actual_but$active)) return()
      if (actual_but$active != "table") {
        dsmodules::downloadImageUI("download_viz", dropdownLabel ="Descargar", formats = c("jpeg", "pdf", "png", "html"), display = "dropdown", text = "Descargar")
      } else {
        dsmodules::downloadTableUI("dropdown_table", dropdownLabel = "Descargar", formats = c("csv", "xlsx", "json"), display = "dropdown", text = "Descargar")
      }
    })

    observe({
      dsmodules::downloadTableServer("dropdown_table", element = reactive(d_filter()), formats = c("csv", "xlsx", "json"))
      dsmodules::downloadImageServer("download_viz", element = reactive(viz_render()), lib = "highcharter", formats = c("jpeg", "pdf", "png", "html"), file_prefix = "plot")
    })

  }


shinyApp(ui, server)



