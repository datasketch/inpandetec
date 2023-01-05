library(inpandetec)
library(hgchmagic)
library(lfltmagic)

theme <-  list(
  title_size = 15,
  orientation = "hor",
  ver_title = " ",
  hor_title = " ",
  text_family = "Mulish",
  title_family = "Mulish",
  label_wrap_legend = 100,
  label_wrap = 40,
  background_color = "transparent",
  axis_line_y_size = 1,
  axis_line_color = "#dbd9d9",
  grid_y_color = "#dbd9d9",
  grid_x_color = "#fafafa",
  title_family = "Lato",
  text_family  = "Lato",
  cursor = "pointer",
  map_name = "latamcaribbean_countries",
  #map_tiles = "OpenStreetMap",
  legend_position = "bottomleft",
  border_weight = 0.3,
  map_min_zoom = 3,
  map_max_zoom = 5,
  na_color = "transparent",
  palette_colors = rev(c("#EA524E", "#F16E54", "#F68660", "#F08D45", "#F8A557", "#FDBD6B", "#FDD783"))
)

pais <- data_to_app |>
  dplyr::select(respondent_id, País) |>
  dplyr::group_by(respondent_id) |>
  dplyr::summarise(País = unique(País)) |>
  dplyr::group_by(País) |>
  dplyr::summarise(Total = dplyr::n())

viz_map <- lfltmagic::lflt_choropleth_GnmNum(data = pais, theme = theme) |>
  leaflet::setView(lng = -79.5, lat = 18.5, 4)
htmlwidgets::saveWidget(viz_map, file = "mapa_encuestados.html", background = "transparent")

hostigamiento <- data_to_app |>
  dplyr::select(`Tipo de violencia experimentada`, Frecuencia) |>
  dplyr::filter(`Tipo de violencia experimentada` != "Sin respuesta")

cols <- c("#4BAEE1", "#EA524E", "#50C8AC", "#F4E62F", "#FF8000", "#5151F2", "#F7DBCB", "#F8A557", "#AEF0F9", "#908AFF", "#F4B3BE")
names(cols) <- unique(hostigamiento$`Tipo de violencia experimentada`)

theme$palette_colors <- NULL
viz_sank <- hgch_sankey_CatCat(hostigamiento, palette_colors = cols, theme = theme, title = " ", subtitle =" ")
htmlwidgets::saveWidget(viz_sank, file = "sankey_freq_violencia.html", background = "transparent")

freq <- data_to_app |>
  dplyr::filter(`Tipo de violencia experimentada` %in% "Acoso y hostigamiento") |>
  dplyr::group_by(Frecuencia) |>
  dplyr::summarise(Total = dplyr::n())

theme$palette_colors <- "#4BAEE1"
viz_bar <- hgch_bar_CatNum(freq, theme = theme, title = " ", subtitle =" ")
htmlwidgets::saveWidget(viz_bar,
                        file = "bar_freq_acoso.html",
                        background = "transparent")



genero <- data_to_app |>
  dplyr::select(respondent_id, `Identidad de género`) |>
  dplyr::group_by(respondent_id) |>
  dplyr::summarise(`Identidad de género` = unique(`Identidad de género`)) |>
  dplyr::group_by(`Identidad de género`) |>
  dplyr::summarise(Total = dplyr::n())
theme$palette_colors <- c("#4BAEE1", "#EA524E", "#50C8AC", "#F4E62F", "#FF8000", "#5151F2", "#F7DBCB", "#F8A557", "#AEF0F9", "#908AFF", "#F4B3BE")
viz_item <- hgch_item_CatNum(genero, theme = theme, color_by = "Identidad de género",  title = " ", subtitle =" ")
htmlwidgets::saveWidget(viz_item, file = "tem_identidad.html", background = "transparent")

