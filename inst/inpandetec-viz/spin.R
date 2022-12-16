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
  map_max_zoom = 15,
  na_color = "transparent",
  palette_colors = rev(c("#EA524E", "#F16E54", "#F68660", "#F08D45", "#F8A557", "#FDBD6B", "#FDD783"))
)

pais <- data_to_app |>
  dplyr::select(respondent_id, País) |>
  dplyr::group_by(respondent_id) |>
  dplyr::summarise(País = unique(País)) |>
  dplyr::group_by(País) |>
  dplyr::summarise(Total = dplyr::n())

lfltmagic::lflt_choropleth_GnmNum(data = pais, theme = theme) |>
  leaflet::setView(lng = -79.5, lat = 17.5, 5.2)




