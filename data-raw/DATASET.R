## code to prepare `DATASET` dataset goes here
library(googlesheets4)
library(tidyr)
library(dplyr)


data <- read_sheet("https://docs.google.com/spreadsheets/d/1WNIG9kASXbjtX9No8nWddPxN-uSoqqp-2yCh6W1Td5M/edit#gid=1704864021")
data <- data[-1,]

data_01 <- data |> select(respondent_id,
                          País = `País en el que vives`,
                          Edad = `Edad en años cumplidos`,
                          `Identidad de género`,
                          `Otra identidad de género` = `...20`,
                          `Orientación sexual`,
                          `Otra orientación sexual` = `...22`)


idQ <- names(data)[grep("cuán frecuente", names(data))]
data_02 <- data[,c("respondent_id", idQ)]
names(data_02) <- c("respondent_id",
                    "Acoso y hostigamiento",
                    "Acoso sexual",
                    "Difusión de contenido íntimo sin consentimiento",
                    "Explotación sexual facilitada por tecnologías",
                    "Acceso, uso, control, manipulación y/o publicación de información privada y datos personales",
                    "Suplantación y robo de identidad en línea",
                    "Amenazas en línea",
                    "Extorsión en línea",
                    "Desprestigio en línea",
                    "Expresiones discriminatorias y/o discurso de odio",
                    "Afectaciones a canales de expresión y/o ataques coordinados")
data_02 <- data_02 |>
  gather("Tipo de violencia experimentada", "Frecuencia", -respondent_id) |>
  drop_na(Frecuencia)

idE <- grep("De los siguientes ejemplos de ", names(data))
idE <- purrr::map(idE, ~ .x:(.x+7)) |> unlist()
data_03 <- data[,c(1, idE)]
idQ <- grep("Entre 2020 y 2022|Identifica|Por cuál tipo", names(data_03))
data_03 <- data_03[,-idQ]
dic <- data.frame(id_agresion =
                    c(names(data_03)[2:9],#7
                      names(data_03)[10:17],#7
                      names(data_03)[18:25],#7
                      names(data_03)[26:32],#6
                      names(data_03)[33:40],#7
                      names(data_03)[41:47],#6
                      names(data_03)[48:53],#5
                      names(data_03)[54:61],#7
                      names(data_03)[62:66],#4
                      names(data_03)[67:73],#6
                      names(data_03)[74:79]),#5
                  label =
                    c(rep("Acoso y hostigamiento", 8),
                      rep("Acoso sexual", 8),
                      rep("Difusión de contenido íntimo sin consentimiento", 8),
                      rep("Explotación sexual facilitada por tecnologías", 7),
                      rep("Acceso, uso, control, manipulación y/o publicación de información privada y datos personales", 8),
                      rep("Suplantación y robo de identidad en línea", 7),
                      rep("Amenazas en línea", 6),
                      rep("Extorsión en línea", 8),
                      rep("Desprestigio en línea", 5),
                      rep("Expresiones discriminatorias y/o discurso de odio", 7),
                      rep("Afectaciones a canales de expresión y/o ataques coordinados", 6)))


data_03 <- data_03 |>
  tidyr::gather("id_agresion", "ejemplos",-1) |>
  tidyr::drop_na() |>
  dplyr::left_join(dic) %>%
  dplyr::select(-id_agresion)

data_03 <- data_03 |>
  dplyr::group_by(respondent_id, `Tipo de violencia experimentada` = label) |>
  dplyr::summarise(`Ejemplos de violencia experimentada` = paste0(unique(ejemplos), collapse = '-'))

data_02 <- data_02 %>% inner_join(data_03)


data_to_app <- data_01 |> left_join(data_02)
data_to_app$Edad <- as.numeric(data_to_app$Edad)
data_to_app <- data_to_app |> tidyr::drop_na(Frecuencia)
#data_to_app$`Tipo de violencia experimentada`[is.na(data_to_app$`Tipo de violencia experimentada`)] <- "Sin información"
#data_to_app$Frecuencia[is.na(data_to_app$Frecuencia)] <- "Sin información"

usethis::use_data(data_to_app, overwrite = TRUE)


data_modal <- read_sheet("https://docs.google.com/spreadsheets/d/1Nn-eqTZNGaXhZKMT3gQmBXAQy-rfA35nBxIXqCZlPj4/edit#gid=1361438895", 2)
usethis::use_data(data_modal, overwrite = TRUE)





