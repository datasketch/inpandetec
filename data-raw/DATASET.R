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


data_to_app <- data_01 |> left_join(data_02)
data_to_app$Edad <- as.numeric(data_to_app$Edad)
data_to_app$`Tipo de violencia experimentada`[is.na(data_to_app$`Tipo de violencia experimentada`)] <- "Sin información"
data_to_app$Frecuencia[is.na(data_to_app$Frecuencia)] <- "Sin información"

usethis::use_data(data_to_app, overwrite = TRUE)
