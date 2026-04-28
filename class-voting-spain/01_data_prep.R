# =============================================================================
# Voto de Clase en España - Preparación de datos
# Carga de archivos SPSS (CIS), recodificación de clase social, educación,
# sexo, edad, tamaño de municipio e ideología
# =============================================================================

library(readr)
library(tidyverse)
library(haven)

# --- Carga de datos ---
# Los archivos .sav se encuentran en la carpeta data/ local.
# Si no se encuentran, se muestra un mensaje y se detiene la ejecución.

tryCatch({
  df11 <- read_sav("data/2011.sav")
  df15 <- read_sav("data/2015.sav")
  df16 <- read_sav("data/2016.sav")
  df19a <- read_sav("data/2019a.sav")
  df19b <- read_sav("data/2019b.sav")
}, error = function(e) {
  stop("No se encontraron los archivos .sav en data/. ",
       "Descarga los datos del CIS y colócalos en class-voting-spain/data/.\n",
       "Error original: ", e$message)
})

# --- Función de recodificación de clase social ---

my_recodeclass <- function(P52, OCUMAR11) {
  ifelse(P52 == 4 & OCUMAR11 > 3, "Empresarios",
         ifelse(P52 == 3, "Empresarios",
                ifelse(OCUMAR11 == 1.0 & (P52 < 3 | P52 == 4), "Directivos y gerentes",
                       ifelse(OCUMAR11 == 2.0 & (P52 < 3 | P52 == 4), "Profesionales culturales",
                              ifelse(OCUMAR11 == 3.0 & (P52 < 3 | P52 == 4), "Profesionales técnicos",
                                     ifelse(OCUMAR11 == 4.0 & P52 < 3, "Oficinistas",
                                            ifelse(OCUMAR11 == 5.0 & P52 < 3, "Trabajadores de servicios",
                                                   ifelse((OCUMAR11 == 7.0 | OCUMAR11 == 8.0) & P52 < 3, "Obreros cualificados",
                                                          ifelse((OCUMAR11 == 6.0 | OCUMAR11 == 9.0) & P52 < 3, "Obreros no cualificados",
                                                                 NA)))))))))
}

vec_recodeclass <- Vectorize(my_recodeclass)

# --- Aplicar clase social ---

df11$Clase <- vec_recodeclass(df11$C17, df11$C16)
df15$Clase <- vec_recodeclass(df15$C17, df15$C16)
df16$Clase <- vec_recodeclass(df16$C17, df16$C16)
df19a$Clase <- vec_recodeclass(df19a$C17, df19a$C16)
df19b$Clase <- vec_recodeclass(df19b$C17, df19b$C16)

# --- Comicio ---

df11$Comicio = "2011"
df15$Comicio = "2015"
df16$Comicio = "2016"
df19a$Comicio = "2019a"
df19b$Comicio = "2019b"

# --- Educación ---

df11$EDU = factor(df11$ESTR, levels = c(1, 2, 3, 4), labels = c("Primaria o sin estudios", "Secundaria", "FP", "Estudios superiores"))
df15$EDU = factor(df15$ESTR, levels = c(1, 2, 3, 4), labels = c("Primaria o sin estudios", "Secundaria", "FP", "Estudios superiores"))
df16$EDU = factor(df16$ESTR, levels = c(1, 2, 3, 4), labels = c("Primaria o sin estudios", "Secundaria", "FP", "Estudios superiores"))
df19a$EDU = factor(df19a$ESTR, levels = c(1, 2, 3, 4), labels = c("Primaria o sin estudios", "Secundaria", "FP", "Estudios superiores"))

df19b$C11A_transformed <- ifelse(df19b$C11A %in% c(0, 1, 2), 1, 
                                 ifelse(df19b$C11A %in% c(3, 5, 7), 2, 
                                        ifelse(df19b$C11A %in% c(4, 6), 3, 
                                               ifelse(df19b$C11A %in% c(8, 9, 10, 11, 12, 13, 14, 15), 4, 
                                                      ifelse(df19b$C11A %in% c(16, 98, 99), 5, NA)))))

df19b$EDU <- factor(df19b$C11A_transformed, levels = c(1, 2, 3, 4, 5), 
                     labels = c("Primaria o sin estudios", "FP", "Secundaria", "Estudios superiores", "Secundaria"))

# --- Sexo ---

df11$SEX = factor(df11$SEXO, levels = c(1, 2), labels = c("Hombre", "Mujer"))
df15$SEX = factor(df15$PSEXO, levels = c(1, 2), labels = c("Hombre", "Mujer"))
df16$SEX = factor(df16$P47, levels = c(1, 2), labels = c("Hombre", "Mujer"))
df19a$SEX = factor(df19a$P41, levels = c(1, 2), labels = c("Hombre", "Mujer"))
df19b$SEX = factor(df19b$C9, levels = c(1, 2), labels = c("Hombre", "Mujer"))

# --- Edad ---

df11$EDAD <- as.integer(df11$EDAD)
df11$EDAD <- ifelse(df11$EDAD == 99, NA, df11$EDAD)

df15$EDAD <- as.integer(df15$P28)
df15$EDAD <- ifelse(df15$P28 == 99, NA, df15$P28)

df16$EDAD <- as.integer(df16$P48)
df16$EDAD <- ifelse(df16$P48 == 99, NA, df16$P48)

df19a$EDAD <- as.integer(df19a$P42)
df19a$EDAD <- ifelse(df19a$P42 == 99, NA, df19a$P42)

df19b$EDAD <- as.integer(df19b$C10)
df19b$EDAD <- ifelse(df19b$C10 == 99, NA, df19b$C10)

# --- Voto / Ideología ---

df11$PNAC <- ifelse(df11$p36 %in% c(2, 3, 11), 1, 
                    ifelse(df11$p36 %in% c(1, 4), 2, NA))
df11$LEFT = factor(df11$PNAC, levels = c(1, 2), labels = c("Izquierda", "Otros"))

df15$PNAC <- ifelse(df15$PRECUERDO %in% c(2, 3, 5, 6, 7, 10), 1, 
                    ifelse(df15$PRECUERDO %in% c(1, 4), 2, NA))
df15$LEFT = factor(df15$PNAC, levels = c(1, 2), labels = c("Izquierda", "Otros"))

df16$PNAC <- ifelse(df16$RECUERDO16 %in% c(2, 3, 5, 6, 9), 1, 
                     ifelse(df16$RECUERDO16 %in% c(1, 4), 2, NA))
df16$LEFT = factor(df16$PNAC, levels = c(1, 2), labels = c("Izquierda", "Otros"))

df11$RIG <- NA
df11$EXRIG <- NA
df15$RIG <- NA
df15$EXRIG <- NA
df16$RIG <- NA
df16$EXRIG <- NA

df19a$PNAC <- ifelse(df19a$P23R %in% c(2, 6, 7, 21), 1, 
                     ifelse(df19a$P23R %in% c(1, 4, 14), 2, 
                            ifelse(df19a$P23R %in% c(18), 3, NA)))

df19b$PNAC <- ifelse(df19b$B22R %in% c(2, 5, 6, 7, 21, 50, 67), 1, 
                                 ifelse(df19b$B22R %in% c(1, 4, 14), 2, 
                                        ifelse(df19b$B22R %in% c(18), 3, NA)))

df19a$LEFT = factor(df19a$PNAC, levels = c(1, 2, 3), labels = c("Izquierda", "Otros", "Otros"))
df19a$RIG = factor(df19a$PNAC, levels = c(1, 2, 3), labels = c("Otros", "Derecha", "Otros"))
df19a$EXRIG = factor(df19a$PNAC, levels = c(1, 2, 3), labels = c("Otros", "Otros", "Extrema derecha"))

df19b$LEFT = factor(df19b$PNAC, levels = c(1, 2, 3), labels = c("Izquierda", "Otros", "Otros"))
df19b$RIG = factor(df19b$PNAC, levels = c(1, 2, 3), labels = c("Otros", "Derecha", "Otros"))
df19b$EXRIG = factor(df19b$PNAC, levels = c(1, 2, 3), labels = c("Otros", "Otros", "Extrema derecha"))

# --- Tamaño de municipio ---

recode_tamuni <- function(df) {
  df %>% mutate(TAMUNI = case_when(
    TAMUNI == 1 ~ 1000,
    TAMUNI == 2 ~ 6000,
    TAMUNI == 3 ~ 35000,
    TAMUNI == 4 ~ 75000,
    TAMUNI == 5 ~ 250000,
    TAMUNI == 6 ~ 700000,
    TAMUNI == 7 ~ 1000000,
    TRUE ~ TAMUNI
  ))
}

df11 <- recode_tamuni(df11)
df15 <- recode_tamuni(df15)
df16 <- recode_tamuni(df16)
df19a <- recode_tamuni(df19a)
df19b <- recode_tamuni(df19b)

# --- Seleccionar columnas y unir ---

cols <- c("Clase", "EDU", "EDAD", "SEX", "TAMUNI", "Comicio", "LEFT", "RIG", "EXRIG")
df11 <- df11[cols]
df15 <- df15[cols]
df16 <- df16[cols]
df19a <- df19a[cols]
df19b <- df19b[cols]

df <- rbind(df11, df15, df16, df19a, df19b)
df <- df[complete.cases(df[c("LEFT")]), ]

# --- Imputación por mediana y moda ---

getmode <- function(v) {
  uniqv <- unique(na.omit(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

for (col_name in names(df)) {
  if (is.character(df[[col_name]])) {
    df[[col_name]] <- as.factor(df[[col_name]])
  }
}

variables_to_impute <- c("Clase", "EDU", "salesforEDAD", "SEX", "TAMUNI")

for (col in variables_to_impute) {
  if (is.numeric(df[[col]])) {
    median_value <- median(df[[col]], na.rm = TRUE)
    df[[col]][is.na(df[[col]])] <- median_value
  } else if (is.factor(df[[col]])) {
    mode_value <- getmode(df[[col]])
    df[[col]][is.na(df[[col]])] <- mode_value
  }
}

message("Datos CIS preparados. Objeto 'df' disponible con ", nrow(df), " observaciones.")
