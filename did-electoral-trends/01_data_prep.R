# =============================================================================
# Diferencias en Diferencias - Tendencias electorales Murcia/Sevilla
# Preparación de datos y variables de tratamiento
# =============================================================================

library(tidyverse)
library(readxl)
library(lfe)
library(broom)

# --- Carga de datos ---

tryCatch({
  DiD <- read_excel("DiD.xlsx", 
                    col_types = c("numeric", "numeric", "text", 
                                  "numeric", "numeric", "text"))
}, error = function(e) {
  stop("No se encontró DiD.xlsx. Coloca el archivo en esta carpeta.\n",
       "Error: ", e$message)
})

# --- Variables dummy para treatment ---

DiD <- DiD %>% filter(Año < 2019.5) %>%
  mutate(Dummy_crisis = (Año > 2008),
         Dummy_reconversion = (Año > 1992),
         treat_group = (Provincia %in% "Murcia"),
         treat_reconversion = Dummy_reconversion*treat_group,
         treat_crisis = Dummy_crisis*treat_group)

message("Datos DiD preparados. ", nrow(DiD), " observaciones.")
