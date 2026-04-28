# =============================================================================
# Desigualdad Electoral en Europa - Preparación de datos
# Carga de ESS9 y datos WID, recodificación de variables
# =============================================================================

library(foreign)
library(tidyverse)
library(dplyr)
library(readr)
library(survey)
library(srvyr)
library(stats)
library(readxl)
library(writexl)
library(epiDisplay)
library(ggrepel)

# --- Carga de ESS9 ---

tryCatch({
  ESS <- read.spss("ESS9.sav", to.data.frame=TRUE)
}, error = function(e) {
  stop("No se encontró ESS9.sav. Descarga la ESS9 en formato SPSS y colócala en esta carpeta.\n",
       "Error: ", e$message)
})

# --- Carga de datos WID (desigualdad de ingresos) ---

tryCatch({
  WID_Data <- read_excel("WID.xls", col_names = c("cntry", "C1", "C2", "Inequality_inc"))
  WID_Data <- select(WID_Data, "cntry", "Inequality_inc")
}, error = function(e) {
  stop("No se encontró WID.xls. Descárgalo de wid.world y colócalo en esta carpeta.\n",
       "Error: ", e$message)
})

# --- Recodificación de variables ---

# Voto
ESS$voter[ESS$vote == "Yes"] <- 1
ESS$voter[ESS$vote == "No"] <- 0
ESS$voter[ESS$vote == "Not eligible to vote"] <- NA

# Manifestación democrática
ESS$dem[ESS$pbldmn == "Yes"] <- 1
ESS$dem[ESS$pbldmn == "No"] <- 0

# Estratificación por renta (P50 y P90)
ESS$income[ESS$hinctnta == "J - 1st decile"] <- "P50"
ESS$income[ESS$hinctnta == "R - 2nd decile"] <- "P50"
ESS$income[ESS$hinctnta == "C - 3rd decile"] <- "P50"
ESS$income[ESS$hinctnta == "M - 4th decile"] <- "P50"
ESS$income[ESS$hinctnta == "F - 5th decile"] <- "P50"
ESS$income[ESS$hinctnta == "H - 10th decile"] <- "P90"

message("Datos ESS y WID preparados.")
