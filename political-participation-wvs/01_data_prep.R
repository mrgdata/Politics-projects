# =============================================================================
# Participación Política en las Américas (WVS7) - Preparación de datos
# Carga de WVS, índices de descentralización, creación de variables
# =============================================================================

library(tidyverse)
library(stringr)
library(naniar)
library(dplyr)
library(readxl)

# --- Carga de la encuesta WVS ---

tryCatch({
  load("WVS_Cross-National_Wave_7_R_v2_0.rdata")
}, error = function(e) {
  message("No se encontró el archivo WVS Wave 7 (.rdata). ",
          "Descárgalo de www.worldvaluessurvey.org y colócalo en esta carpeta.\n",
          "Error: ", e$message)
  stop(e)
})

# --- Índice de descentralización fiscal ---

tryCatch({
  FD <- read_excel("Fiscal_Decentralization.xlsx", sheet=2)
  colnames(FD) <- c("B_COUNTRY_ALPHA", "FD")
}, error = function(e) {
  message("No se encontró Fiscal_Decentralization.xlsx. ", e$message)
  stop(e)
})

# --- Índice de autonomía política regional ---

tryCatch({
  RAI <- read_excel("RAI_country-2021.xlsx")
  RAI <- RAI %>% filter(year == 2017) %>% select(c("abbr_country", n_RAI))
  colnames(RAI) <- c("B_COUNTRY_ALPHA", "RAI")
}, error = function(e) {
  message("No se encontró RAI_country-2021.xlsx. ", e$message)
  stop(e)
})

# --- Merge de datos ---

df <- merge(RAI, FD, by="B_COUNTRY_ALPHA", all.y=TRUE)
wvs0 <- merge(df, WVS_Cross_National_Wave_7_v2_0, by = "B_COUNTRY_ALPHA", all.y=TRUE)

# Variable peso
wvs0 <- mutate(wvs0, peso= W_WEIGHT*pwght)

# Variable Identificación partidista
wvs0$Q223[wvs0$Q223 < 10] <- 0
wvs0$Q223[wvs0$Q223 > 10] <- 1

# Variable denominación religiosa
wvs0$Q289[wvs0$Q289>0] <- 1

# Variable etnia
wvs0$ethnic <- str_sub(wvs0$Q290, start=-1)
wvs0$ethnic <- as.numeric(wvs0$ethnic)
wvs0$ethnic[wvs0$ethnic == 1] <- 0
wvs0$ethnic[wvs0$ethnic < 0] <- NA
wvs0$ethnic[wvs0$ethnic > 0] <- 1

# Variable grupo_ocupacional
wvs0$Q281[wvs0$Q281 == 0] <- "No ha trabajado"
wvs0$Q281[wvs0$Q281 == 1 | wvs0$Q281 == 2] <- "Clase de servicio"
wvs0$Q281[wvs0$Q281 > 2 & wvs0$Q281 < 6] <- "Trabajadores no manuales"
wvs0$Q281[wvs0$Q281 > 5 & wvs0$Q281 < 9] <- "Clase obrera"
wvs0$Q281[wvs0$Q281 == 9 | wvs0$Q281 == 10] <- "Trabajadores del campo"
wvs0$Q281[wvs0$Q281 == 11 | wvs0$Q281 < 0] <- NA
wvs0$Q281 <- as.factor(wvs0$Q281)

# Variable Tipo de empleo
wvs0$Q279[wvs0$Q279 == 1] <- "Trabajador a tiempo completo"
wvs0$Q279[wvs0$Q279 == 2] <- "Trabajador a tiempo parcial"
wvs0$Q279[wvs0$Q279 == 3] <- "Autónomo"
wvs0$Q279[wvs0$Q279 > 3 & wvs0$Q279 < 7] <- "Inactivo"
wvs0$Q279[wvs0$Q279 == 7] <- "Desempleado"
wvs0$Q279[wvs0$Q279 == 8 | wvs0$Q279 < 0] <- NA
wvs0$Q279 <- as.factor(wvs0$Q279)

# Variable Nivel educativo
wvs0$Q275R[wvs0$Q275R == 1] <- "Primaria"
wvs0$Q275R[wvs0$Q275R == 2] <- "Secundaria"
wvs0$Q275R[wvs0$Q275R == 3] <- "Pos-secundaria"
wvs0$Q275R[wvs0$Q275R == 4] <- "Terciaria"
wvs0$Q275R[wvs0$Q275R < 0] <- NA
wvs0$Q275R <- as.factor(wvs0$Q275R)

# Marcamos los valores sin respuesta por NA
wvs0[,1:362][wvs0[,1:362] <0] <- NA
wvs0[,363:401][wvs0[,363:401]== -99] <- NA
wvs0[,402:515][wvs0[,402:515]== -9999] <- NA
wvs0[,516:550][wvs0[,516:550]== -999] <- NA

# Invertir escalas para que mayor valor = más positivo
wvs0 <- mutate(wvs0, Q4 = (Q4-4)*-1) 
wvs0 <- mutate(wvs0, Q199 = (Q199-4)*-1)
wvs0 <- mutate(wvs0, Q200 = (Q200-3)*-1)
wvs0 <- mutate(wvs0, Q202 = (Q202-5)*-1)
wvs0 <- mutate(wvs0, Q206 = (Q206-5)*-1)
wvs0 <- mutate(wvs0, Q207 = (Q207-5)*-1)
wvs0 <- mutate(wvs0, Q224 = (Q224-4)*-1)
wvs0 <- mutate(wvs0, Q234 = (Q234-4)*-1)
wvs0 <- mutate(wvs0, Q235 = (Q235-4)*-1)

# Filtrar por regiones de las Américas
wvs0 <- filter(wvs0, regionWB == 3 | regionWB == 5)

# --- Variables dependientes ---

# Participación en partidos
wvs0$partido[wvs0$Q98 == 0] <- 0
wvs0$partido[wvs0$Q98 > 0] <- 1
wvs0$partido <- as.numeric(wvs0$partido)

# Acciones políticas (Q209-Q220): Lo ha hecho vs no
for (q in c("Q209","Q210","Q211","Q212","Q213","Q214","Q215","Q217","Q218","Q219","Q220")) {
  new_col <- paste0(q, "rec")
  wvs0[[new_col]][wvs0[[q]] == 1] <- 1
  wvs0[[new_col]][wvs0[[q]] > 1] <- 0
  wvs0[[new_col]] <- as.numeric(wvs0[[new_col]])
}

# Activismo (Q216)
wvs0$activismo[wvs0$Q216 == 1] <- 1
wvs0$activismo[wvs0$Q216 > 1] <- 0
wvs0$activismo <- as.numeric(wvs0$activismo)

# Voto local y nacional
wvs0$voto_local[wvs0$Q221 == 4] <- NA
wvs0$voto_local[wvs0$Q221 == 3] <- 0
wvs0$voto_local[wvs0$Q221 == 1 | wvs0$Q221 == 2] <- 1
wvs0$voto_local <- as.numeric(wvs0$voto_local)

wvs0$voto_nacional[wvs0$Q222 == 4] <- NA
wvs0$voto_nacional[wvs0$Q222 == 3] <- 0
wvs0$voto_nacional[wvs0$Q222 == 1 | wvs0$Q222 == 2] <- 1
wvs0$voto_nacional <- as.numeric(wvs0$voto_nacional)

# Índice compuesto de voto
wvs0 <- mutate(wvs0, voto=((wvs0$voto_nacional + wvs0$voto_local)/2))

# Índice compuesto de participación política
wvs0 <- mutate(wvs0, zPPOL = ((wvs0$partido + wvs0$Q213rec + wvs0$Q214rec + wvs0$Q209rec + wvs0$Q217rec 
                                + wvs0$Q218rec +  wvs0$Q219rec  + wvs0$Q220rec +wvs0$Q210rec + wvs0$Q211rec + 
                                  wvs0$Q212rec + wvs0$activismo)/12))

# --- Confianza endógena y exógena ---

wvs0 <- mutate(wvs0, endtrust = ((wvs0$Q58-4) + (wvs0$Q59-4) +(wvs0$Q60-4))/-9)
wvs0 <- mutate(wvs0, exotrust = ((wvs0$Q61-4) + (wvs0$Q62-4) +(wvs0$Q63-4))/-9)

# --- Participación en organizaciones no políticas ---

for (q in c("Q94","Q95","Q96","Q97","Q99","Q100","Q101","Q102","Q103","Q104","Q105")) {
  wvs0[[q]][wvs0[[q]]>0] <- 1
}

wvs0 <- wvs0 %>% mutate(no_polpart = c(Q94 + Q95 + Q96 + Q97 + Q99 + Q100
                                       + Q101 + Q102 + Q103 + Q104 + Q105)) 

# --- Confianza Política ---

wvs0 <- wvs0 %>% mutate(poltrust = ((c(Q71 + Q72 + Q73 + Q76)-16)*-1))

# --- Limpieza de columnas ---

wvs0 <- wvs0[-c(4:23, 27:38, 44, 46, 49, 55, 65, 67:68, 71:72, 74, 76, 86, 99:104, 106, 110:114, 117, 124:139, 151:162,
                209:214, 228, 230, 235, 237:239, 241:242, 266:279, 281:282, 314, 321, 323:324, 328:330, 334,
                337:338, 340:341, 343:344, 346:347, 358, 361:362, 364:365, 368:402, 417, 431:432, 435:437, 444:445, 450, 459,
                465:467, 469, 516:521, 553:567)]

# Eliminar variables con >30% NA
wvs0 <- wvs0[colSums(is.na(wvs0))/nrow(wvs0) < .3]

# Recodificar voto como dicotómica
wvs0$voto[wvs0$voto > 0] <- 1
wvs0$voto <- as.factor(wvs0$voto)

# Separar por región
wvsNA <- filter(wvs0, regionWB==3)
wvsLA <- filter(wvs0, regionWB==5)

wvsNA <- wvsNA[,-237]
wvsLA <- wvsLA[,-237]

wvsNA <- wvsNA[,-1]
wvsLA <- wvsLA[,-1]

wvsNA <- na.roughfix(wvsNA)
wvsLA <- na.roughfix(wvsLA)

a <- wvsNA$peso
b <- wvsLA$peso

wvsNA <- wvsNA[,-309]
wvsLA <- wvsLA[,-309]

message("Datos WVS preparados. wvsNA (", nrow(wvsNA), " obs) y wvsLA (", nrow(wvsLA), " obs) disponibles.")
