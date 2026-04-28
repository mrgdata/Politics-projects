# =============================================================================
# Participación Política en las Américas (WVS7) - ACP y descriptivos
# Análisis de Componentes Principales sobre variables de participación
# =============================================================================

library(FactoMineR)
library(factoextra)
library(tidyverse)

# --- Cargar datos ---
source("01_data_prep.R")

# --- ACP sobre variables de participación ---
# (Se ejecuta antes de la recodificación dicotómica del voto)
# Nota: el ACP se usa para explorar la dimensionalidad de las variables dependientes

# Descriptivos de voto por país
df_desc <- wvs0 %>% group_by(B_COUNTRY_ALPHA) %>% summarise(voto = mean(as.numeric(as.character(voto)), na.rm=TRUE))
ggplot(df_desc, aes(x=voto, y=reorder(B_COUNTRY_ALPHA, voto), fill=B_COUNTRY_ALPHA)) + 
  geom_col(show.legend = FALSE) + labs(y=NULL) + theme_minimal()

df_desc2 <- wvs0 %>% group_by(B_COUNTRY_ALPHA) %>% summarise(zPPOL = mean(zPPOL, na.rm=TRUE))
ggplot(df_desc2, aes(x=zPPOL, y=reorder(B_COUNTRY_ALPHA, zPPOL), fill=B_COUNTRY_ALPHA)) + 
  geom_col(show.legend = FALSE) + labs(y=NULL) + theme_minimal()

# ACP sobre confianza
# PCA(wvs0[,99:104]) — confianza endógena y exógena

message("ACP y descriptivos completados.")
