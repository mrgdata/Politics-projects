# =============================================================================
# Voto de Clase en España - Modelos de regresión logística
# Tres modelos: LEFT (2011-2016), LEFT (todos), EXRIG (2019)
# =============================================================================

library(tidyverse)
library(nnet)
library(ggeffects)

# --- Cargar datos preparados ---
source("01_data_prep.R")

# --- Modelo 1: Izquierda vs Otros (2011-2016) ---

filtered_df <- subset(df, Comicio %in% c('2011', '2015', '2016'))
filtered_df$LEFT <- relevel(filtered_df$LEFT, ref = "Otros")

model1 <- glm(LEFT ~ Clase * Comicio + EDU + SEX + EDAD + TAMUNI, 
              data = filtered_df, family=binomial)
summary(model1)

# --- Modelo 2: Izquierda vs Otros (todos los comicios) ---

df_clean <- na.omit(df)
df_clean$LEFT <- relevel(df_clean$LEFT, ref = "Otros")

model2 <- glm(LEFT ~ Clase * Comicio + EDU + SEX + EDAD + TAMUNI, 
              data = df_clean, family=binomial)
summary(model2)

# --- Modelo 3: Extrema derecha vs Otros (2019) ---

df_clean$EXRIG <- relevel(df_clean$EXRIG, ref = "Otros")

model3 <- glm(EXRIG ~ Clase * Comicio + EDU + SEX + EDAD + TAMUNI, 
              data = df_clean, family=binomial)
summary(model3)

message("Modelos estimados: model1 (LEFT 2011-16), model2 (LEFT todos), model3 (EXRIG).")
