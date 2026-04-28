# =============================================================================
# Participación Política en las Américas (WVS7) - Bosques Aleatorios
# 4 modelos: Voto NA, Voto LA, PPOL NA, PPOL LA
# =============================================================================

library(caret)
library(randomForest)
library(tidyverse)

# --- Cargar datos ---
source("01_data_prep.R")

# --- MODELO 1: Voto - Norteamérica ---

set.seed(25)
model1 <- randomForest(voto ~ ., data=wvsNA[,-311], weights=a, proximity=TRUE)
model1
i_scores <- varImp(model1, conditional=TRUE, scale=TRUE)

# --- MODELO 2: Voto - Latinoamérica ---

model2 <- randomForest(voto ~ ., data=wvsLA[,-311], weights=b, proximity=TRUE)
model2
i_scores <- varImp(model2, conditional=TRUE, scale=TRUE)

# --- MODELO 3: PPOL - Norteamérica ---

model3 <- randomForest(zPPOL ~ ., data=wvsNA[,-310], weights=a)
model3
i_scores <- varImp(model3, conditional=TRUE, scale=TRUE)

# --- MODELO 4: PPOL - Latinoamérica ---

model4 <- randomForest(zPPOL ~ ., data=wvsLA[,-310], weights=b)
model4
i_scores <- varImp(model4, conditional=TRUE, scale=TRUE)

# --- Recodificación de principales variables para modelos interpretables ---

wvsNA <- wvsNA %>% mutate(
  Importancia_Politica = (Q4 - mean(Q4))/sd(Q4), 
  Interés_Política = (Q199 - mean(Q199))/sd(Q199), 
  Elecciones_honestas_afectan = (Q234 - mean(Q234))/sd(Q234), 
  Identificación_partidista = Q223, 
  Votos_justos = (Q224 - mean(Q224))/sd(Q224), 
  Grupo_ocupacional = Q281, 
  Edad = (Q262 - mean(Q262))/sd(Q262), 
  Importancia_democracia = (Q250 - mean(Q250))/sd(Q250),
  Valores_emancipatorios = (resemaval - mean(resemaval))/sd(resemaval),  
  Valores_seculares = (sacsecval - mean(sacsecval))/sd(sacsecval), 
  Líderes_elecciones_libres = (Q243 - mean(Q243))/sd(Q243), 
  Afiliación_org_cívicas = (no_polpart - mean(no_polpart))/sd(no_polpart),
  Tipo_empleo = Q279,
  Información_TV = (Q202 - mean(Q202))/sd(Q202), 
  N_hijos = (Q274 - mean(Q274))/sd(Q274), 
  N_personas_hogar = (Q270 - mean(Q270))/sd(Q270), 
  Derechos_civiles = (Q246 - mean(Q246))/sd(Q246),
  Discusion_pol_amigos = (Q200 - mean(Q200))/sd(Q200),
  Ideología = (Q240 - mean(Q240))/sd(Q240),
  Líder_fuerte_autocrático = (Q235 - mean(Q235))/sd(Q235),
  Información_RRSS = (Q207 - mean(Q207))/sd(Q207),
  Información_Internet = (Q206 - mean(Q206))/sd(Q206),
  Nivel_Educativo = Q275R
)

wvsLA <- wvsLA %>% mutate(
  Importancia_Politica = (Q4 - mean(Q4))/sd(Q4), 
  Interés_Política = (Q199 - mean(Q199))/sd(Q199), 
  Elecciones_honestas_afectan = (Q234 - mean(Q234))/sd(Q234), 
  Identificación_partidista = Q223, 
  Votos_justos = (Q224 - mean(Q224))/sd(Q224), 
  Grupo_ocupacional = Q281, 
  Edad = (Q262 - mean(Q262))/sd(Q262), 
  Importancia_democracia = (Q250 - mean(Q250))/sd(Q250),
  Valores_emancipatorios = (resemaval - mean(resemaval))/sd(resemaval),  
  Valores_seculares = (sacsecval - mean(sacsecval))/sd(sacsecval), 
  Líderes_elecciones_libres = (Q243 - mean(Q243))/sd(Q243), 
  Afiliación_org_cívicas = (no_polpart - mean(no_polpart))/sd(no_polpart),
  Tipo_empleo = Q279,
  Información_TV = (Q202 - mean(Q202))/sd(Q202), 
  N_hijos = (Q274 - mean(Q274))/sd(Q274), 
  N_personas_hogar = (Q270 - mean(Q270))/sd(Q270),
  Derechos_civiles = (Q246 - mean(Q246))/sd(Q246),
  Discusion_pol_amigos = (Q200 - mean(Q200))/sd(Q200),
  Ideología = (Q240 - mean(Q240))/sd(Q240),
  Líder_fuerte_autocrático = (Q235 - mean(Q235))/sd(Q235),
  Información_RRSS = (Q207 - mean(Q207))/sd(Q207),
  Información_Internet = (Q206 - mean(Q206))/sd(Q206),
  Nivel_Educativo = Q275R, 
  endtrust = (endtrust - mean(endtrust))/sd(endtrust)
)

# --- Modelos RF con variables seleccionadas ---

# MODELO 1 NA - voto (variables seleccionadas)
model1 <- randomForest(voto ~ Interés_Política + Elecciones_honestas_afectan + Identificación_partidista + Edad +
                         Importancia_democracia + Votos_justos + Importancia_Politica + Líderes_elecciones_libres +
                         Valores_seculares + Valores_emancipatorios, data=wvsNA[,-311], weights=a, proximity=TRUE)

# MODELO 2 LA - voto (variables seleccionadas)
wvsLA0 <- sample_n(wvsLA, 6000)
model2 <- randomForest(voto ~ Edad + Valores_seculares + Valores_emancipatorios + Información_TV + N_hijos + 
                         Grupo_ocupacional + Tipo_empleo + N_personas_hogar + endtrust + Elecciones_honestas_afectan, 
                       data=wvsLA0[,-311], weights=wvsLA0$peso, proximity=TRUE)

# MODELO 3 NA - PPOL
model3 <- randomForest(zPPOL ~ Valores_seculares + Valores_emancipatorios + Información_RRSS + Interés_Política + Derechos_civiles +
                         Ideología + Importancia_Politica + Líder_fuerte_autocrático + Afiliación_org_cívicas + Discusion_pol_amigos, 
                       data=wvsNA[,-310], weights=a)

# MODELO 4 LA - PPOL
model4 <- randomForest(zPPOL ~ Edad + Valores_seculares + Valores_emancipatorios + Información_Internet + Interés_Política + 
                         Grupo_ocupacional + Importancia_Politica + Nivel_Educativo + no_polpart + Discusion_pol_amigos, 
                       data=wvsLA[,-310], weights=b)

message("Bosques aleatorios completados: 4 modelos estimados.")
