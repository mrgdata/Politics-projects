# =============================================================================
# Participación Política en las Américas (WVS7) - Visualizaciones y descriptivos
# Variable importance plots y tablas descriptivas
# =============================================================================

library(caret)
library(randomForest)
library(vtable)
library(tidyverse)

# --- Cargar modelos RF (incluye recodificación) ---
source("03_random_forest.R")

# --- Variable importance plots ---

plot_varimp <- function(model, title="") {
  i_scores <- varImp(model, conditional=TRUE, scale=TRUE)
  i_scores <- i_scores %>% tibble::rownames_to_column("var") 
  i_scores <- i_scores[order(-i_scores$Overall),]
  i_scores$var <- i_scores$var %>% as.factor()
  i_scores <- i_scores[1:10,]
  i_scores %>% ggplot(aes(x=Overall, y=reorder(var, Overall), fill=var)) + 
    geom_col(show.legend = FALSE) + theme_minimal() + labs(x = NULL, y = NULL, title = title)
}

plot_varimp(model1, "Voto - Norteamérica")
plot_varimp(model2, "Voto - Latinoamérica")
plot_varimp(model3, "Participación Política - Norteamérica")
plot_varimp(model4, "Participación Política - Latinoamérica")

# --- Descriptivos ---

st(wvsLA, vars = c("Edad", "Valores_seculares","Valores_emancipatorios", "Información_TV", "N_hijos",
                   "Interés_Política", "N_personas_hogar", "endtrust", "Elecciones_honestas_afectan",
                   "Información_Internet", "Afiliación_org_cívicas","Discusion_pol_amigos", "Importancia_Politica"))

st(wvsLA, vars = c("Nivel_Educativo", "Grupo_ocupacional", "Tipo_empleo"))

st(wvsNA, vars = c("Edad", "Valores_seculares","Valores_emancipatorios", "Ideología", "Líder_fuerte_autocrático",
                   "Interés_Política", "Elecciones_honestas_afectan", "Derechos_civiles", "Identificación_partidista",
                   "Importancia_democracia", "Votos_justos", "Líderes_elecciones_libres",
                   "Información_RRSS", "Afiliación_org_cívicas","Discusion_pol_amigos", "Importancia_Politica"))

message("Visualizaciones y descriptivos completados.")
