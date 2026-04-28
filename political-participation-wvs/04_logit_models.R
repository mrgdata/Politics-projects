# =============================================================================
# Participación Política en las Américas (WVS7) - Modelos Logit y Lineales
# Modelos con interacciones para NA y LA
# =============================================================================

library(performance)
library(ggeffects)
library(dotwhisker)
library(tidyverse)

# --- Cargar datos y RF (incluye recodificación de variables) ---
source("03_random_forest.R")

# --- MODELO LOGIT 1: Voto - Norteamérica ---

model10 <- glm(voto ~ Interés_Política + Elecciones_honestas_afectan + Identificación_partidista + Edad +
                Importancia_democracia + Votos_justos + Importancia_Politica + Líderes_elecciones_libres +
                Valores_seculares + Valores_emancipatorios,
              family= quasibinomial, data=wvsNA, weights=a)
summary(model10)
r2_nagelkerke(model10)

model11 <- glm(voto ~ Interés_Política + Elecciones_honestas_afectan + Identificación_partidista + Edad +
                Importancia_democracia + Votos_justos + Importancia_Politica + Líderes_elecciones_libres +
              Valores_seculares + Valores_emancipatorios + Edad*Elecciones_honestas_afectan + Edad*Valores_seculares,
              family= quasibinomial, data=wvsNA, weights=a)
summary(model11)
r2_nagelkerke(model11)

dwplot(list(model10, model11), ci_method="wald",
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2))

# --- MODELO LOGIT 2: Voto - Latinoamérica ---

model20 <- glm(voto ~ Edad + Valores_seculares + Valores_emancipatorios + Información_TV + N_hijos +
                 Grupo_ocupacional + Tipo_empleo + N_personas_hogar + endtrust + Elecciones_honestas_afectan,
               family= quasibinomial, data=wvsLA, weights=b)
summary(model20)
r2_nagelkerke(model20)

model21 <- glm(voto ~ Edad + Valores_seculares + Valores_emancipatorios + Información_TV + N_hijos +
                Grupo_ocupacional + Tipo_empleo + N_personas_hogar + endtrust + Elecciones_honestas_afectan +
                Grupo_ocupacional*endtrust + Grupo_ocupacional*Elecciones_honestas_afectan + Edad*N_personas_hogar + Edad*Elecciones_honestas_afectan + Edad*Valores_seculares,
              family= quasibinomial, data=wvsLA, weights=b)
summary(model21)
r2_nagelkerke(model21)

dwplot(list(model20, model21), ci_method="wald",
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2))

# --- MODELO LINEAL 3: PPOL - Norteamérica ---

wvsNA <- mutate(wvsNA, Ideología_sqr = Ideología^2)
model30 <- lm(zPPOL ~ Valores_seculares + Valores_emancipatorios + Información_RRSS + Interés_Política + Derechos_civiles +
                Ideología + Ideología_sqr + Importancia_Politica + Líder_fuerte_autocrático + Afiliación_org_cívicas + Discusion_pol_amigos, data=wvsNA[,-310], weights=a)
summary(model30)
r2(model30)

dwplot(model30, ci_method="wald",
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2))

par(mfrow = c(2, 2))
plot(model30)

# --- MODELO LINEAL 4: PPOL - Latinoamérica ---

model40 <- lm(zPPOL ~ Edad + Valores_seculares + Valores_emancipatorios + Información_Internet + Interés_Política + 
               Grupo_ocupacional + Importancia_Politica + Nivel_Educativo + no_polpart + Discusion_pol_amigos, data=wvsLA[,-310], weights=b, proximity=TRUE)
summary(model40)
r2(model40)

par(mfrow = c(2, 2))
plot(model40)

model41 <- lm(zPPOL ~ Edad + Valores_seculares + Valores_emancipatorios + Información_Internet + Interés_Política + 
                Grupo_ocupacional + Importancia_Politica + Nivel_Educativo + no_polpart + Discusion_pol_amigos +
                Edad*Valores_emancipatorios + Edad*Importancia_Politica + Edad*no_polpart +
                Grupo_ocupacional*Valores_emancipatorios + Grupo_ocupacional*Importancia_Politica + Grupo_ocupacional*no_polpart +
                Nivel_Educativo*Valores_emancipatorios + Nivel_Educativo*Importancia_Politica + Nivel_Educativo*no_polpart, data=wvsLA[,-310], weights=b, proximity=TRUE)
summary(model41)
r2(model41)

dwplot(list(model40, model41), ci_method="wald",
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2))

message("Modelos logit y lineales completados.")
