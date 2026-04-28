# =============================================================================
# Diferencias en Diferencias - Modelos DiD y ANOVA
# =============================================================================

library(lfe)
library(broom)

# --- Cargar datos ---
source("01_data_prep.R")

# --- Modelos DiD ---

# Reconversión industrial
felm(Voto ~ treat_reconversion | Dummy_reconversion + treat_group | 0 | Provincia, DiD) %>% tidy()
felm(Voto ~ treat_reconversion | Dummy_reconversion + treat_group, DiD) %>% tidy()

# Crisis del 2008
felm(Voto ~ treat_crisis | Dummy_crisis + treat_group, DiD) %>% tidy()

# --- ANOVA por provincia ---

a <- DiD %>% filter(Provincia == "Murcia")
summary(aov(Voto ~ Dummy_reconversion + RpCap + Paro, a))
summary(aov(Voto ~ Dummy_crisis + RpCap + Paro, a))

b <- DiD %>% filter(Provincia == "Sevilla")
summary(aov(Voto ~ Dummy_crisis, b))
summary(aov(Voto ~ Dummy_reconversion, b))

message("Modelos DiD y ANOVA completados.")
