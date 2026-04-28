# =============================================================================
# Diferencias en Diferencias - Visualizaciones
# Tendencias electorales, boxplots y efectos DiD
# =============================================================================

library(tidyverse)
library(ggpubr)

# --- Cargar datos ---
source("01_data_prep.R")

# --- Gráfico de tendencias electorales ---

DiD %>% ggplot(aes(x=Año, y= Voto, group = Provincia, color = Provincia)) +
  geom_line() + geom_point() + theme_bw() + 
  geom_vline(xintercept=c(1992, 2008), linetype= "dashed") + 
  ylab("Porcentaje de Voto al PSOE")

# --- Boxplots ---

DiD %>% ggplot(aes(x=Dummy_reconversion, y= Voto, color=Provincia)) + geom_boxplot() +
  theme_bw() + xlab("Reconversión industrial: antes y después")

DiD %>% ggplot(aes(x=Dummy_crisis, y= Voto, color=Provincia)) + geom_boxplot() +
  theme_bw() + xlab("Crisis económica: antes y después")

# --- Efectos DiD ---

df_efectos <- data.frame(
  efecto = c(-13.7, -3.99), 
  em = c(1.64*7.81, 1.64*7.60), 
  Periodo = c("reconversión", "crisis")
)

df_efectos %>% ggplot(aes(x=Periodo, y=efecto, color=Periodo)) + geom_point(size=2) + 
  geom_errorbar(aes(ymin=efecto-em, ymax=efecto+em), width=.2) + theme_bw() +
  ylab("Efecto diferencial en el voto") + labs(subtitle = "CI 90%")
