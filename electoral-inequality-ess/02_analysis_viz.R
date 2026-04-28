# =============================================================================
# Desigualdad Electoral en Europa - Análisis y visualización
# Indicadores P90/P50, correlación con desigualdad de ingresos, gráfico
# =============================================================================

library(tidyverse)
library(ggrepel)
library(writexl)

# --- Cargar datos ---
source("01_data_prep.R")

# --- Cálculo de indicadores de desigualdad política ---

df0 <- ESS %>% group_by(cntry) %>% summarize(
  voteP90 = sum(voter[income == "P90"], w=anweight, na.rm=TRUE), 
  voteP50 = sum(voter[income == "P50"], w=anweight, na.rm=TRUE),
  Inequality_vote = (voteP90/voteP50)
)

DF1 <- ESS %>% group_by(cntry) %>% summarize(voteP50 = sum(voter[income == "P50"], w=anweight, na.rm=TRUE))
DF2 <- ESS %>% group_by(cntry) %>% summarize(demP50 = sum(dem[income == "P50"], w=anweight, na.rm=TRUE))
DF3 <- ESS %>% group_by(cntry) %>% summarize(demP90 = sum(dem[income == "P90"], w=anweight, na.rm=TRUE))

df0 <- merge(df0, DF1)
df1 <- merge(DF2, DF3)
df <- merge(df1, df0)

INEPOL <- merge(df, WID_Data)
INEPOL <- INEPOL %>% mutate(Inequality_vote = (voteP90/voteP50)) %>% mutate(Inequality_dem = (demP90/demP50)) 

# --- Gráfico: desigualdad electoral vs manifestación ---

ggplot(INEPOL, aes(Inequality_vote, Inequality_dem)) + 
  geom_point(size=2) + geom_abline(slope=1, intercept=0) +
  xlim(0, 1.1) + ylim(0, 1.1) + 
  labs(x="Voto", y="Manifestación", title= "Desigualdad política en Europa") + 
  theme_bw() + 
  geom_label_repel(aes(label = cntry),
                   box.padding   = 0.4, 
                   point.padding = 0.05,
                   nudge_x=0.1,
                   segment.color = 'grey50') +
  theme_classic()

# --- Exportar resultados ---

tryCatch({
  write_xlsx(INEPOL, "INEPOL.xlsx")
  message("Resultados exportados a INEPOL.xlsx")
}, error = function(e) {
  message("No se pudo exportar INEPOL.xlsx: ", e$message)
})

# --- Gráfico extra: distribución de renta ---

ESS %>% ggplot(aes(hinctnta, fill = hinctnta, show.legend = FALSE)) + geom_bar() + coord_flip() + theme_bw()
