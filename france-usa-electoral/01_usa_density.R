# =============================================================================
# Análisis Electoral EEUU - Densidad de población vs voto Trump/Biden
# =============================================================================

library(tidyverse)
library(readr)
library(ggpubr)

# --- Carga de datos ---

tryCatch({
  DensityUS <- read_csv("Average_Household_Size_and_Population_Density_-_County.csv")
}, error = function(e) {
  stop("No se encontró el archivo de densidad US. Descárgalo y colócalo en esta carpeta.\n",
       "Error: ", e$message)
})

tryCatch({
  PresidentUS <- read_csv("countypres_2000-2020.csv")
}, error = function(e) {
  stop("No se encontró countypres_2000-2020.csv.\n", "Error: ", e$message)
})

# --- Preparación ---

DensityUS <- DensityUS %>% separate(NAME, c("county_name", "word"))
DensityUS$county_name <- toupper(DensityUS$county_name)
DensityUS <- DensityUS %>% select(county_name, B01001_calc_PopDensity)

PresidentUS <- PresidentUS %>% filter(year == 2020, mode == "TOTAL",
                                      candidate == "DONALD J TRUMP" | candidate == "JOSEPH R BIDEN JR") %>%
  select(county_name, candidate, candidatevotes, totalvotes)
PresidentUS <- PresidentUS %>% mutate(vote_share = (candidatevotes/totalvotes)*100)

US <- merge(DensityUS, PresidentUS)

# --- Gráficos ---

USA <- US %>% filter(candidate == "DONALD J TRUMP") %>% ggplot(aes(B01001_calc_PopDensity, vote_share)) + 
  geom_point() + geom_smooth(color = "red2") + theme_bw() + labs(y = "Voto", x= "Densidad de población") + ggtitle("Trump") + ylim(0,100)

USB <- US %>% filter(candidate == "JOSEPH R BIDEN JR") %>% ggplot(aes(B01001_calc_PopDensity, vote_share)) + 
  geom_point() + geom_smooth(color = "dodgerblue4") + theme_bw() + labs(y = "Voto", x= "Densidad de población") + ggtitle("Biden") + ylim(0,100)

ggarrange(USA, USB)
