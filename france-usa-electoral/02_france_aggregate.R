# =============================================================================
# Análisis Electoral Francia - Datos agregados
# Diacrónico (WPID) y sincrónico (presidenciales por comuna)
# =============================================================================

library(tidyverse)
library(readxl)
library(readr)
library(ggpubr)
library(stringr)

# --- Carga WPID ---

tryCatch({
  WPID_graphdatas <- read_excel("WPID-graphdatas.xlsx")
}, error = function(e) {
  stop("No se encontró WPID-graphdatas.xlsx.\n", "Error: ", e$message)
})

# --- Análisis diacrónico ---

WPID_graphdatas <- WPID_graphdatas %>%
  filter(variable_name == "Income group"  | variable_name == "Rural-urban location" | variable_name == "Education group") %>%
  mutate(share=as.numeric(share)) %>% mutate(share=share*100) %>%
  select(variable_name, party_name, subvariable_name, election, share, lower, upper)

# Le Pen por territorio
WPID_graphdatas %>% mutate(Territorio = subvariable_name) %>%
  filter(variable_name == "Rural-urban location" & election > 2002 & party_name == "National Front (FN)")  %>%
  ggplot(aes(x=as.factor(election), y=share, fill=Territorio)) + geom_col(position="dodge") +
  geom_errorbar(aes(ymin=as.numeric(lower)*100, ymax=as.numeric(upper)*100), width=0.3, position = position_dodge(width = 0.9)) + theme_bw() +
  labs(y="Voto a Le Pen", x="Elecciones legislativas")

# Le Pen por ingresos
WPID_graphdatas %>% mutate(Ingresos = subvariable_name) %>%
  filter(variable_name == "Income group" & election > 2002 & party_name == "National Front (FN)")  %>%
  ggplot(aes(x=as.factor(election), y=share, fill=Ingresos)) + geom_col(position="dodge") +
  geom_errorbar(aes(ymin=as.numeric(lower)*100, ymax=as.numeric(upper)*100), width=0.3, position = position_dodge(width = 0.9)) + theme_bw() +
  labs(y="Voto a Le Pen", x="Elecciones legislativas")

# Izquierda por territorio
WPID_graphdatas %>% mutate(Territorio = subvariable_name) %>%
  filter(variable_name == "Rural-urban location" & election > 2002 & party_name == "<special>Left-wing parties (SFIO/PS, PCF, radicals, greens, other left)</special>")  %>%
  ggplot(aes(x=as.factor(election), y=share, fill=Territorio)) + geom_col(position="dodge") +
  geom_errorbar(aes(ymin=as.numeric(lower)*100, ymax=as.numeric(upper)*100), width=0.3, position = position_dodge(width = 0.9)) + theme_bw() +
  labs(y="Voto a la izquierda", x="Elecciones legislativas")

# Izquierda por ingresos
WPID_graphdatas %>% mutate(Ingresos = subvariable_name) %>%
  filter(variable_name == "Income group" & election > 2002 & party_name == "<special>Left-wing parties (SFIO/PS, PCF, radicals, greens, other left)</special>")  %>%
  ggplot(aes(x=as.factor(election), y=share, fill=Ingresos)) + geom_col(position="dodge") +
  geom_errorbar(aes(ymin=as.numeric(lower)*100, ymax=as.numeric(upper)*100), width=0.3, position = position_dodge(width = 0.9)) + theme_bw() +
  labs(y="Voto a la izquierda", x="Elecciones legislativas")

# =============================================================================
# Sincrónico - Presidenciales por comuna (datos agregados)
# =============================================================================

tryCatch({
  Taux_de_motorisation_a_lIRIS_Global_Map_Solution <- read_excel("Taux_de_motorisation_a_lIRIS-Global_Map_Solution.xlsx", 
                                                                 col_types = c("text", "text", "text", 
                                                                               "text", "numeric", "numeric", "numeric"))
  insee_rp_hist_1968 <- read_excel("insee_rp_hist_1968.xlsx", skip = 4)
  TCRD_014 <- read_excel("TCRD_014.xlsx", skip = 3)
  X04_resultats_par_commune <- read_csv("04-resultats-par-commune.csv")
}, error = function(e) {
  stop("No se encontraron los archivos de datos franceses (motorización, INSEE, TCRD, resultados).\n",
       "Error: ", e$message)
})

insee_rp_hist_1968 <- insee_rp_hist_1968 %>% mutate(commune_name = libgeo) %>% filter(an == 2018)

Taux_de_motorisation_a_lIRIS_Global_Map_Solution <- Taux_de_motorisation_a_lIRIS_Global_Map_Solution %>% 
  mutate(commune_name = `Nom Commune`) %>% select(commune_name, `Taux de motorisation Commune`)

TOTAL <- X04_resultats_par_commune %>% select(dep_name, commune_name, cand_nom,
                                              cand_rapport_exprim, cand_rapport_inscrits, exprimes_pourc_inscrits)

TOTAL <- merge(insee_rp_hist_1968, TOTAL)
TOTAL <- merge(Taux_de_motorisation_a_lIRIS_Global_Map_Solution, TOTAL)
TCRD_014 <- TCRD_014 %>% mutate(dep_name = `...2`)
TOTAL <- merge(TOTAL, TCRD_014)

TOTAL <- drop_na(TOTAL)

quantile(TOTAL$`Taux de motorisation Commune`)
TOTAL$Tasa[TOTAL$`Taux de motorisation Commune` < 0.8] <- "<80%"
TOTAL$Tasa[TOTAL$`Taux de motorisation Commune`  > 0.8 & TOTAL$`Taux de motorisation Commune` < 0.95] <- "80-95%"
TOTAL$Tasa[TOTAL$`Taux de motorisation Commune`  > 0.95] <- ">95%"
TOTAL$Tasa <- factor(TOTAL$Tasa, levels=c("<80%", "80-95%", ">95%"))

# --- Gráficos sincronicos ---

# Densidad vs voto
A <- TOTAL %>% filter(cand_nom == "LE PEN") %>% ggplot(aes(dens_pop, cand_rapport_exprim)) + 
  geom_point() + geom_smooth(color = "dodgerblue4") + theme_bw() + labs(y = "Voto", x= "Densidad de población") + ggtitle("Le Pen") + ylim(0,80)

B <- TOTAL %>% filter(cand_nom == "MACRON") %>% ggplot(aes(dens_pop, cand_rapport_exprim)) + 
  geom_point() + geom_smooth(color = "deepskyblue") + theme_bw() + labs(y = " ", x= "Densidad de población") + ggtitle("Macron") + ylim(0,80)
ggarrange(A, B)

# Motorización
C <- TOTAL %>% na.omit() %>% filter(cand_nom == "LE PEN" | cand_nom == "MÉLENCHON" | cand_nom == "MACRON") %>%
  mutate(candidato = cand_nom) %>%
  ggplot(aes(Tasa, cand_rapport_exprim, fill = candidato)) + 
  geom_boxplot(outlier.shape = NA) + theme_bw() + labs(y = "Voto", x= "Vehículos por habitante")  +
  scale_fill_manual(values = c("dodgerblue3", "deepskyblue", "red2")) + ylim(0,60)
C

# Obreros
D <- TOTAL %>% na.omit() %>% select(!commune_name) %>% filter(cand_nom == "LE PEN") %>% mutate(ouvriers = `Part des ouvriers`) %>% 
  group_by(dep_name) %>% summarise(cand_rapport_exprim=mean(cand_rapport_exprim), ouvriers=mean(ouvriers)) %>% 
  ggplot(aes(ouvriers, cand_rapport_exprim)) + 
  geom_point() + geom_smooth(method=lm, color = "dodgerblue4") + theme_bw() +
  labs(y = "Voto", x= "Porcentaje de obreros") + ggtitle("Le Pen") + ylim(0,40)

E <- TOTAL %>% na.omit() %>% select(!commune_name) %>% filter(cand_nom == "MÉLENCHON") %>% 
  mutate(ouvriers = `Part des ouvriers`) %>% 
  group_by(dep_name) %>% summarise(cand_rapport_exprim=mean(cand_rapport_exprim), ouvriers=mean(ouvriers)) %>% 
  ggplot(aes(ouvriers, cand_rapport_exprim)) + 
  geom_point() + geom_smooth(method=lm, color = "red2") + theme_bw() + labs(y = "Voto", x= "Porcentaje de obreros") + ggtitle("Mélenchon") + ylim(0,40)

ggarrange(D, E)

# Profesionales
Z <- TOTAL %>% na.omit() %>% select(!commune_name) %>% filter(cand_nom == "MACRON") %>% 
  mutate(Profesionales = `Part des cadres, professions intellect. supérieures`) %>% 
  group_by(dep_name) %>% summarise(cand_rapport_exprim=mean(cand_rapport_exprim), Profesionales=mean(Profesionales)) %>% 
  ggplot(aes(Profesionales, cand_rapport_exprim)) + 
  geom_point() + geom_smooth(method=lm, color = "deepskyblue") + theme_bw() + labs(y = "Voto", x= "Porcentaje de profesionales y directivos") + 
  ggtitle("Macron") + ylim(0, 40)

Y <- TOTAL %>% na.omit() %>% select(!commune_name) %>% filter(cand_nom == "LE PEN") %>% 
  mutate(Profesionales = `Part des cadres, professions intellect. supérieures`) %>% 
  group_by(dep_name) %>% summarise(cand_rapport_exprim=mean(cand_rapport_exprim), Profesionales=mean(Profesionales)) %>% 
  ggplot(aes(Profesionales, cand_rapport_exprim)) + 
  geom_point() + geom_smooth(method=lm, color = "dodgerblue4") + theme_bw() + labs(y = "Voto", x= "Porcentaje de profesionales y directivos") + 
  ggtitle("Le Pen") + ylim(0, 40)

ggarrange(Z, Y)
