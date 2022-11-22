library(tidyverse)
library(dplyr)
library(readxl)
library(readr)
library(haven)
library(stats)
library(ggpubr)
library(ggeffects)
library(stringr)


WPID_graphdatas <- read_excel("C:/Users/USUARIO/Downloads/WPID-graphdatas.xlsx")
View(WPID_graphdatas)

ESS9 <- read_sav("~/MEGA/Manu/UPO/Curso 5º-2/Pol/ESS9.sav")
View(ESS9)

DensityUS <- read_csv("C:/Users/USUARIO/Downloads/Average_Household_Size_and_Population_Density_-_County.csv")
View(DensityUS)

PresidentUS <- read_csv("MEGA/Manu/Mapa voto/countypres_2000-2020.csv")
View(PresidentUS)

Taux_de_motorisation_a_lIRIS_Global_Map_Solution <- read_excel("C:/Users/USUARIO/Downloads/Taux_de_motorisation_a_lIRIS-Global_Map_Solution.xlsx", 
                                                               col_types = c("text", "text", "text", 
                                                                             "text", "numeric", "numeric", "numeric"))
View(Taux_de_motorisation_a_lIRIS_Global_Map_Solution)

insee_rp_hist_1968 <- read_excel("C:/Users/USUARIO/Downloads/insee_rp_hist_1968.xlsx", skip = 4)
View(insee_rp_hist_1968)

TCRD_014 <- read_excel("C:/Users/USUARIO/Downloads/TCRD_014.xlsx", 
                       skip = 3)
View(TCRD_014)

X04_resultats_par_commune <- read_csv("C:/Users/USUARIO/Downloads/04-resultats-par-commune.csv")
View(X04_resultats_par_commune)



# USA

DensityUS <- DensityUS %>% separate(NAME, c("county_name", "word"))
DensityUS$county_name <- toupper(DensityUS$county_name)

DensityUS <- DensityUS %>% select(county_name, B01001_calc_PopDensity)

PresidentUS <- PresidentUS %>% filter(year == 2020, mode == "TOTAL",
                                      candidate == "DONALD J TRUMP" | candidate == "JOSEPH R BIDEN JR") %>%
  select(county_name, candidate, candidatevotes, totalvotes)

PresidentUS <- PresidentUS %>% mutate(vote_share = (candidatevotes/totalvotes)*100)

US <- merge(DensityUS, PresidentUS)

USA <- US %>% filter(candidate == "DONALD J TRUMP") %>% ggplot(aes(B01001_calc_PopDensity, vote_share)) + 
  geom_point() + geom_smooth(color = "red2") + theme_bw() + labs(y = "Voto",
                                                                        x= "Densidad de población") + ggtitle("Trump") + ylim(0,100)

USB <- US %>% filter(candidate == "JOSEPH R BIDEN JR") %>% ggplot(aes(B01001_calc_PopDensity, vote_share)) + 
  geom_point() + geom_smooth(color = "dodgerblue4") + theme_bw() + labs(y = "Voto",
                                                                        x= "Densidad de población") + ggtitle("Biden") + ylim(0,100)

ggarrange(USA, USB)


#FRANCIA

# Diacrónico

WPID_graphdatas <- WPID_graphdatas %>%
  filter(variable_name == "Income group"  | variable_name == "Rural-urban location" | variable_name == "Education group") %>%
  mutate(share=as.numeric(share)) %>% mutate(share=share*100) %>%
  select(variable_name, party_name, subvariable_name, election, share, lower, upper)

WPID_graphdatas %>% mutate(Territorio = subvariable_name) %>%
  filter(variable_name == "Rural-urban location" & election > 2002 & party_name == "National Front (FN)")  %>%
ggplot(aes(x=as.factor(election), y=share, fill=Territorio)) + geom_col(position="dodge") +
  geom_errorbar(aes(ymin=as.numeric(lower)*100, ymax=as.numeric(upper)*100), width=0.3, position = position_dodge(width = 0.9)) + theme_bw() +
  labs(y="Voto a Le Pen", x="Elecciones legislativas")

WPID_graphdatas %>% mutate(Ingresos = subvariable_name) %>%
  filter(variable_name == "Income group" & election > 2002 & party_name == "National Front (FN)")  %>%
  ggplot(aes(x=as.factor(election), y=share, fill=Ingresos)) + geom_col(position="dodge") +
  geom_errorbar(aes(ymin=as.numeric(lower)*100, ymax=as.numeric(upper)*100), width=0.3, position = position_dodge(width = 0.9)) + theme_bw() +
  labs(y="Voto a Le Pen", x="Elecciones legislativas")

WPID_graphdatas %>% mutate(Territorio = subvariable_name) %>%
  filter(variable_name == "Rural-urban location" & election > 2002 & party_name == "<special>Left-wing parties (SFIO/PS, PCF, radicals, greens, other left)</special>")  %>%
  ggplot(aes(x=as.factor(election), y=share, fill=Territorio)) + geom_col(position="dodge") +
  geom_errorbar(aes(ymin=as.numeric(lower)*100, ymax=as.numeric(upper)*100), width=0.3, position = position_dodge(width = 0.9)) + theme_bw() +
  labs(y="Voto a la izquierda", x="Elecciones legislativas")


WPID_graphdatas %>% mutate(Ingresos = subvariable_name) %>%
  filter(variable_name == "Income group" & election > 2002 & party_name == "<special>Left-wing parties (SFIO/PS, PCF, radicals, greens, other left)</special>")  %>%
  ggplot(aes(x=as.factor(election), y=share, fill=Ingresos)) + geom_col(position="dodge") +
  geom_errorbar(aes(ymin=as.numeric(lower)*100, ymax=as.numeric(upper)*100), width=0.3, position = position_dodge(width = 0.9)) + theme_bw() +
  labs(y="Voto a la izquierda", x="Elecciones legislativas")



# Sincrónico presidenciales datos agregados

insee_rp_hist_1968 <- insee_rp_hist_1968 %>% mutate(commune_name = libgeo) %>% filter(an == 2018)

Taux_de_motorisation_a_lIRIS_Global_Map_Solution <- Taux_de_motorisation_a_lIRIS_Global_Map_Solution %>% 
  mutate(commune_name = `Nom Commune`) %>% select(commune_name, `Taux de motorisation Commune`)


TOTAL <-X04_resultats_par_commune %>% select(dep_name, commune_name, cand_nom,
                                             cand_rapport_exprim, cand_rapport_inscrits, exprimes_pourc_inscrits)



TOTAL <- merge(insee_rp_hist_1968, TOTAL)
TOTAL <- merge(Taux_de_motorisation_a_lIRIS_Global_Map_Solution, TOTAL)
TCRD_014 <- TCRD_014 %>% mutate(dep_name = `...2`)
TOTAL <- merge(TOTAL, TCRD_014)




TOTAL <- drop_na(TOTAL)
View(TOTAL)

quantile(TOTAL$`Taux de motorisation Commune`)
TOTAL$Tasa[TOTAL$`Taux de motorisation Commune` < 0.8] <- "<80%"
TOTAL$Tasa[TOTAL$`Taux de motorisation Commune`  > 0.8 & TOTAL$`Taux de motorisation Commune` < 0.95] <- "80-95%"
TOTAL$Tasa[TOTAL$`Taux de motorisation Commune`  > 0.95] <- ">95%"

TOTAL$Tasa <- factor(TOTAL$Tasa, levels=c("<80%", "80-95%", ">95%"))

#Gráfico


A <- TOTAL %>% filter(cand_nom == "LE PEN") %>% ggplot(aes(dens_pop, cand_rapport_exprim)) + 
  geom_point() + geom_smooth(color = "dodgerblue4") + theme_bw() + labs(y = "Voto",
                                                                        x= "Densidad de población") + ggtitle("Le Pen") + ylim(0,80)

B <- TOTAL %>% filter(cand_nom == "MACRON") %>% ggplot(aes(dens_pop, cand_rapport_exprim)) + 
  geom_point() + geom_smooth(color = "deepskyblue") + theme_bw() + labs(y = " ",
                                                                        x= "Densidad de población") + ggtitle("Macron") + ylim(0,80)
ggarrange(A, B)

C <- TOTAL %>% na.omit() %>% filter(cand_nom == "LE PEN" | cand_nom == "MÉLENCHON" | cand_nom == "MACRON") %>%
  mutate(candidato = cand_nom) %>%
  ggplot(aes(Tasa, cand_rapport_exprim, fill = candidato)) + 
  geom_boxplot(outlier.shape = NA) + theme_bw() + labs(y = "Voto", x= "Vehículos por habitante")  +
  scale_fill_manual(values = c("dodgerblue3", "deepskyblue", "red2")) + ylim(0,60)
C



D <- TOTAL %>% na.omit() %>% select(!commune_name) %>% filter(cand_nom == "LE PEN") %>% mutate(ouvriers = `Part des ouvriers`) %>% 
  group_by(dep_name) %>% summarise(cand_rapport_exprim=mean(cand_rapport_exprim), ouvriers=mean(ouvriers)) %>% 
  ggplot(aes(ouvriers, cand_rapport_exprim)) + 
  geom_point() + geom_smooth(method=lm, color = "dodgerblue4") + theme_bw() +
  labs(y = "Voto", x= "Porcentaje de obreros") + ggtitle("Le Pen") + ylim(0,40)
D


unique(TOTAL$cand_nom)


E <- TOTAL %>% na.omit() %>% select(!commune_name) %>% filter(cand_nom == "MÉLENCHON") %>% 
  mutate(ouvriers = `Part des ouvriers`) %>% 
  group_by(dep_name) %>% summarise(cand_rapport_exprim=mean(cand_rapport_exprim), ouvriers=mean(ouvriers)) %>% 
  ggplot(aes(ouvriers, cand_rapport_exprim)) + 
  geom_point() + geom_smooth(method=lm, color = "red2") + theme_bw() + labs(y = "Voto", x= "Porcentaje de obreros") + ggtitle("Mélenchon") + ylim(0,40)
E

ggarrange(D, E)

Z <- TOTAL %>% na.omit() %>% select(!commune_name) %>% filter(cand_nom == "MACRON") %>% 
  mutate(Profesionales = `Part des cadres, professions intellect. supérieures`) %>% 
  group_by(dep_name) %>% summarise(cand_rapport_exprim=mean(cand_rapport_exprim), Profesionales=mean(Profesionales)) %>% 
  ggplot(aes(Profesionales, cand_rapport_exprim)) + 
  geom_point() + geom_smooth(method=lm, color = "deepskyblue") + theme_bw() + labs(y = "Voto", x= "Porcentaje de profesionales y directivos") + 
  ggtitle("Macron") + ylim(0, 40)
Z

Y <- TOTAL %>% na.omit() %>% select(!commune_name) %>% filter(cand_nom == "LE PEN") %>% 
  mutate(Profesionales = `Part des cadres, professions intellect. supérieures`) %>% 
  group_by(dep_name) %>% summarise(cand_rapport_exprim=mean(cand_rapport_exprim), Profesionales=mean(Profesionales)) %>% 
  ggplot(aes(Profesionales, cand_rapport_exprim)) + 
  geom_point() + geom_smooth(method=lm, color = "dodgerblue4") + theme_bw() + labs(y = "Voto", x= "Porcentaje de profesionales y directivos") + 
  ggtitle("Le Pen") + ylim(0, 40)
Y

ggarrange(Z, Y)

###ESS


is.numeric(ESS9$imwbcnt)
is.numeric(ESS9$stfdem)

ESS9$domicil <- as.numeric(ESS9$domicil)
ESS9$domicil[ESS9$domicil == 1 | ESS9$domicil == 2] <- "Ciudades"
ESS9$domicil[ESS9$domicil > 2 & ESS9$domicil < 6] <- "Mundo rural"
unique(ESS9$domicil)
is.factor(ESS9$domicil)

ESS9$domicil <- factor(ESS9$domicil, levels=c("Ciudades", "Mundo rural"))


ESS9$prtvtdfr <- as.numeric(ESS9$prtvtdfr)
ESS9$prtvtdfr[ESS9$prtvtdfr != 7 & ESS9$prtvtdfr != 11] <- NA
ESS9$prtvtdfr[ESS9$prtvtdfr == 7] <- 0
ESS9$prtvtdfr[ESS9$prtvtdfr == 11] <- 1

df <- ESS9 %>% filter(!is.na(ESS9$domicil), !is.na(ESS9$imwbcnt), !is.na(ESS9$stfdem), !is.na(ESS9$prtvtdfr), cntry == "FR")
df <- as.data.frame(df)

model1 <- glm(prtvtdfr ~ domicil + imwbcnt + stfdem, df, family=quasibinomial)
summary(model1)

dat <- ggpredict(model1, terms = c("stfdem"))
a <- plot(dat) + labs(x="Satisfacción con la democracia", y="Voto a Le Pen", title="") 
dt <- ggpredict(model1, terms = c("imwbcnt"))
b <- plot(dt) + labs(x="La inmigración es positiva", y= "", title="") 

ggarrange(a, b)







library(plyr)

df <- df %>% ddply(c("domicil"), summarize, 
        mean1=mean(imwbcnt), mean2=mean(stfdem),
        upper1= mean(imwbcnt)+1.96*sd(imwbcnt)/sqrt(length(ESS9)/2),
        lower1=mean(imwbcnt)-1.96*sd(imwbcnt)/sqrt(length(ESS9)/2),
        upper2= mean(stfdem)+1.96*sd(stfdem)/sqrt(length(ESS9)/2),
        lower2=mean(stfdem)-1.96*sd(stfdem)/sqrt(length(ESS9)/2))


unique(ESS9$imwbcnt)
unique(ESS9$stfdem)


IM <- df %>% 
  ggplot(aes(domicil,mean1)) + geom_col(fill="lightskyblue", position="dodge") + 
  geom_errorbar(aes(ymin=lower1, ymax=upper1), 
                width=0.3, position = position_dodge(width = 0.9)) +
  theme_bw() + labs(x= " ", y=" ", title="La inmigración es positiva") + ylim(0, 6)


ST <- df %>% 
  ggplot(aes(domicil,mean2)) + geom_col(fill="lightsalmon", position="dodge") + 
  geom_errorbar(aes(ymin=lower2, ymax=upper2), 
                width=0.3, position = position_dodge(width = 0.9)) +
  theme_bw() + labs(x= " ", y=" ", title="Satisfacción con la democracia") + ylim(0, 6)

ggarrange(IM, ST)
