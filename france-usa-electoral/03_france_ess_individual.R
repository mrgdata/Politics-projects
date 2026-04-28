# =============================================================================
# Análisis Electoral Francia - ESS9 nivel individual
# Regresión logística: satisfacción democracia/inmigración -> voto Le Pen
# =============================================================================

library(tidyverse)
library(haven)
library(ggpubr)
library(ggeffects)
library(plyr)

# --- Carga ESS9 ---

tryCatch({
  ESS9 <- read_sav("ESS9.sav")
}, error = function(e) {
  stop("No se encontró ESS9.sav. Descarga la ESS9 en formato SPSS y colócala en esta carpeta.\n",
       "Error: ", e$message)
})

# --- Preparación ---

ESS9$domicil <- as.numeric(ESS9$domicil)
ESS9$domicil[ESS9$domicil == 1 | ESS9$domicil == 2] <- "Ciudades"
ESS9$domicil[ESS9$domicil > 2 & ESS9$domicil < 6] <- "Mundo rural"
ESS9$domicil <- factor(ESS9$domicil, levels=c("Ciudades", "Mundo rural"))

ESS9$prtvtdfr <- as.numeric(ESS9$prtvtdfr)
ESS9$prtvtdfr[ESS9$prtvtdfr != 7 & ESS9$prtvtdfr != 11] <- NA
ESS9$prtvtdfr[ESS9$prtvtdfr == 7] <- 0
ESS9$prtvtdfr[ESS9$prtvtdfr == 11] <- 1

df <- ESS9 %>% filter(!is.na(ESS9$domicil), !is.na(ESS9$imwbcnt), !is.na(ESS9$stfdem), !is.na(ESS9$prtvtdfr), cntry == "FR")
df <- as.data.frame(df)

# --- Modelo logístico ---

model1 <- glm(prtvtdfr ~ domicil + imwbcnt + stfdem, df, family=quasibinomial)
summary(model1)

dat <- ggpredict(model1, terms = c("stfdem"))
a <- plot(dat) + labs(x="Satisfacción con la democracia", y="Voto a Le Pen", title="") 
dt <- ggpredict(model1, terms = c("imwbcnt"))
b <- plot(dt) + labs(x="La inmigración es positiva", y= "", title="") 

ggarrange(a, b)

# --- Descriptivos rural/urbano ---

df <- df %>% ddply(c("domicil"), summarize, 
        mean1=mean(imwbcnt), mean2=mean(stfdem),
        upper1= mean(imwbcnt)+1.96*sd(imwbcnt)/sqrt(length(ESS9)/2),
        lower1=mean(imwbcnt)-1.96*sd(imwbcnt)/sqrt(length(ESS9)/2),
        upper2= mean(stfdem)+1.96*sd(stfdem)/sqrt(length(ESS9)/2),
        lower2=mean(stfdem)-1.96*sd(stfdem)/sqrt(length(ESS9)/2))

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
