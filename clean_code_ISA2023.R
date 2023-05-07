# libraries

library(tidyverse)
library(readr)
library(sem)
library(randomForest)
library(lmtest)
library(AER)
library(lavaan)
library(cowplot)
library(semPlot)
library(vtable)
# import data


USUARIO <- Sys.getenv("USERNAME")

df <- read_delim(paste0("C:/Users/",USUARIO,"/Downloads/RCS_PT3_270223_Indexes.csv"), 
                 delim = ";", escape_double = TRUE, trim_ws = TRUE)

# 
USUARIO <- Sys.getenv("USERNAME")

#df <- read_delim(paste0("C:/Users/",USUARIO,"/Downloads/RCS_PT3_270223_Indexes.csv"), delim = ";", escape_double = TRUE, trim_ws = TRUE)

# Years living in the neighbourhood

df$P0109 <- ifelse(df$P0109 == 98, df$C02, df$P0109)

# deal with NA

df[, !(names(df) %in% c("P0109"))] <- lapply(df[, !(names(df) %in% c("P0109"))], function(x) ifelse(x > 76, NA, x))

# Estimating NA for more than 30% missing is not considered:

df <- df[colSums(is.na(df))/nrow(df) < .30]

# character and numeric to factor

df <- df %>% mutate_if(is.character, as.factor)


df$TIPO_ZONA <- cut(df$TIPO_ZONA, breaks = c(0, 1, 2, 3), labels = c("Experimental", "Control", "Other"))



df <- df %>% 
  mutate(exp = ifelse(TIPO_ZONA == "Experimental", 1, 0),
         equiv = ifelse(TIPO_ZONA == "Control", 1, 0))

# eliminate  variables with unique values

df <- select(df, -one_of(names(df)[sapply(df, function(x) n_distinct(x) == nrow(df))]))


sum(is.na(df$P0102_C))/length(df$P0102_C)

# median and mode AND other na impute
df <- na.roughfix(df)


df$P0209[df$P0209 > 1] <- 0
df$P0214[df$P0214 == 2] <- 0
df$P0219[df$P0219 == 2] <- 0


# building "social support" variable
df <- df %>% mutate(apoyo = log((P0905_A + P0905_B + P0905_C + P0905_D + P0905_E) / 5))

# for log(0)
df$apoyo[df$apoyo == -Inf] <- min(df$apoyo[is.finite(df$apoyo)])

# building "housing problems" variable:


df$P1009_A[df$P1009_A == 2] <- 0
df$P1009_B[df$P1009_B == 2] <- 0
df$P1009_C[df$P1009_C == 2] <- 0
df$P1009_G[df$P1009_G == 2] <- 0

df <- df %>% mutate(problemviv = (P1009_A + P1009_B + P1009_C + P1009_G) / 4)

# building "participation" variable (considered but not included)

var_names <- paste0("P0401_", LETTERS[1:10])

# Loop through the variable names and replace values
for (var_name in var_names) {
  df[[var_name]][df[[var_name]] < 4] <- 1
  df[[var_name]][df[[var_name]] == 4] <- 0
}

df <-  df %>% mutate(relvel= (P0401_A + P0401_B + P0401_C + P0401_D + P0401_E + P0401_F + P0401_G + P0401_H + P0401_I + P0401_J) / 10)

# building "increase in housing satisfaction variable"
df = df %>% mutate(diffviv =(P0224 - P0225))

# categories of families
df$P0902 <- cut(df$P0902,
                breaks = c(-Inf, 1, 2, 3, 4, 5, 6, 7, 8),
                labels = c("Couple with children", 
                           "Couple without children",
                           "Mother with children",
                           "Father with Children",
                           "Alone woman",
                           "Alone man",
                           "Several families",
                           "Other"))

###################################################################################################
# DiD estimation for each rehab variable:

model <- lm(P0224 ~ P0209 + P0225 + P0209*P0225 + problemviv + P1007 + apoyo  + exp:P0209 + equiv:P0209 + relvel + P0901 + P0902, data = df) # hay que controlar por tipo de zona?
coeftest(model, vcov = vcovHC(model, type = "HC1"))

model <- lm(P0224 ~ P0214 + P0225 + P0214*P0225 + problemviv + P1007 + apoyo  + exp:P0214 + equiv:P0214 + relvel + P0901 + P0902, data = df)
coeftest(model, vcov = vcovHC(model, type = "HC1"))

model <- lm(P0224 ~ P0219 + P0225 + P0219*P0225 + problemviv + P1007 + apoyo + exp:P0219 + equiv:P0219 + relvel + P0901 + P0902, data = df)
coeftest(model, vcov = vcovHC(model, type = "HC1"))

########################################################################################################
#SEM Estimation

df$hsn <- df$P0209
df$bld <- df$P0214
df$env <- df$P0219
df$eco <- df$P1007
df$hspr <- df$problemviv
df$stng <- df$P0102_A
df$sths <- df$diffviv
df$resmov <- df$P0102_C
df$support <- df$apoyo

indirind <- '
# measurement model
rehab =~ hsn + bld + env
# regressions
rehab ~ exp + equiv + support + eco + hspr #+ P0901
sths ~ rehab + support + hspr  # + eco
stng ~ sths + support  + eco + hspr # +relvel
resmov ~ stng + support + eco + hspr
'


fit <- sem(indirind, data=df)
summary(fit, standardized=TRUE, fit.measures=TRUE)

semPaths(fit,style = "lisrel", "std","hide") 

###############################################################################################
# Summary statistics

df$hsn <- cut(df$P0209, breaks = c(-Inf, 0, Inf), labels = c("No", "Yes"))
df$bld <- cut(df$P0214, breaks = c(-Inf, 0, Inf), labels = c("No", "Yes"))
df$env <- cut(df$P0219, breaks = c(-Inf, 0, Inf), labels = c("No", "Yes"))

st(df, vars = c("TIPO_ZONA", "hsn", "bld", "env"))

st(df, vars = c("eco", "hspr", "support", "stng", "sths", "resmov"))

################################################################################################################
# first graph
means <- df %>% group_by(TIPO_ZONA) %>% summarise(mean_A = mean(P0102_C), sd_A = sd(P0102_C), n = n()) %>% ungroup()
means$se_A <- means$sd_A / sqrt(means$n)
means$lower_ci <- means$mean_A - 1.96 * means$se_A
means$upper_ci <- means$mean_A + 1.96 * means$se_A

means %>% ggplot(aes(x = TIPO_ZONA, y = mean_A)) +
  geom_bar(stat = "identity", fill = c("#F8766D", "#00BA38", "#619CFF")) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  labs(x = "Zones", y = "Residential Mobility Preferences (1-5)", title="Propensity of leaving the neighbourhood based on the zone") + theme_classic()


# second graph

means <- df %>% 
  group_by(TIPO_ZONA) %>% 
  summarise(mean_var1 = mean(P0209), sd_var1 = sd(P0209), 
            mean_var2 = mean(P0214), sd_var2 = sd(P0214), 
            mean_var3 = mean(P0219), sd_var3 = sd(P0219), 
            n = n()) %>% 
  ungroup()

means$se_var1 <- means$sd_var1 / sqrt(means$n)
means$lower_ci_var1 <- means$mean_var1 - 1.96 * means$se_var1
means$upper_ci_var1 <- means$mean_var1 + 1.96 * means$se_var1

means$se_var2 <- means$sd_var2 / sqrt(means$n)
means$lower_ci_var2 <- means$mean_var2 - 1.96 * means$se_var2
means$upper_ci_var2 <- means$mean_var2 + 1.96 * means$se_var2

means$se_var3 <- means$sd_var3 / sqrt(means$n)
means$lower_ci_var3 <- means$mean_var3 - 1.96 * means$se_var3
means$upper_ci_var3 <- means$mean_var3 + 1.96 * means$se_var3

# Plot the means for each variable based on the group
a <- ggplot(means, aes(x = TIPO_ZONA)) +
  geom_bar(aes(y = mean_var1), stat = "identity", fill = c("#F8766D", "#00BA38", "#619CFF"), position = "dodge") +
  geom_errorbar(aes(ymin = lower_ci_var1, ymax = upper_ci_var1), width = 0.2, position = position_dodge(0.9)) +
  labs(x = "", y = "% of neighbors who have been rehabilitated", title="House") + ylim(0, 0.5) +
  theme_classic()
b <- ggplot(means, aes(x = TIPO_ZONA)) +
  geom_bar(aes(y = mean_var2), stat = "identity", fill = c("#F8766D", "#00BA38", "#619CFF"), position = "dodge") +
  geom_errorbar(aes(ymin = lower_ci_var2, ymax = upper_ci_var2), width = 0.2, position = position_dodge(0.9)) +
  labs(x = "", y = "", title="Building") + ylim(0, 0.5) +
  theme_classic()
c <- ggplot(means, aes(x = TIPO_ZONA)) +
  geom_bar(aes(y = mean_var3), stat = "identity", fill = c("#F8766D", "#00BA38", "#619CFF"), position = "dodge") +
  geom_errorbar(aes(ymin = lower_ci_var3, ymax = upper_ci_var3), width = 0.2, position = position_dodge(0.9)) +
  labs(x = "", y = "", title="Environment") + ylim(0, 0.5) +
  theme_classic()


title <- ggdraw() + draw_label("Incidence of Rehabilitation by Zone", fontface = "bold", x = 0, hjust = -0.05)
plot_grid(title, plot_grid(a, b, c, ncol = 3, labels = "AUTO", align = "h"), legend, ncol = 1, rel_heights = c(0.1, 0.9))

# third graph

means <- df %>%
  group_by(hsn, TIPO_ZONA) %>%
  summarise(
    Mean_diff = mean(diffviv),
    SE_diff = sd(diffviv) / sqrt(n())
  )

# plot means
a <- ggplot(means, aes(x = hsn, y = Mean_diff, fill = TIPO_ZONA)) +
  geom_col(position = "dodge", alpha = 0.8, color = "black", size = 0.5) +
  geom_errorbar(aes(ymin = Mean_diff - 1.64*SE_diff, ymax = Mean_diff + 1.64*SE_diff), 
                position = position_dodge(width = 0.9), 
                width = 0.2, size = 0.5) + 
  ylab("Increase in Housing Satisfaction (0-10)") +
  xlab("House") +
  ggtitle("") + ylim(-0.05, 1.2) +
  guides(fill=guide_legend(title="Zone")) + theme_classic() +
  theme(legend.position = "top", legend.text = element_text(size = 12), legend.title = element_text(face="bold")) 


means <- df %>%
  group_by(bld, TIPO_ZONA) %>%
  summarise(
    Mean_diff = mean(diffviv),
    SE_diff = sd(diffviv) / sqrt(n())
  )

b <- ggplot(means, aes(x = bld, y = Mean_diff, fill = TIPO_ZONA)) +
  geom_col(position = "dodge", alpha = 0.8, color = "black", size = 0.5) +
  geom_errorbar(aes(ymin = Mean_diff - 1.64*SE_diff, ymax = Mean_diff + 1.64*SE_diff), 
                position = position_dodge(width = 0.9), 
                width = 0.2, size = 0.5) + 
  ylab("") +
  xlab("Building") +
  ggtitle("") + theme_classic() + ylim(-0.05, 1.2) +
  theme(legend.position = "none")

means <- df %>%
  group_by(env, TIPO_ZONA) %>%
  summarise(
    Mean_diff = mean(diffviv),
    SE_diff = sd(diffviv) / sqrt(n())
  )

c <- ggplot(means, aes(x = env, y = Mean_diff, fill = TIPO_ZONA)) +
  geom_col(position = "dodge", alpha = 0.8, color = "black", size = 0.5) +
  geom_errorbar(aes(ymin = Mean_diff - 1.64*SE_diff, ymax = Mean_diff + 1.64*SE_diff), 
                position = position_dodge(width = 0.9), 
                width = 0.2, size = 0.5) +
  ylab("") + 
  xlab("Environment") +
  ggtitle("") + theme_classic() + ylim(-0.05, 1.2) +
  theme(legend.position = "none")

legend <- get_legend(a)

title <- ggdraw() + draw_label("Increase in Housing Satisfaction Based on the type of Rehabilitation and Zone of Intervention", fontface = "bold", x = 0, hjust = -0.05)
plot_grid(title, plot_grid(a +  theme(legend.position = "none"), b, c, ncol = 3, labels = "AUTO", align = "h"), legend, ncol = 1, rel_heights = c(0.1, 0.9))

#########################################################################################################################
