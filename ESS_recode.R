library(foreign)
library(tidyverse)
library(dplyr)
library(readr)
library(survey)
library(srvyr)
library(stats)
library(readxl)
library(writexl) 
library(epiDisplay)
library(ggrepel)

ESS <- read.spss("C:/Users/USUARIO/Documents/MEGA/Manu/UPO/Curso 5º-2/Pol/ESS9.sav",to.data.frame=TRUE)
warnings()

View(ESS) 
WID_Data <- read_excel("C:/Users/Manu/Downloads/WID.xls", 
                                       col_names =c("cntry", "C1", "C2", "Inequality_inc"))
WID_Data <- select(WID_Data, "cntry", "Inequality_inc")
as.numeric("Inequality_inc")

unique(ESS$vote)

ESS$voter[ESS$vote == "Yes"] <- 1
ESS$voter[ESS$vote == "No"] <- 0
ESS$voter[ESS$vote == "Not eligible to vote"] <- NA
ESS %>% group_by(cntry) %>% summarize(a= mean(voter, w=anweight, na.rm = TRUE))

unique(ESS$pbldmn)

ESS$dem[ESS$pbldmn == "Yes"] <- 1
ESS$dem[ESS$pbldmn == "No"] <- 0

unique(ESS$gndr)

unique(ESS$hinctnta)

tab1(ESS$hinctnta)

ESS$income[ESS$hinctnta == "J - 1st decile"] <- "P50"
ESS$income[ESS$hinctnta == "R - 2nd decile"] <- "P50"
ESS$income[ESS$hinctnta == "C - 3rd decile"] <- "P50"
ESS$income[ESS$hinctnta == "M - 4th decile"] <- "P50"
ESS$income[ESS$hinctnta == "F - 5th decile"] <- "P50"
ESS$income[ESS$hinctnta == "H - 10th decile"] <- "P90"

df0 <- ESS %>% group_by(cntry) %>%  summarize(voteP90 = sum(voter[income == "P90"], w=anweight, na.rm=TRUE), 
                                              voteP50 = sum(voter[income == "P50"], w=anweight, na.rm=TRUE),
                                              Inequality_vote = (voteP90/voteP50))

DF1 <- ESS %>% group_by(cntry) %>% summarize(voteP50 = sum(voter[income == "P50"], w=anweight, na.rm=TRUE))

DF2 <- ESS %>% group_by(cntry) %>%  summarize(demP50 = sum(dem[income == "P50"], w=anweight, na.rm=TRUE))

DF3 <- ESS %>% group_by(cntry) %>%  summarize(demP90 = sum(dem[income == "P90"], w=anweight, na.rm=TRUE))

df0 <- merge(DF, DF1)
df1 <-  merge(DF2, DF3)

df <- merge(df1, df)

INEPOL <- merge(Inepol,WID_Data)

INEPOL <- INEPOL %>% mutate(Inequality_vote = (voteP90/voteP50)) %>% mutate(Inequality_dem = (demP90/demP50)) 

INEPOL %>% cor.test(Inequality_vote, Inequality_dem) %>%
  cor.test(Inequality_vote, Inequality_inc) %>%
  cor.test(Inequality_dem, Inequality_inc)

ggplot(INEPOL, aes(Inequality_vote, Inequality_dem)) + 
  geom_point(size=2) + geom_abline(slope=1, intercept=0) +
  xlim(0, 1.1) + ylim(0, 1.1) + 
  labs(x="Voto",y="Manifestación", title= "Desigualdad política en Europa") + 
    theme_bw() + 
  geom_label_repel(aes(label = cntry),
                   box.padding   = 0.4, 
                   point.padding = 0.05,
                   nudge_x=0.1,
                   segment.color = 'grey50') +
  theme_classic()

write_xlsx(INEPOL, "C:/Users/Manuel Romero García/Desktop/INEPOL.xlsx")



ESS %>% ggplot(aes(hinctnta, fill = hinctnta, show.legend = FALSE)) + geom_bar() + coord_flip() + theme_bw()


