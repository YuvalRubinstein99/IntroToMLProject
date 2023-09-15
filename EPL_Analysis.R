library(tidyverse)
library(lmtest)
library(sandwich)
library(lfe)
library(MASS)
library(glmnet)
rm(list=ls())
setwd("C:/Users/Yuval/OneDrive/Desktop/EPLproject")


# Loading the data --------------------------------------------------------
data = read_csv('MatchesAndRatings.csv')
test <- read_csv("TestCSV.csv")

data <- data %>%
  rowwise() %>%
  mutate(HomeAVG = (HomeAtt + HomeDef + HomeMid) / 3,
         HomeSTD = sd(c(HomeAtt, HomeDef, HomeMid)),
         AwayAVG = (AwayAtt + AwayDef + AwayMid) / 3,
         AwaySTD = sd(c(AwayAtt, AwayDef, AwayMid)))%>%
  mutate(GD = HomeScore - AwayScore)%>%
  mutate(HomeOffset = (HomeAtt-HomeOVR)^2 +(HomeDef-HomeOVR)^2  +
           (HomeMid-HomeOVR)^2,
         AwayOffset= (AwayAtt-HomeOVR)^2 +(AwayDef-HomeOVR)^2  +
           (AwayMid-HomeOVR)^2)
test <- test %>%
  rowwise() %>%
  mutate(HomeAVG = (HomeAtt + HomeDef + HomeMid) / 3,
         HomeSTD = sd(c(HomeAtt, HomeDef, HomeMid)),
         AwayAVG = (AwayAtt + AwayDef + AwayMid) / 3,
         AwaySTD = sd(c(AwayAtt, AwayDef, AwayMid)))%>%
  mutate(GD = HomeScore - AwayScore)%>%
  mutate(HomeOffset = (HomeAtt-HomeOVR)^2 +(HomeDef-HomeOVR)^2  +
           (HomeMid-HomeOVR)^2,
         AwayOffset= (AwayAtt-HomeOVR)^2 +(AwayDef-HomeOVR)^2  +
           (AwayMid-HomeOVR)^2)


data <- data%>%
  mutate(logHomeSd = log(HomeSTD + 1),
         logAwaySd = log(AwaySTD + 1),
         logHomeOS = log(HomeOffset + 1),
         logAwayOS = log(AwayOffset + 1))
test <- test%>%
  mutate(logHomeSd = log(HomeSTD + 1),
         logAwaySd = log(AwaySTD + 1),
         logHomeOS = log(HomeOffset + 1),
         logAwayOS = log(AwayOffset + 1))
data <- data%>%
  mutate(status = ifelse(HomeScore > AwayScore, 'Win',
                         ifelse(HomeScore==AwayScore, 'Tie', 'Loss')))
# Visualizing connections between min-max-std-mean and the GD -------------

ggplot(data = data, aes(x = HomeAtt, y = GD)) +
  geom_point()

ggplot(data = data, aes(x = HomeDef, y = GD)) +
  geom_point()

ggplot(data = data, aes(x = HomeMid, y = GD)) +
  geom_point()

ggplot(data = data, aes(x = HomeSTD, y = GD)) +
  geom_point()


ggplot(data, aes(x = HomeSTD)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of HomeSTD",
       x = "HomeSTD",
       y = "Density")

ggplot(data, aes(x = log(AwaySTD+1))) +
  geom_density(fill = "red", alpha = 0.5) +
  labs(title = "Density Plot of AwaySTD",
       x = "AwaySTD",
       y = "Density")


gd_model <- lm(data = data, formula =  GD ~ log(HomeAtt) + log(HomeMid) + log(HomeDef) +
                 log(AwayAtt) + log(AwayMid) + log(AwayDef) + HomeOffset + AwayOffset+
                 HomeAverageAge + AwayAverageAge +
                 HomeAgeSq + AwayAgeSq + 
                 HomeMissingPlayersAverageRating + 
                 AwayMissingPlayersAverageRating + 
                 HomeMissingPlayers + AwayMissingPlayers + 
                 HomeTeam + AwayTeamName)


summary(gd_model)


gd_fe_model <- lm(data = data, formula =  GD ~ log(HomeAtt) + log(HomeMid) + log(HomeDef) +
                 log(AwayAtt) + log(AwayMid) + log(AwayDef) +
                 HomeAverageAge + AwayAverageAge + HomeAgeSq + AwayAgeSq 
                 + logHomeSd +logAwaySd
                   HomeMissingPlayers + AwayMissingPlayers)

summary(gd_fe_model)

data_ratio <- data%>%
  mutate(AttVAtt = HomeAtt / AwayAtt,
         MidVMid = HomeMid / AwayMid,
         DefVDef = HomeDef / AwayDef,
         OvO = HomeOVR / AwayOVR,
         AdvantageSd = sd(c(AttVAtt, MidVMid, DefVDef)))
test_ratio <- test%>%
  mutate(AttVAtt = HomeAtt / AwayAtt,
         MidVMid = HomeMid / AwayMid,
         DefVDef = HomeDef / AwayDef,
         OvO = HomeOVR / AwayOVR,
         AdvantageSd = sd(c(AttVAtt, MidVMid, DefVDef)))

gd_model <- lm(data = data_ratio, formula =  GD ~ 
                 HomeMissingPlayers +
                 AwayMissingPlayers + HomeTeam:AwayTeam)
step <- stepAIC(gd_model, direction = 'both', trace=TRUE)
summary(gd_model)
summary(step)

ggplot(data, aes(x = status)) +
geom_bar()+   labs(title = "Bar Plot for End Game Status",
       x = "Status",
       y = "count")





orderd_model <- polr(data = data_ratio, method = 'cloglog',
                     formula =  as.factor(y) ~ 
                       HomeMissingPlayers +
                       AwayMissingPlayers + OvO
                     + AttVAtt + MidVMid+ DefVDef + HomeForm 
                     + AwayForm + as.factor(Round_Number) + AdvantageSd+
                       Month+ logHomeSd + logAwaySd + logHomeOS + logAwayOS)
summary(orderd_model)


orderd_step <- stepAIC(orderd_model, direction = 'both', trace=TRUE)
summary(orderd_step)
summary(orderd_model)
pred = predict(orderd_step, type='probs')
res = step$deviance
summary(as.factor(data_ratio$status))
summary(pred)


test_pred = predict(orderd_step, newdata = test_ratio, type='probs')

summary(test_pred)



# Adding Defense / Offense / MF offsets from the OVR ----------------------

# Logic: if a team's defensive rating is above the ovr, we can say
# that there is a relatively strong def (emphasis on defense)

data_offsets <- data%>%
  mutate(HomeDefToOvr = log(HomeDef) - log(HomeOVR),
         HomeMFToOvr = log(HomeMid) - log(HomeOVR),
         HomeAttToOvr = log(HomeAtt) - log(HomeOVR),
         AwayDefToOvr = log(AwayDef) - log(AwayOVR),
         AwayMFToOvr = log(AwayMid) - log(AwayOVR),
         AwayAttToOvr = log(AwayAtt) - log(AwayOVR))
test_offsets <- test%>%
  mutate(HomeDefToOvr = log(HomeDef) - log(HomeOVR),
         HomeMFToOvr = log(HomeMid) - log(HomeOVR),
         HomeAttToOvr = log(HomeAtt) - log(HomeOVR),
         AwayDefToOvr = log(AwayDef) - log(AwayOVR),
         AwayMFToOvr = log(AwayMid) - log(AwayOVR),
         AwayAttToOvr = log(AwayAtt) - log(AwayOVR))

os_ordered = polr(data = data_offsets, method='probit',
                  formula = as.factor(status) ~ 
                    HomeTeam*AwayTeam +
                    HomeForm + AwayForm + as.factor(Month))
summary(os_ordered)
test_pred = predict(os_ordered, newdata = test_offsets, type='probs')

summary(test_pred)