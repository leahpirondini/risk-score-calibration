##############################################################################################
### Name: ASSIGN risk model
### Description: Calculate ASSIGN risk predictions
##############################################################################################

# load packages
library(dplyr)
library(tidyverse)
library(survival)

# load data
data <- gscot

# for people with RA, add 10 to number of cigarettes per day
data <- data %>%
  mutate(cpd = ifelse(ra==1, cig + 10, cig))

# males
data <- data %>%
  mutate(L.male = 
           0.05698*age + 
           0.22286*tc + 
           -0.53684*hdl + 
           0.01183*sbp + 
           0.81558*dm + 
           0.27500*fhx + 
           0.02005*cpd + 
           0.06296*simdscore10,
         Lbar.male = 
           0.05698*48.8706 + 
           0.22286*6.22520 + 
           -0.53684*1.35042 + 
           0.01183*133.810 + 
           0.81558*0.0152905 + 
           0.27500*0.263762 + 
           0.02005*7.95841 + 
           0.06296*2.74038,
         B.male = exp(L.male - Lbar.male))

# females
data <- data %>%
  mutate(L.female =
           0.07203*age + 
           0.12720*tc + 
           -0.55836*hdl + 
           0.01064*sbp + 
           0.97727*dm + 
           0.49159*fhx + 
           0.02724*cpd + 
           0.09386*simdscore10,
         Lbar.female =
           0.07203*48.7959 + 
           0.12720*6.40706 + 
           -0.55836*1.62837 + 
           0.01064*130.115 + 
           0.97727*0.0127275 + 
           0.49159*0.326328 + 
           0.02724*6.44058 + 
           0.09386*2.82470,
         B.female = exp(L.female - Lbar.female))

# 10-year risk of CVD and corresponding linear predictor
data <- data %>%
  mutate(pred = ifelse(gender==1, (1-(0.8831^B.male)), (1-(0.9365^B.female))),
         lp = ifelse(gender==1, L.male - Lbar.male, L.female - Lbar.female)
  )

### re-calibrate the baseline hazard

# males
data <- data %>%
  mutate(offset.males = L.male - Lbar.male)
data.males <- subset(data, gender==1)

fit.offset.males <- coxph(Surv(time = ptcvd, event = cvd) ~ 1 + offset(offset.males),
                          data = data.males)

basehaz.males <- as.data.frame(basehaz(fit.offset.males,centered = FALSE))
index.males <- which.min(abs(basehaz.males$time-10))
s0.new.males <- (1-basehaz.males[index.males,1])^(1/exp(mean(data.males$offset.males)))
s0.new.males

# females
data <- data %>%
  mutate(offset.females = data$L.female - data$Lbar.female)
data.females <- subset(data, gender==0)

fit.offset.females <- coxph(Surv(time = ptcvd, event = cvd) ~ 1 + offset(offset.females),
                            data = data.females)

basehaz.females <- as.data.frame(basehaz(fit.offset.females,centered = FALSE))
index.females <- which.min(abs(basehaz.females$time-10))
s0.new.females <- (1-basehaz.females[index.females,1])^(1/exp(mean(data.females$offset.females)))
s0.new.females

# updated predictions
data <- data %>%
  mutate(pred.rc = ifelse(data$gender==0, 1-(s0.new.females^data$B.female),
                      1-(s0.new.males^data$B.male)))

# summary
summary(data$pred)
summary(data$pred.rc)
