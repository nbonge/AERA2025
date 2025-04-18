# == == == == == == == == == == == == == == == 
# ==              Nicole Bonge              ==
# ==      Wednesday, January 15, 2025       ==
# ==        Project Code - MACH-IV          ==
# ==             Response Time              ==
# == == == == == == == == == == == == == == ==

# 1.0 Preliminaries ----
rm(list=ls())
getwd()

## 1.1 Libraries ----
library(plyr)
library(tidyverse)
library(readr)
library(careless)
library(psych)

library(mirt)
library(lme4)
library(lmerTest)
library(performance)
library(car)
library(reshape2)


library(sjstats)
library(sjPlot)
# 1.2 Functions ----


# 2.0 Import Dataset ----
dat_all <- read_delim("~/Library/CloudStorage/OneDrive-UniversityofArkansas/School/Research/Datasets/MACH_data/data.csv", 
                      delim = "\t", escape_double = FALSE, 
                      trim_ws = TRUE)
na.rows <- which(is.na(dat_all$testelapse))

# filtering out respondents who did not take the test at all (testelapse time is NA)
dat_all1 <- dat_all |>
  filter(!(row_number() %in% na.rows))

## 2.1 Data Cleaning ----
# filtering responses to US only & age between 18-99 & English-native speakers
dat_us <- dat_all1 %>%
  filter(country == "US") %>%
  filter(age > 17) %>%
  filter(age < 100) %>%
  filter(engnat == 1)

# adding "id" column
dat_us$id <- c(1:nrow(dat_us))

# trimming top and bottom 10% of times
dat_us_time_filter <- data.frame( "id" = c(1:nrow(dat_us)),
                                  "test.time" = dat_us$testelapse,
                                  "test.ntile" = ntile(dat_us$testelapse, 10),
                                  "survey.time" = dat_us$surveyelapse,
                                  "survey.ntile" = ntile(dat_us$surveyelapse, 10)) %>%
  filter(test.ntile != 10) %>%
  filter(test.ntile != 1) %>%
  filter(survey.ntile != 10) %>%
  filter(survey.ntile != 1)

dems <- c("education", "urban", "gender", "engnat", "age", 
          "screenw", "screenh", "hand", "religion", "orientation",
          "race", "voted", "married", "familysize")

dat_us_time_trim <- dat_us %>%
  filter(id %in% dat_us_time_filter$id) %>%
  dplyr::select(c("id", paste0("Q", 1:20, "A"), 
                  paste0("Q", 1:20, "E"),
                  paste0("VCL", 1:16),
                  dems))

# VCL check
data_filtered <- dat_us_time_trim %>%
  filter(VCL6 == 0,
         VCL9 == 0,
         VCL12 == 0) %>%
  dplyr::select("id",
                paste0("Q", 1:20, "A"), 
                paste0("Q", 1:20, "E"),
                dems)

# checking for longstrings
long <- longstring(data_filtered, avg = TRUE)
long$id <- data_filtered$id

describe(long$longstr, quant = c(.01, .05, .10, .25, .50, .75, .90, .95, .99))


data_filtered2 <- left_join(data_filtered, long, by = "id")

data_filtered3 <- data_filtered2 %>%
  filter(longstr <= 5)


# Recoding reverse-oriented items
dat <- data_filtered3 %>%
  mutate(item1 = Q1A,
         item2 = Q2A,
         item3 = Q3A,
         item4 = Q4A,
         item5 = Q5A,
         item6 = Q6A,
         item7 = Q7A,
         item8 = Q8A,
         item9 = Q9A,
         item10 = Q10A,
         item11 = Q11A,
         item12 = Q12A,
         item13 = Q13A,
         item14 = Q14A,
         item15 = Q15A,
         item16 = Q16A,
         item17 = Q17A,
         item18 = Q18A,
         item19 = Q19A,
         item20 = Q20A) %>%
  mutate(item1.time = Q1E,
         item2.time = Q2E,
         item3.time = Q3E,
         item4.time = Q4E,
         item5.time = Q5E,
         item6.time = Q6E,
         item7.time = Q7E,
         item8.time = Q8E,
         item9.time = Q9E,
         item10.time = Q10E,
         item11.time = Q11E,
         item12.time = Q12E,
         item13.time = Q13E,
         item14.time = Q14E,
         item15.time = Q15E,
         item16.time = Q16E,
         item17.time = Q17E,
         item18.time = Q18E,
         item19.time = Q19E,
         item20.time = Q20E) %>%
  dplyr::select("id",paste0("item",c(1:20)),
                paste0("item",c(1:20),".time"),
                dems)

dat_recoded <- dat %>%
  mutate(item3R = 6 - item3,
         item4R = 6 - item4,
         item6R = 6 - item6,
         item7R = 6 - item7,
         item9R = 6 - item9,
         item10R = 6 - item10,
         item11R = 6 - item11,
         item14R = 6 - item14,
         item16R = 6 - item16,
         item17R = 6 - item17) %>%
  dplyr::select("id","item1","item2","item3R","item4R","item5",
                "item6R","item7R","item8","item9R","item10R",
                "item11R","item12", "item13", "item14R","item15",
                "item16R", "item17R", "item18","item19", "item20",
                paste0("item",1:20,".time"),
                "age", "gender")
# cleaning up
rm(dat_us, dat_us_time_filter, dat_us_time_trim, data_filtered,
   dems, data_filtered2, long)

# 3.0 Data Analysis ----
## 3.1 Descriptives ----
desc <- describe(dat_recoded)
desc

tabulate(data_filtered3$gender)

corr <- pdata_filtered3corr <- polychoric(dat_recoded[2:21])
corr
## 3.2 IRT Suitability ----
scree(corr$rho)
nfactors(dat_recoded[2:21])

efa <- fa(corr$rho, nfactors = 1, n.obs = nrow(dat_recoded),
          fm = "wls")
efa
### 3.2.1 Dropping poor quality items ----

dat_recoded1 <- dat_recoded %>%
  dplyr::select(!"item19")

corr1 <- polychoric(dat_recoded1[,2:20])

scree(corr1$rho)
nfactors(dat_recoded1[2:20])

efa1 <- fa(corr1$rho, nfactors = 1, n.obs = nrow(dat_recoded1),
           fm = "wls")
efa1

### Cleaning up
rm(efa, efa1)

## 3.3 IRT Analysis ----
# comparing GRM with GPCM
mod.grm <- mirt(dat_recoded1[,2:20], itemtype = "graded")
mod.gpcm <- mirt(dat_recoded1[,2:20], itemtype = "gpcm")

model.comparison <- data.frame(
  "AIC" = c(extract.mirt(mod.grm, "AIC"), extract.mirt(mod.gpcm, "AIC")),
  "BIC" = c(extract.mirt(mod.grm, "BIC"), extract.mirt(mod.gpcm, "BIC")),
  "SABIC" = c(extract.mirt(mod.grm, "SABIC"), extract.mirt(mod.gpcm, "SABIC")),
  "log.lik" = c(extract.mirt(mod.grm, "logLik"), extract.mirt(mod.gpcm, "logLik")),
  row.names = c("grm", "gpcm")
)
View(model.comparison)
# grm wins!

# empirical reliabilities
rxx.gpcm <- fscores(mod.gpcm, returnER = TRUE)
rxx.grm <- fscores(mod.grm, returnER = TRUE)

# cleaning up
rm(mod.gpcm)

# inspecting GRM pars
pars <- as.data.frame(coef(mod.grm, IRTpars = TRUE, simplify = TRUE)$items) %>%
  round(digits = 2)


### 3.3.1 Plots ----
icc <- plot(mod.grm, type = "trace")
icc
iic <- plot(mod.grm, type = "infotrace")
iic
info.plot <- plot(mod.grm, type = "info")
info.plot

# 4.0 Main Analysis ----
# Not using recoded responses now!!

## 4.1 Descriptives ----

mean.resp <- colMeans(dat_recoded1[,2:20])

mean.time <- colMeans(dat_recoded1[,c(21:38,40)])

sd.resp <- c(sd(dat_recoded1$item1), 
             sd(dat_recoded1$item2), 
             sd(dat_recoded1$item3R),
             sd(dat_recoded1$item4R), 
             sd(dat_recoded1$item5), 
             sd(dat_recoded1$item6R), 
             sd(dat_recoded1$item7R), 
             sd(dat_recoded1$item8), 
             sd(dat_recoded1$item9R),
             sd(dat_recoded1$item10R), 
             sd(dat_recoded1$item11R), 
             sd(dat_recoded1$item12),
             sd(dat_recoded1$item13), 
             sd(dat_recoded1$item14R), 
             sd(dat_recoded1$item15), 
             sd(dat_recoded1$item16R), 
             sd(dat_recoded1$item17R), 
             sd(dat_recoded1$item18)
             ,sd(dat_recoded1$item20)
)

sd.time <- c(sd(dat$item1.time), 
             sd(dat$item2.time), 
             sd(dat$item3.time), 
             sd(dat$item4.time), 
             sd(dat$item5.time), 
             sd(dat$item6.time), 
             sd(dat$item7.time), 
             sd(dat$item8.time), 
             sd(dat$item9.time), 
             sd(dat$item10.time), 
             sd(dat$item11.time), 
             sd(dat$item12.time),
             sd(dat$item13.time), 
             sd(dat$item14.time), 
             sd(dat$item15.time), 
             sd(dat$item16.time), 
             sd(dat$item17.time), 
             sd(dat$item18.time)
             ,sd(dat$item20.time)
)

# Combining descriptives to "descriptives" table
itemsR <- c("1", "2", "3R","4R", "5", "6R", "7R",
            "8", "9R", "10R", "11R", "12", "13", "14R", 
            "15", "16R", "17R","18", "20")
items <- c(1:18,20) |> as.character()
descriptives <- data.frame("item" = itemsR,
                           "sample" = rep(nrow(dat),length(itemsR)),
                           "tau3" = pars$b3,
                           "discrimination" = pars$a,
                           "resp.Mean" = mean.resp,
                           "resp.Sdev" = sd.resp,
                           "resp.Time.Mean" = mean.time,
                           "resp.Time.Sdev" = sd.time)
descriptives[,3:8] <- descriptives[,3:8] %>% round(digits = 2)

# checking factor scores
mod.theta <- data.frame("id" = dat$id, "theta" = fscores(mod.grm)[,1])

# cleaning up
rm(mean.resp,sd.resp,mean.time,sd.time)

## 4.2 Data Transformation for Main Analysis ----

# combining factor scores with item responses
mach.combined <- merge(mod.theta, dat[c("id",paste0("item",items))], by = "id")

colnames(mach.combined) <- c("id","theta",paste0("item",items))

# create long response time 
mach.RT <- dat %>%
  dplyr::select("id", paste0("item",items,".time"))

time.long <- melt(mach.RT, id.vars = "id",
                  measure.vars = c(paste0("item",items,".time")),
                  variable.name = "item", value.name = "time")


# transforming items to factors
# **THIS CHANGES ITEM 20 to ITEM 19**
time.long$item <- as.numeric(as.factor(time.long$item))


# transform trust responses & factor sores to long form
resp.long <- melt(mach.combined, id.vars = "id", 
                  measure.vars = paste0("item",items),
                  variable.name = "item", value.name = "response")

# transform item variable to numeric
# !!This changes items to 1-19!!
resp.long$item <- as.numeric(as.factor(resp.long$item))

# merging responses with thetas 
mach.tau <- merge(resp.long, mod.theta, by = "id")

mach.tau <- mach.tau %>%
  mutate("tau3" = case_when(
    item == 1 ~ pars["item1","b3"],
    item == 2 ~ pars["item2","b3"],
    item == 3 ~ pars["item3", "b3"],
    item == 4 ~ pars["item4R","b3"],
    item == 5 ~ pars["item5","b3"],
    item == 6 ~ pars["item6R","b3"],
    item == 7 ~ pars["item7R","b3"],
    item == 8 ~ pars["item8","b3"],
    item == 9 ~ pars["item9R","b3"],
    item == 10 ~ pars["item10R","b3"],
    item == 11 ~ pars["item11R", "b3"],
    item == 12 ~ pars["item12","b3"],
    item == 13 ~ pars["item13","b3"],
    item == 14 ~ pars["item14R","b3"],
    item == 15 ~ pars["item15","b3"],
    item == 16 ~ pars["item16R", "b3"],
    item == 17 ~ pars["item17R", "b3"],
    item == 18 ~ pars["item18","b3"],
    item == 19 ~ pars["item20", "b3"])) %>%
  mutate("discrim" = case_when(
    item == 1 ~ pars["item1","a"],
    item == 2 ~ pars["item2","a"],
    item == 3 ~ pars["item3", "a"],
    item == 4 ~ pars["item4R","a"],
    item == 5 ~ pars["item5","a"],
    item == 6 ~ pars["item6R","a"],
    item == 7 ~ pars["item7R","a"],
    item == 8 ~ pars["item8","a"],
    item == 9 ~ pars["item9R","a"],
    item == 10 ~ pars["item10R","a"],
    item == 11 ~ pars["item11R", "a"],
    item == 12 ~ pars["item12","a"],
    item == 13 ~ pars["item13","a"],
    item == 14 ~ pars["item14R","a"],
    item == 15 ~ pars["item15","a"],
    item == 16 ~ pars["item16R", "a"],
    item == 17 ~ pars["item17R", "a"],
    item == 18 ~ pars["item18","a"],
    item == 19 ~ pars["item20", "a"])) %>%
  mutate("endorse1" = case_when(
    response > 3 ~ 1,
    response <= 3 ~ 0
  ))

# computing distances from theta to tau2
mach.dist <- mach.tau %>%
  mutate("dist" = mach.tau$theta - mach.tau$tau3)

mach.dist <- mach.dist %>%
  mutate("abs.dist" = abs(dist))

# computing a_j * |\theta - \tau|
mach.dist <- mach.dist %>%
  mutate("discrim.abs.dist" = mach.dist$discrim * mach.dist$abs.dist)

mach.dist.small <- mach.dist %>%
  dplyr::select("id", "item", "theta", "tau3", "discrim",
                "abs.dist", "discrim.abs.dist", "endorse1")

# merging mach.long.small with response times
mach.all <- merge(mach.dist.small, time.long, by = c("id","item")) %>%
  na.omit()

### 4.1.1 Cleaning up ----
rm(mach.dist,mach.dist.small,mach.RT,mach.tau,
   mach.combined,resp.long,time.long, mod.theta, corr1,
   dat_recoded, dat_recoded1)

## 4.3 Models ----

### 4.3.1 Null Model ----
model.0 <- lmer(log(time) ~ 1 + (1 | id) + (1 | item), data = mach.all) # intercept

model.0.res <- list("model" = model.0, # model
                    "summary" = summary(model.0), # model coefficients
                    "random.effects" = rand(model.0), # significance test for random effects
                    "conf.ints" = confint(model.0), # confidence intervals for parameters
                    "r2" = performance::r2(model.0))  # R-squared statistic
# note: random effects are in std. deviations squared to obtain variance

### 4.3.2 Model A1 ----
## (R > E phenomenon only)
model.a1 <- lmer(log(time) ~ endorse1 + (1 | id) + (1 | item), data = mach.all)
model.a1.res <- list("model" = model.a1, # model
                     "summary" = summary(model.a1), # model coefficients
                     "random.effects" = rand(model.a1), # significance test for random effects
                     "conf.ints" = confint(model.a1), # confidence intervals for parameters
                     "r2" = performance::r2(model.a1))  # R-squared statistic
# note: random effects are in std. deviations squared to obtain variance


### 4.3.3 Model A2 ----
## (controlling for difficulty and ability)
model.a2 <- lmer(log(time) ~ endorse1 + tau3 + theta + (1 | id) + (1 | item), 
                 data = mach.all, control = lmerControl(optimizer = "bobyqa"))

model.a2.res <- list("model" = model.a2, # model
                     "summary" = summary(model.a2), # model coefficients
                     "vif" = vif(model.a2), # multicollinearity check
                     "random.effects" = rand(model.a2), # significance test for random effects
                     "conf.ints" = confint(model.a2), # confidence intervals for parameters
                     "r2" = performance::r2(model.a2))  # R-squared statistic
# note: random effects are in std. deviations squared to obtain variance


### 4.3.4 Model A3 ---- 
## (R > E and ability interaction)
model.a3 <- lmer(log(time) ~ endorse1 + tau3 + theta + theta:endorse1 + 
                   (1 | id) + (1 | item), data = mach.all)

model.a3.res <- list("model" = model.a3, # model
                     "summary" = summary(model.a3), # model coefficients
                     "vif" = vif(model.a3), # multicollinearity check
                     "random.effects" = rand(model.a3), # significance test for random effects
                     "conf.ints" = confint(model.a3), # confidence intervals for parameters
                     "r2" = performance::r2(model.a3))  # R-squared statistic
# note: random effects are in std. deviations squared to obtain variance


### 4.3.5 Model A4 ----
# ## (Exploratory model with difficulty and ability interaction)
# model.a4 <- lmer(log(time) ~ endorse1 + tau2 + theta + theta:endorse1 + tau2:theta + 
#                    (1 | id) + (1 | item), data = mach.all)
# 
# model.a4.res <- list("model" = model.a4, # model
#                      "summary" = summary(model.a4), # model coefficients
#                      "vif" = vif(model.a4), # multicollinearity check
#                      "random.effects" = rand(model.a4), # significance test for random effects
#                      "conf.ints" = confint(model.a4), # confidence intervals for parameters
#                      "r2" = r2(model.a4))  # R-squared statistic
# # note: random effects are in std. deviations squared to obtain variance


## Comparison of 0, A1, A2, A3, (and A4) models
model.comparison1 <- anova(model.0, 
                           model.a1, 
                           model.a2, 
                           model.a3
                           #, model.a4
) %>%
  round(digits = 3)

### 4.3.6 Model B1 ----
## (Thissen's model)
model.b1 <- lmer(log(time) ~ discrim.abs.dist + (1 | id) + (1 | item), 
                 data = mach.all)

model.b1.res <- list("model" = model.b1, # model
                     "summary" = summary(model.b1), # model coefficients
                     "random.effects" = rand(model.b1), # significance test for random effects
                     "conf.ints" = confint(model.b1), # confidence intervals for parameters
                     "r2" = performance::r2(model.b1))  # R-squared statistic
# note: random effects are in std. deviations squared to obtain variance



### 4.3.7 Model B2 ----
## (controlling for item endorsement)
model.b2 <- lmer(log(time) ~ discrim.abs.dist + endorse1 + (1 | id) + (1 | item), 
                 data = mach.all)

model.b2.res <- list("model" = model.b2, # model
                     "summary" = summary(model.b2), # model coefficients
                     "vif" = vif(model.b2), # multicollinearity check
                     "random.effects" = rand(model.b2), # significance test for random effects
                     "conf.ints" = confint(model.b2), # confidence intervals for parameters
                     "r2" = performance::r2(model.b2))  # R-squared statistic
# note: random effects are in std. deviations squared to obtain variance



### 4.3.8 Model B3 ----
## (ability-difficulty distance and item endorsement interaction)
model.b3 <- lmer(log(time) ~ discrim.abs.dist + endorse1 + discrim.abs.dist:endorse1 + 
                   (1 | id) + (1 | item), data = mach.all)

model.b3.res <- list("model" = model.b3, # model
                     "summary" = summary(model.b3), # model coefficients
                     "vif" = vif(model.b3), # multicollinearity check
                     "random.effects" = rand(model.b3), # significance test for random effects
                     "conf.ints" = confint(model.b3), # confidence intervals for parameters
                     "r2" = performance::r2(model.b3))  # R-squared statistic
# note: random effects are in std. deviations squared to obtain variance




## Comparison of 0, B1, B2, and B3 models
model.comparison2 <- anova(model.0, model.b1, model.b2, model.b3) %>%
  round(digits = 3)

### 4.3.9 Model Overview ----
model.overview <- list("descriptives" = descriptives,
                       "model.0" = model.0.res,
                       "model.a1" = model.a1.res,
                       "model.a2" = model.a2.res,
                       "model.a3" = model.a3.res,
                       "model.b1" = model.b1.res,
                       "model.b2" = model.b2.res,
                       "model.b3" = model.b3.res
)

# Cleaning Up
rm(list = c("model.0","model.0.res","model.a1",
            "model.a2","model.a3",
            #"model.a4",
            "model.b1", "model.b2", "model.b3", 
            "model.a1.res", "model.a2.res",
            "model.a3.res",
            #"model.a4.res",
            "model.b1.res", "model.b2.res", "model.b3.res"))


# 5.0 Plots ----
library("ggplot2")
library("ggeffects")
library("signs")
library("extrafont")

## 5.1 Figure 2.1 ----
figure.1.data <- ggpredict(model.overview$model.a2$model, terms = c("theta [all]", "endorse1"))

# alternative plot syntax without custom font setting
figure.1a <- plot(figure.1.data, show_ci = F, ci_style = c("ribbon"), line_size = 0.75) + 
  labs(x = "Trait Level (θ)", y = "Reponse time (in milliseconds)", title = NULL, color = "Response") + 
  scale_x_continuous(breaks = seq(-3.5,3.5, by = 1), limits = c(-3.5,3.5), labels = signs_format()) + 
  scale_y_continuous(breaks = seq(3500, 8500, by = 1000), limits = c(3500, 8500)) + 
  scale_color_manual(values = c("#DB060B", "#4DB264"), labels = c("Reject", "Endorse")) + 
  scale_fill_manual(values = c("#DB060B", "#4DB264")) + 
  theme_bw(base_size = 11) + 
  theme(axis.text = element_text(color = "black"))

figure.1b <- plot(figure.1.data, show_ci = F, ci_style = c("ribbon"), line_size = 2) + 
  labs(x = "Trait Level (θ)", y = "Reponse time (in seconds)", title = NULL, color = "Response") + 
  scale_x_continuous(breaks = seq(-3.5,3.5, by = 1), limits = c(-3.5,3.5), labels = signs_format()) + 
  scale_y_continuous(breaks = seq(5500, 8500, by = 1000), limits = c(5500, 8500), labels = seq(5.5, 8.5, by = 1)) + 
  scale_color_manual(values = c("#DB060B", "#4DB264"), labels = c("Reject", "Endorse")) + 
  scale_fill_manual(values = c("#DB060B", "#4DB264")) + 
  theme_bw(base_size = 25) + 
  theme(axis.text = element_text(color = "black"),
        legend.position = "bottom") 


figure.1c <- ggplot(data = figure.1.data)


# visualize the plot
figure.1a

# save the plot in your device (located in your set working directory)
ggsave(figure.1, file = "Figure_1.jpeg", width = 14, height = 9.5, units = "cm")

## 5.2 Figure 3.1 ----

# compute predicted values of Model B3 as a data source for the plot
figure.2.data <- ggpredict(model.overview$model.b3$model, terms = c("discrim.abs.dist [all]", "endorse1"))

# plot syntax
figure.2 <- plot(figure.2.data, show_ci = F, line_size = 0.75) + 
  labs(x = expression(paste("Discrimination-adjusted person-item distance ",italic("a"), "|θ − τ|", sep = "")), 
       y = "Reponse time (in milliseconds)", 
       title = NULL, 
       color = "Response") + 
  scale_x_continuous(breaks = seq(0, 9.5, by = 1.5), limits = c(0, 9.5), labels = signs_format()) +
  scale_y_continuous(breaks = seq(3500, 8500, by = 1000), limits = c(3500, 8500)) + 
  scale_color_manual(values = c("#DB060B", "#4DB264"), labels = c("Reject", "Endorse")) + 
  theme_bw(base_size = 11) + 
  theme(axis.text = element_text(color = "black"),
        legend.position = "bottom")


figure.2 <- plot(figure.2.data, show_ci = F, line_size = 2) + 
  labs(x = expression(paste("Discrimination-adjusted person-item distance ",italic("a"), "|θ − τ|", sep = "")), 
       y = "Reponse time (in seconds)", 
       title = NULL, 
       color = "Response") + 
  scale_x_continuous(breaks = seq(0, 9.5, by = 1.5), limits = c(0, 9.5), labels = signs_format()) +
  scale_y_continuous(breaks = seq(3500, 8500, by = 1000), limits = c(3500, 8500), labels = seq(3.5, 8.5, by = 1)) + 
  scale_color_manual(values = c("#DB060B", "#4DB264"), labels = c("Reject", "Endorse")) + 
  theme_bw(base_size = 25) + 
  theme(axis.text = element_text(color = "black"),
        legend.position = "bottom")

# visualize the plot
figure.2

# save the plot in your device (located in your set working directory)
ggsave(figure.2, file = "Figure_2.jpeg", width = 14, height = 9.5, units = "cm")

# 6.0 Generating Outputs ----

mod.0.result.out <- data.frame("estimate" = round(model.overview$model.0[["model"]]@beta, digits = 2),
                               "significance" = round(model.overview$model.0[["summary"]][["coefficients"]][,5], 
                                                      digits = 2),
                               "lowerCI" = round(model.overview$model.0[["conf.ints"]][4,1], digits = 2),
                               "upperCI" = round(model.overview$model.0[["conf.ints"]][4,2], digits = 2)
)

mod.a1.result.out <- data.frame("estimate" = round(model.overview$model.a1[["model"]]@beta, digits = 2),
                                "significance" = round(model.overview$model.a1[["summary"]][["coefficients"]][,5], 
                                                       digits = 2),
                                "lowerCI" = round(model.overview$model.a1[["conf.ints"]][4:5,1], digits = 2),
                                "upperCI" = round(model.overview$model.a1[["conf.ints"]][4:5,2], digits = 2)
)

mod.a2.result.out <- data.frame("estimate" = round(model.overview$model.a2[["model"]]@beta, digits = 2),
                                "significance" = round(model.overview$model.a2[["summary"]][["coefficients"]][,5], 
                                                       digits = 2),
                                "lowerCI" = round(model.overview$model.a2[["conf.ints"]][4:7,1], digits = 2),
                                "upperCI" = round(model.overview$model.a2[["conf.ints"]][4:7,2], digits = 2)
)

mod.a3.result.out <- data.frame("estimate" = round(model.overview$model.a3[["model"]]@beta, digits = 2),
                                "significance" = round(model.overview$model.a3[["summary"]][["coefficients"]][,5], 
                                                       digits = 2),
                                "lowerCI" = round(model.overview$model.a3[["conf.ints"]][4:8,1], digits = 2),
                                "upperCI" = round(model.overview$model.a3[["conf.ints"]][4:8,2], digits = 2)
)

mod.b1.result.out <- data.frame("estimate" = round(model.overview$model.b1[["model"]]@beta, digits = 2),
                                "significance" = round(model.overview$model.b1[["summary"]][["coefficients"]][,5], 
                                                       digits = 2),
                                "lowerCI" = round(model.overview$model.b1[["conf.ints"]][4:5,1], digits = 2),
                                "upperCI" = round(model.overview$model.b1[["conf.ints"]][4:5,2], digits = 2)
)

mod.b2.result.out <- data.frame("estimate" = round(model.overview$model.b2[["model"]]@beta, digits = 2),
                                "significance" = round(model.overview$model.b2[["summary"]][["coefficients"]][,5], 
                                                       digits = 2),
                                "lowerCI" = round(model.overview$model.b2[["conf.ints"]][4:6,1], digits = 2),
                                "upperCI" = round(model.overview$model.b2[["conf.ints"]][4:6,2], digits = 2)
)

mod.b3.result.out <- data.frame("estimate" = round(model.overview$model.b3[["model"]]@beta, digits = 2),
                                "significance" = round(model.overview$model.b3[["summary"]][["coefficients"]][,5], 
                                                       digits = 2),
                                "lowerCI" = round(model.overview$model.b3[["conf.ints"]][4:7,1], digits = 2),
                                "upperCI" = round(model.overview$model.b3[["conf.ints"]][4:7,2], digits = 2)
)

model.fits <- data.frame("conditional.R2" = c(model.overview$model.0[["r2"]][["R2_conditional"]],
                                              model.overview$model.a1[["r2"]][["R2_conditional"]],
                                              model.overview$model.a2[["r2"]][["R2_conditional"]],
                                              model.overview$model.a3[["r2"]][["R2_conditional"]],
                                              model.overview$model.b1[["r2"]][["R2_conditional"]],
                                              model.overview$model.b2[["r2"]][["R2_conditional"]],
                                              model.overview$model.b3[["r2"]][["R2_conditional"]]),
                         "marginal.R2" = c(model.overview$model.0[["r2"]][["R2_marginal"]],
                                           model.overview$model.a1[["r2"]][["R2_marginal"]],
                                           model.overview$model.a2[["r2"]][["R2_marginal"]],
                                           model.overview$model.a3[["r2"]][["R2_marginal"]],
                                           model.overview$model.b1[["r2"]][["R2_marginal"]],
                                           model.overview$model.b2[["r2"]][["R2_marginal"]],
                                           model.overview$model.b3[["r2"]][["R2_marginal"]]),
                         "Log Likelihood" = c(model.comparison1$logLik[1:4], 
                                              model.comparison2$logLik[2:4]),
                         "AIC" = c(model.comparison1$AIC[1:4], 
                                   model.comparison2$AIC[2:4]),
                         "BIC" = c(model.comparison1$BIC[1:4], 
                                   model.comparison2$BIC[2:4]),
                         "Chi.Sq" = c(model.comparison1$Chisq[1:4], 
                                      model.comparison2$Chisq[2:4]),
                         "df" = c(model.comparison1$Df[1:4], 
                                  model.comparison2$Df[2:4]),
                         "p.val" = c(model.comparison1$`Pr(>Chisq)`[1:4], 
                                     model.comparison2$`Pr(>Chisq)`[2:4]),
                         row.names = c("Model 0","Model A1","Model A2","Model A3",
                                       "Model B1","Model B2","Model B3")) %>%
  round(digits = 3)


table.modelA.comparison <- sjPlot::tab_model(model.overview$model.0$model, 
                                             model.overview$model.a1$model, 
                                             model.overview$model.a2$model, 
                                             model.overview$model.a3$model, digits = 3,
                                             dv.labels = c("Model 0", "Model A1",
                                                           "Model A2", "Model A3"))

table.modelB.comparison <- sjPlot::tab_model(model.overview$model.0$model, 
                                             model.overview$model.b1$model, 
                                             model.overview$model.b2$model, 
                                             model.overview$model.b3$model, digits = 3,
                                             dv.labels = c("Model 0", "Model B1",
                                                           "Model B2", "Model B3"))