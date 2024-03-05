# Title: "RCL and immigration enforcement, Sensitivity analyses"
# Author: "PHIOS CUEpidemiology"
# Date: "010/02/2023"

library(tidyverse)
library(broom)
library(lubridate)
library(AER)
library(data.table)
library(fixest)
options(scipen = 999)

# Load the county median household income

data <- read_csv("BMC/BMC_rev1/GitHub/dataOut3.2024.csv")
#data <- read_csv("dataOut3.2024.csv")

# Arrest models, event study 

a1SE1 = feglm(arrestCannabis ~ i(time_to_treat_m, treat, ref = -1) + 
                MCL + Decriminalization + hhi + popBIPOC + popNotProfE +
                popPolice + party + season + ICE_SC | 
                State + date, data = data, family = "poisson")

iplot(a1SE1, xlab = 'Month to RCL')

d1SE1 = feglm(deportCannabis ~ i(time_to_treat_m, treat, ref = -1) + 
                MCL + Decriminalization + hhi + popBIPOC + popNotProfE +
                popPolice + party + season + ICE_SC | 
                State + date, data = data, family = "poisson")

iplot(d1SE1, xlab = 'Month to RCL')

# Arrest models, replacing population with estimated undocumented population (SE2)

a1SE2 <- tidy(a1SE2 <- glm(arrestCannabis ~ State + factor(time) + RCL + offset(log(PopUI)), 
                      data = data, family = "poisson"), exponentiate = TRUE) %>% 
  filter(term=="RCL") %>% 
  mutate(lci=estimate-(1.96*std.error), uci=estimate+(1.96*std.error)) %>% 
  mutate_if(is.numeric, round, digits=2) %>% 
  mutate(type="Arrest, No Lag, Unadjusted, SE2") %>% 
  select(type, estimate, lci, uci, p.value)

a2SE2 <- tidy(a2SE2 <- glm(arrestCannabis ~ State + factor(time) + 
                        MCL + Decriminalization + hhi + popBIPOC + popNotProfE + 
                        popPolice + party + season + ICE_SC +
                        RCL +
                        offset(log(PopUI)), data = data, 
                      family = "poisson"), exponentiate = TRUE) %>% 
  filter(term=="RCL") %>% 
  mutate(lci=estimate-(1.96*std.error), uci=estimate+(1.96*std.error))  %>% 
  mutate_if(is.numeric, round, digits=2) %>% 
  mutate(type="Arrest, No Lag, Adjusted, SE2") %>% 
  select(type, estimate, lci, uci, p.value)

a3SE2 <- tidy(a3SE2 <- glm(arrestCannabis ~ State + factor(time) + RCL + offset(log(PopUI)), 
                      data = data, family = "poisson"), exponentiate = TRUE) %>% 
  filter(term=="RCL") %>% 
  mutate(lci=estimate-(1.96*std.error), uci=estimate+(1.96*std.error)) %>% 
  mutate_if(is.numeric, round, digits=2) %>% 
  mutate(type="Arrest, Lagged, Unadjusted, SE2") %>% 
  select(type, estimate, lci, uci, p.value)

a4SE2 <- tidy(a4SE2 <- glm(arrestCannabis ~ State + factor(time) + 
                        MCL + Decriminalization + hhi + popBIPOC + popNotProfE + 
                        popPolice + party + season + ICE_SC +
                        RCL +
                        offset(log(PopUI)), data = data, 
                      family = "poisson"), exponentiate = TRUE) %>% 
  filter(term=="RCL") %>% 
  mutate(lci=estimate-(1.96*std.error), uci=estimate+(1.96*std.error))  %>% 
  mutate_if(is.numeric, round, digits=2) %>% 
  mutate(type="Arrest, Lagged, Adjusted, SE2") %>% 
  select(type, estimate, lci, uci, p.value)

# Deportation models, replacing population with estimated undocumented population (SE2)

d1SE2 <- tidy(d1SE2 <- glm(deportCannabis ~ State + factor(time) + RCL + offset(log(PopUI)), 
                      data = data, family = "poisson"), exponentiate = TRUE) %>% 
  filter(term=="RCL") %>% 
  mutate(lci=estimate-(1.96*std.error), uci=estimate+(1.96*std.error)) %>% 
  mutate_if(is.numeric, round, digits=2) %>% 
  mutate(type="Deport, No Lag, Unadjusted, SE2") %>% 
  select(type, estimate, lci, uci, p.value)

d2SE2 <- tidy(d2SE2 <- glm(arrestCannabis ~ State + factor(time) + 
                        MCL + Decriminalization + hhi + popBIPOC + popNotProfE + 
                        popPolice + party + season + ICE_SC +
                        RCL +
                        offset(log(PopUI)), data = data, 
                      family = "poisson"), exponentiate = TRUE) %>% 
  filter(term=="RCL") %>% 
  mutate(lci=estimate-(1.96*std.error), uci=estimate+(1.96*std.error))  %>% 
  mutate_if(is.numeric, round, digits=2) %>% 
  mutate(type="Arrest, No Lag, Adjusted, SE2") %>% 
  select(type, estimate, lci, uci, p.value)

d3SE2 <- tidy(d3SE2 <- glm(deportCannabis ~ State + factor(time) + RCL + offset(log(PopUI)), 
                      data = data, family = "poisson"), exponentiate = TRUE) %>% 
  filter(term=="RCL") %>% 
  mutate(lci=estimate-(1.96*std.error), uci=estimate+(1.96*std.error)) %>% 
  mutate_if(is.numeric, round, digits=2) %>% 
  mutate(type="Arrest, Lagged, Unadjusted, SE2") %>% 
  select(type, estimate, lci, uci, p.value)

d4SE2 <- tidy(d4SE2 <- glm(deportCannabis ~ State + factor(time) + 
                        MCL + Decriminalization + hhi + popBIPOC + popNotProfE + 
                        popPolice + party + season + ICE_SC +
                        RCL +
                        offset(log(PopUI)), data = data, 
                      family = "poisson"), exponentiate = TRUE) %>% 
  filter(term=="RCL") %>% 
  mutate(lci=estimate-(1.96*std.error), uci=estimate+(1.96*std.error))  %>% 
  mutate_if(is.numeric, round, digits=2) %>% 
  mutate(type="Arrest, Lagged, Adjusted, SE2") %>% 
  select(type, estimate, lci, uci, p.value)

# Arrest models, excluding always treated units (SE3)
dataSE3 <- filter(data, State != "CO")
dataSE3 <- filter(dataSE3, State != "WA")

a1SE3 <- tidy(a1SE3 <- glm(arrestCannabis ~ State + factor(time) + RCL + offset(log(PopUI)), 
                           data = dataSE3, family = "poisson"), exponentiate = TRUE) %>% 
  filter(term=="RCL") %>% 
  mutate(lci=estimate-(1.96*std.error), uci=estimate+(1.96*std.error)) %>% 
  mutate_if(is.numeric, round, digits=2) %>% 
  mutate(type="Arrest, No Lag, Unadjusted, SE3") %>% 
  select(type, estimate, lci, uci, p.value)

a2SE3 <- tidy(a2SE3 <- glm(arrestCannabis ~ State + factor(time) + 
                             MCL + Decriminalization + hhi + popBIPOC + popNotProfE + 
                             popPolice + party + season + ICE_SC +
                             RCL +
                             offset(log(PopUI)), data = dataSE3, 
                           family = "poisson"), exponentiate = TRUE) %>% 
  filter(term=="RCL") %>% 
  mutate(lci=estimate-(1.96*std.error), uci=estimate+(1.96*std.error))  %>% 
  mutate_if(is.numeric, round, digits=2) %>% 
  mutate(type="Arrest, No Lag, Adjusted, SE3") %>% 
  select(type, estimate, lci, uci, p.value)

a3SE3 <- tidy(a3SE3 <- glm(arrestCannabis ~ State + factor(time) + RCL + offset(log(PopUI)), 
                           data = dataSE3, family = "poisson"), exponentiate = TRUE) %>% 
  filter(term=="RCL") %>% 
  mutate(lci=estimate-(1.96*std.error), uci=estimate+(1.96*std.error)) %>% 
  mutate_if(is.numeric, round, digits=2) %>% 
  mutate(type="Arrest, Lagged, Unadjusted, SE3") %>% 
  select(type, estimate, lci, uci, p.value)

a4SE3 <- tidy(a4SE3 <- glm(arrestCannabis ~ State + factor(time) + 
                             MCL + Decriminalization + hhi + popBIPOC + popNotProfE + 
                             popPolice + party + season + ICE_SC +
                             RCL +
                             offset(log(PopUI)), data = dataSE3, 
                           family = "poisson"), exponentiate = TRUE) %>% 
  filter(term=="RCL") %>% 
  mutate(lci=estimate-(1.96*std.error), uci=estimate+(1.96*std.error))  %>% 
  mutate_if(is.numeric, round, digits=2) %>% 
  mutate(type="Arrest, Lagged, Adjusted, SE3") %>% 
  select(type, estimate, lci, uci, p.value)

# Deportation models, dropping 2020 (SE4)

dataSE4 <- filter(data, Year != 2020)

d1SE4 <- tidy(d1SE4 <- glm(deportCannabis ~ State + factor(time) + RCL + offset(log(PopUI)), 
                           data = dataSE4, family = "poisson"), exponentiate = TRUE) %>% 
  filter(term=="RCL") %>% 
  mutate(lci=estimate-(1.96*std.error), uci=estimate+(1.96*std.error)) %>% 
  mutate_if(is.numeric, round, digits=2) %>% 
  mutate(type="Deport, No Lag, Unadjusted, SE4") %>% 
  select(type, estimate, lci, uci, p.value)

d2SE4 <- tidy(d2SE4 <- glm(arrestCannabis ~ State + factor(time) + 
                             MCL + Decriminalization + hhi + popBIPOC + popNotProfE + 
                             popPolice + party + season + ICE_SC +
                             RCL +
                             offset(log(PopUI)), data = dataSE4, 
                           family = "poisson"), exponentiate = TRUE) %>% 
  filter(term=="RCL") %>% 
  mutate(lci=estimate-(1.96*std.error), uci=estimate+(1.96*std.error))  %>% 
  mutate_if(is.numeric, round, digits=2) %>% 
  mutate(type="Arrest, No Lag, Adjusted, SE4") %>% 
  select(type, estimate, lci, uci, p.value)

d3SE4 <- tidy(d3SE4 <- glm(deportCannabis ~ State + factor(time) + RCL + offset(log(PopUI)), 
                           data = dataSE4, family = "poisson"), exponentiate = TRUE) %>% 
  filter(term=="RCL") %>% 
  mutate(lci=estimate-(1.96*std.error), uci=estimate+(1.96*std.error)) %>% 
  mutate_if(is.numeric, round, digits=2) %>% 
  mutate(type="Arrest, Lagged, Unadjusted, SE4") %>% 
  select(type, estimate, lci, uci, p.value)

d4SE4 <- tidy(d4SE4 <- glm(deportCannabis ~ State + factor(time) + 
                             MCL + Decriminalization + hhi + popBIPOC + popNotProfE + 
                             popPolice + party + season + ICE_SC +
                             RCL +
                             offset(log(PopUI)), data = dataSE4, 
                           family = "poisson"), exponentiate = TRUE) %>% 
  filter(term=="RCL") %>% 
  mutate(lci=estimate-(1.96*std.error), uci=estimate+(1.96*std.error))  %>% 
  mutate_if(is.numeric, round, digits=2) %>% 
  mutate(type="Arrest, Lagged, Adjusted, SE4") %>% 
  select(type, estimate, lci, uci, p.value)

