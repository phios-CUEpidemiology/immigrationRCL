# Title: "RCL and immigration enforcement, Main Model Script"
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

#data <- read_csv("BMC/BMC_rev1/GitHub/dataOut3.2024.csv")
#data <- read_csv("dataOut3.2024.csv")

# Main arrest models

a1 <- tidy(a1m <- glm(arrestCannabis ~ State + factor(time) + RCL + offset(log(Population)), 
                      data = data, family = "poisson"), exponentiate = TRUE) %>% 
  filter(term=="RCL") %>% 
  mutate(lci=estimate-(1.96*std.error), uci=estimate+(1.96*std.error)) %>% 
  mutate_if(is.numeric, round, digits=2) %>% 
  mutate(type="Arrest, No Lag, Unadjusted") %>% 
  select(type, estimate, lci, uci, p.value)

a2 <- tidy(a2m <- glm(arrestCannabis ~ State + factor(time) + 
                        MCL + Decriminalization + hhi + popBIPOC + popNotProfE + 
                        popPolice + party + season + ICE_SC +
                        RCL +
                        offset(log(Population)), data = data, 
                      family = "poisson"), exponentiate = TRUE) %>% 
  filter(term=="RCL") %>% 
  mutate(lci=estimate-(1.96*std.error), uci=estimate+(1.96*std.error))  %>% 
  mutate_if(is.numeric, round, digits=2) %>% 
  mutate(type="Arrest, No Lag, Adjusted") %>% 
  select(type, estimate, lci, uci, p.value)

dispersiontest(a2m)

a3 <- tidy(a3m <- glm(arrestCannabis ~ State + factor(time) + RCL + offset(log(Population)), 
                      data = data, family = "poisson"), exponentiate = TRUE) %>% 
  filter(term=="RCL") %>% 
  mutate(lci=estimate-(1.96*std.error), uci=estimate+(1.96*std.error)) %>% 
  mutate_if(is.numeric, round, digits=2) %>% 
  mutate(type="Arrest, Lagged, Unadjusted") %>% 
  select(type, estimate, lci, uci, p.value)

a4 <- tidy(a4m <- glm(arrestCannabis ~ State + factor(time) + 
                        MCL + Decriminalization + hhi + popBIPOC + popNotProfE + 
                        popPolice + party + season + ICE_SC +
                        RCL +
                        offset(log(Population)), data = data, 
                      family = "poisson"), exponentiate = TRUE) %>% 
  filter(term=="RCL") %>% 
  mutate(lci=estimate-(1.96*std.error), uci=estimate+(1.96*std.error))  %>% 
  mutate_if(is.numeric, round, digits=2) %>% 
  mutate(type="Arrest, Lagged, Adjusted") %>% 
  select(type, estimate, lci, uci, p.value)

dispersiontest(a4m)

# Main deportation models

d1 <- tidy(d1m <- glm(deportCannabis ~ State + factor(time) + RCL + offset(log(Population)), 
                      data = data, family = "poisson"), exponentiate = TRUE) %>% 
  filter(term=="RCL") %>% 
  mutate(lci=estimate-(1.96*std.error), uci=estimate+(1.96*std.error)) %>% 
  mutate_if(is.numeric, round, digits=2) %>% 
  mutate(type="Deport, No Lag, Unadjusted") %>% 
  select(type, estimate, lci, uci, p.value)

d2 <- tidy(d2m <- glm(arrestCannabis ~ State + factor(time) + 
                        MCL + Decriminalization + hhi + popBIPOC + popNotProfE + 
                        popPolice + party + season + ICE_SC +
                        RCL +
                        offset(log(Population)), data = data, 
                      family = "poisson"), exponentiate = TRUE) %>% 
  filter(term=="RCL") %>% 
  mutate(lci=estimate-(1.96*std.error), uci=estimate+(1.96*std.error))  %>% 
  mutate_if(is.numeric, round, digits=2) %>% 
  mutate(type="Arrest, No Lag, Adjusted") %>% 
  select(type, estimate, lci, uci, p.value)

dispersiontest(d2m)

d3 <- tidy(d3m <- glm(deportCannabis ~ State + factor(time) + RCL + offset(log(Population)), 
                      data = data, family = "poisson"), exponentiate = TRUE) %>% 
  filter(term=="RCL") %>% 
  mutate(lci=estimate-(1.96*std.error), uci=estimate+(1.96*std.error)) %>% 
  mutate_if(is.numeric, round, digits=2) %>% 
  mutate(type="Arrest, Lagged, Unadjusted") %>% 
  select(type, estimate, lci, uci, p.value)

d4 <- tidy(d4m <- glm(deportCannabis ~ State + factor(time) + 
                        MCL + Decriminalization + hhi + popBIPOC + popNotProfE + 
                        popPolice + party + season + ICE_SC +
                        RCL +
                        offset(log(Population)), data = data, 
                      family = "poisson"), exponentiate = TRUE) %>% 
  filter(term=="RCL") %>% 
  mutate(lci=estimate-(1.96*std.error), uci=estimate+(1.96*std.error))  %>% 
  mutate_if(is.numeric, round, digits=2) %>% 
  mutate(type="Arrest, Lagged, Adjusted") %>% 
  select(type, estimate, lci, uci, p.value)

dispersiontest(d4m)

