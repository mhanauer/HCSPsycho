---
title: "CCPEBaseline"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Cleaning HCS data
```{r}
setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/HealthCapitalScale")
HCS = read.csv("CIL_RCS_Variables_6142018.csv", header = TRUE)
library(lavaan)
library(psych)
library(semTools)
library(dplyr)
head(HCS)
HCS = HCS[c("work_environment", "work_job", "fin_resources", "leg_comply", "rel_partner", "rel_family", "rel_friends", "rel_clean_users", "rel_groups_access", "rel_group_member", "rel_sponsor", "rel_online", "rel_sup_others", "liv_symbols", "men_profound", "rel_participate", "form_sup_rec_prog", "form_sup_pcp", "rel_program", "drug_mat", "form_sup_insurance", "med_health", "health_plan", "men_rituals", "prob_skills", "men_self", "men_purpose", "rel_service", "men_hopes", "rel_values", "liv_safe", "liv_nut_meals", "liv_transport", "liv_clothes", "drug_environment", "men_activity", "drug_abstain", "form_sup_bhealth_prog", "men_manage")]
head(HCS)
```
Build model for initial testing.  Get rid of missing values for cronbach alpha
```{r}
dim(HCS)
typeof(HCS)
write.csv(HCS, "HCS.csv", row.names = FALSE)
HCS = read.csv("HCS.csv", header = TRUE, na.strings = c("NULL"))
alpha(HCS)
```
Now put together CFA model
```{r}
model1 = "HCS =~ work_environment+ work_job+ fin_resources+ leg_comply+ rel_partner+ rel_family+ rel_friends+ rel_clean_users+ rel_groups_access+ rel_group_member+ rel_sponsor+ rel_online+ rel_sup_others+ liv_symbols+ men_profound+ rel_participate+ form_sup_rec_prog+ form_sup_pcp+ rel_program+ drug_mat+ form_sup_insurance+ med_health+ health_plan+ men_rituals+ prob_skills+ men_self+ men_purpose+ rel_service+ men_hopes+ rel_values+ liv_safe+ liv_nut_meals+ liv_transport+ liv_clothes+ drug_environment+ men_activity+ drug_abstain+ form_sup_bhealth_prog+ men_manage"

fit1 = cfa(model1, estimator = "MLR", missing = "fiml", std.lv = TRUE, data = HCS)
summary(fit1, fit.measures = TRUE)

```







