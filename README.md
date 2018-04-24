---
title: "CCPEBaseline"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Cleaning HCS data
```{r}
#setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/HealthCapitalScale")
#HCS = read.csv("CKY_HealthCapitalScales_20180330.csv", header = TRUE)
library(lavaan)
library(psych)
library(semTools)
library(dplyr)
head(HCS)
HCS = HCS[c("GoodHealth", "ManageHealthProblems", "KnowHealthConditions", "PhysicalActivity", "ManageMentalWellness", "HopesForFuture", "NotOverwhelmed", "Attending_PCP", "ProvidersSimilarGoals", "CommunicateHealthcareNeeds", "NoFutureHospitalization", "No_ED_Use", "KnowPrescribedMeds", "TakePrescribedMeds", "NoSideEffectsConcerns", "CookNutritiousMeals", "NutritiousFoodPhysicalBarrier", "NutritiousFoodFinancialBarrier", "NutritiousWellBalanced", "HealthyHomeEnvironment", "SafeMovingAroundHome", "LivingSituationSatisfaction", "HaveHome", "PhysicallySafeNeighborhood", "LiveCloseLovedOnes", "TransportationAccess", "SupportiveFriends", "ProvideFinancialSupportFamily", "ParticipateSocialActivity", "DifficultHealthWellness", "ProvideFinancialSupportFamily", "ManageFinances", "FinancialNegativeAffectHealth", "EducationSatisfaction", "EmploymentSatisfaction")]
```
Build model for initial testing
```{r}
dim(HCS)
typeof(HCS)
write.csv(HCS, "HCS.csv", row.names = FALSE)
HCS = read.csv("HCS.csv", header = TRUE)
alpha(HCS)
summary(HCS)
library(fmsb)
CronbachAlpha(HCS)
```
Now put together CFA model
```{r}
model1 = "HCS =~ GoodHealth+ ManageHealthProblems+ KnowHealthConditions+ PhysicalActivity+ ManageMentalWellness+ HopesForFuture+ NotOverwhelmed+ Attending_PCP+ ProvidersSimilarGoals+ CommunicateHealthcareNeeds+ NoFutureHospitalization+ No_ED_Use+ KnowPrescribedMeds+ TakePrescribedMeds+ NoSideEffectsConcerns+ CookNutritiousMeals+ NutritiousFoodPhysicalBarrier+ NutritiousFoodFinancialBarrier+ NutritiousWellBalanced+ HealthyHomeEnvironment+ SafeMovingAroundHome+ LivingSituationSatisfaction+ HaveHome+ PhysicallySafeNeighborhood+ LiveCloseLovedOnes+ TransportationAccess+ SupportiveFriends+ ProvideFinancialSupportFamily+ ParticipateSocialActivity+ DifficultHealthWellness+ ProvideFinancialSupportFamily+ ManageFinances+ FinancialNegativeAffectHealth+ EducationSatisfaction+ EmploymentSatisfaction"

fit1 = cfa(model1, estimator = "MLR", missing = "fiml", std.lv = TRUE, data = HCS)
summary(fit1, fit.measures = TRUE)

```







