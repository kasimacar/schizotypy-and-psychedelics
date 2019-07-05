# clear workspace:
rm(list=ls())

# Load libraries:
library(reghelper)
library(ggplot2)
library(ggstance)
library(radarchart)
library(coefplot)
library(psych)
library(effsize)
library(lm.beta)
library(mlogit)
library(jtools)

# Load data
load("HUD_anonymized.rda")


#subset young adults
SCREEN_df <- SCREEN_df[(SCREEN_df$age<36),]
CONSP_df <- CONSP_df[(CONSP_df$age<36),]

SCREEN_df <- SCREEN_df[(SCREEN_df$age>17),]
CONSP_df <- CONSP_df[(CONSP_df$age>17),]

#identify and option to remove outliers
outlier <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(dt))
  } else{
    cat("Nothing changed", "n")
    return(invisible(var_name))
  }
}
outlier(CONSP_df, DP) #data, variable


# Selecting subsamples of those without/with  psychiatric diagnoses:
SCREEN_df_noPsych <- subset(SCREEN_df, SCREEN_df$PsychDiagAny==0)
CONSP_df_noPsych <- subset(CONSP_df, CONSP_df$PsychDiagAny==0)

SCREEN_df_Psych <- subset(SCREEN_df, SCREEN_df$PsychDiagAny==1)
CONSP_df_Psych <- subset(CONSP_df, CONSP_df$PsychDiagAny==1)

#########################
### Group comparisons ###
#########################


### Whole sample:
# T-test: 
t.test(SCREEN_df$DP[SCREEN_df$drug_psychedelics==0],SCREEN_df$DP[SCREEN_df$drug_psychedelics==1])

# effect size:
cohen.d(SCREEN_df$DP[SCREEN_df$drug_psychedelics==0],SCREEN_df$DP[SCREEN_df$drug_psychedelics==1], na.rm = T)


# Plot whole sample
boxplot(SCREEN_df$DP[SCREEN_df$drug_psychedelics==0],SCREEN_df$DP[SCREEN_df$drug_psychedelics==1], ylab="Schizotypy (z-score)", outline = F)
axis(side = 1, at = 1, labels = 'Non-users')
axis(side = 1, at = 2, labels = 'Psychedelic Drug-users')
points(cbind(jitter(rep(1, table(SCREEN_df$drug_psychedelics==0)[2])), SCREEN_df$DP[SCREEN_df$drug_psychedelics==0]), pch=16)
points(cbind(jitter(rep(2, table(SCREEN_df$drug_psychedelics==1)[2])), SCREEN_df$DP[SCREEN_df$drug_psychedelics==1]), pch=16)

### Only non-psychiatric diagnoses
# Two-sample t-test: 
t.test(SCREEN_df_noPsych$DP[SCREEN_df_noPsych$drug_psychedelics==0],SCREEN_df_noPsych$DP[SCREEN_df_noPsych$drug_psychedelics==1])
# Plot:
boxplot(SCREEN_df_noPsych$DP[SCREEN_df_noPsych$drug_psychedelics==0],SCREEN_df_noPsych$DP[SCREEN_df_noPsych$drug_psychedelics==1], ylab="Schizotypy (Z-score)", outline = F)
axis(side = 1, at = 1, labels = 'Non-users')
axis(side = 1, at = 2, labels = 'Psychedelic Drug-users', pos = -2.68)
points(cbind(jitter(rep(1, table(SCREEN_df_noPsych$drug_psychedelics==0)[2])), SCREEN_df_noPsych$DP[SCREEN_df_noPsych$drug_psychedelics==0]), pch=16)
points(cbind(jitter(rep(2, table(SCREEN_df_noPsych$drug_psychedelics==1)[2])), SCREEN_df_noPsych$DP[SCREEN_df_noPsych$drug_psychedelics==1]), pch=16)

### Only psychiatric diagnoses
t.test(SCREEN_df_Psych$DP[SCREEN_df_Psych$drug_psychedelics==0],SCREEN_df_Psych$DP[SCREEN_df_Psych$drug_psychedelics==1])

### General Linear Modelling
# Whole sample
mydatawhole <- data.frame(Psychedelics = SCREEN_df$drug_psychedelics, Opiates = SCREEN_df$drug_opi,
                     MDMA = SCREEN_df$drug_mdma, Alcohol = SCREEN_df$drug_alc,
                     Cannabis = SCREEN_df$drug_cannabis, Tobacco = SCREEN_df$drug_tobacco,
                     Stimulants = SCREEN_df$drug_stim, Schizotypy = SCREEN_df$DP, Age = SCREEN_df$age, Sex = SCREEN_df$sex)
All.Subjects <- glm (Schizotypy ~ Age+Sex+Psychedelics+Opiates+MDMA+Alcohol+Cannabis+Tobacco+Stimulants, data= mydatawhole)
summary(All.Subjects)
beta(All.Subjects)
coefplot.glm(All.Subjects, intercept = F, decreasing = T, title = NULL, xlab = "Estimate", color = "black")

summ(All.Subjects)

# Only no psychiatric diagnoses
mydatanodiag <- data.frame(Psychedelics = SCREEN_df_noPsych$drug_psychedelics, Opiates = SCREEN_df_noPsych$drug_opi,
                          MDMA = SCREEN_df_noPsych$drug_mdma, Alcohol = SCREEN_df_noPsych$drug_alc,
                          Cannabis = SCREEN_df_noPsych$drug_cannabis, Tobacco = SCREEN_df_noPsych$drug_tobacco,
                          Stimulants = SCREEN_df_noPsych$drug_stim, Schizotypy = SCREEN_df_noPsych$DP, Age = SCREEN_df_noPsych$age, Sex = SCREEN_df_noPsych$sex)
No.Diagnoses <- glm(Schizotypy ~ Age+Sex+Psychedelics+Opiates+MDMA+Alcohol+Cannabis+Tobacco+Stimulants, data= mydatanodiag)
summary(No.Diagnoses)
beta(No.Diagnoses)
coefplot.glm(No.Diagnoses, intercept = F, decreasing = T, title = NULL, xlab = "Estimate", color = "black")

# Create plot for both models
multiplot(All.Subjects, No.Diagnoses, intercept = F, decreasing = T, title = NULL,
          xlab = "Estimate", numeric = F, zeroColor = "black")


###############################
####  Continious variables ####
### (overall drug exposure) ###
###############################

# Whole sample
mydatawhole <- data.frame(Psychedelics = CONSP_df$PSY_freqprox, Opiates = CONSP_df$OPI_freqprox,
                     MDMA = CONSP_df$MDMA_freqprox, Alcohol = CONSP_df$ALC_freqprox,
                     Cannabis = CONSP_df$CAN_freqprox, Tobacco = CONSP_df$TOB_freqprox,
                     Stimulants = CONSP_df$STIM_freqprox, Schizotypy = CONSP_df$DP, Age = CONSP_df$age, Sex = CONSP_df$sex)
Fit.AllDrugs.All <- glm (Schizotypy ~ Age+Sex+Psychedelics+Opiates+MDMA+Alcohol+Cannabis+Tobacco+Stimulants, data= mydatawhole)
summary(Fit.AllDrugs.All)
beta(Fit.AllDrugs.All)
coefplot.glm(Fit.AllDrugs.All, intercept = F, decreasing = T, title = NULL, xlab = "Estimate", color = "black")

# Only non-psychiatric diagnoses
mydata <- data.frame(Psychedelics = CONSP_df_noPsych$PSY_freqprox, Opiates = CONSP_df_noPsych$OPI_freqprox,
                     MDMA = CONSP_df_noPsych$MDMA_freqprox, Alcohol = CONSP_df_noPsych$ALC_freqprox,
                     Cannabis = CONSP_df_noPsych$CAN_freqprox, Tobacco = CONSP_df_noPsych$TOB_freqprox,
                     Stimulants = CONSP_df_noPsych$STIM_freqprox, Schizotypy = CONSP_df_noPsych$DP, Age = CONSP_df_noPsych$age, Sex = CONSP_df_noPsych$sex)
Fit.AllDrugs <- glm (Schizotypy ~ Age+Sex+Psychedelics+Opiates+MDMA+Alcohol+Cannabis+Tobacco+Stimulants, data= mydata)
summary(Fit.AllDrugs)
beta(Fit.AllDrugs)
coefplot.glm(Fit.AllDrugs, intercept = F, decreasing = T, title = NULL, xlab = "Estimate", color = "black")


###############################
####          BADE         ####
###     Continuous data     ###
###############################

#Standardized scores EII
EII1scaled <- scale(HUDMAIN_df$EII2)
EII1scaled

EII2scaled <- scale(HUDMAIN_df$EII)
EII2scaled

#EII (old)
mydata.EII1 <- data.frame(Psychedelics = HUDMAIN_df$PSY_freqprox,
                       MDMA = HUDMAIN_df$MDMA_freqprox, Alcohol = HUDMAIN_df$ALC_freqprox, Opiates = HUDMAIN_df$OPI_freqprox,
                       Cannabis = HUDMAIN_df$CAN_freqprox, Tobacco = HUDMAIN_df$TOB_freqprox,
                       Stimulants = HUDMAIN_df$STIM_freqprox, EII = HUDMAIN_df$EII2, Age = HUDMAIN_df$age, Sex = HUDMAIN_df$sex)
EII.1 <- glm (EII ~ Age+Sex+Psychedelics+Opiates+MDMA+Alcohol+Cannabis+Tobacco+Stimulants, data= mydata.EII1)
EII.1.S <- lm.beta(EII.1)
summary(EII.1.S)
summary(EII.1)
beta(EII.1)
coefplot.glm(EII.1, intercept = F, decreasing = T, title = NULL, xlab = "Estimate", color = "black")

#EII (new)
mydata.EII2 <- data.frame(Psychedelics = HUDMAIN_df$PSY_freqprox,
                         MDMA = HUDMAIN_df$MDMA_freqprox, Alcohol = HUDMAIN_df$ALC_freqprox, Opiates = HUDMAIN_df$OPI_freqprox,
                         Cannabis = HUDMAIN_df$CAN_freqprox, Tobacco = HUDMAIN_df$TOB_freqprox,
                         Stimulants = HUDMAIN_df$STIM_freqprox, EII = HUDMAIN_df$EII, Age = HUDMAIN_df$age, Sex = HUDMAIN_df$sex)
EII.2 <- glm (EII ~ Age+Sex+Psychedelics+Opiates+MDMA+Alcohol+Cannabis+Tobacco+Stimulants, data= mydata.EII2)
EII.2.S <- lm.beta(EII.2)
summary(EII.2)
summary(EII.2.S)
beta(EII.2)
coefplot.glm(EII.2, intercept = F, decreasing = T, title = NULL, xlab = "Estimate", color = "black")

# Create plot for both models
multiplot(EII.1, EII.2, intercept = F, decreasing = T, title = NULL,
          xlab = "Estimate", numeric = F, zeroColor = "black")



### Aversive Fear Learning

# Change NA value to 1 (for Participant AL)
HUDMAIN_df$drug_psychedelics.x[is.na(HUDMAIN_df$drug_psychedelics.x)] <- 1

# T-test alpha between groups
t.test(HUDMAIN_df$alpha[HUDMAIN_df$drug_psychedelics.x==0],HUDMAIN_df$alpha[HUDMAIN_df$drug_psychedelics.x==1])

#effect size:
cohen.d(HUDMAIN_df$alpha[HUDMAIN_df$drug_psychedelics.x==0],HUDMAIN_df$alpha[HUDMAIN_df$drug_psychedelics.x==1], na.rm = T)


# change numbers of plots
par(mfrow=c(1,2))

# Z-score (normalise) alpha for plot
HUDMAIN_df$alphascaled <- scale(HUDMAIN_df$alpha)

#boxplot:
alphaplot <- boxplot(HUDMAIN_df$alphascaled[HUDMAIN_df$drug_psychedelics.x==0],HUDMAIN_df$alphascaled[HUDMAIN_df$drug_psychedelics.x==1], ylab="α (z-score)", outline = F, xlim=c(0.5,3), ylim=c(-1.5,1.5))
axis(side = 1, at = 1, labels = 'Non-users')
axis(side = 1, at = 2, labels = 'Psychedelic\nDrug-users', pos = -1.78)
rect(2.85 - 0.2, max(HUDMAIN_df$drug_psychedelics.x==0) - 0.12, 2.85 + 0.2, max(HUDMAIN_df$drug_psychedelics.x==1) + 0.12)
text(2.84, max(HUDMAIN_df$drug_psychedelics.x==0), paste0("p < ", round(t.test(HUDMAIN_df$alphascaled[HUDMAIN_df$drug_psychedelics.x==0],HUDMAIN_df$alphascaled[HUDMAIN_df$drug_psychedelics.x==1])$p.value, 3)))
points(cbind(jitter(rep(1, table(HUDMAIN_df$drug_psychedelics.x==0)[2])), HUDMAIN_df$alphascaled[HUDMAIN_df$drug_psychedelics.x==0]), pch=16)
points(cbind(jitter(rep(2, table(HUDMAIN_df$drug_psychedelics.x==1)[2])), HUDMAIN_df$alphascaled[HUDMAIN_df$drug_psychedelics.x==1]), pch=16)


# T-test rho between groups
t.test(HUDMAIN_df$rhos[HUDMAIN_df$drug_psychedelics.x==0],HUDMAIN_df$rhos[HUDMAIN_df$drug_psychedelics.x==1])

#effect size:
cohen.d(HUDMAIN_df$rhos[HUDMAIN_df$drug_psychedelics.x==0],HUDMAIN_df$rhos[HUDMAIN_df$drug_psychedelics.x==1], na.rm = T)


#z-score (normalise) rho
HUDMAIN_df$rhosscaled <- scale(HUDMAIN_df$rhos)

#boxplot:
rhoplot <- boxplot(HUDMAIN_df$rhosscaled[HUDMAIN_df$drug_psychedelics.x==0],HUDMAIN_df$rhosscaled[HUDMAIN_df$drug_psychedelics.x==1], ylab="ρ (z-score)", outline = F, xlim=c(0.5,3), ylim=c(-3,1.5))
axis(side = 1, at = 1, labels = 'Non-users')
axis(side = 1, at = 2, labels = 'Psychedelic\nDrug-users', pos = -3.43)
rect(2.85 - 0.2, max(HUDMAIN_df$drug_psychedelics.x==0) - 0.12, 2.85 + 0.2, max(HUDMAIN_df$drug_psychedelics.x==1) + 0.12)
text(2.84, max(HUDMAIN_df$drug_psychedelics.x==0), paste0("p < ", round(t.test(HUDMAIN_df$rhosscaled[HUDMAIN_df$drug_psychedelics.x==0],HUDMAIN_df$rhosscaled[HUDMAIN_df$drug_psychedelics.x==1])$p.value, 3)))
points(cbind(jitter(rep(1, table(HUDMAIN_df$drug_psychedelics.x==0)[2])), HUDMAIN_df$rhosscaled[HUDMAIN_df$drug_psychedelics.x==0]), pch=16)
points(cbind(jitter(rep(2, table(HUDMAIN_df$drug_psychedelics.x==1)[2])), HUDMAIN_df$rhosscaled[HUDMAIN_df$drug_psychedelics.x==1]), pch=16)



#GLM test regression of rho on Psychedelic temporal proximity
rhotest <- glm(rhos ~ PSY_prox, data = HUDMAIN_df)
summary(rhotest)
beta(rhotest)


# (Below are the plots for SEPI. Keep in mind that the distributuions are not transformed to meet normality criteria):
boxplot(SCREEN_df$SEPI_tot_drug[SCREEN_df$drug_psychedelics==0],SCREEN_df$SEPI_tot_drug[SCREEN_df$drug_psychedelics==1])
points(cbind(jitter(rep(1, table(SCREEN_df$drug_psychedelics==0)[2])), SCREEN_df$SEPI_tot_drug[SCREEN_df$drug_psychedelics==0]), pch=16)
points(cbind(jitter(rep(2, table(SCREEN_df$drug_psychedelics==1)[2])), SCREEN_df$SEPI_tot_drug[SCREEN_df$drug_psychedelics==1]), pch=16)

boxplot(SCREEN_df$SEPI_tot[SCREEN_df$drug_psychedelics==0],SCREEN_df$SEPI_tot[SCREEN_df$drug_psychedelics==1])
points(cbind(jitter(rep(1, table(SCREEN_df$drug_psychedelics==0)[2])), SCREEN_df$SEPI_tot[SCREEN_df$drug_psychedelics==0]), pch=16)
points(cbind(jitter(rep(2, table(SCREEN_df$drug_psychedelics==1)[2])), SCREEN_df$SEPI_tot[SCREEN_df$drug_psychedelics==1]), pch=16)


boxplot(SCREEN_df$SEPI_tot_drug[SCREEN_df$drug_psychedelics==0],SCREEN_df$SEPI_tot_drug[SCREEN_df$drug_psychedelics==1])
points(cbind(jitter(rep(1, table(SCREEN_df$drug_psychedelics==0)[2])), SCREEN_df$SEPI_tot_drug[SCREEN_df$drug_psychedelics==0]), pch=16)
points(cbind(jitter(rep(2, table(SCREEN_df$drug_psychedelics==1)[2])), SCREEN_df$SEPI_tot_drug[SCREEN_df$drug_psychedelics==1]), pch=16)


######################
### LOG REGRESSION ###
######################

# Change all values above "1" to "1" (binomial)
SCREEN_df$SEPI_tot[SCREEN_df$SEPI_tot > "1"] <- "1"
CONSP_df$SEPI_tot_drug[CONSP_df$SEPI_tot_drug > "1"] <- "1"

#Endogenous ego-pathology
mydatawhole <- data.frame(Psychedelics = SCREEN_df$drug_psychedelics, Opiates = SCREEN_df$drug_opi,
                          MDMA = SCREEN_df$drug_mdma, Alcohol = SCREEN_df$drug_alc,
                          Cannabis = SCREEN_df$drug_cannabis, Tobacco = SCREEN_df$drug_tobacco,
                          Stimulants = SCREEN_df$drug_stim, Sex = SCREEN_df$sex,
                          EgoPathology_Endo = as.factor(SCREEN_df$SEPI_tot), Age = SCREEN_df$age)
Endogenous <- glm (EgoPathology_Endo ~ Age+Sex+Psychedelics+Opiates+MDMA+Alcohol+Cannabis+Tobacco+Stimulants, data= mydatawhole, family = "binomial")
summary(Endogenous)
beta(Endogenous)
coefplot.glm(Endogenous, intercept = F, decreasing = T, title = NULL, xlab = "Estimate", color = "black")
#odds ratio
exp(coef(Endogenous))
#confidence intervals
exp(confint(Endogenous))

#summary of results
summ(Endogenous)



# Endogenous ego-pathology Non-psychiatric
mydatanodiag <- data.frame(Psychedelics = SCREEN_df_noPsych$drug_psychedelics, Opiates = SCREEN_df_noPsych$drug_opi,
                          MDMA = SCREEN_df_noPsych$drug_mdma, Alcohol = SCREEN_df_noPsych$drug_alc,
                          Cannabis = SCREEN_df_noPsych$drug_cannabis, Tobacco = SCREEN_df_noPsych$drug_tobacco,
                          Stimulants = SCREEN_df_noPsych$drug_stim, Sex = SCREEN_df_noPsych$sex,
                          EgoPathology_Endo = as.factor(SCREEN_df_noPsych$SEPI_tot), Age = SCREEN_df_noPsych$age)
No.Diagnoses <- glm (EgoPathology_Endo ~ Age+Sex+Psychedelics+Opiates+MDMA+Alcohol+Cannabis+Tobacco+Stimulants, data= mydatanodiag, family = "binomial")
summary(No.Diagnoses)
beta(No.Diagnoses)
coefplot.glm(No.Diagnoses, intercept = F, decreasing = T, title = NULL, xlab = "Estimate", color = "black")
#odds ratio
exp(coef(No.Diagnoses))
#confidence intervals
exp(confint(No.Diagnoses))



#Drug-Induced Ego-Pathology
mydatawhole <- data.frame(Psychedelics = SCREEN_df$drug_psychedelics, Opiates = SCREEN_df$drug_opi,
                          MDMA = SCREEN_df$drug_mdma, Alcohol = SCREEN_df$drug_alc,
                          Cannabis = SCREEN_df$drug_cannabis, Tobacco = SCREEN_df$drug_tobacco,
                          Stimulants = SCREEN_df$drug_stim, Sex = SCREEN_df$sex,
                          EgoPathology_Drug = as.factor(SCREEN_df$SEPI_tot_drug), Age = SCREEN_df$age)
Drug.Induced <- glm (EgoPathology_Drug ~ Age+Sex+Psychedelics+Opiates+MDMA+Alcohol+Cannabis+Tobacco+Stimulants, data= mydatawhole, family = "binomial")
summary(Drug.Induced)
beta(Drug.Induced)
coefplot.glm(Drug.Induced, intercept = F, decreasing = T, title = NULL, xlab = "Estimate", color = "black")
#odds ratio
exp(coef(Drug.Induced))
#confidence intervals
exp(confint(Drug.Induced))


# Drug-induced ego-pathology Non-psychiatric
mydatanodiag <- data.frame(Psychedelics = SCREEN_df_noPsych$drug_psychedelics, Opiates = SCREEN_df_noPsych$drug_opi,
                           MDMA = SCREEN_df_noPsych$drug_mdma, Alcohol = SCREEN_df_noPsych$drug_alc,
                           Cannabis = SCREEN_df_noPsych$drug_cannabis, Tobacco = SCREEN_df_noPsych$drug_tobacco,
                           Stimulants = SCREEN_df_noPsych$drug_stim, Sex = SCREEN_df_noPsych$sex,
                           EgoPathology_Drug = as.factor(SCREEN_df_noPsych$SEPI_tot_drug), Age = SCREEN_df_noPsych$age)
No.Diagnoses <- glm (EgoPathology_Drug ~ Age+Sex+Psychedelics+Opiates+MDMA+Alcohol+Cannabis+Tobacco+Stimulants, data= mydatanodiag, family = "binomial")
summary(No.Diagnoses)
beta(No.Diagnoses)
coefplot.glm(No.Diagnoses, intercept = F, decreasing = T, title = NULL, xlab = "Estimate", color = "black")
#odds ratio
exp(coef(No.Diagnoses))
#confidence intervals
exp(confint(No.Diagnoses))


#create coeffplot of both models
multiplot(Endogenous, Drug.Induced, intercept = F, decreasing = T, title = NULL,
          xlab = "Estimate", numeric = F, zeroColor = "black")


#Contionus Log-regression
#Endogenous ego-pathology FreqProx
mydatawhole <- data.frame(Psychedelics = CONSP_df$PSY_freqprox, Opiates = CONSP_df$OPI_freqprox,
                          MDMA = CONSP_df$MDMA_freqprox, Alcohol = CONSP_df$ALC_freqprox,
                          Cannabis = CONSP_df$CAN_freqprox, Tobacco = CONSP_df$TOB_freqprox,
                          Stimulants = CONSP_df$STIM_freqprox, Sex = CONSP_df$sex,
                          EgoPathology_Endo = as.factor(CONSP_df$SEPI_tot), Age = CONSP_df$age)
All.Subjects <- glm (EgoPathology_Endo ~ Age+Sex+Psychedelics+Opiates+MDMA+Alcohol+Cannabis+Tobacco+Stimulants, data= mydatawhole, family = "binomial")
summary(All.Subjects)
beta(All.Subjects)
coefplot.glm(All.Subjects, intercept = F, decreasing = T, title = NULL, xlab = "Estimate", color = "black")
#odds ratio
exp(coef(All.Subjects))
#confidence intervals
exp(confint(All.Subjects))

#Drug-induced ego-pathology FreqProx
mydatawhole <- data.frame(Psychedelics = CONSP_df$PSY_freqprox, Opiates = CONSP_df$OPI_freqprox,
                          MDMA = CONSP_df$MDMA_freqprox, Alcohol = CONSP_df$ALC_freqprox,
                          Cannabis = CONSP_df$CAN_freqprox, Tobacco = CONSP_df$TOB_freqprox,
                          Stimulants = CONSP_df$STIM_freqprox, Sex = CONSP_df$sex,
                          EgoPathology_Drug = as.factor(CONSP_df$SEPI_tot_drug), Age = CONSP_df$age)
All.Subjects <- glm (EgoPathology_Drug ~ Age+Sex+Psychedelics+Opiates+MDMA+Alcohol+Cannabis+Tobacco+Stimulants, data= mydatawhole, family = "binomial")
summary(All.Subjects)
beta(All.Subjects)
coefplot.glm(All.Subjects, intercept = F, decreasing = T, title = NULL, xlab = "Estimate", color = "black")
#odds ratio
exp(coef(All.Subjects))
#confidence intervals
exp(confint(All.Subjects))


