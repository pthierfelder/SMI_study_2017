library(lme4)
library(dplyr)
library(huxtable)
library(Rmisc)

# Pre-processing
## Removing missing observations
data <- filter(SMI_Fixation_Data_2017, FFD_ms > 0)

## Set and label reference condition
data$Condition <- factor(data$Condition, levels = c(4, 1, 2, 3), labels=c("UNR","COR", "ORT", "HOM"))

## Filter observations and standardize predictability values
FFD_GD_Data <- filter(data, FFD_ms < 1000 & GD_ms < 2000) 
FFD_GD_Data$Word_pred_z <- scale(FFD_GD_Data$Word_pred, center = TRUE, scale = TRUE)

TRT_Data <- filter(data, FFD_ms < 1000 & TRT_ms < 3000)
TRT_Data$Word_pred_z <- scale(TRT_Data$Word_pred, center = TRUE, scale = TRUE)

# Build models
## FFD GLMM
FFD_GLMM <- glmer(FFD_ms ~ Condition * Word_pred_z + (1 | Participant) + (1 | Frame_ID), FFD_GD_Data,
                            family = Gamma(link = "identity"),
                            glmerControl(optimizer = "bobyqa"))
## GD GLMM
GD_GLMM <- glmer(GD_ms ~ Condition * Word_pred_z + (1 | Participant) + (1 | Frame_ID), FFD_GD_Data,
                           family = Gamma(link = "identity"),
                           glmerControl(optimizer = "bobyqa"))
## TRT GLMM
TRT_GLMM <- glmer(TRT_ms ~ Condition * Word_pred_z + (1 | Participant) + (1 | Frame_ID), TRT_Data,
                            family = Gamma(link = "identity"),
                            glmerControl(optimizer = "bobyqa"))

# Print table
Fixation_GLMM_Table <- huxreg("FFD" = FFD_GLMM, "GD" = GD_GLMM, "TRT" = TRT_GLMM,
                                coefs= c("(Intercept)" = "(Intercept)",
                                         "Ort" = "ConditionORT",
                                         "Hom" = "ConditionHOM",
                                         "Cor" = "ConditionCOR",
                                         "PV" = "Word_pred_z" ,
                                         "Ort × PV" = "ConditionORT:Word_pred_z",
                                         "Hom × PV" = "ConditionHOM:Word_pred_z",
                                         "Cor × PV" = "ConditionCOR:Word_pred_z"),
                                statistics = c("# of Observations" = "nobs", "logLik", "AIC"),
                                number_format = 2,
                                bold_signif = 0.05,
                                error_format = "(SE={std.error}, t={statistic})",
                                error_pos = 'same',
                                stars = NULL)
print(Fixation_GLMM_Table)
