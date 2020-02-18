library(dplyr)
library(emmeans)
library(huxtable)
library(lme4)
library(lmerTest)
library(Rmisc)

data <- SMI_Pupil_Data_2017

# Preprocessing: remove missing observations & set reference condition
data <- filter(data, Pupil_Size > 0)
data$Condition <- factor(data$Condition, levels = c(1, 2, 3, 4),labels=c("Correct", "Orthographic", "Homophonic", "Unrelated"))

# Build LMM
Pupil_LMM <- lmer(Pupil_Size ~ Condition * Fixation_Num + (1 | Participant_Num) + (1 | Frame_Num), data)
summary(Pupil_LMM)

## Check post-hoc comparisons
Pupil_LMM_Post_Hoc <- emmeans(Pupil_LMM, ~ Condition)
Pupil_LMM_Post_Hoc_Pairs <- pairs(Pupil_LMM_Post_Hoc)
summary(Pupil_LMM_Post_Hoc_Pairs[4:6], adjust="Bon")

# Build LMM table
Pupil_Table <- huxreg("Pupil Diameter" = Pupil_LMM,
                        coefs= c("(Intercept)" = "(Intercept)",
                                 "Ort" = "ConditionOrthographic",
                                 "Hom" = "ConditionHomophonic",
                                 "Unr" = "ConditionUnrelated",
                                 "Fixation Number" = "Fixation_Num" ,
                                 "Fixation Number × Ort" = "ConditionOrthographic:Fixation_Num",
                                 "Fixation Number × Hom" = "ConditionHomophonic:Fixation_Num",
                                 "Fixation Number × Unr" = "ConditionUnrelated:Fixation_Num"),
                        statistics = c("# of Observations" = "nobs", "logLik", "AIC"),
                        number_format = 4,
                        bold_signif = 0.05,
                        error_format = "(SE={std.error}, t={statistic})",
                        error_pos = 'same',
                        stars = NULL)

print(Pupil_Table)