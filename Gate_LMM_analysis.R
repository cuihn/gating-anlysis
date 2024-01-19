# Analysis of gate data (v2) from an accent-emotion recognition experiment

# Install paks -------------------------------------------------------------
# install.packages("lmerTest")
# library(lsmeans)
# install.packages("devtools")
# install.packages
# install.packages("purrr")
# install.packages('ggpubr')
# Import libs -------------------------------------------------------------
library(lme4)
library(emmeans)
library(lmerTest)
library(tidyverse)
library(ggplot2)
library(arm)
library(forcats)
library(psych)
library(stringr)
library(dplyr)
library(ggpubr)

# set work directory ------------------------------------------------------

setwd('C://Users//hcui8//Dropbox//Trying//Gate_analysis')

#import data of all participants
gate_all_rating <- read.csv('CCGating2_ALL DATA_YM_july232018.csv')
gate_EIP_score <- read.csv('CCGating2_EIP_ALL PARTICIPANTSjuly2018.csv')

summary(gate_all_rating)
# clean data  -------------------------------------------------------------
# replace EIP none data from '?' to 'NA"  and drop NA
gate_EIP_score <- gate_EIP_score %>%
  mutate(EIP = ifelse(EIP == "?", NA, EIP)) %>%
  filter(!is.na(EIP))

# convert EIP to integer
gate_EIP_score$EIP <- as.integer(gate_EIP_score$EIP)

# summary ---------------------------------------------------------
summary(gate_all_rating)

summary(gate_EIP_score)

#convert to factors 
columns_to_converrt_to_factors <- c("ItemEmotion", "ItemType", "ItemLang",
                                    "ListenerLang","ItemType2","ItemEmotion2",
                                   "ProsodybyGrp", "ListenerGender" ,"Group", 
                                    "Gate2", "Language")
gate_all_rating[columns_to_converrt_to_factors] <- lapply(gate_all_rating
                                                          [columns_to_converrt_to_factors], factor)
# Check if each column in the gate_all_rating dataframe is a factor
factors_check <- sapply(gate_all_rating, is.factor)

# Print the results
print(factors_check)

# Function to summarize levels if the column is a factor
summarize_levels <- function(column) {
  if (is.factor(column)) {
    return(table(column))
  } else {
    return(NULL)
  }
}

# Apply the function to each column and store the results
level_summary <- lapply(gate_all_rating, summarize_levels)

# Print the summary
level_summary
#* 112 rows containing non-finite values

# check data normality 
# create density curve
ggdensity(gate_all_rating$HuScore_raw, add = 'mean', fill='grey')

# creat the qqplot
ggqqplot(gate_all_rating$HuScore_raw,add = 'mean')

#shapiro wilk test
shapiro.test(gate_all_rating$HuScore_raw)

#ks test
ks.test(gate_all_rating$HuScore_raw, 'pnorm')

# LMM model 1 ---------------------------------------------------------
# first LMM model for F tests and separated tests for main and interactions effect
# {use data for GFULL condition only}
# {run separately on Hu scores and EIP}
# MODEL 1:	Perceiver (MAND, ARAB) x Event (VOC, L1)

# manipulate the data for LMM models to drop L2 and foreign conditions and keep GFULL
Gfull_listener_L1_VOC <- gate_all_rating %>%
  filter(ProsodybyGrp != "foreign", ProsodybyGrp != "L2") %>%
  filter(Gate2 == "GFULL") %>%
  droplevels() %>%
  group_by(ListenerLang, ProsodybyGrp, Gate2)


# Plot Hu scores of GFULL by listeners' L1 and type of utterance (L1 or VOC)

boxplot(HuScore_raw~ProsodybyGrp, Gfull_listener_L1_VOC)
boxplot(HuScore_raw ~ interaction(ProsodybyGrp, ItemEmotion2), data = Gfull_listener_L1_VOC)



LMM1 <- lmer(HuScore_raw ~ Group*ProsodybyGrp + (1|Subject) + (1|ItemEmotion), data = Gfull_listener_L1_VOC, REML = FALSE)

summary(LMM1)
anova(LMM1)
summary(LMM1)$coefficients

# Get the summary output as a character vector
summary_LMM1 <- capture.output(summary(LMM1), anova(LMM1))
write.csv(summary_LMM1,"summary_LMM1.csv")


# Group1: The estimate for Group1 is -0.06070. This suggests that, holding all other variables constant, being in Group1 (as opposed to the baseline group) is associated with a decrease of 0.06070 units in the dependent variable. However, the p-value (0.138565) is greater than 0.05, indicating that this effect is not statistically significant.
# 
# ProsodybyGrpnvv: The estimate for ProsodybyGrpnvv is 0.09740. This indicates that, holding other variables constant, being in the nvv level of ProsodybyGrp (as opposed to its baseline level) is associated with an increase of 0.09740 units in the dependent variable. The p-value (0.000121) suggests that this effect is statistically significant.
# 
# Group1:ProsodybyGrpnvv: The estimate for the interaction term is 0.11006. This suggests that the effect of being in Group1 on the dependent variable is different for the nvv level of ProsodybyGrp compared to its baseline level. Specifically, the presence of Group1 and nvv together is associated with an increase of 0.11006 units in the dependent variable, compared to not being in Group1 and/or being at the baseline level of ProsodybyGrp. The p-value (0.000622) indicates that this interaction effect is statistically significant.



# t-test (pairwise comparison) Estimated marginal means ------------------------------------------------------------------
# t-test for direction of statistical significance 

# simple effect on level of Truth value collapsed on Memory strength and REG----------------------------------------------

(p_emm1 <- emmeans(LMM1,~TruthValue, pbkrtest.limit = 7808) %>% pairs(adjust="Tukey", side = "="))

## Effect size
(emm1 <- emmeans(LMM1,~TruthValue, pbkrtest.limit = 7808))
(eff1 <- eff_size(emm1, sigma = sigma(LMM1), edf = df.residual(LMM1)))
# Negligible [0,0.2)
# Small [0.2, 0.5)
# Medium [0.5, 0.8)
# Large [0.8, inf)

## Output
(CI_emm1 <- confint(p_emm1))
p_emm1<-as.data.frame(p_emm1)
eff1<-as.data.frame(eff1)
output1<-data.frame(p_emm1$contrast,round(p_emm1$estimate,1),round(CI_emm1$lower.CL,1),
                    round(CI_emm1$upper.CL,1),round(p_emm1$p.value,3),round(eff1$effect.size,2))

names(output1)<-c("Effect","EMMeans","95% CI (Lower)","95% CI (Upper)","p-value","Effect size")
output1
# correct p value
output1$`p-value`<-ifelse(output1$`p-value`<0.001,"<0.001",round(output1$`p-value`,3))

# simple effect on level of Memory strength collapsed on Truth Value and REG ------------------------------------------
(p_emm2 <-emmeans(LMM1,~Familiarity, pbkrtest.limit = 7808) %>% pairs(adjust="Tukey", side = "="))

## Effect size
(emm2<-emmeans(LMM1,~Familiarity, pbkrtest.limit = 7808))
(eff2<-eff_size(emm2, sigma = sigma(LMM1), edf = df.residual(LMM1)))
# Negligible [0,0.2)
# Small [0.2, 0.5)
# Medium [0.5, 0.8)
# Large [0.8, inf)

## Output
(CI_emm2<-confint(p_emm2))
p_emm2<-as.data.frame(p_emm2)
eff2<-as.data.frame(eff2)
output2<-data.frame(p_emm2$contrast,round(p_emm2$estimate,1),round(CI_emm2$lower.CL,1),
                    round(CI_emm2$upper.CL,1),round(p_emm2$p.value,3),round(eff2$effect.size,2))

names(output2)<-c("Effect","EMMeans","95% CI (Lower)","95% CI (Upper)","p-value","Effect size")
output2

output2$`p-value`<-ifelse(output2$`p-value`<0.001,"<0.001",round(output2$`p-value`,3))

# simple effect between Truth Value and REG collapsed on memory strength (familiarity)  --------------------------------------------------

(p_emm3 <-emmeans(LMM1,~TruthValue:REG, pbkrtest.limit = 7808) %>% pairs(adjust="Tukey", side = "="))

## Effect size
(emm3<-emmeans(LMM1,~TruthValue:REG, pbkrtest.limit = 7808))
(eff3<-eff_size(emm3, sigma = sigma(LMM1), edf = df.residual(LMM1)))
# Negligible [0,0.2)
# Small [0.2, 0.5)
# Medium [0.5, 0.8)
# Large [0.8, inf)

## Output
(CI_emm3<-confint(p_emm3))
p_emm3<-as.data.frame(p_emm3)
eff3<-as.data.frame(eff3)
output3<-data.frame(p_emm3$contrast,round(p_emm3$estimate,1),round(CI_emm3$lower.CL,1),
                    round(CI_emm3$upper.CL,1),round(p_emm3$p.value,3),round(eff3$effect.size,2))

names(output3)<-c("Effect","EMMeans","95% CI (Lower)","95% CI (Upper)","p-value","Effect size")
output3

output3$`p-value`<-ifelse(output3$`p-value`<0.001,"<0.001",round(output3$`p-value`,3))

## Effect size
(emm4<-emmeans(LMM1,~Familiarity:REG, pbkrtest.limit = 7808))
(eff4<-eff_size(emm4, sigma = sigma(LMM1), edf = df.residual(LMM1)))
# Negligible [0,0.2)
# Small [0.2, 0.5)
# Medium [0.5, 0.8)
# Large [0.8, inf)

## Output
(CI_emm4<-confint(p_emm4))
p_emm4<-as.data.frame(p_emm4)
eff4<-as.data.frame(eff4)
output4<-data.frame(p_emm4$contrast,round(p_emm4$estimate,1),round(CI_emm4$lower.CL,1),
                    round(CI_emm4$upper.CL,1),round(p_emm4$p.value,3),round(eff4$effect.size,2))

names(output4)<-c("Effect","EMMeans","95% CI (Lower)","95% CI (Upper)","p-value","Effect size")
output4

output4$`p-value`<-ifelse(output4$`p-value`<0.001,"<0.001",round(output4$`p-value`,3))


# simple effect between Memory Strength, Truth Value, and REG  --------------------------------------------------

(p_emm5 <-emmeans(LMM1,~Familiarity:TruthValue:REG, pbkrtest.limit = 7808) %>% pairs(adjust="Tukey", side = "="))

## Effect size
(emm5<-emmeans(LMM1,~Familiarity:TruthValue:REG, pbkrtest.limit = 7808)) # nolint: infix_spaces_linter.
(eff5<-eff_size(emm5, sigma = sigma(LMM1), edf = df.residual(LMM1)))
# Negligible [0,0.2)
# Small [0.2, 0.5)
# Medium [0.5, 0.8)
# Large [0.8, inf)

## Output
(CI_emm5<-confint(p_emm5))
p_emm5<-as.data.frame(p_emm5)
eff5<-as.data.frame(eff5)
output5<-data.frame(p_emm5$contrast,round(p_emm5$estimate,1),round(CI_emm5$lower.CL,1),
                    round(CI_emm5$upper.CL,1),round(p_emm5$p.value,3),round(eff5$effect.size,2))

names(output5)<-c("Effect","EMMeans","95% CI (Lower)","95% CI (Upper)","p-value","Effect size")
output5

output5$`p-value`<-ifelse(output5$`p-value`<0.001,"<0.001",round(output5$`p-value`,3))

# simple effect for testing the interaction between Memory Strength and Truth Value collapsed on REG  --------------------------------------------------

(p_emm6 <-emmeans(LMM1,~Familiarity:TruthValue, pbkrtest.limit = 7808) %>% pairs(adjust="Tukey", side = "="))

## Effect size
(emm6<-emmeans(LMM1,~Familiarity:TruthValue, pbkrtest.limit = 7808)) 
(eff6<-eff_size(emm6, sigma = sigma(LMM1), edf = df.residual(LMM1)))
# Negligible [0,0.2)
# Small [0.2, 0.5)
# Medium [0.5, 0.8)
# Large [0.8, inf)

## Output
(CI_emm6<-confint(p_emm6))
p_emm6<-as.data.frame(p_emm6)
eff6<-as.data.frame(eff6)
output6<-data.frame(p_emm6$contrast,round(p_emm6$estimate,1),round(CI_emm6$lower.CL,1),
                    round(CI_emm6$upper.CL,1),round(p_emm6$p.value,3),round(eff6$effect.size,2))

names(output6)<-c("Effect","EMMeans","95% CI (Lower)","95% CI (Upper)","p-value","Effect size")
output6

output6$`p-value`<-ifelse(output6$`p-value`<0.001,"<0.001",round(output6$`p-value`,3))


# write out post hoc results in one csv -----------------------------------

write.csv(rbind(output1,output2,output3,output4,output5,output6), "postHoc_N400_results_P_effectSize.csv")
write.csv(rbind(p_emm1,p_emm2,p_emm3,p_emm4,p_emm5,p_emm6), "postHoc_N400_t-test_results.csv")





