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

# setwd('C://Users//hcui8//Dropbox//Trying//Gate_analysis')
setwd('/Users/hainingcui/Dropbox/Trying/Gate_analysis')

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

# LMM model for Gfulll, HuScore ---------------------------------------------------------

# Plot Hu scores of GFULL by listeners' L1 and type of utterance (L1 or VOC)
# To plot the second boxplot in a new window, open another quartz window
quartz(width=10, height=8)  # Adjust size as needed
boxplot(HuScore_raw ~ interaction(ProsodybyGrp, ItemEmotion2), data = Gfull_listener_L1_VOC)
# {use data for GFULL condition only}
# {run separately on Hu scores and EIP}
# MODEL 1:	Perceived (MAND, ARAB) x Event (VOC, L1)
# manipulate the data for LMM models to drop L2 and foreign conditions and keep GFULL
Gfull_listener_L1_VOC <- gate_all_rating %>%
  filter(ProsodybyGrp != "foreign", ProsodybyGrp != "L2") %>%
  filter(Gate2 == "GFULL") %>%
  droplevels() %>%
  group_by(ListenerLang, ProsodybyGrp, Gate2)

Gfull_listener_L1_VOC <- Gfull_listener_L1_VOC %>%
  mutate(ItemEmotion2 = case_when(
    ItemEmotion2 == "1" ~ "Anger",
    ItemEmotion2 == "2" ~ "Fear",
    ItemEmotion2 == "3" ~ "Happiness",
    ItemEmotion2 == "4" ~ "Sadness",
    TRUE ~ ItemEmotion2 
    ))

Gfull_listener_L1_VOC <- Gfull_listener_L1_VOC %>%
  mutate(ProsodybyGrp = case_when(
    ProsodybyGrp == "native" ~ "L1",
    ProsodybyGrp == "nvv" ~ "Vocalization",
    TRUE ~ ProsodybyGrp
  ))

# Using dplyr to change VOC 'happiness_laughter', 'happiness_pleasure' to 'Happiness" 
Gfull_listener_L1_VOC <- Gfull_listener_L1_VOC %>%
  mutate(ItemEmotion = case_when(
    ItemEmotion %in% c('Happiness_laughter', 'Happiness_pleasure') ~ 'Happiness',
    TRUE ~ ItemEmotion
  ))

# first LMM model for F tests and separated tests for main and interactions effect

LMM_Gfull_Hu  <- lmer(
  HuScore_raw ~ (ListenerLang + ItemType)^2 + ListenerLang:ItemType + 
    (1 | Subject) + (1 | ItemEmotion),
  data = Gfull_listener_L1_VOC
)

library(sjPlot)
# write LMM results to a table in HTML format
tab_model(LMM_Gfull_Hu , show.df = TRUE)

# pairwise compare
# t-test (pairwise comparison) Estimated marginal means ------------------------------------------------------------------
# t-test for direction of statistical significance 
emm_Gfull_Hu <- emmeans(LMM_Gfull_Hu , pairwise ~ ItemType|ListenerLang,
                  lmer.df = "satterthwaite", 
                  lmerTest.limit = 450)  # Adjust pbkrtest.limit if needed
summary(emm_Gfull_Hu)

# write out the summary of LMM and pairwise output as a character vector
summary_LMM_Gfull_Hu <- capture.output(summary(emm_Gfull_Hu), anova(LMM_Gfull_Hu))
write.csv(summary_LMM_Gfull_Hu,"summary_summary_LMM_Gfull_Hu.csv")








