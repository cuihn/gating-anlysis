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

gate_all_rating <- gate_all_rating %>%
  mutate(ItemEmotion2 = case_when(
    ItemEmotion2 == "1" ~ "Anger",
    ItemEmotion2 == "2" ~ "Fear",
    ItemEmotion2 == "3" ~ "Happiness",
    ItemEmotion2 == "4" ~ "Sadness",
    TRUE ~ ItemEmotion2 
  ))

gate_all_rating <- gate_all_rating %>%
  mutate(ProsodybyGrp = case_when(
    ProsodybyGrp == "native" ~ "L1",
    ProsodybyGrp == "nvv" ~ "Vocalization",
    TRUE ~ ProsodybyGrp
  ))

# Using dplyr to change VOC 'happiness_laughter', 'happiness_pleasure' to 'Happiness" 
gate_all_rating <- gate_all_rating %>%
  mutate(ItemEmotion = case_when(
    ItemEmotion %in% c('Happiness_laughter', 'Happiness_pleasure') ~ 'Happiness',
    TRUE ~ ItemEmotion
  ))

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

# Q1 LMM model for Gfulll, HuScore ---------------------------------------------------------

# Plot Hu scores of GFULL by listeners' L1 and type of utterance (L1 or VOC)
# To plot the second boxplot in a new window, open another quartz window
custom_colors <- c(
  "nv_vocalization.Arabic" = "#90EE90",
  # 
  "utterance.Arabic" = "#ADD8E6",
  # 
  "nv_vocalization.Mandarin"   = "#006400",
  # 
  "utterance.Mandarin"   = "#0000FF"   
)

# Create the boxplot with an interaction fill between two fixed terms
quartz(width=10, height=8)  # Adjust size as needed
box_P <-
  ggplot(Gfull_listener_L1_VOC,
         aes(
           x = ListenerLang,
           y = HuScore_raw,
           fill = interaction(ItemType, ListenerLang)
         )) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  labs(title = "Accuracy as a function of Speech type and L1 background",
       x = "ItemType devided by L1 background of listeners",
       y = "Mean HuScore",
       fill = "Speech Type* L1 Interaction")+
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "darkred", 
               position = position_dodge(width = 0.8))

box_P

# {use data for GFULL condition only}
# {run separately on Hu scores and EIP}
# MODEL 1:	Perceived (MAND, ARAB) x Event (VOC, L1)
# manipulate the data for LMM models to drop L2 and foreign conditions and keep GFULL
Gfull_listener_L1_VOC <- gate_all_rating %>%
  filter(ProsodybyGrp != "foreign", ProsodybyGrp != "L2") %>%
  filter(Gate2 == "GFULL") %>%
  droplevels() %>%
  group_by(ListenerLang, ProsodybyGrp, Gate2)

# first LMM model of Gfull gate-Hu Score

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

# clean EIP data for Gfull gate 
EIP_listener_L1_VOC_Gfull <- gate_EIP_score %>%
  filter(ExpressionTypebyGrp != "foreign", ExpressionTypebyGrp != "L2") %>%
  filter(EIP == "5") %>%
  droplevels() %>%
  group_by(ListenerLang, ExpressionTypebyGrp, EIP)


EIP_listener_L1_VOC_Gfull <- EIP_listener_L1_VOC_Gfull %>%
  mutate(ItemType = case_when(
    ItemType == "nonverbal vocalization" ~ "nv_vocalization",
    TRUE ~ ItemType
  ))


EIP_listener_L1_VOC_Gfull <- EIP_listener_L1_VOC_Gfull %>%
  mutate(ItemEmotion = case_when(
    ItemEmotion == "Pleasure" ~ "Happiness",
    TRUE ~ ItemEmotion
  ))

# second LMM model of Gfull gate-EIP

LMM_Gfull_EIP  <- lmer(
  EIPtime ~ (ListenerLang + ItemType)^2 + ListenerLang:ItemType + 
    (1 | ListenerID) + (1 | ItemEmotion),
  data = EIP_listener_L1_VOC_Gfull
)

library(sjPlot)
# write LMM results to a table in HTML format
tab_model(LMM_Gfull_EIP , show.df = TRUE)

# pairwise compare
# t-test (pairwise comparison) Estimated marginal means ------------------------------------------------------------------
# t-test for direction of statistical significance 
LMM_Gfull_EIP  <- emmeans(LMM_Gfull_EIP, pairwise ~ ItemType|ListenerLang,
                        lmer.df = "satterthwaite", 
                        lmerTest.limit = 876)  # Adjust pbkrtest.limit if needed
summary(LMM_Gfull_EIP)

# Create the boxplot with an interaction fill between two fixed terms
quartz(width=10, height=8)  # Adjust size as needed

EIP_summary <- EIP_listener_L1_VOC_Gfull %>%
  group_by(ListenerLang, ItemType) %>%
  summarize(
    mean_EIP = mean(EIPtime, na.rm = TRUE),
    SE = sd(EIPtime, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )

box_P_EIP_Gfull <- ggplot(EIP_listener_L1_VOC_Gfull,
                          aes(
                            x = ListenerLang,
                            y = EIPtime,
                            fill = interaction(ItemType, ListenerLang)
                          )) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  labs(title = "EIP as a function of Speech type and L1 background",
       x = "ItemType divided by L1 background of listeners",
       y = "Mean EIP",
       fill = "Speech Type* L1 Interaction") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "darkred", 
               position = position_dodge(width = 0.8))

box_P_EIP_Gfull

# Model for 2a ----------------------------------------------------
# MODEL 2 clusters:	Mandarin.Arabix Perceiver/Vocalizations/Speech: Emotion (ANG, FER, SAD, HAP-Amuse, HAP-Pleasure) 
 # x Duration (G200, G400, G500, G600, GFULL)

# clean, fillter data
L1_mandarin_VOC_gate <- gate_all_rating %>%
  filter(ProsodybyGrp != "foreign", ProsodybyGrp != "L2", ProsodybyGrp != "L1", ListenerLang != "Arabic") %>%
  droplevels() %>%
  group_by(ListenerLang, ProsodybyGrp)

#LMM model 
LMM_Hu_mandarin_nvv_gate  <- lmer(
  HuScore_raw ~ (ItemEmotion + Gate2)^2 + ItemEmotion:Gate2 + 
    (1 | Subject),
  data = L1_mandarin_VOC_gate
)

# write LMM results to a table in HTML format
tab_model(LMM_Hu_mandarin_nvv_gate, show.df = TRUE)

# pairwise comparison, t-test for direction of statistical significance 
LMM_Hu_mandarin_nvv_gate <- emmeans(LMM_Hu_mandarin_nvv_gate , pairwise ~ Gate2|ItemEmotion,
                        lmer.df = "satterthwaite", 
                        lmerTest.limit = 622)  # Adjust pbkrtest.limit if needed
summary(LMM_Hu_mandarin_nvv_gate)

# plot LMM2a
box_P_NVV_manda_gate <- ggplot(L1_mandarin_VOC_gate,
                          aes(
                            x = Gate2,
                            y = HuScore_raw,
                            fill = interaction(ItemEmotion, Gate2)
                          )) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  labs(title = "Hu score as a function of NVV type and gate for Mandarin L1",
       x = "NVV Emotion divided by Gates",
       y = "Mean Hu score",
       fill = "NVVEmotion* game") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "darkred", 
               position = position_dodge(width = 0.8))

box_P_NVV_manda_gate


# # Model 2b Arabic NVV vs. Gate ------------------------------------------

# clean, fillter data
L1_Arabic_VOC_gate <- gate_all_rating %>%
  filter(ProsodybyGrp != "foreign", ProsodybyGrp != "L2", ProsodybyGrp != "L1", ListenerLang != "Mandarin") %>%
  droplevels() %>%
  group_by(ListenerLang, ProsodybyGrp)

#LMM model 
LMM_Hu_Arabic_nvv_gate  <- lmer(
  HuScore_raw ~ (ItemEmotion + Gate2)^2 + ItemEmotion:Gate2 + 
    (1 | Subject),
  data = L1_Arabic_VOC_gate
)

# write LMM results to a table in HTML format
tab_model(LMM_Hu_Arabic_nvv_gate, show.df = TRUE)

# t-tests
LMM_Hu_Arabic_nvv_gate <- emmeans(LMM_Hu_Arabic_nvv_gate , pairwise ~ Gate2|ItemEmotion,
                                    lmer.df = "satterthwaite", 
                                    lmerTest.limit = 622)  # Adjust pbkrtest.limit if needed
summary(LMM_Hu_Arabic_nvv_gate)



# # Model 2c Mandarin Speech vs. Gate -------------------------------------

# clean, fillter data
L1_Mandarin_speech_gate <- gate_all_rating %>%
  filter(ProsodybyGrp != "foreign", ProsodybyGrp != "L2", ProsodybyGrp != "Vocalization", ListenerLang != "Arabic") %>%
  droplevels() %>%
  group_by(ListenerLang, ProsodybyGrp)

#LMM model 
LMM_Hu_Mandarin_speech_gate  <- lmer(
  HuScore_raw ~ (ItemEmotion + Gate2)^2 + ItemEmotion:Gate2 + 
    (1 | Subject),
  data = L1_Mandarin_speech_gate 
)

# write LMM results to a table in HTML format
tab_model(LMM_Hu_Mandarin_speech_gate, show.df = TRUE)

# t-tests
LMM_Hu_Mandarin_speech_gate <- emmeans(LMM_Hu_Mandarin_speech_gate , pairwise ~ Gate2|ItemEmotion,
                                  lmer.df = "satterthwaite", 
                                  lmerTest.limit = 622)  # Adjust pbkrtest.limit if needed
summary(LMM_Hu_Mandarin_speech_gate)


# # Model 2d Arabic Speech vs. Gate -------------------------------------
# clean, fillter data
L1_Arabic_speech_gate <- gate_all_rating %>%
  filter(ProsodybyGrp != "foreign", ProsodybyGrp != "L2", ProsodybyGrp != "Vocalization", ListenerLang != "Mandarin") %>%
  droplevels() %>%
  group_by(ListenerLang, ProsodybyGrp)

#LMM model 
LMM_Hu_Arabic_speech_gate  <- lmer(
  HuScore_raw ~ (ItemEmotion + Gate2)^2 + ItemEmotion:Gate2 + 
    (1 | Subject),
  data = L1_Arabic_speech_gate 
)

# write LMM results to a table in HTML format
tab_model(LMM_Hu_Arabic_speech_gate, show.df = TRUE)

# t-tests
LMM_Hu_Arabic_speech_gate <- emmeans(LMM_Hu_Arabic_speech_gate , pairwise ~ Gate2|ItemEmotion,
                                       lmer.df = "satterthwaite", 
                                       lmerTest.limit = 622)  # Adjust pbkrtest.limit if needed
summary(LMM_Hu_Arabic_speech_gate)


# LMMs for Q3s  -----------------------------------------------------------


