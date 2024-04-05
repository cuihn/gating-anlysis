# Analysis of gate data (v2) from an accent-emotion recognition experiment

# Install paks -------------------------------------------------------------
# install.packages("lmerTest")
# library(lsmeans)
# install.packages("devtools")
# install.packages
# install.packages("purrr")
# install.packages('ggpubr')
# install.packages("htmlTable")
# install.packages("knitr")
# library(htmlTable)
# library(knitr)
# library(htmltools)
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
library(rempsyc)
pkgs <- c("flextable", "broom", "report", "effectsize")
library(sjPlot)
library(RColorBrewer)

# set work directory ------------------------------------------------------
# setwd('C://Users//hcui8//Dropbox//Trying//Gate_analysis')
setwd('/Users/hainingcui/Dropbox/Trying/Gate_analysis')

# import Hu Score and EIP data from csv file, and clean data  -------------------------------------------------------------

#import data of all participants
gate_all_rating <- read.csv('CCGating2_ALL DATA_YM_july232018.csv') 
gate_EIP_score <- read.csv('CCGating2_EIP_ALL PARTICIPANTSjuly2018.csv')

# summary(gate_all_rating)

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


gate_EIP_score <- gate_EIP_score %>%
  mutate(ItemType = case_when(
    ItemType == "nonverbal vocalization" ~ "nv_vocalization",
    TRUE ~ ItemType
  ))


gate_EIP_score <- gate_EIP_score %>%
  mutate(ItemEmotion = case_when(
    ItemEmotion == "Pleasure" ~ "Happiness",
    TRUE ~ ItemEmotion
  ))

# summary Hu Score and EIP data ---------------------------------------------------------
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


# Q1a LMM model for Hu Score as a function of L1 vs. Speech type (Gfulll gate only) ---------------------------------------------------------
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

# Q1a t-test (pairwise comparison) Estimated marginal means ------------------------------------------------------------------
emm_Gfull_Hu <- emmeans(LMM_Gfull_Hu , pairwise ~ ItemType|ListenerLang,
                        lmer.df = "satterthwaite", 
                        lmerTest.limit = 450)  # Adjust pbkrtest.limit if needed
summary(emm_Gfull_Hu)

# write out the summary of LMM and pairwise output as a character vector

summary_LMM_Gfull_Hu <- capture.output(summary(emm_Gfull_Hu), anova(LMM_Gfull_Hu))
write.csv(summary_LMM_Gfull_Hu,"summary_summary_LMM_Gfull_Hu.csv")

# Q1a Plot Hu scores of GFULL by listeners' L1 and type of utteran --------
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

# Q1b LMM model for EIP as function of L1 vs. Speech type (Gfull gate only)   -------------------------------

# fillter  EIP data for Gfull gate 
EIP_listener_L1_VOC_Gfull <- gate_EIP_score %>%
  filter(ExpressionTypebyGrp != "foreign", ExpressionTypebyGrp != "L2") %>%
  filter(EIP == "5") %>%
  droplevels() %>%
  group_by(ListenerLang, ExpressionTypebyGrp, EIP)

# LMM model
LMM_Gfull_EIP  <- lmer(
  EIPtime ~ (ListenerLang + ItemType)^2 + ListenerLang:ItemType + 
    (1 | ListenerID) + (1 | ItemEmotion),
  data = EIP_listener_L1_VOC_Gfull
)


# write LMM results to a table in HTML format
tab_model(LMM_Gfull_EIP , show.df = TRUE)


# Q1b t-test (pairwise comparison) Estimated marginal means ------------------------------------------------------------------

LMM_Gfull_EIP  <- emmeans(LMM_Gfull_EIP, pairwise ~ ItemType|ListenerLang,
                        lmer.df = "satterthwaite", 
                        lmerTest.limit = 876)  # Adjust pbkrtest.limit if needed
summary(LMM_Gfull_EIP)

# Q1b plot ----------------------------------------------------------------
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

# Q2a LMM model for Hu Score as a function of NVV vs. Gate by Mandarin L1 listeners ----------------------------------------------------
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


# Q2a t-test for direction of statistical significance --------

LMM_Hu_mandarin_nvv_gate <- emmeans(LMM_Hu_mandarin_nvv_gate , pairwise ~ Gate2|ItemEmotion,
                        lmer.df = "satterthwaite", 
                        lmerTest.limit = 622)  # Adjust pbkrtest.limit if needed
summary(LMM_Hu_mandarin_nvv_gate)


# Q2a plot faced by gates -----------------------------------------------

# Define shades of orange for each gate
gate_colors <- c(
  "G200" = "#feedde",  # lightest orange
  "G400" = "#fdbe85",
  "G500" = "#fd8d3c",
  "G600" = "#e6550d",
  "GFULL" = "#a63603"  # darkest orange
)

# Ensure your Gate2 variable is a factor with the levels in the correct order for the plot
L1_mandarin_VOC_gate$Gate2 <- factor(L1_mandarin_VOC_gate$Gate2, levels = c("G200", "G400", "G500", "G600", "GFULL"))

# Create the plot
box_P_NVV_manda_gate <- ggplot(L1_mandarin_VOC_gate,
                               aes(
                                 x = Gate2,
                                 y = HuScore_raw,
                                 fill = Gate2  # Fill boxes with gate color
                               )) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = gate_colors) +  # Use the defined orange shades
  theme_minimal() +
  labs(title = "Hu score as a function of NVV type and gate for Mandarin L1",
       x = "Gate",
       y = "Mean Hu score",
       fill = "Gate") +
  facet_wrap(~ItemEmotion) +  # Facet by emotion
  theme(strip.text.x = element_text(size = 12, color = "black", face = "bold"))

# Display the plot
box_P_NVV_manda_gate

# Assign names to the colors for the respective emotions
# names(emotion_colors) <- c('Anger', 'Fear', 'Happiness', 'Sadness')
# 
# emotion_colors <- c('Anger' = "#fcb4a5", 'Fear' = "#b3cde3", 
#                     'Happiness' = "#ccebc5", 'Sadness' = "#decbe4")
# 
# # Now create the plot with these colors
# box_P_NVV_manda_gate <- ggplot(L1_mandarin_VOC_gate,
#                                aes(
#                                  x = ItemEmotion,
#                                  y = HuScore_raw,
#                                  fill = ItemEmotion  # Use ItemEmotion for fill
#                                )) +
#   geom_boxplot(position = position_dodge(width = 0.8)) +
#   scale_fill_manual(values = emotion_colors) +
#   theme_minimal() +
#   labs(title = "Hu score as a function of NVV type and gate for Mandarin L1",
#        x = "NVV Emotion",
#        y = "Mean Hu score",
#        fill = "NVV Emotion") +
#   stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "darkred",
#                position = position_dodge(width = 0.8)) +
#   facet_wrap(~factor(Gate2)) +  # Add faceting for 'Gate2'
#   theme(strip.text.x = element_text(size = 12, color = "black", face = "bold"))
# 
# box_P_NVV_manda_gate
# 
# # face by emotion 
# gate_colors <- c("G200" = "#D3D3D3", "G400" = "#BDBDBD",
#                  "G500" = "#A9A9A9", "G600" = "#949494",
#                  "GFULL" = "#696969")
# 
# box_P_NVV_manda_gate <- ggplot(L1_mandarin_VOC_gate,
#                                aes(
#                                  x = ItemEmotion ,
#                                  y = HuScore_raw,
#                                  fill = Gate2  # Use gate for fill to distinguish Acc change
#                                )) +
#   geom_boxplot(position = position_dodge(width = 0.8)) +
#   scale_fill_manual(values = gate_colors) +  # Use custom colors for emotions
#   theme_minimal() +
#   labs(title = "Hu score as a function of NVV type and gate for Mandarin L1",
#        x = "NVV Emotion",
#        y = "Mean Hu score",
#        fill = "Gate2") +
#   stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "darkred",
#                position = position_dodge(width = 0.8)) +
#   theme(strip.text.x = element_text(size = 12, color = "black", face = "bold"))
# 
# box_P_NVV_manda_gate






# Q2b LMM model for Hu Score as a function of NVV vs. Gate by Arabic L1 listeners------------------------------------------

# clean, filler data
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


# Q2b t-tests -----------------------------------------------------------

LMM_Hu_Arabic_nvv_gate <- emmeans(LMM_Hu_Arabic_nvv_gate , pairwise ~ Gate2|ItemEmotion,
                                    lmer.df = "satterthwaite", 
                                    lmerTest.limit = 622)  # Adjust pbkrtest.limit if needed
summary(LMM_Hu_Arabic_nvv_gate)




# Q2c LMM model for Hu score as a function of Utterance vs. Gate by Mandarin L1 listeners -------------------------------------

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


# Q2c model pairwise comparison t-tests ---------------------------------

LMM_Hu_Mandarin_speech_gate <- emmeans(LMM_Hu_Mandarin_speech_gate , pairwise ~ Gate2|ItemEmotion,
                                  lmer.df = "satterthwaite", 
                                  lmerTest.limit = 622)  # Adjust pbkrtest.limit if needed
summary(LMM_Hu_Mandarin_speech_gate)



# Q2d LMM Model for Hu Score as a function of Utterance vs. Gate by Arabic L1 listeners -------------------------------------

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


# Q2d pairwise comparison t-tests ---------------------------------------

LMM_Hu_Arabic_speech_gate <- emmeans(LMM_Hu_Arabic_speech_gate , pairwise ~ Gate2|ItemEmotion,
                                       lmer.df = "satterthwaite", 
                                       lmerTest.limit = 622)  # Adjust pbkrtest.limit if needed
summary(LMM_Hu_Arabic_speech_gate)


summary.Q2d.stats.table <- as.data.frame(summary(LMM_Hu_Arabic_speech_gate))
nice_table(summary.Q2d.stats.table)


# Q3a LMM for EIP of NVV as a function of L1 background vs. ItemEmotion  -----------------------------------------------------------
# Vocalizations: Perceiver (MAND, ARAB) x Emotion (ANG, FER, SAD, HAP-Amuse, HAP-Pleasure)

# clean EIP data for Gfull gate 
EIP_listener_L1_VOC_Only_Gfull <- gate_EIP_score %>%
  filter(ExpressionTypebyGrp != "foreign", ExpressionTypebyGrp != "L2", ItemType != "utterance") %>%
  filter(EIP == "5") %>%
  droplevels() %>%
  group_by(ListenerLang, ExpressionTypebyGrp, EIP)


# LMM model
LMM_Q3a_nvvEmotion_L1_Gfull_EIP  <- lmer(
  EIPtime ~ (ListenerLang + ItemEmotion)^2 + ListenerLang:ItemEmotion + 
    (1 | ListenerID),
  data = EIP_listener_L1_VOC_Only_Gfull
)

# write LMM results to a table in HTML format
tab_model(LMM_Q3a_nvvEmotion_L1_Gfull_EIP, show.df = TRUE)



# Q3a t-tests -------------------------------------------------------------

T_EIP_EIP_L1_Nvv_emo <- emmeans(LMM_Q3a_nvvEmotion_L1_Gfull_EIP , pairwise ~ ItemEmotion|ListenerLang,
                                     lmer.df = "satterthwaite", 
                                     lmerTest.limit = 622)  # Adjust pbkrtest.limit if needed
summary(T_EIP_EIP_L1_Nvv_emo )


summary.Q3a.stats.table <- as.data.frame(summary(T_EIP_EIP_L1_Nvv_emo ))

summary.Q3a.stats.table <- summary.Q3a.stats.table[ , -(1:7)]

nice_table(summary.Q3a.stats.table)



# Q3b LMM for EIP of Speech utterance as a function of L1 background vs. ItemEmotion  -----------------------------------------------------------
# Vocalizations: Perceiver (MAND, ARAB) x Emotion (ANG, FER, SAD, HAP-Amuse, HAP-Pleasure)

# clean EIP data for Gfull gate 
EIP_listener_L1_utter_Only_Gfull <- gate_EIP_score %>%
  filter(ExpressionTypebyGrp != "foreign", ExpressionTypebyGrp != "L2", ItemType != "nv_vocalization") %>%
  filter(EIP == "5") %>%
  droplevels() %>%
  group_by(ListenerLang, ExpressionTypebyGrp, EIP)

# LMM model
LMM_Q3b_UtterEmotion_L1_Gfull_EIP  <- lmer(
  EIPtime ~ (ListenerLang + ItemEmotion)^2 + ListenerLang:ItemEmotion + 
    (1 | ListenerID),
  data = EIP_listener_L1_utter_Only_Gfull
)

# write LMM results to a table in HTML format
tab_model(LMM_Q3b_UtterEmotion_L1_Gfull_EIP, show.df = TRUE)


# Q3b t-tests -------------------------------------------------------------

T_EIP_L1_utter_emo <- emmeans(LMM_Q3b_UtterEmotion_L1_Gfull_EIP, pairwise ~ ItemEmotion|ListenerLang,
                                lmer.df = "satterthwaite", 
                                lmerTest.limit = 622)  # Adjust pbkrtest.limit if needed
summary(T_EIP_L1_utter_emo )

summary.Q3b.stats.table <- as.data.frame(summary(T_EIP_L1_utter_emo))
summary.Q3b.stats.table <- summary.Q3b.stats.table[ , -(1:7)]
nice_table(summary.Q3b.stats.table)


# Q4a LMM model for Hu Score as a function of L1 vs. Speech type (Gfulll gate only) ---------------------------------------------------------

# manipulate the data for LMM models to drop L2 and foreign conditions and keep GFULL
Speech_emotion_gate <- gate_all_rating %>%
  filter(ProsodybyGrp != "Vocalization") %>%
  filter(ListenerLang == "Mandarin") %>%
  droplevels() %>%
  group_by(ProsodybyGrp, Gate2)

# LMM model of gate, emotion, speech-Hu Score

LMM_Q4a_Hu_utter_emo_gate_man  <- lmer(
  HuScore_raw ~ (ProsodybyGrp + ItemEmotion + Gate2)^2 + ProsodybyGrp:ItemEmotion:Gate2 + 
    (1 | Subject),
  data = Speech_emotion_gate
)

# write LMM results to a table in HTML format
tab_model(LMM_Q4a_Hu_utter_emo_gate_man, show.df = TRUE)

# pairwise compare
# Q4a t-test (pairwise comparison) Estimated marginal means ------------------------------------------------------------------
emm_Q4a_Hu <- emmeans(LMM_Q4a_Hu_utter_emo_gate_man, pairwise ~ ItemEmotion|ProsodybyGrp|Gate2,
                        lmer.df = "satterthwaite", 
                        lmerTest.limit = 1437)  # Adjust pbkrtest.limit if needed
summary(emm_Q4a_Hu)

# write out the summary of LMM and pairwise output as a character vector
summary.Q4a.stats.table <- as.data.frame(summary(emm_Q4a_Hu))

summary.Q4a.stats.table <- summary.Q4a.stats.table[ , -(1:7)]

nice_table(summary.Q4a.stats.table)




# Q4b LMM model for Hu Score as a function of L1 vs. Speech type (Gfulll gate only) ---------------------------------------------------------

# manipulate the data for LMM models to drop L2 and foreign conditions and keep GFULL
Speech_emotion_gate_arb <- gate_all_rating %>%
  filter(ProsodybyGrp != "Vocalization") %>%
  filter(ListenerLang == "Arabic") %>%
  droplevels() %>%
  group_by(ProsodybyGrp, Gate2)

# LMM model of gate, emotion, speech-Hu Score

LMM_Q4b_Hu_utter_emo_gate_arb  <- lmer(
  HuScore_raw ~ (ProsodybyGrp + ItemEmotion + Gate2)^2 + ProsodybyGrp:ItemEmotion:Gate2 + 
    (1 | Subject),
  data = Speech_emotion_gate_arb 
)

# write LMM results to a table in HTML format
tab_model(LMM_Q4a_Hu_utter_emo_gate_man, show.df = TRUE)

# pairwise compare
# Q4b t-test (pairwise comparison) Estimated marginal means ------------------------------------------------------------------
emm_Q4b_Hu <- emmeans(LMM_Q4a_Hu_utter_emo_gate_man, pairwise ~ ItemEmotion|ProsodybyGrp|Gate2,
                      lmer.df = "satterthwaite", 
                      lmerTest.limit = 1437)  # Adjust pbkrtest.limit if needed
summary(emm_Q4b_Hu)

# write out the summary of LMM and pairwise output as a character vector

library(broom)

# Convert emmGrid object to a summary table
emm_4bsummary <- summary(emm_Q4b_Hu, infer = c(TRUE, TRUE), level = 0.95)

# Manually create a dataframe if tidy doesn't work directly
stats.4b.table <- data.frame(
  estimate = emm_4bsummary$emmean,
  conf.low = emm_4bsummary$lower.CL,
  conf.high = emm_4bsummary$upper.CL
)


summary.Q4b.stats.table <- as.data.frame(emm_4bsummary)

summary.Q4b.stats.table <- summary.Q4b.stats.table[ , -(1:7)]

nice_table(summary.Q4b.stats.table)




