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
# install.packages(ggpattern)
# install.packages("papaja")
# install.packages("formatR")
# Import libs -------------------------------------------------------------
library(rmarkdown)
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
library(knitr)
library(sjPlot)
library(ggpattern)
library(apaTables)
library(papaja)
library(formatR)
# set work directory ------------------------------------------------------
setwd('C://Users//hcui8//Dropbox//Trying//Gate_analysis')
# setwd('/Users/hainingcui/Dropbox/Trying/Gate_analysis')

tidy_source("C://Users//hcui8//Dropbox//Trying//Gate_analysis//gating-anlysis//Gate_LMM_analysis.R")

# clean Hu Score  -------------------------------------------------------------
#import data of all participants
gate_HuScore <- read.csv('combined_HuScore.csv') 

#convert to factors for HuScore using lapply function
columns_to_converrt_to_factors <-
  c("ItemEmotion",
    "ItemType",
    "ItemLang",
    "Gate",
    "ListenerLang",
    "ItemToListener")

gate_HuScore[columns_to_converrt_to_factors] <- lapply(gate_HuScore[columns_to_converrt_to_factors], factor)

# drop Happiness_please level
gate_HuScore_drop_pleasure <- gate_HuScore %>%
  filter(ItemEmotion != "Happiness_pleasure") %>%
  mutate(ItemEmotion = droplevels(ItemEmotion))

# rename label Happiness_laughter to happiness
gate_HuScore_drop_pleasure <- gate_HuScore_drop_pleasure %>%
  mutate(ItemEmotion = case_when(
    ItemEmotion == "Happiness_laughter" ~ "Happiness",
    TRUE ~ ItemEmotion
  ))

# Check if each column in the gate_HuScore dataframe is a factor
factors_check <- sapply(gate_HuScore, is.factor)
# Print the results
print(factors_check)
print(str(gate_HuScore))

# Clean EIP value ---------------------------------------------------------------------
# import EIP data (Yondu created dataframe in excel)
gate_EIP <- read.csv('CCGating2_EIP_ALL PARTICIPANTSjuly2018.csv')

# rename "ItemToListener" to "ItemToListener"
gate_EIP <- gate_EIP %>%
  rename(ItemToListener = ExpressionTypebyGrp )

# gate EIP original
# Rename level names
gate_EIP_ori <- gate_EIP %>%
  mutate(ItemToListener = case_when(
    ItemToListener == "foreign" ~ "Foreign",
    ItemToListener == "nvv" ~ "Vocalization",
    ItemToListener == "native" ~ "L1",
    TRUE ~ ItemToListener 
  )) %>%
  # mutate(ItemEmotion = case_when(
  #   ItemEmotion == "Laughter" ~ "Happiness",
  #   TRUE ~ ItemEmotion
  # )) %>%
  mutate(ItemType = case_when(
    ItemType == "nonverbal vocalization" ~ "vocalization",
    TRUE ~ ItemType
  )) %>%
  mutate(EIP = ifelse(EIP == "?", NA, EIP)) %>%
  filter(!is.na(EIP))

# Rename level names
gate_EIP <- gate_EIP %>%
  mutate(ItemToListener = case_when(
    ItemToListener == "foreign" ~ "Foreign",
    ItemToListener == "nvv" ~ "Vocalization",
    ItemToListener == "native" ~ "L1",
    TRUE ~ ItemToListener 
  )) %>%
  mutate(ItemEmotion = case_when(
    ItemEmotion == "Laughter" ~ "Happiness",
    TRUE ~ ItemEmotion
  )) %>%
  mutate(ItemType = case_when(
    ItemType == "nonverbal vocalization" ~ "vocalization",
    TRUE ~ ItemType
  )) %>%
  mutate(EIP = ifelse(EIP == "?", NA, EIP)) %>%
  filter(!is.na(EIP))

# convert EIP to integer
gate_EIP$EIP <- as.integer(gate_EIP$EIP)

# convert variables to factors
col_EIP_factors <- c(
  "ListenerLang",
  "ItemToListener",
  "ItemEmotion"
)

gate_EIP[col_EIP_factors] <- lapply(gate_EIP[col_EIP_factors], factor)


# drop Pleasure level
gate_EIP_drop_pleasure <- gate_EIP %>%
  filter(ItemEmotion != "Pleasure") %>%
  droplevels()

print(str(gate_EIP))

# summarize Hu Score and EIP data ---------------------------------------------------------
summary(gate_HuScore)

#summarize Discrib Stats
summary_gate_HuScore <- gate_HuScore %>%
  group_by(ListenerLang, ItemToListener, Gate, ItemEmotion) %>%
  summarise(
    Mean = mean(HuScore, na.rm = TRUE)
  ) %>%
  ungroup()  # Remove grouping

summary_gate_HuScore_wide <- summary_gate_HuScore %>%
  group_by(ListenerLang, ItemToListener, Gate) %>%
  droplevels() %>%
  pivot_wider(names_from = Gate, values_from = Mean)
  

# print a HTML table
nice_table(summary_gate_HuScore_wide)

# summary EIP data
summary(gate_EIP)

# summarize EIP
summary_gate_EIP <- gate_EIP_ori %>%
  group_by(ListenerLang, ItemToListener, ItemEmotion) %>%
  summarise(
    EIP_Mean = mean(EIPtime, na.rm = TRUE),
    EIP_Median = median(EIPtime, na.rm = TRUE)
  ) %>%
  ungroup()  # Remove grouping

# print out a HTML table
nice_table(summary_gate_EIP)

summary_gate_EIP_NVV <- summary_gate_EIP %>%
  filter(ItemToListener == "Vocalization") %>%
  group_by(ListenerLang, ItemEmotion) %>%
  droplevels() 

# print out a HTML table
nice_table(summary_gate_EIP_NVV)

# check data normality  -------------------------------------------------

# # create density curve
# ggdensity(gate_HuScore$HuScore, add = 'mean', fill='grey')
# 
# # creat the qqplot
# ggqqplot(gate_HuScore$HuScore,add = 'mean')
# 
# #shapiro wilk test
# shapiro.test(gate_HuScore$HuScore)
# 
# #ks test
# ks.test(gate_HuScore$HuScore, 'pnorm')

##### (done)

# #### ------------------------------------------------------------------------- (done)


# model 1 --------------------------------------------------------------
# Q1a LMM model for Hu Score as a function of L1 vs. Speech type (Gfulll gate only) ---------------------------------------------------------
# {use data of GFULL condition only}, {run separately on Hu scores and EIP}
# filter data for LMM Q1a, manipulate the data for LMM models to drop L2 and Foreign conditions and keep GFULL

df_Q1a_Gfull_L1_NVV <- gate_HuScore_drop_pleasure %>%
  filter(ItemToListener != "Foreign", ItemToListener != "L2") %>%
  filter(Gate == "GFULL") %>%
  droplevels() %>%
  group_by(ListenerLang, ItemToListener, Gate) %>%
  mutate(ItemType = case_when(ItemType == "Utterance" ~ "Speech",
                              TRUE ~ ItemType)) %>%
  mutate(ListenerLang = case_when(ListenerLang == "Mandarin" ~ "Chinese",
                                  TRUE ~ ListenerLang))

# add one more variable for ItemEmotion (VOC happiness combined in to happiness)
# df_Q1a_Gfull_L1_NVV$ItemEmotion2 <- df_Q1a_Gfull_L1_NVV$ItemEmotion
# levels(df_Q1a_Gfull_L1_NVV$ItemEmotion2)[levels(df_Q1a_Gfull_L1_NVV$ItemEmotion2) == "Happiness_laughter"] <- "Happiness"
# levels(df_Q1a_Gfull_L1_NVV$ItemEmotion2)[levels(df_Q1a_Gfull_L1_NVV$ItemEmotion2) == "Happiness_pleasure"] <- "Happiness"

# convert to factors
#df_Q1a_Gfull_L1_NVV$ItemEmotion2 <- factor(df_Q1a_Gfull_L1_NVV$ItemEmotion2)
df_Q1a_Gfull_L1_NVV$ItemEmotion <- factor(df_Q1a_Gfull_L1_NVV$ItemEmotion)
df_Q1a_Gfull_L1_NVV$ListenerLang <- factor(df_Q1a_Gfull_L1_NVV$ListenerLang)
df_Q1a_Gfull_L1_NVV$ItemType <- factor(df_Q1a_Gfull_L1_NVV$ItemType)


# first LMM model of Gfull gate-Hu Score
# MODEL 1:	Perceived (MAND, ARAB) x Event (VOC, L1) as fixed factors
# Subject and item emotion as random factors
LMM_Q1a_Gfull_Hu_L1_NVV  <- lmer(
  HuScore ~ (ListenerLang + ItemType)^2  + 
    (1 | Subject) + (1 | ItemEmotion), REML = FALSE,
  data = df_Q1a_Gfull_L1_NVV
)

summary(LMM_Q1a_Gfull_Hu_L1_NVV)

# write LMM_Q1a results to a table in HTML format
tab_model(LMM_Q1a_Gfull_Hu_L1_NVV, show.df = TRUE)

# f-test
print(anova(LMM_Q1a_Gfull_Hu_L1_NVV))

# Q1a t-test (pairwise comparison) Estimated marginal means ------------------------------------------------------------------

Q1a_emm_Gfull_Hu <- emmeans(LMM_Q1a_Gfull_Hu_L1_NVV, revpairwise ~ ItemType|ListenerLang, adjust="tukey",
                            lmer.df = "satterthwaite", 
                            lmerTest.limit = 450)  # Adjust pbkrtest.limit if needed

summary(Q1a_emm_Gfull_Hu)
summary.Q1a.stats.table <- as.data.frame(summary(Q1a_emm_Gfull_Hu))
contrasts_df_1a <- summary.Q1a.stats.table$contrasts
nice_table(contrasts_df_1a)

# Q1a bar plot Hu Score -------------------------------------------------

#summary data
summary_df_Q1a <- df_Q1a_Gfull_L1_NVV %>%
  group_by(ListenerLang, ItemType) %>%
  summarise(
    Mean_hu = mean(HuScore, na.rm = TRUE),
    SD = sd(HuScore, na.rm = TRUE)
  ) %>%
  ungroup()  

print(summary_df_Q1a)

summary_df_Q1a <- summary_df_Q1a %>%
  mutate(
    lower = Mean_hu - SD,
    upper = Mean_hu + SD
  )

print(summary_df_Q1a)

nice_table(summary_df_Q1a)

Q1a_custom_colors <- c(
  "Vocalization.Arabic" = "#8FBC8F",
  "Speech.Arabic" = "#2E8B57",
  "Vocalization.Chinese" = "#fcb4a5",
  "Speech.Chinese" = "#e6550d"
)

# Function to calculate standard error
standard_error <- function(x) {
  sd(x, na.rm = TRUE) / sqrt(length(na.omit(x)))
}

bar_1a_EIP_Gfull <-
  ggplot(summary_df_Q1a,
         aes(
           x = interaction(ItemType, ListenerLang),
           y = Mean_hu,
           fill = interaction(ItemType, ListenerLang)
         )) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5), width = 0.5) +
  geom_col(position = position_dodge(width = 0.5), width = 0.3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.5), width = 0.25) +
  scale_fill_manual(values = Q1a_custom_colors) +
  theme_minimal() +
  labs(title = "Figure Q1a. Mean HuScore as a function of Event type and L1 background",
       x = "",
       y = "Mean HuScore",
       fill = "") +
  theme(
    strip.text.x = element_text(size = 11, color = "black", face = "bold"),
    axis.text.x = element_text(hjust = 0.5),  # Optional: rotate x-axis labels if needed
    panel.spacing = unit(1, "lines"),  # Adjust spacing between panels if necessary
    axis.ticks.length.x = unit(1, "pt"),  # Adjust length of ticks if needed
    axis.text.x.bottom = element_text(margin = margin(1, 0, 1, 0, "lines")), # Adjust margin around text if needed
    legend.position = "top",
    plot.title = element_text(hjust = 0.5))  # Center the title

print(bar_1a_EIP_Gfull)

ggsave("bar_1a_EIP_Gfull.png", plot = bar_1a_EIP_Gfull, width = 10, height = 8, units = "in")

# Q1a Violin-plot Q1a vocalization (Vocalization-Emotion ) -------------------------------------------------
# # filter data (VOC with all emotion type)
# df_Q1a_Gfull_VOC <- gate_HuScore %>%
#   filter(ItemToListener != "Foreign", ItemToListener != "L2", ItemToListener != "L1") %>%
#   filter(Gate == "GFULL") %>%
#   droplevels() %>%
#   group_by(ListenerLang, ItemToListener, Gate)
# 
# 
# emotion_colors <- c(
#   Anger = "#E63946",  # A vivid, slightly desaturated red
#   Fear = "#457B9D",  # A soft, desaturated blue
#   #Happiness_laughter = "#F4A261", 
#   Happiness = "#F4A261",# A warm, muted orange
#   #Happiness_pleasure = "#2A9D8F",  # A calming, desaturated teal
#   Sadness = "#9C89B8"  # A gentle, muted purple
# )
# 
# # Function to lighten or darken colors
# lighten_color <- function(color, amount = 0.4) adjustcolor(color, amount)
# darken_color <- function(color, amount = 0.8) adjustcolor(color, amount)
# 
# # Generate variations for each language
# emotion_colors_arabic <- setNames(sapply(emotion_colors, lighten_color), paste(names(emotion_colors), "Arabic", sep = "."))
# emotion_colors_mandarin <- setNames(sapply(emotion_colors, darken_color), paste(names(emotion_colors), "Mandarin", sep = "."))
# 
# # Combine into one vector for use in ggplot
# all_colors <- c(emotion_colors_arabic, emotion_colors_mandarin)
# 
# #plot box
# violin_Q1a_VOC <-
#   ggplot(df_Q1a_Gfull_VOC,
#          aes(
#            x = ListenerLang,
#            y = HuScore,
#            fill = interaction(ItemEmotion, ListenerLang)
#          )) +
#   geom_violin(position = position_dodge(width = 0.8), color = NA, trim = FALSE) +
#   scale_fill_manual(values = all_colors) +
#   theme_minimal() +
#   labs(title = "HuScore(Gfull) as a function of vocalization emotion and L1 background",
#        x = "",
#        y = "HuScore",
#        fill = "Speech Type divided by L1 background")+
#   facet_wrap(~ItemEmotion, nrow=1) +
#   stat_summary(fun = mean, geom = "point", shape = 20, size = 5, color = "darkred",
#                position = position_dodge(width = 0.3)) +
#   theme(
#     strip.text.x = element_text(size = 11, color = "black", face = "bold"),
#     axis.text.x = element_text(angle = 25, hjust = 1,color = "black", face = "bold"),  # Optional: rotate x-axis labels if needed
#     panel.spacing = unit(1, "lines"),  # Adjust spacing between panels if necessary
#     axis.ticks.length.x = unit(4, "pt"),  # Adjust length of ticks if needed
#     axis.text.x.bottom = element_text(margin = margin(1, 0, 1, 0, "lines")), # Adjust margin around text if needed
#     legend.position = "none" ,
#     plot.title = element_text(hjust = 0.5)  # Center the title
#   ) 
# 
# print(violin_Q1a_VOC)
# 
# Q1a violin-plot Q1a vocalization (L1-Emotion ) -------------------------------------------------

# filter data (L1 with all emotion type)
df_Q1a_Gfull_L1 <- gate_HuScore %>%
  filter(ItemToListener != "Foreign", ItemToListener != "L2", ItemToListener != "Vocalization") %>%
  filter(Gate == "GFULL") %>%
  droplevels() %>%
  group_by(ListenerLang, ItemToListener, Gate)

#set color
emotion_colors_L1 <- c(
  Anger = "#E63946",  # A vivid, slightly desaturated red
  Fear = "#457B9D",  # A soft, desaturated blue
  Happiness = "#F4A261",  # A warm, muted orange
  Sadness = "#9C89B8"  # A gentle, muted purple
)

# Function to lighten or darken colors
lighten_color <- function(color, amount = 0.4) adjustcolor(color, amount)
darken_color <- function(color, amount = 1.4) adjustcolor(color, amount)

# Generate variations for each language
emotion_colors_arabic_L1 <- setNames(sapply(emotion_colors_L1, lighten_color), paste(names(emotion_colors_L1), "Arabic", sep = "."))
emotion_colors_mandarin_L1 <- setNames(sapply(emotion_colors_L1, darken_color), paste(names(emotion_colors_L1), "Mandarin", sep = "."))

# Combine into one vector for use in ggplot
all_colors_L1 <- c(emotion_colors_arabic_L1, emotion_colors_mandarin_L1)

#plot box
violin_Q1a_L1 <-
  ggplot(df_Q1a_Gfull_L1,
         aes(
           x = ListenerLang,
           y = HuScore,
           fill = interaction(ItemEmotion, ListenerLang)
         )) +
  geom_violin(position = position_dodge(width = 0.8), color = NA, trim = FALSE) +
  scale_fill_manual(values = all_colors_L1) +
  theme_minimal() +
  labs(title = "HuScore(Gfull) as a function of utterance emotion and L1 background",
       x = "",
       y = "HuScore",
       fill = "Speech Type divided by L1 background")+
  facet_wrap(~ItemEmotion, nrow=1) +
  geom_violin_pattern(
    pattern = "stripe",     # Choose a pattern type, e.g., "stripe", "crosshatch", etc.
    pattern_density = 0.1,  # Adjust the density of the pattern lines
    pattern_spacing = 0.08, # Spacing between pattern lines
    pattern_angle = 45,     # Angle of the pattern lines
    pattern_color = "white" , #Color of the pattern
    trim = FALSE ,
    color = NA
  ) +
  theme(
    strip.text.x = element_text(size = 11, color = "black", face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black", face = "bold"),  # Optional: rotate x-axis labels if needed
    panel.spacing = unit(1, "lines"),  # Adjust spacing between panels if necessary
    axis.ticks.length.x = unit(4, "pt"),  # Adjust length of ticks if needed
    axis.text.x.bottom = element_text(margin = margin(1, 0, 1, 0, "lines")), # Adjust margin around text if needed
    legend.position = "none" ,
    plot.title = element_text(hjust = 0.5)  # Center the title
  ) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 5, color = "darkred",
               position = position_dodge(width = 0.3))

print(violin_Q1a_L1)


# #### -------------------------------------------------------------------- (done)


# #### -------------------------------------------------------------------- (done)


# model 2 ---------------------------------------------------------------
# Q1b LMM model for EIP as function of L1 vs. Speech type (Gfull gate only)   -------------------------------

# filler  EIP data for Gfull gate 

df_Q1b_EIP_listener_L1_VOC_Gfull <- gate_EIP_drop_pleasure %>%
  filter(ItemToListener != "Foreign", ItemToListener != "L2") %>%
  droplevels() %>%
  group_by(ListenerLang, ItemToListener, EIP) %>%
  mutate(ItemType = case_when(ItemType == "utterance" ~ 'Speech',
                              ItemType == "vocalization" ~ "Vocalization",
                              TRUE ~ ItemType)) %>%
  mutate(ListenerLang = case_when(ListenerLang == "Mandarin" ~ "Chinese",
                                  TRUE ~ ListenerLang))


# new EIP analysis 20240521
df_Q1b_EIP_median <- df_Q1b_EIP_listener_L1_VOC_Gfull %>%
  group_by(ListenerLang, ItemType, ItemEmotion,ListenerID, ItemLang, ItemToListener) %>%
  summarize(
    median_EIP = median(EIPtime, na.rm = TRUE),
    .groups = 'drop'
  )
  
print(df_Q1b_EIP_median)

# new LMM model using median value of EIP 
LMM_Median_EIP_L1_VOC  <- lmer(
  median_EIP ~ (ListenerLang + ItemType)^2 + 
    (1 | ListenerID) + (1 | ItemEmotion), REML = FALSE,
    data = df_Q1b_EIP_median 
)

# write LMM results to a table in HTML format
tab_model(LMM_Median_EIP_L1_VOC, show.df = TRUE)

# f-test
print(anova(LMM_Median_EIP_L1_VOC))

# Q1b t-test (pairwise comparison) Estimated marginal means ------------------------------------------------------------------
Q1b_emm_EIP <- emmeans(LMM_Median_EIP_L1_VOC, revpairwise ~ ItemType|ListenerLang, adjust="tukey",
                        lmer.df = "satterthwaite", 
                        lmerTest.limit = 5283)  # Adjust pbkrtest.limit if needed
summary(Q1b_emm_EIP)

summary.Q1b.stats.table <- as.data.frame(summary(Q1b_emm_EIP))

contrasts_df_1b <- summary.Q1b.stats.table$contrasts

nice_table(contrasts_df_1b ) # sig for both directions

# Q1b bar plot EIP (SE, sig contrasts added) ----------------------------------------------------------------

# Create the boxplot with an interaction fill between two fixed terms
custom_colors_1b <- c(
  "Vocalization.Arabic" = "#679267",  # Darker than #8FBC8F
  "Speech.Arabic" = "#20693D",       # Darker than #2E8B57
  "Vocalization.Chinese" = "#e9967a",  # Darker than #fcb4a5
  "Speech.Chinese" = "#cc4c02"        # Darker than #e6550d
)

# Function to calculate standard error
standard_error <- function(x) {
  sd(x, na.rm = TRUE) / sqrt(length(na.omit(x)))
}

# Summary data Q1b median and get se value
Q1b_EIP_summary <- df_Q1b_EIP_median  %>%
  group_by(ListenerLang, ItemType) %>%
  summarize(
    mean_median = mean(median_EIP, na.rm = TRUE),
    se_EIP = standard_error(median_EIP),
    .groups = 'drop'
  )

Q1b_EIP_summary <- Q1b_EIP_summary %>%
  mutate(
    lower = mean_median - se_EIP,
    upper = mean_median + se_EIP
  )

print(Q1b_EIP_summary)

nice_table(Q1b_EIP_summary)

#plot Q1b
bar_1b_EIP_Gfull <-
  ggplot(Q1b_EIP_summary,
         aes(
           x = interaction(ItemType, ListenerLang),
           y = mean_median,
           fill = interaction(ItemType, ListenerLang)
         )) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5), width = 0.5) +
  geom_col(position = position_dodge(width = 0.5), width = 0.3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.5), width = 0.25) +
  scale_fill_manual(values = custom_colors_1b) +
  theme_minimal() +
  labs(title = "Figure Q1b. Median EIP as a function of Event type and L1 background",
       x = "",
       y = "Median EIP",
       fill = "") +
  theme(
    strip.text.x = element_text(size = 11, color = "black", face = "bold"),
    axis.text.x = element_text(hjust = 0.5),  # Optional: rotate x-axis labels if needed
    panel.spacing = unit(1, "lines"),  # Adjust spacing between panels if necessary
    axis.ticks.length.x = unit(1, "pt"),  # Adjust length of ticks if needed
    axis.text.x.bottom = element_text(margin = margin(1, 0, 1, 0, "lines")), # Adjust margin around text if needed
    legend.position = "top",
    plot.title = element_text(hjust = 0.5)) + # Center the title
    coord_flip()


#quartz(width=10, height=8)  # Adjust size as needed
bar_1b_EIP_Gfull

bar_1b_EIP_Gfull +
  geom_line(data=tibble(x=c(1.25, 1.75), y=c(900, 900)), 
            aes(x=x, y=y),
            inherit.aes = FALSE) +
  geom_text(data=tibble(x=1.5, y=910), 
            aes(x=x, y=y, label="*"), size=6,
            inherit.aes = FALSE) +
  geom_line(data=tibble(x=c(3.25, 3.75), y=c(650, 650)), 
            aes(x=x, y=y),
            inherit.aes = FALSE) +
  geom_text(data=tibble(x=3.5, y=660), 
            aes(x=x, y=y, label="*"),size=6,
            inherit.aes = FALSE)


ggsave("Q1b_EIP.png", plot = bar_1b_EIP_Gfull, width = 10, height = 8, units = "in")


# #### -------------------------------------------------------------------- (done)


# model 3 -------------------------------------------------------------
# Q2a LMM model for HuScore as a function of NVV vs. Gate by Mandarin L1 listeners ----------------------------------------------------
#  Emotion (ANG, FER, SAD, HAP-Amuse, HAP-Pleasure) vs. Duration (G200, G400, G500, G600, GFULL)

# clean, filter data
df_Q2a_L1_mandarin_VOC_gate  <- gate_HuScore %>%
  filter(ItemToListener != "Foreign", 
         ItemToListener != "L2", 
         ItemToListener != "L1", 
         ListenerLang != "Arabic") %>%
  droplevels() %>%
  group_by(ListenerLang, ItemToListener)

summary_L1_mandarin_VOC_gate <- df_Q2a_L1_mandarin_VOC_gate %>%
  group_by(Gate, ItemEmotion, ListenerLang, ItemType) %>%
  summarize(mean=mean(HuScore, na.rm=TRUE),
            sd=sd(HuScore, na.rm=TRUE))
 
# print out summarized data as a HTML table 
nice_table(summary_L1_mandarin_VOC_gate)

#LMM model 2a
LMM_Hu_mandarin_nvv_gate  <- lmer(
  HuScore ~ (ItemEmotion + Gate)^2 + 
    (1 | Subject), REML = FALSE,
  data = df_Q2a_L1_mandarin_VOC_gate
)


# write LMM results to a table in HTML format
tab_model(LMM_Hu_mandarin_nvv_gate, show.df = TRUE)

anova_Q2a <- anova(LMM_Hu_mandarin_nvv_gate)
print(anova_Q2a)

# Q2a t-test for direction of statistical significance --------
Q2a_emm_mandarin_nvv_gate <- emmeans(LMM_Hu_mandarin_nvv_gate , pairwise ~ ItemEmotion|Gate, methods="turkey",
                        lmer.df = "satterthwaite", 
                        lmerTest.limit = 625)  # Adjust pbkrtest.limit if needed

summary(Q2a_emm_mandarin_nvv_gate)
summary.Q2a.stats.table <- as.data.frame(summary(Q2a_emm_mandarin_nvv_gate))
contrasts_df_2a <- summary.Q2a.stats.table$contrasts
nice_table(contrasts_df_2a) 

# Q2a violin-plot faced, VOC, emotion, Mandarin listeners -----------------------------------------------
# # Define shades of orange for each gate
# emotion_colors <- c(
#   Anger = "#E63946",  # A vivid, slightly desaturated red
#   Fear = "#457B9D",  # A soft, desaturated blue
#   Happiness = "#F4A261",  # A warm, muted orange
#   #Happiness_pleasure = "#2A9D8F",  # A calming, desaturated teal
#   Sadness = "#9C89B8"  # A gentle, muted purple
# )
# 
# # Function to lighten or darken colors
# lighten_color1 <-
#   function(color, amount = 0.2)
#     adjustcolor(color, amount)
# lighten_color2 <-
#   function(color, amount = 0.4)
#     adjustcolor(color, amount)
# lighten_color3 <-
#   function(color, amount = 0.6)
#     adjustcolor(color, amount)
# lighten_color4 <-
#   function(color, amount = 0.8)
#     adjustcolor(color, amount)
# lighten_color5 <-
#   function(color, amount = 1.2)
#     adjustcolor(color, amount)
# 
# # Generate variations for each language
# emotion_colors_G200 <-
#   setNames(sapply(emotion_colors, lighten_color1),
#            paste(names(emotion_colors), "G200", sep = "."))
# emotion_colors_G400 <-
#   setNames(sapply(emotion_colors, lighten_color2),
#            paste(names(emotion_colors), "G400", sep = "."))
# emotion_colors_G500 <-
#   setNames(sapply(emotion_colors, lighten_color3),
#            paste(names(emotion_colors), "G500", sep = "."))
# emotion_colors_G600 <-
#   setNames(sapply(emotion_colors, lighten_color4),
#            paste(names(emotion_colors), "G600", sep = "."))
# emotion_colors_Gfull <-
#   setNames(sapply(emotion_colors, lighten_color5),
#            paste(names(emotion_colors), "GFULL", sep = "."))
# 
# # Combine into one vector for use in ggplot
# all_colors <-
#   c(
#     emotion_colors_G200,
#     emotion_colors_G400,
#     emotion_colors_G500,
#     emotion_colors_G600,
#     emotion_colors_Gfull
#   )
# 
# # Ensure your Gate variable is a factor with the levels in the correct order for the plot
# df_Q2a_L1_mandarin_VOC_gate$Gate <-
#   factor(L1_mandarin_VOC_gate$Gate,
#          levels = c("G200", "G400", "G500", "G600", "GFULL"))
# 
# # Create the plot
# vilion_Q2a_NVV_gate_manda <- ggplot(df_Q2a_L1_mandarin_VOC_gate,
#                                aes(
#                                  x = Gate,
#                                  y = HuScore,
#                                  fill = interaction(ItemEmotion, Gate)  # Fill boxes with gate color
#                                )) +
#   geom_violin(position = position_dodge(width = 0.8),
#               color = NA,
#               trim = FALSE) +
#   scale_fill_manual(values = all_colors) +  # Use the defined orange shades
#   theme_minimal() +
#   labs(title = "FigureQ2a. Hu score as a function of NVV type and gate (Mandarin)",
#        x = "Gate",
#        y = "Mean Hu score",
#        fill = "Gate") +
#   stat_summary(
#     fun = mean,
#     geom = "point",
#     shape = 20,
#     size = 3,
#     color = "black",
#     position = position_dodge(width = 0.8)
#   ) +
#   facet_wrap( ~ ItemEmotion, nrow = 1) +  # Facet by emotion
#   theme(
#     strip.text.x = element_text(
#       size = 12,
#       color = "black",
#       face = "bold"
#     ),
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     # Optional: rotate x-axis labels if needed
#     panel.spacing = unit(1, "lines"),
#     # Adjust spacing between panels if necessary
#     axis.ticks.length.x = unit(4, "pt"),
#     # Adjust length of ticks if needed
#     axis.text.x.bottom = element_text(margin = margin(1, 0, 1, 0, "lines")),
#     legend.position = "none",
#     plot.title = element_text(hjust = 0.5)
#   )
# 
# # Display the plot (mac only)
# #quartz(width=10, height=8)  # Adjust size as needed
# vilion_Q2a_NVV_gate_manda
# 
# Q2a Hu score, connected pointed plot -----------------------------------------------

# Filter data, create mean for the last gate point
L1_Mandarin_VOC_gate_mean <- df_Q2a_L1_mandarin_VOC_gate %>%
  filter(Gate %in% c("G200", "G400", "G500", "G600", "GFULL")) %>%
  group_by(Gate,ItemEmotion) %>%
  summarize(mean_HuScore = mean(HuScore, na.rm = TRUE), sd = sd(HuScore, na.rm = TRUE)) %>%
  mutate(ItemEmotion = case_when(
    ItemEmotion == "Happiness_laughter" ~ "Laughter",
    ItemEmotion == "Happiness_pleasure" ~ "Pleasure",
    TRUE ~ ItemEmotion
  ))

# create a new colomn to specify tick position
L1_Mandarin_VOC_gate_mean <- L1_Mandarin_VOC_gate_mean %>%
  mutate(Gate_pos = case_when(
    Gate == "G200" ~ 2,
    Gate == "G400" ~ 4,
    Gate == "G500" ~ 5,
    Gate == "G600" ~ 6,
    Gate == "GFULL" ~ 8  # Double the distance for "GFULL"
  ))

# Create last_points_Q2a with additional Gate_pos
last_points_Q2a <- data.frame(
  ItemEmotion = c("Anger", "Fear", "Laughter", "Pleasure", "Sadness"),
  mean_HuScore = c(0.634, 0.708, 0.559, 0.183, 0.735),
  Gate = factor("GFULL", levels = c("G200", "G400", "G500", "G600", "GFULL"))
)

# Merge last_points_Q2a with L1_Mandarin_VOC_gate_mean to get Gate_pos
last_points_Q2a <- last_points_Q2a %>%
 mutate(Gate_pos = case_when(
   Gate == "GFULL" ~ 8.3
 ))

# # custom label position 
# last_points_Q2a <- data.frame(
#   ItemEmotion = c("Anger", "Fear", "Laughter", "Pleasure", "Sadness" ),
#   mean_HuScore = c(0.634, 0.708, 0.559, 0.183, 0.735),
#   Gate = "GFULL"  # Assuming the last point should be labeled at the GFULL gate
# )

# scatter line plot 
p_Q2a <-
  ggplot(
    L1_Mandarin_VOC_gate_mean,
    aes(
      x = Gate_pos,
      y = mean_HuScore,
      group = ItemEmotion,
      color = ItemEmotion
    )
  ) +
  geom_point(size = 3) +
  geom_path(arrow = arrow(length = unit(0.3, "mm"))) +
  #geom_errorbar(aes(ymin=mean_HuScore-sd, ymax=mean_HuScore+sd),width = 0.03) +
  labs(x = "Gate Interval (ms)", 
       y = "HuScore", 
       title = "Figure 2b. Recognition of emotional vocalizations evolved as a function of stimulus duration (Chinese)") +
  geom_text(data = last_points_Q2a, 
            aes(x = Gate_pos, label = ItemEmotion), 
            vjust = -1, 
            hjust = 1, 
            check_overlap = TRUE) + 
  theme(panel.background = element_blank()) +
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(vjust = -1.5, 
                                    hjust = 0.5, 
                                    size = 12),  # Center x-axis label
        axis.title.y = element_text(vjust = 1, 
                                    hjust = 0.5, 
                                    size = 12), # Center y-axis label
        axis.line = element_line(color = "black", 
                                 size = 0.5),
        axis.ticks.length = unit(1.4,"mm"),
        axis.ticks = element_line(size = .5, 
                                  colour = "black")) +
  scale_x_continuous(breaks = c(2, 4, 5, 6, 8), labels = c("G200", "G400", "G500", "G600", "GFULL")) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2))  # Specify y-axis tick locations# Adjust tick length

print(p_Q2a)

ggsave("Q2b_NVV_Hu_Chi.png", plot = p_Q2a, width = 10, height = 8, units = "in")

#####(done)

# #### -------------------------------------------------------------------- (done)


# model 3 -------------------------------------------------------------
# Q2b LMM model for Hu Score as a function of NVV vs. Gate by Arabic L1 listeners------------------------------------------

# clean, filler data
df_Q2b_L1_Arabic_VOC_gate <- gate_HuScore %>%
  filter(
    ItemToListener != "Foreign",
    ItemToListener != "L2",
    ItemToListener != "L1",
    ListenerLang != "Mandarin"
  ) %>%
  droplevels() %>%
  group_by(ListenerLang, ItemToListener)

#LMM model 
LMM_Hu_Arabic_nvv_gate  <- lmer(
  HuScore ~ (ItemEmotion + Gate)^2 + 
    (1 | Subject), REML = FALSE,
  data = df_Q2b_L1_Arabic_VOC_gate
)

# write LMM results to a table in HTML format
tab_model(LMM_Hu_Arabic_nvv_gate, show.df = TRUE)

# F-test results for fixed effects
print(anova(LMM_Hu_Arabic_nvv_gate))

# Q2b t-tests -----------------------------------------------------------
LMM_Hu_Arabic_nvv_gate <- emmeans(LMM_Hu_Arabic_nvv_gate , pairwise ~ ItemEmotion|Gate,
                                    lmer.df = "satterthwaite", 
                                    lmerTest.limit = 625)  # Adjust pbkrtest.limit if needed
summary(LMM_Hu_Arabic_nvv_gate)

summary.Q2b.stats.table <- as.data.frame(summary(LMM_Hu_Arabic_nvv_gate))

contrasts_df_2b <- summary.Q2b.stats.table $contrasts

nice_table(contrasts_df_2b )

# Q2b connected point plot, VOC emotion, Arabic listeners, Hu Score -----------------------------------------------

# scatter line plot 
L1_Arabic_VOC_gate_mean <- df_Q2b_L1_Arabic_VOC_gate %>%
  filter(Gate %in% c("G200", "G400", "G500", "G600", "GFULL")) %>%
  group_by(Gate,ItemEmotion) %>%
  summarize(mean_HuScore = mean(HuScore, na.rm = TRUE), sd = sd(HuScore, na.rm = TRUE))

# create a new colomn to specify tick position
L1_Arabic_VOC_gate_mean <- L1_Arabic_VOC_gate_mean %>%
  mutate(Gate_pos = case_when(
    Gate == "G200" ~ 2,
    Gate == "G400" ~ 4,
    Gate == "G500" ~ 5,
    Gate == "G600" ~ 6,
    Gate == "GFULL" ~ 8  # Double the distance for "GFULL"
  ))

last_points_Q2b <- data.frame(
  ItemEmotion = c("Anger", "Fear", "Sadness", "Happiness_laughter", "Happiness_pleasure"),
  mean_HuScore = c(0.75621366, 0.71333102, 0.81856648, 0.56574978, 0.2237),
  Gate = "GFULL"  # Assuming the last point should be labeled at the GFULL gate
)

# Merge last_points_Q2a with L1_Mandarin_VOC_gate_mean to get Gate_pos
last_points_Q2b <- last_points_Q2b %>%
  mutate(Gate_pos = case_when(
    Gate == "GFULL" ~ 8.3
  ))

# plot
p_Q2b <-
  ggplot(
    L1_Arabic_VOC_gate_mean,
    aes(
      x = Gate_pos,
      y = mean_HuScore,
      group = ItemEmotion,
      color = ItemEmotion
    )
  ) +
  geom_point(size = 3) +
  geom_path(arrow = arrow(length = unit(0.3, "mm"))) +
  #geom_errorbar(aes(ymin=mean_HuScore-sd, ymax=mean_HuScore+sd),width = 0.03) +
  labs(x = "Gate Interval (ms)", 
       y = "HuScore", 
       title = "Figure 2a. Recognition of emotional vocalizations evolved as a function of stimulus duration (Arabic)") +
  geom_text(data = last_points_Q2b, 
            aes(x = Gate_pos, label = ItemEmotion), 
            vjust = -1, 
            hjust = 1, 
            check_overlap = TRUE) + 
  theme(panel.background = element_blank()) +
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(vjust = -1.5, 
                                    hjust = 0.5, 
                                    size = 12),  # Center x-axis label
        axis.title.y = element_text(vjust = 1, 
                                    hjust = 0.5, 
                                    size = 12), # Center y-axis label
        axis.line = element_line(color = "black", 
                                 size = 0.5),
        axis.ticks.length = unit(1.4,"mm"),
        axis.ticks = element_line(size = .5, 
                                  colour = "black")) +
  scale_x_continuous(breaks = c(2, 4, 5, 6, 8), labels = c("G200", "G400", "G500", "G600", "GFULL")) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2))  # Specify y-axis tick locations# Adjust tick length

print(p_Q2b)

ggsave("Q2a_NVV_Hu_Arb.png", plot = p_Q2b, width = 10, height = 8, units = "in")
##### (done)

# #### -------------------------------------------------------------------- (done)


# model 4 ------------------------------------------------------------
# Q2c LMM model for Hu score as a function of Utterance vs. Gate by Mandarin L1 listeners -------------------------------------

# clean, fillter data
L1_Mandarin_speech_gate <- gate_HuScore %>%
  filter(
    ItemToListener != "Foreign",
    ItemToListener != "L2",
    ItemToListener != "Vocalization",
    ListenerLang != "Arabic"
  ) %>%
  droplevels() %>%
  group_by(ListenerLang, ItemToListener)

#LMM model 
LMM_Hu_Mandarin_speech_gate  <- lmer(
  HuScore ~ (ItemEmotion + Gate)^2 + ItemEmotion:Gate + 
    (1 | Subject),REML = FALSE,
  data = L1_Mandarin_speech_gate 
)

# write LMM results to a table in HTML format
tab_model(LMM_Hu_Mandarin_speech_gate, show.df = TRUE)

# f-test
print(anova(LMM_Hu_Mandarin_speech_gate))

# Q2c pairwise comparison t-tests ---------------------------------

LMM_Hu_Mandarin_speech_gate <- emmeans(LMM_Hu_Mandarin_speech_gate , pairwise ~ Gate|ItemEmotion,
                                  lmer.df = "satterthwaite", REML = FALSE,
                                  lmerTest.limit = 622)  # Adjust pbkrtest.limit if needed
summary(LMM_Hu_Mandarin_speech_gate)


summary.Q2c.stats.table <- as.data.frame(summary(LMM_Hu_Mandarin_speech_gate))

contrasts_df_2c <- summary.Q2c.stats.table$contrasts

nice_table(contrasts_df_2c)

# Q2c connected point plot-----------------------------------------------

# scatter line plot 
L1_Mandarin_speech_gate_mean <- L1_Mandarin_speech_gate %>%
  filter(Gate %in% c("G200", "G400", "G500", "G600", "GFULL")) %>%
  group_by(Gate, ItemEmotion) %>%
  summarize(mean_HuScore = mean(HuScore, na.rm = TRUE),
            sd = sd(HuScore, na.rm = TRUE))


last_points_speech_Man <- data.frame(
  ItemEmotion = c("Anger", "Fear", "Sadness", "Happiness"),
  mean_HuScore = c(0.588, 0.568, 0.659, 0.611),
  Gate = "GFULL"  # Assuming the last point should be labeled at the GFULL gate
)

# create a new colomn to specify tick position
L1_Mandarin_speech_gate_mean <- L1_Mandarin_speech_gate_mean %>%
  mutate(Gate_pos = case_when(
    Gate == "G200" ~ 2,
    Gate == "G400" ~ 4,
    Gate == "G500" ~ 5,
    Gate == "G600" ~ 6,
    Gate == "GFULL" ~ 8  # Double the distance for "GFULL"
  ))


# Merge last_points_Q2a with L1_Mandarin_VOC_gate_mean to get Gate_pos
last_points_Q2c <- last_points_speech_Man  %>%
  mutate(Gate_pos = case_when(
    Gate == "GFULL" ~ 8.5
  ))

#plot
p_Q2c <-
  ggplot(
    L1_Mandarin_speech_gate_mean ,
    aes(
      x = Gate_pos,
      y = mean_HuScore,
      group = ItemEmotion,
      color = ItemEmotion
    )
  ) +
  geom_point(size = 3) +
  geom_path(arrow = arrow(length = unit(0.3, "mm"))) +
  #geom_errorbar(aes(ymin=mean_HuScore-sd, ymax=mean_HuScore+sd),width = 0.03) +
  labs(x = "Gate Interval (ms)", 
       y = "HuScore", 
       title = "Figure 3b. Recognition of emotional speech evolved as a function of duration (Chinese)") +
  geom_text(data = last_points_Q2c, 
            aes(x = Gate_pos, label = ItemEmotion), 
            vjust = -1, 
            hjust = 1, 
            check_overlap = FALSE) + 
  theme(panel.background = element_blank()) +
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(vjust = -1.5, 
                                    hjust = 0.5, 
                                    size = 12),  # Center x-axis label
        axis.title.y = element_text(vjust = 1, 
                                    hjust = 0.5, 
                                    size = 12), # Center y-axis label
        axis.line = element_line(color = "black", 
                                 size = 0.5),
        axis.ticks.length = unit(1.4,"mm"),
        axis.ticks = element_line(size = .5, 
                                  colour = "black")) +
  scale_x_continuous(breaks = c(2, 4, 5, 6, 8), labels = c("G200", "G400", "G500", "G600", "GFULL")) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2))  # Specify y-axis tick locations# Adjust tick length

print(p_Q2c)

ggsave("Q2c_Speech_Hu_Chi.png", plot = p_Q2c, width = 10, height = 8, units = "in")
# Display the plot
#quartz(width=10, height=8)  # Adjust size as needed

##### (done)
##### (done)

# #### -------------------------------------------------------------------- (done)


# model 5 -----------------------------------------------------------
# Q2d LMM Model for Hu Score as a function of Utterance vs. Gate by Arabic L1 listeners -------------------------------------
# clean, filter data
L1_Arabic_speech_gate <- gate_HuScore %>%
  filter(
    ItemToListener != "Foreign",
    ItemToListener != "L2",
    ItemToListener != "Vocalization",
    ListenerLang != "Mandarin"
  ) %>%
  droplevels() %>%
  group_by(ListenerLang, ItemToListener)

#LMM model 
LMM_Hu_Arabic_speech_gate  <- lmer(
  HuScore ~ (ItemEmotion + Gate)^2 + ItemEmotion:Gate + 
    (1 | Subject), REML = FALSE,
  data = L1_Arabic_speech_gate 
)

# write LMM results to a table in HTML format
tab_model(LMM_Hu_Arabic_speech_gate, show.df = TRUE)

# f-test
print(anova(LMM_Hu_Arabic_speech_gate))

# Q2d pairwise comparison t-tests ---------------------------------------

LMM_Hu_Arabic_speech_gate <- emmeans(LMM_Hu_Arabic_speech_gate , pairwise ~ Gate|ItemEmotion,
                                       lmer.df = "satterthwaite", 
                                       lmerTest.limit = 622)  # Adjust pbkrtest.limit if needed
summary(LMM_Hu_Arabic_speech_gate)
summary.Q2d.stats.table <- as.data.frame(summary(LMM_Hu_Arabic_speech_gate))
contrasts_df_2d <- summary.Q2d.stats.table$contrasts
nice_table(contrasts_df_2d)


# Q2d connected point-----------------------------------------------

# Define shades of orange for each gate

# gate_colors_Areb_sp <- c(
#   "G200" = "#deebf7",  # lightest blue
#   "G400" = "#9ecae1",
#   "G500" = "#3182bd",
#   "G600" = "#2171b5",
#   "GFULL" = "#08519c"  # darkest blue
# )
# 
# 
# # Ensure your Gate variable is a factor with the levels in the correct order for the plot
# L1_Arabic_speech_gate $Gate <- factor(L1_Arabic_speech_gate $Gate, levels = c("G200", "G400", "G500", "G600", "GFULL"))
# 
# # Create the plot
# box_P_Sp_gate_Arb <- ggplot(L1_Arabic_speech_gate,
#                             aes(
#                               x = Gate,
#                               y = HuScore,
#                               fill = Gate  # Fill boxes with gate color
#                             )) +
#   geom_boxplot(position = position_dodge(width = 0.8)) +
#   scale_fill_manual(values = gate_colors_Areb_sp) +  # Use the defined orange shades
#   theme_minimal() +
#   labs(title = "Figure2d. Hu score as a function of Speech type and gate (Arabic)",
#        x = "Gate",
#        y = "Mean Hu score",
#        fill = "Gate") +
#   stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "black",
#                position = position_dodge(width = 0.8)) +
#   facet_wrap(~ItemEmotion) +  # Facet by emotion
#   theme(strip.text.x = element_text(size = 12, color = "black", face = "bold"))

# Display the plot
#quartz(width=10, height=8)  # Adjust size as needed

# scatter line plot 
L1_Arabic_speech_gate_mean <- L1_Arabic_speech_gate %>%
  filter(Gate %in% c("G200", "G400", "G500", "G600", "GFULL")) %>%
  group_by(Gate, ItemEmotion) %>%
  summarize(mean_HuScore = mean(HuScore, na.rm = TRUE),
            sd = sd(HuScore, na.rm = TRUE))


last_points_speech_arb <- data.frame(
  ItemEmotion = c("Anger", "Fear", "Sadness", "Happiness"),
  mean_HuScore = c(0.5228, 0.5848, 0.4429, 0.7131),
  Gate = "GFULL"  # Assuming the last point should be labeled at the GFULL gate
)

# create a new colomn to specify tick position
L1_Arabic_speech_gate_mean <- L1_Arabic_speech_gate_mean %>%
  mutate(Gate_pos = case_when(
    Gate == "G200" ~ 2,
    Gate == "G400" ~ 4,
    Gate == "G500" ~ 5,
    Gate == "G600" ~ 6,
    Gate == "GFULL" ~ 8  # Double the distance for "GFULL"
  ))


# Merge last_points_Q2a with L1_Mandarin_VOC_gate_mean to get Gate_pos
last_points_Q2d <- L1_Arabic_speech_gate_mean  %>%
  mutate(Gate_pos = case_when(
    Gate == "GFULL" ~ 8.4
  ))

#plot
p_Q2d <-
  ggplot(
    L1_Arabic_speech_gate_mean ,
    aes(
      x = Gate_pos,
      y = mean_HuScore,
      group = ItemEmotion,
      color = ItemEmotion
    )
  ) +
  geom_point(size = 3) +
  geom_path(arrow = arrow(length = unit(0.3, "mm"))) +
  #geom_errorbar(aes(ymin=mean_HuScore-sd, ymax=mean_HuScore+sd),width = 0.03) +
  labs(x = "Gate Interval (ms)", 
       y = "HuScore", 
       title = "Figure 3a. Recognition of emotional speech evolved as a function of duration (Arabic)") +
  geom_text(data = last_points_Q2d, 
            aes(x = Gate_pos, label = ItemEmotion), 
            vjust = -1, 
            hjust = 1, 
            check_overlap = TRUE) + 
  theme(panel.background = element_blank()) +
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(vjust = -1.5, 
                                    hjust = 0.5, 
                                    size = 12),  # Center x-axis label
        axis.title.y = element_text(vjust = 1, 
                                    hjust = 0.5, 
                                    size = 12), # Center y-axis label
        axis.line = element_line(color = "black", 
                                 size = 0.5),
        axis.ticks.length = unit(1.4,"mm"),
        axis.ticks = element_line(size = .5, 
                                  colour = "black")) +
  scale_x_continuous(breaks = c(2, 4, 5, 6, 8), labels = c("G200", "G400", "G500", "G600", "GFULL")) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2))  # Specify y-axis tick locations# Adjust tick length

print(p_Q2d)


ggsave("Q2d_Speech_Hu_Arb.png", plot = p_Q2d, width = 10, height = 8, units = "in")
##### (done)

# #### -------------------------------------------------------------------- (done)


# model 6 ----------------------------------------------------------
# Q3a LMM for EIP of NVV as a function of L1 background vs.ItemEmotion  -----------------------------------------------------------
# Vocalizations: Listener (MAND, ARAB) x Emotion (ANG, FER, SAD, HAP-Amuse, HAP-Pleasure) at Gfull
# filter EIP data for Q3a
df_Q3a_L1_VOC_EIP <- gate_EIP_drop_pleasure  %>%
  filter(ItemToListener != "Foreign", ItemToListener != "L2", ItemType != "utterance") %>%
  #filter(EIP == "5") %>%
  droplevels() %>%
  group_by(ListenerLang, ItemToListener, EIP)

# new median filtered value df
df_Q3a_L1_VOC_EIPMedian <- df_Q3a_L1_VOC_EIP %>%
  group_by(ListenerLang, ItemType, ItemEmotion,ListenerID, ItemLang, ItemToListener) %>%
  summarize(
    #mean_EIP = mean(EIPtime, na.rm = TRUE),
    median_EIP = median(EIPtime, na.rm = TRUE),
    #sd_EIP = sd(EIPtime, na.rm = TRUE),
    .groups = 'drop'
  )

# Display the summary table
print(df_Q3a_L1_VOC_EIPMedian )

nice_table(df_Q3a_L1_VOC_EIPMedian ) 

# LMM model using median value df
LMM_Q3a_EIP_VOC_L1  <- lmer(
  median_EIP ~ (ListenerLang + ItemEmotion)^2 +
    (1 | ListenerID), REML = FALSE,
  data = df_Q3a_L1_VOC_EIPMedian 
)


# write LMM results to a table in HTML format
tab_model(LMM_Q3a_EIP_VOC_L1, show.df = TRUE)

# Perform the F-test
print(anova(LMM_Q3a_EIP_VOC_L1))

# Q3a t-tests -------------------------------------------------------------

Q3a_emm_EIP_L1_Nvv <- emmeans(LMM_Q3a_EIP_VOC_L1, pairwise ~ ItemEmotion, methods ="turkey",
                                     lmer.df = "satterthwaite", 
                                     lmerTest.limit = 622)  # Adjust pbkrtest.limit if needed

summary(Q3a_emm_EIP_L1_Nvv)
summary.Q3a.stats.table <- as.data.frame(summary(Q3a_emm_EIP_L1_Nvv))
contrasts_df_3a <- as.data.frame(summary.Q3a.stats.table$contrasts)
nice_table(contrasts_df_3a)

# Q3a EPI bar plot (SE added) ---------------------------------------------------------------

# filter data for ploting Q3a EIP 
Q3a_EIPMedian_summary <- df_Q3a_L1_VOC_EIPMedian  %>%
  group_by(ListenerLang, ItemType, ItemEmotion) %>%
  summarize(
    mean_median = mean(median_EIP, na.rm = TRUE),
    se_EIP = standard_error(median_EIP),
    .groups = 'drop'
  ) %>%
  mutate(ListenerLang = case_when(ListenerLang == "Mandarin" ~ "Chinese",
                                  TRUE ~ ListenerLang))
# Q3a table 
nice_table(Q3a_EIPMedian_summary)

#SE
Q3a_EIPMedian_summary <- Q3a_EIPMedian_summary %>%
  mutate(
    lower = median - se_EIP,
    upper = median + se_EIP
  )

print(Q3a_EIPMedian_summary)
# print table
nice_table(Q3a_EIPMedian_summary)

Q3a_bar_NVV_L1 <- ggplot(Q3a_EIPMedian_summary, aes(x = ItemEmotion, y = median, fill = ListenerLang)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5), width = 0.5) +
  geom_col(position = position_dodge(width = 0.5), width = 0.3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.5), width = 0.25) +
  labs(title = "Figure 2c. Median EIP as a function of Emotional vocalizations and L1 background",
       x = "Type of Emotional vocalizations",
       y = "Median EIP",
       fill = "Listener group") +
  theme_minimal() +
  theme(legend.position = "top") +
  coord_flip() # Flip the coordinates to make the bars horizontal

Q3a_bar_NVV_L1

ggsave("Q3a_NVV_EIP.png", plot = Q3a_bar_NVV_L1, width = 10, height = 8, units = "in")

##### (done)
# #### -------------------------------------------------------------------- (done)


# model 7 -----------------------------------------------------------
# Q3b LMM for EIP of Speech utterance as a function of L1 background vs.ItemEmotion  -----------------------------------------------------------

# clean EIP data for Gfull gate 
df_Q3b_L1_Speech_EIP <- gate_EIP %>%
  filter(ItemToListener != "Foreign", ItemToListener != "L2", ItemType != "vocalization") %>%
  droplevels() %>%
  group_by(ListenerLang, ItemToListener, EIP)


# new median value
df_Q3b_L1_Speech_EIPMedian <- df_Q3b_L1_Speech_EIP %>%
  group_by(ListenerLang, ItemType, ItemEmotion,ListenerID, ItemLang, ItemToListener) %>%
  summarize(
    median_EIP = median(EIPtime, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(ListenerLang = case_when(ListenerLang == "Mandarin" ~ "Chinese",
                                  TRUE ~ ListenerLang))

nice_table(df_Q3b_L1_Speech_EIPMedian)

# LMM model (median)
LMM_Q3b_L1_Speech_EIP  <- lmer(
  median_EIP ~ (ListenerLang + ItemEmotion) ^ 2 + 
  (1 | ListenerID),
  REML = FALSE,
  data = df_Q3b_L1_Speech_EIPMedian)

# write LMM results to a table in HTML format
tab_model(LMM_Q3b_L1_Speech_EIP, show.df = TRUE)

# f-test
print(anova(LMM_Q3b_L1_Speech_EIP))

# Q3b t-tests -------------------------------------------------------------

Q3b_emm_L1_speech <- emmeans(LMM_Q3b_L1_Speech_EIP, pairwise ~ ListenerLang|ItemEmotion,
                                lmer.df = "satterthwaite", 
                                lmerTest.limit = 1346)  # Adjust pbkrtest.limit if needed

summary.Q3b.stats.table <- summary(Q3b_emm_L1_speech)

summary.Q3b.stats.table <- as.data.frame(summary(Q3b_emm_L1_speech))
contrasts_df_3b <- as.data.frame(summary.Q3b.stats.table$contrast)
nice_table(contrasts_df_3b)

# Q3b EPI bar plot ---------------------------------------------------------------

# summarize filtered data
summary_df_Q3b <- df_Q3b_L1_Speech_EIPMedian %>%
  group_by(ListenerLang, ItemToListener,ItemEmotion) %>%
  summarise(
    median = mean(median_EIP, na.rm = TRUE),
    se_EIP = standard_error(median_EIP),
  ) %>% 
  mutate(ListenerLang = case_when(ListenerLang == "Mandarin" ~ "Chinese",
                                  TRUE ~ ListenerLang)) %>%
  ungroup()  # Remove grouping

# add SE on the bar
summary_df_Q3b <- summary_df_Q3b %>%
  mutate(
    lower = median - se_EIP,
    upper = median + se_EIP
  )

# HTML table
nice_table(summary_df_Q3b)

# plot
Q3b_bar_L1_speech_EIP <- ggplot(summary_df_Q3b, aes(x = ItemEmotion, y = median, fill = ListenerLang)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5), width = 0.5) +
  geom_col(position = position_dodge(width = 0.5), width = 0.3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.5), width = 0.25) +
  labs(title = "Figure 3c. Median EIP as a function of Emotional speech and L1 background",
       x = "Type of Emotional Speech",
       y = "Median EIP",
       fill = "") +
  theme_minimal() +
  theme(legend.position = "top") +
  coord_flip() # Flip the coordinates to make the bars horizontal

Q3b_bar_L1_speech_EIP

ggsave("Q3b_Speech_EIP.png", plot = Q3b_bar_L1_speech_EIP, width = 10, height = 8, units = "in")
##### (done)
##### (done)
##### (done)
##### (done)

##### ------------------------------------------------------------------- (done)


# model 8 ------------------------------------------------------------
# Q4a LMM model for Hu Score as a function of L1 vs. Speech type (Mandarin) ---------------------------------------------------------
# manipulate the data for LMM models to drop L2 and Foreign conditions and keep GFULL
df_Q4a_all_speech_gate_man <- gate_HuScore_drop_pleasure %>%
  filter(ItemToListener != "Vocalization") %>%
  filter(ListenerLang == "Mandarin") %>%
  droplevels() %>%
  group_by(ItemToListener, Gate, ItemEmotion)

# summarize Q4a data
summary_df_Q4a <- df_Q4a_all_speech_gate_man %>%
  group_by(Gate, ItemEmotion, ListenerLang, ItemType) %>%
  summarize(mean=mean(HuScore, na.rm=TRUE),
            sd=sd(HuScore, na.rm=TRUE))

nice_table(summary_df_Q4a)

# LMM test
LMM_Q4a_all_speech_gate_man  <- lmer(
  HuScore ~ (ItemToListener + Gate) ^ 2 +
    (1 | Subject) + (1 | ItemEmotion),
  REML = FALSE,
  data = df_Q4a_all_speech_gate_man
)

# write LMM results to a table in HTML format
tab_model(LMM_Q4a_all_speech_gate_man, show.df = TRUE)

#f-test
print(anova(LMM_Q4a_all_speech_gate_man))


# Q4a pairwise comparison t-test ------------------------------------------------------------------
Q4a_emm_Q4a_all_speech_man <- emmeans(LMM_Q4a_all_speech_gate_man, revpairwise ~ ItemToListener|Gate,
                                      lmer.df = "satterthwaite", 
                                      lmerTest.limit = 1500)  # Adjust pbkrtest.limit if needed

summary(Q4a_emm_Q4a_all_speech_man)
summary.Q4a.stats.table <- as.data.frame(summary(Q4a_emm_Q4a_all_speech_man))
contrasts_df_4a <- as.data.frame(summary.Q4a.stats.table$contrasts)
nice_table(contrasts_df_4a)

# Q4a plot HuScore connected point ----------------------------------------------------------------

Man_speech_gate_mean <- df_Q4a_all_speech_gate_man %>%
  filter(Gate %in% c("G200", "G400", "G500", "G600", "GFULL")) %>%
  group_by(Gate,ItemToListener) %>%
  summarize(mean_HuScore = mean(HuScore, na.rm = TRUE), sd = sd(HuScore, na.rm = TRUE)) %>%
  mutate(Gate_pos = case_when(
  Gate == "G200" ~ 2,
  Gate == "G400" ~ 4,
  Gate == "G500" ~ 5,
  Gate == "G600" ~ 6,
  Gate == "GFULL" ~ 8  # Double the distance for "GFULL"
))

last_points_Q4a <- Man_speech_gate_mean %>%
  filter(Gate == "GFULL") %>%
  mutate(Gate_pos = 8.2)

p_Q4a <- ggplot(Man_speech_gate_mean, aes(x = Gate_pos, y = mean_HuScore, group = ItemToListener, color = ItemToListener)) +
  geom_point(size = 2) +
  geom_path(arrow = arrow(length = unit(0.3, "mm"))) +
  labs(x = "Gate Interval (ms)", y = "HuScore", title = "Figure 4a. Connected Scatter Plot of HuScore divided by Gate and Language familiarity (Chinese)") +
  geom_text(data = last_points_Q4a, 
            aes(x = Gate_pos, label = ItemToListener), 
            vjust = -1, 
            hjust = 1, 
            check_overlap = TRUE) + 
  theme(panel.background = element_blank()) +
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(vjust = -1.5, hjust = 0.5, size = 12),  # Center x-axis label
        axis.title.y = element_text(vjust = 1, hjust = 0.5, size = 12), # Center y-axis label
        axis.line = element_line(color = "black", size = 0.5),
        axis.ticks.length = unit(1.4, "mm"),
        axis.ticks = element_line(size = 0.5, color = "black")) +
  scale_x_continuous(breaks = c(2, 4, 5, 6, 8), labels = c("G200", "G400", "G500", "G600", "GFULL")) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2))  # Specify y-axis tick locations

# Print the plot
print(p_Q4a)

# Save the plot as a square image
ggsave("Q4a_Hu_Chi.png", plot = p_Q4a, width = 10, height = 8, units = "in")

##### (done)

# #### -------------------------------------------------------------------- (done)


# model 9 ------------------------------------------------------------
# Q4b LMM model for Hu Score as a function of L1 vs. Speech type (Arabic)  ---------------------------------------------------------

# filter data for LMM models to VOC condition
df_Q4b_all_speech_gate_arb <- gate_HuScore_drop_pleasure %>%
  filter(ItemToListener != "Vocalization") %>%
  filter(ListenerLang == "Arabic") %>%
  droplevels() %>%
  group_by(ItemToListener, Gate, ItemEmotion)

# summarize data for Q4b
summary_df_Q4b <- df_Q4a_all_speech_gate_arb %>%
  group_by(ItemEmotion, Gate, ListenerLang, ItemType) %>%
  summarize(mean=mean(HuScore, na.rm=TRUE),
            sd=sd(HuScore, na.rm=TRUE))

nice_table(summary_df_Q4b)

# LMM model of gate, emotion, speech-Hu Score
LMM_Q4b_all_speech_gate_arb  <- lmer(
  HuScore ~ (ItemToListener + Gate)^2  + 
    (1 | Subject) + (1 | ItemEmotion), REML = FALSE,
  data = df_Q4b_all_speech_gate_arb 
)

# write LMM results to a table in HTML format
tab_model(LMM_Q4b_all_speech_gate_arb, show.df = TRUE)

# f-test
print(anova(LMM_Q4b_all_speech_gate_arb))

# Q4b t-test (pairwise comparison) Estimated marginal means ------------------------------------------------------------------
Q4b_emm_all_speech_arb <- emmeans(LMM_Q4b_all_speech_gate_arb, revpairwise ~ Gate|ItemToListener,
                      lmer.df = "satterthwaite", 
                      lmerTest.limit = 1500)  # Adjust pbkrtest.limit if needed

summary(Q4b_emm_all_speech_arb)
summary.Q4b.stats.table <- as.data.frame(summary(Q4b_emm_all_speech_arb))
contrasts_df_4b <- as.data.frame(summary.Q4b.stats.table$contrasts)
nice_table(contrasts_df_4b)

# Q4b plot HuScore connected point ----------------------------------------------------------------

arb_speech_gate_mean <- df_Q4b_all_speech_gate_arb %>%
  filter(Gate %in% c("G200", "G400", "G500", "G600", "GFULL")) %>%
  group_by(Gate,ItemToListener) %>%
  summarize(mean_HuScore = mean(HuScore, na.rm = TRUE), sd = sd(HuScore, na.rm = TRUE)) %>%
  mutate(Gate_pos = case_when(
    Gate == "G200" ~ 2,
    Gate == "G400" ~ 4,
    Gate == "G500" ~ 5,
    Gate == "G600" ~ 6,
    Gate == "GFULL" ~ 8  # Double the distance for "GFULL"
  ))

last_points_Q4b <- data.frame(
  ItemToListener= c("L1", "L2", "Foreign"),
  mean_HuScore = c(0.559, 0.546, 0.530),
  Gate_pos = c(8.3, 8.3, 8.4),
  Gate = "GFULL"  # Assuming the last point should be labeled at the GFULL gate
)

# last_points_Q4b <- last_points_Q4b %>%
#   filter(Gate == "GFULL") %>%
#   mutate(Gate_pos = 8.6)

p_Q4b <- ggplot(arb_speech_gate_mean, aes(x = Gate_pos, y = mean_HuScore, group = ItemToListener, color = ItemToListener)) +
  geom_point(size = 2) +
  geom_path(arrow = arrow(length = unit(0.3, "mm"))) +
  labs(x = "Gate Interval (ms)", y = "HuScore", title = "Figure 4b. Connected Scatter Plot of HuScore divided by Gate and Language familiarity (Arabic)") +
  geom_text(data = last_points_Q4b, 
            aes(x = Gate_pos, y = mean_HuScore, label = ItemToListener), 
            vjust = -1, 
            hjust = 1, 
            check_overlap = FALSE) + 
  theme(panel.background = element_blank()) +
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(vjust = -1.5, hjust = 0.5, size = 12),  # Center x-axis label
        axis.title.y = element_text(vjust = 1, hjust = 0.5, size = 12), # Center y-axis label
        axis.line = element_line(color = "black", size = 0.5),
        axis.ticks.length = unit(1.4, "mm"),
        axis.ticks = element_line(size = 0.5, color = "black")) +
  scale_x_continuous(breaks = c(2, 4, 5, 6, 8), labels = c("G200", "G400", "G500", "G600", "GFULL")) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1))  # Specify y-axis tick locations

# Print the plot
print(p_Q4b)

# Save the plot as a square image
ggsave("Q4b_Hu_Arb.png", plot = p_Q4b, width = 10, height = 8, units = "in")

##### (done)

# #### -------------------------------------------------------------------- (done)


# model 10 -----------------------------------------------------------
# Q4a-b_combine -----------------------------------------------------------

# filter data for LMM Q4a-4b
df_Q4a_all_speech_Gfull_man_arab <- gate_HuScore_drop_pleasure %>%
  filter(ItemToListener != "Vocalization") %>%
  #filter(ListenerLang == "Mandarin") %>%
  filter(Gate == "GFULL") %>%
  droplevels() %>%
  group_by(ItemToListener, Gate, ItemEmotion)

str(df_Q4a_all_speech_Gfull_man_arab)

# LMM test
LMM_Q4a_all_speech_gate_man_arb  <- lmer(
  HuScore ~ (ItemLang + ListenerLang) ^ 2 +
    (1 | Subject) + (1 | ItemEmotion),
  REML = FALSE,
  data = df_Q4a_all_speech_Gfull_man_arab
)

# write LMM results to a table in HTML format
tab_model(LMM_Q4a_all_speech_gate_man_arb, show.df = TRUE)

# f-test
print(anova(LMM_Q4a_all_speech_gate_man_arb))

# Q4a-b t-tests pairwise comparison -----------------------------------------------
Q4a_emm_Q4a_all_speech_man_arb <- emmeans(LMM_Q4a_all_speech_gate_man_arb, revpairwise ~ ItemLang|ListenerLang,
                                          lmer.df = "satterthwaite", 
                                          lmerTest.limit = 1500)  # Adjust pbkrtest.limit if needed

summary(Q4a_emm_Q4a_all_speech_man_arb)
summary.Q4ab.stats.table <- as.data.frame(summary(Q4a_emm_Q4a_all_speech_man_arb))
contrasts_df_4ab <- as.data.frame(summary.Q4ab.stats.table$contrasts)
nice_table(contrasts_df_4ab)



# #####  ------------------------------------------------------------ (done)

# model 11 ------------------------------------------------------------
# Q4c LMM for EIP of speech as a function of L1 background, event language and ItemEmotion  -----------------------------------------------------------
# Listener (MAND, ARAB) x Emotion , Event (L1, L2, Foreign) , emotion (ANG, FER, SAD, HAP)

# filter EIP data for Q4C
df_Q4c_all_speech_EIP <- gate_EIP %>%
  filter(ItemToListener != "vocalization", ItemType != "vocalization") %>%
  droplevels() %>%
  group_by(ListenerLang, ItemToListener, EIP)

# the DV is named EIPtime
summary_df_Q4c <- df_Q4c_all_speech_EIP  %>%
  group_by(ListenerLang, ItemToListener, ItemEmotion) %>%
  summarise(
    Mean = mean(EIPtime, na.rm = TRUE),
    Min = min(EIPtime, na.rm = TRUE),
    Max = max(EIPtime, na.rm = TRUE),
    SD = sd(EIPtime, na.rm = TRUE),
    Median = median(EIPtime, na.rm = TRUE)
  ) %>%
  ungroup()  # Remove grouping

# Display the summary table
print(summary_df_Q4c)
nice_table(summary_df_Q4c) 

#new median value df
df_Q4c_all_speech_EIPMedian <- df_Q4c_all_speech_EIP %>%
  group_by(ListenerLang, ItemType, ItemEmotion,ListenerID, ItemLang, ItemToListener) %>%
  summarize(
    median_EIP = median(EIPtime, na.rm = TRUE),
    .groups = 'drop'
  )

# LMM model with median EIP value (new collapsed on emotion types)
LMM_Q4c_all_speech_ListLanf_EIP  <- lmer(
  median_EIP ~ (ListenerLang + ItemToListener)^2 +
    (1 | ListenerID) + (1 | ItemEmotion), REML = FALSE,
  data = df_Q4c_all_speech_EIPMedian  
)

# write LMM results to a table in HTML format
tab_model(LMM_Q4c_all_speech_ListLanf_EIP, show.df = TRUE)


# f-test
print(anova(LMM_Q4c_all_speech_ListLanf_EIP))

# Q4c t-tests -------------------------------------------------------------
Q4c_emm_EIP_all_speech <- emmeans(LMM_Q4c_all_speech_ListLanf_EIP, revpairwise ~ ItemToListener|ListenerLang, methods ="turkey",
                              lmer.df = "satterthwaite", 
                              lmerTest.limit = 4779)  # Adjust pbkrtest.limit if needed

summary(Q4c_emm_EIP_all_speech)
summary.Q4c.stats.table <- as.data.frame(summary(Q4c_emm_EIP_all_speech ))
contrasts_df_4c <- as.data.frame(summary.Q4c.stats.table$contrasts)
nice_table(contrasts_df_4c)

# Q4c bar plot EIP ----------------------------------------------------------------

# filter data for plot the mean of median
summary_df_Q4c_median_mean <- df_Q4c_all_speech_EIPMedian  %>%
  group_by(ListenerLang, ItemToListener) %>%
  summarise(
    Mean_median = mean(median_EIP, na.rm = TRUE),
    se_EIP = standard_error(median_EIP),
  ) %>%
  mutate(ListenerLang = case_when(
    ListenerLang == "Mandarin" ~ "Chinese",
    TRUE ~ ListenerLang
  )) %>%
  ungroup() 

summary_df_Q4c_median_mean$ItemToListener <- factor(summary_df_Q4c_median_mean$ItemToListener, levels = c("L1", "L2", "Foreign"))

summary_df_Q4c_median_mean <- summary_df_Q4c_median_mean %>%
  mutate(
    lower = Mean_median - se_EIP,
    upper = Mean_median + se_EIP
  )

print(summary_df_Q4c_median_mean)


# Now create the plot with these colors
Q4c_bar_AllLang <- ggplot(summary_df_Q4c_median_mean, aes(x = ListenerLang, y = Mean_median, fill = ItemToListener)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5), width = 0.5) +
  geom_col(position = position_dodge(width = 0.5), width = 0.3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.5), width = 0.25) +
  labs(title = "Figure 4c. Median EIP as a function of Langauge Familiarity and L1 background",
       x = "L1 background",
       y = "Median EIP",
       fill = "Language Familiarity") +
  theme_minimal() +
  theme(axis.title.x = element_text(vjust = -1.5, hjust = 0.5, size = 12),  # Center x-axis label
        axis.title.y = element_text(vjust = 1, hjust = 0.5, size = 12), # Center y-axis label
        axis.line = element_line(color = "black", size = 0.5),
        axis.ticks.length = unit(1.4, "mm"),
        axis.ticks = element_line(size = 0.5, color = "black")) +
  theme(legend.position = "top") +
  coord_flip() # Flip the coordinates to make the bars horizontal

print(Q4c_bar_AllLang)

ggsave("Q4c_EIP.png", plot = Q4c_bar_AllLang, width = 10, height = 8, units = "in")
