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
library(sjPlot) # visualize model results
library(RColorBrewer)
library(knitr)
library(ggpattern)
library(apaTables)
library(papaja)
library(formatR)
library(svglite)
library(patchwork)

# set work directory ------------------------------------------------------
setwd('C://Users//hcui8//Dropbox//Trying//Gate_analysis')

setwd('/Users/hainingcui/Dropbox/Trying/Gate_analysis')
# tidy_source("C://Users//hcui8//Dropbox//Trying//Gate_analysis//gating-anlysis//Gate_LMM_analysis.R")

# clean data --------------------------------------------------------------
# clean Hu Score  -------------------------------------------------------------
#import data of all participants
gate_HuScore <- read.csv('combined_HuScore.csv') 

gate_HuScore <- gate_HuScore %>%
  mutate(ItemEmotion = case_when(
    ItemEmotion == "Happiness_laughter" ~ "Happiness-amusement",
    ItemEmotion == "Happiness_pleasure" ~ "Happiness-pleasure",
    TRUE ~ ItemEmotion
  ))

#convert to factors for HuScore using lapply function
columns_to_converrt_to_factors <-
  c("ItemEmotion",
    "ItemType",
    "ItemLang",
    "Gate",
    "ListenerLang",
    "ItemToListener")

gate_HuScore[columns_to_converrt_to_factors] <- lapply(gate_HuScore[columns_to_converrt_to_factors], factor)


print(levels(gate_HuScore$ItemEmotion))

# drop Happiness_please level (attention changed _ or -)
gate_HuScore_drop_pleasure <- gate_HuScore %>%
  filter(ItemEmotion != "Happiness-pleasure") %>%
  mutate(ItemEmotion = droplevels(ItemEmotion))

# rename label Happiness_laughter to happiness (attention changed _ or -)
gate_HuScore_drop_pleasure <- gate_HuScore_drop_pleasure %>%
  mutate(ItemEmotion = case_when(
    ItemEmotion == "Happiness-laughter" ~ "Happiness",
    TRUE ~ ItemEmotion
  ))

# Check if each column in the gate_HuScore dataframe is a factor
factors_check <- sapply(gate_HuScore, is.factor)
# Print the results
print(factors_check)
print(str(gate_HuScore))

# clean EIP value ---------------------------------------------------------------------
# import EIP data (Yondu created dataframe in excel)
gate_EIP <- read.csv('CCGating2_EIP_ALL PARTICIPANTSjuly2018.csv')

gate_full_duration <- read.csv('gate_stimuli_duration_Gfull.csv')

# Find missing or mismatched keys
missing_in_full_duration <- anti_join(gate_EIP, gate_full_duration, by = c("RecordingIndex", "ItemName"))
missing_in_gate_EIP <- anti_join(gate_full_duration, gate_EIP, by = c("RecordingIndex", "ItemName"))

# Display missing entries
print(missing_in_full_duration)
print(missing_in_gate_EIP)

# Trim any leading/trailing spaces and ensure consistent formatting
gate_EIP$ItemName <- trimws(gate_EIP$ItemName)
gate_full_duration$ItemName <- trimws(gate_full_duration$ItemName)

# Convert columns to character type (or numeric, depending on your data)
gate_EIP$RecordingIndex <- as.character(gate_EIP$RecordingIndex)
gate_full_duration$RecordingIndex <- as.character(gate_full_duration$RecordingIndex)
gate_EIP$ItemName <- as.character(gate_EIP$ItemName)

gate_full_duration$ItemName <- as.character(gate_full_duration$ItemName)
gate_EIP_duration <- merge(gate_EIP, gate_full_duration, by = c("ItemName"),all = TRUE)
# write.csv(gate_EIP_duration, file = "gate_EIP_duration_check.csv")

# rename "ItemToListener" to "ItemToListener"
gate_EIP_duration <- gate_EIP_duration %>%
  rename(ItemToListener = ExpressionTypebyGrp )

# Rename level names
gate_EIP_duration <- gate_EIP_duration %>%
  mutate(ItemToListener = case_when(
    ItemToListener == "foreign" ~ "Foreign",
    ItemToListener == "nvv" ~ "Vocalization",
    ItemToListener == "native" ~ "L1",
    TRUE ~ ItemToListener 
  )) %>%
  mutate(ItemEmotion = case_when(
    ItemEmotion == "Laughter" ~ "Happiness-amusement",
    TRUE ~ ItemEmotion
  )) %>%
  mutate(ItemEmotion = case_when(
    ItemEmotion == "Pleasure" ~ "Happiness-pleasure",
    TRUE ~ ItemEmotion
  )) %>%
  mutate(ItemType = case_when(
    ItemType == "nonverbal vocalization" ~ "vocalization",
    TRUE ~ ItemType
  )) %>%
  mutate(EIP = ifelse(EIP == "?", NA, EIP)) %>%
  filter(!is.na(EIP))


# convert EIP to integer
gate_EIP_duration$EIP <- as.integer(gate_EIP_duration$EIP)

# convert variables to factors
col_EIP_factors <- c(
  "ListenerLang",
  "ItemToListener",
  "ItemEmotion"
)

gate_EIP_duration[col_EIP_factors] <- lapply(gate_EIP_duration[col_EIP_factors], factor)

print(str(gate_EIP_duration))

# drop Pleasure level in EIP df for other models except Q2a Q2b-------------------------------------------------
gate_EIP_drop_pleasure <- gate_EIP_duration %>%
  mutate(ItemEmotion = case_when(
    ItemEmotion == "Happiness-amusement" ~ "Happiness",
    TRUE ~ ItemEmotion
  )) %>%
  filter(ItemEmotion != "Happiness-pleasure") %>%
  droplevels()

# convert variables to factors
col_EIP_factors <- c(
  "ListenerLang",
  "ItemToListener",
  "ItemEmotion"
)

gate_EIP_drop_pleasure[col_EIP_factors] <- lapply(gate_EIP_drop_pleasure[col_EIP_factors], factor)

print(str(gate_EIP_drop_pleasure))

write.csv(gate_EIP_drop_pleasure, file = "gate_EIP_drop_pleasure_duration_check.csv")

# summarize Hu Score  ---------------------------------------------------------

#summarize Discrib Stats
summary_gate_HuScore <- gate_HuScore %>%
  group_by(ListenerLang, ItemToListener, Gate, ItemEmotion) %>%
  summarise(
    Mean_HuScore = mean(HuScore, na.rm = TRUE),
    Median_HuScore = median(HuScore, na.rm = TRUE),
    SD_HuScore = sd(HuScore, na.rm = TRUE),
    Max_HuScore = max(HuScore, na.rm = TRUE),
    Min_HuScore = min(HuScore, na.rm = TRUE)
  ) %>%
  ungroup()  # Remove grouping

# print a HTML table
nice_table(summary_gate_HuScore)

# summarize EIP -----------------------------------------------------------

# summarize EIP
summary_gate_EIP <- gate_EIP_duration %>%
  group_by(ListenerLang, ItemToListener, ItemEmotion) %>%
  summarise(
    Mean_EIP = mean(EIPtime, na.rm = TRUE),
    Median_EIP = median(EIPtime, na.rm = TRUE),
    SD_EIP = sd(EIPtime,na.rm = TRUE),
    Max_EIP = max(EIPtime, na.rm = TRUE),
    Min_EIP = min(EIPtime, na.rm = TRUE)
  ) %>%
  ungroup()  # Remove grouping

# print out a HTML table
nice_table(summary_gate_EIP)

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

# Q1a point plot (Fig3) Hu Score (revised 20240813)-------------------------------------------------

# Function to calculate standard error
standard_error <- function(x) {
  sd(x, na.rm = TRUE) / sqrt(length(na.omit(x)))
}

#summary data
summary_df_Q1a <- df_Q1a_Gfull_L1_NVV %>%
  group_by(ListenerLang, ItemType) %>%
  summarise(
    Mean_hu = mean(HuScore, na.rm = TRUE),
    SD = sd(HuScore, na.rm = TRUE)
  ) %>%
  ungroup()  

print(summary_df_Q1a)

# add values for error bar
summary_df_Q1a <- summary_df_Q1a %>%
  mutate(
    lower = Mean_hu - SD,
    upper = Mean_hu + SD
  )

# Change the level order of ListenerLang to make sure Chinese group appears first in the plot
summary_df_Q1a$ListenerLang <- factor(summary_df_Q1a$ListenerLang, 
                                      levels = c("Chinese", "Arabic"))

print(summary_df_Q1a)

nice_table(summary_df_Q1a)

# Ensure the interaction labels are correct
summary_df_Q1a$interaction_label <- interaction(summary_df_Q1a$ItemType, summary_df_Q1a$ListenerLang)

# Define the correct matching of shapes and colors
shape_values <- c("Speech.Chinese" = 17, "Vocalization.Chinese" = 17,
                  "Speech.Arabic" = 16, "Vocalization.Arabic" = 16)

color_values <- c("Speech.Chinese" = "#F8766D", "Vocalization.Chinese" = "#00BFC4",
                  "Speech.Arabic" = "#F8766D", "Vocalization.Arabic" = "#00BFC4")

P_1a_HuScore_Gfull <-
  ggplot(summary_df_Q1a,
         aes(
           x = ListenerLang,
           y = Mean_hu,
           color = interaction_label,  # Use interaction label for color
           shape = interaction_label   # Use interaction label for shape
         )) +
  geom_point(
    size = 3,  # Adjust the size of the points
    position = position_dodge(width = 0.5)
  ) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.5),
    width = 0.25
  ) +
  theme_minimal() +
  labs(
    title = "(3a)",
    x = "",
    y = "Mean HuScore",
    color = "Item Type x Listener Language",  # Legend title for the interaction
    shape = "Item Type x Listener Language"
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2),
    expand = expansion(mult = c(0, 0.05))  # Add a small expansion at the top
  ) +
  scale_shape_manual(values = shape_values) +  # Apply correct shapes
  scale_color_manual(values = color_values) +  # Apply correct colors
  theme(
    text = element_text(family = "Helvetica"),
    legend.position = "top",  # Position the legend on top
    legend.justification = "center",  # Center the legend horizontally
    axis.line = element_line(color = "black"),  # Add black axis lines
    axis.ticks = element_line(color = "black"),  # Add black axis ticks
    axis.ticks.length = unit(0.2, "cm"),  # Length of the ticks
    axis.text = element_text(color = "black"),  # Black axis labels
    axis.title = element_text(color = "black"),  # Black axis titles
    panel.grid = element_blank(),  # Remove grid lines for a clean look
    panel.spacing = unit(1, "cm"),  # Adjust spacing between panels
    strip.background = element_blank(),  # Remove default facet strip background
    strip.text = element_text(color = "black", size = 12)  # Customize facet labels
  ) 


# Print the plot
print(P_1a_HuScore_Gfull)

# save img
ggsave(
  "Q1a_bar_mean_HuScore_v2.svg",
  plot = P_1a_HuScore_Gfull,
  width = 10,
  height = 8,
  units = "in",
  dpi = 300,
  limitsize = FALSE
)

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


# model 3 -------------------------------------------------------------
# Q2a LMM model for Hu Score as a function of NVV vs. Gate by Mandarin L1 listeners ----------------------------------------------------
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
  summarize(mean=mean(Frequency, na.rm=TRUE),
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

# Q2a Hu score, connected pointed plot -----------------------------------------------

# Filter data (use row mean or emmeans)

# L1_Mandarin_VOC_gate_mean <- df_Q2a_L1_mandarin_VOC_gate %>%
#   filter(Gate %in% c("G200", "G400", "G500", "G600", "GFULL")) %>%
#   group_by(Gate,ItemEmotion) %>%
#   summarize(mean_HuScore = mean(HuScore, na.rm = TRUE), sd = sd(HuScore, na.rm = TRUE)) %>%
#   mutate(ItemEmotion = case_when(
#     ItemEmotion == "Happiness_laughter" ~ "Happiness-amusement",
#     ItemEmotion == "Happiness_pleasure" ~ "Happiness-pleasure",
#     TRUE ~ ItemEmotion
#   ))

summary_emmeans_Q2a <- as.data.frame(summary(Q2a_emm_mandarin_nvv_gate$emmeans))

nice_table(summary_emmeans_Q2a)

# create a new colomn to specify tick position
summary_emmeans_Q2a <- summary_emmeans_Q2a %>%
  mutate(Gate_pos = case_when(
    Gate == "G200" ~ 2,
    Gate == "G400" ~ 4,
    Gate == "G500" ~ 5,
    Gate == "G600" ~ 6,
    Gate == "GFULL" ~ 8  # Double the distance for "GFULL"
  ))

# Create last_points_Q2a with additional Gate_pos
last_points_Q2a <- data.frame(
  ItemEmotion = c("Anger", "Fear", "Happiness-amusement", "Happiness-pleasure", "Sadness"),
  emmean = c(0.634, 0.698, 0.559, 0.183, 0.755),
  Gate = factor("GFULL", levels = c("G200", "G400", "G500", "G600", "GFULL"))
)

# define color scale
ItemEmotion_color <- c("Sadness" = "#440154FF",          
                       "Fear" = "#31688EFF",             
                       "Anger" = "#35B779FF", 
                       "Happiness" = "#FF99CC",
                       "Happiness-amusement" = "#FF99CC",   
                       "Happiness-pleasure" = "#F9844A")

p_Q2a <-
  ggplot(
    summary_emmeans_Q2a,
    aes(
      x = Gate_pos,
      y = emmean,
      group = ItemEmotion,
      color = ItemEmotion,
      fill = ItemEmotion   # Ensure that fill matches the ItemEmotion
    )
  ) +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.1, color = NA) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.05) +
  geom_point(size = 5, shape = 17) +
  geom_path(arrow = arrow(length = unit(1.0, "mm"))) +
  labs(
    y = "Vocalization \n Unbiased hit rate (HuScore)", 
    title = "Chinese group"
  ) +
  geom_text(data = last_points_Q2a, 
            aes(x = Gate_pos, y = emmean, label = ItemEmotion), 
            vjust = -1, 
            hjust = 1, 
            check_overlap = TRUE,
            size = 5,
            family = "Helvetica") + 
  theme(panel.background = element_blank()) +
  theme(legend.position = "none") +
  theme(axis.title.x = element_blank(),  
        axis.title.y = element_text(vjust = 0.5, 
                                    hjust = 0.5, 
                                    size = 16,
                                    family = "Helvetica",
                                    angle = 90), 
        axis.line = element_line(color = "black", 
                                 size = 1.0),
        plot.title = element_text(size = 16, 
                                  hjust = 0.5,
                                  family = "Helvetica"),
        axis.ticks.length = unit(1.4, "mm"),
        axis.ticks = element_line(size = 1.0, 
                                  colour = "black"),
        axis.text.y = element_text(size = 16, 
                                   colour = "black",
                                   family = "Helvetica"),
        axis.text.x = element_text(size = 16, 
                                   colour = 'black',
                                   family = "Helvetica")) +
  scale_x_continuous(breaks = c(2, 4, 5, 6, 8), 
                     labels = c("G200", "G400", "G500", "G600", "GFULL"),
                     expand = expansion(mult = c(0.15, 0))) +
  scale_y_continuous(limits = c(-0.1, 1.0), 
                     breaks = seq(0, 1, by = 0.2),
                     expand = expansion(mult = c(0, 0))) + 
  scale_color_manual(values = ItemEmotion_color) +
  scale_fill_manual(values = ItemEmotion_color)  

print(p_Q2a)
# Display the plot
p_Q2a

ggsave("Q2a_NVV_Hu_Chi.svg", plot = p_Q2a, width = 10, height = 8, units = "in", dpi = 300, limitsize = FALSE)


# model 4 -------------------------------------------------------------
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

summary(LMM_Hu_Arabic_nvv_gate )
# write LMM results to a table in HTML format
tab_model(LMM_Hu_Arabic_nvv_gate, show.df = TRUE)

# F-test results for fixed effects
print(anova(LMM_Hu_Arabic_nvv_gate))


# Q2b t-tests -----------------------------------------------------------
Q2b_emm_Hu_Arabic_nvv_gate <- emmeans(LMM_Hu_Arabic_nvv_gate , pairwise ~ ItemEmotion|Gate,
                                    lmer.df = "satterthwaite", 
                                    lmerTest.limit = 625)  # Adjust pbkrtest.limit if needed
summary(Q2b_emm_Hu_Arabic_nvv_gate)

summary.Q2b.stats.table <- as.data.frame(summary(Q2b_emm_Hu_Arabic_nvv_gate))

contrasts_df_2b <- summary.Q2b.stats.table$contrasts

nice_table(contrasts_df_2b )

# Q2b connected point plot, VOC emotion, Arabic listeners, Hu Score -----------------------------------------------

# filter data (use row mean or emmeans)
# L1_Arabic_VOC_gate_mean <- df_Q2b_L1_Arabic_VOC_gate %>%
#   filter(Gate %in% c("G200", "G400", "G500", "G600", "GFULL")) %>%
#   group_by(Gate,ItemEmotion) %>%
#   mutate(ItemEmotion = case_when(
#     ItemEmotion == "Happiness_laughter" ~ "Happiness-amusement",
#     ItemEmotion == "Happiness_pleasure" ~ "Happiness-pleasure",
#     TRUE ~ ItemEmotion
#   ))%>%
#   summarize(mean_HuScore = mean(HuScore, na.rm = TRUE), sd = sd(HuScore, na.rm = TRUE))

summary_emmeans_Q2b <- as.data.frame(summary(Q2b_emm_Hu_Arabic_nvv_gate$emmeans))

# create a new colomn to specify tick position
summary_emmeans_Q2b  <- summary_emmeans_Q2b  %>%
  mutate(Gate_pos = case_when(
    Gate == "G200" ~ 2,
    Gate == "G400" ~ 4,
    Gate == "G500" ~ 5,
    Gate == "G600" ~ 6,
    Gate == "GFULL" ~ 8  # Double the distance for "GFULL"
  ))

last_points_Q2b <- data.frame(
  ItemEmotion = c("Anger", "Fear", "Sadness", "Happiness-amusement", "Happiness-pleasure"),
  emmean = c(0.75621366, 0.71333102, 0.81856648, 0.56574978, 0.2237),
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
    summary_emmeans_Q2b,
    aes(
      x = Gate_pos,
      y = emmean,
      group = ItemEmotion,
      color = ItemEmotion,
      fill = ItemEmotion
    )
  ) +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.1, color = NA) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.05) +
  geom_point(size = 5) +
  geom_path(arrow = arrow(length = unit(1.0, "mm"))) +
  labs(#x = "Gate Interval (ms)", 
       #y = "HuScore (vocalization - Arabic group)", 
       title = "Arabic group") +
  geom_text(data = last_points_Q2b, 
            aes(x = Gate_pos, label = ItemEmotion), 
            vjust = -1, 
            hjust = 1, 
            check_overlap = TRUE,
            size = 5) + 
  theme(panel.background = element_blank()) +
  theme(legend.position = "none") +
  theme(axis.title.x = element_blank(),  # Center x-axis label
        axis.title.y = element_blank(), # Center y-axis label
        axis.line = element_line(color = "black", 
                                 size = 1.0),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.ticks.length = unit(1.4,"mm"),
        axis.ticks = element_line(size = 1.0, 
                                  colour = "black"),
        axis.text.x = element_text(size = 16, colour = "black"),
        axis.text.y = element_text(size = 16, colour = "black")) +
  scale_x_continuous(breaks = c(2, 4, 5, 6, 8), 
                     labels = c("G200", "G400", "G500", "G600", "GFULL"),
                     expand = expansion(mult = c(0.15, 0))) +    # add padding only to the start of x-axis
  scale_y_continuous(limits = c(-0.1, 1.0), 
                     breaks = seq(0, 1, by = 0.2),
                     expand = expansion(mult = c(0, 0))) +   # Specify y-axis tick locations# Adjust tick length
  scale_color_manual(values = ItemEmotion_color) +
  scale_fill_manual(values = ItemEmotion_color) 

print(p_Q2b)

ggsave("Q2b_NVV_Hu_Arb.svg", plot = p_Q2b, width = 10, height = 8, units = "in", dpi = 300, limitsize = FALSE)


# model 5 ------------------------------------------------------------
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

Q2c_emm_Hu_Mandarin_speech_gate <- emmeans(LMM_Hu_Mandarin_speech_gate , pairwise ~ ItemEmotion|Gate,
                                  lmer.df = "satterthwaite", REML = FALSE,
                                  lmerTest.limit = 622)  # Adjust pbkrtest.limit if needed

summary(Q2c_emm_Hu_Mandarin_speech_gate)

summary.Q2c.stats.table <- as.data.frame(summary(Q2c_emm_Hu_Mandarin_speech_gate))

contrasts_df_2c <- summary.Q2c.stats.table$contrasts

nice_table(contrasts_df_2c)

# Q2c connected point plot-----------------------------------------------

# filter data (use raw mean or emmeans)
# L1_Mandarin_speech_gate_mean <- L1_Mandarin_speech_gate %>%
#   filter(Gate %in% c("G200", "G400", "G500", "G600", "GFULL")) %>%
#   group_by(Gate, ItemEmotion) %>%
#   summarize(mean_HuScore = mean(HuScore, na.rm = TRUE),
#             sd = sd(HuScore, na.rm = TRUE))

summary_emmeans_Q2c <- as.data.frame(summary(Q2c_emm_Hu_Mandarin_speech_gate$emmeans))

# create a new colomn to specify tick position
summary_emmeans_Q2c <- summary_emmeans_Q2c %>%
  mutate(Gate_pos = case_when(
    Gate == "G200" ~ 2,
    Gate == "G400" ~ 4,
    Gate == "G500" ~ 5,
    Gate == "G600" ~ 6,
    Gate == "GFULL" ~ 8  # Double the distance for "GFULL"
  ))

last_points_speech_Man <- data.frame(
  ItemEmotion = c("Anger", "Fear", "Sadness", "Happiness"),
  emmean = c(0.588, 0.568, 0.659, 0.611),
  Gate = "GFULL"  # Assuming the last point should be labeled at the GFULL gate
)

# Merge last_points_Q2a with L1_Mandarin_VOC_gate_mean to get Gate_pos
last_points_Q2c <- last_points_speech_Man  %>%
  mutate(Gate_pos = case_when(
    Gate == "GFULL" ~ 8.5
  ))

#plot
p_Q2c <-
  ggplot(
    summary_emmeans_Q2c,
    aes(
      x = Gate_pos,
      y = emmean,
      group = ItemEmotion,
      color = ItemEmotion,
      fill = ItemEmotion
    )
  ) +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.1, color = NA) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.05) +
  geom_point(size = 5, shape = 17) +
  geom_path(arrow = arrow(length = unit(1.0, "mm"))) +
  labs(x = "Gate duration", 
       y = "Native prosody \n unbiased hit rate (HuScore)") + 
       #title = "Figure 2c. HuScore as a function of native speech prosody and gate duration (Chinese)") +
  geom_text(data = last_points_Q2c, 
            aes(x = Gate_pos, label = ItemEmotion), 
            vjust = -1, 
            hjust = 1, 
            check_overlap = FALSE,
            size = 5) + 
  theme(panel.background = element_blank()) +
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(vjust = 1, 
                                     hjust = 0.5, 
                                     size = 16),  # Center x-axis label
        axis.title.y = element_text(vjust = 1, 
                                    hjust = 0.5, 
                                    size = 16), # Center y-axis label
        axis.line = element_line(color = "black", 
                                 size = 1.0),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.ticks.length = unit(1.4,"mm"),
        axis.ticks = element_line(size = 1,0, 
                                  colour = "black"),
        axis.text.x = element_text(size = 16, colour = "black"),
        axis.text.y = element_text(size = 16, colour = "black")) +
  scale_x_continuous(breaks = c(2, 4, 5, 6, 8), 
                     labels = c("G200", "G400", "G500", "G600", "GFULL"),
                     expand = expansion(mult = c(0.15, 0))) +
  scale_y_continuous(limits = c(-0.1, 1.0), 
                     breaks = seq(0, 1, by = 0.2),
                     expand = expansion(mult = c(0, 0))) +  # Specify y-axis tick locations# Adjust tick length
  scale_color_manual(values = ItemEmotion_color) +
  scale_fill_manual(values = ItemEmotion_color) 

print(p_Q2c)

ggsave("Q2c_Speech_Hu_Chi.svg", plot = p_Q2c,  width = 10, height = 8, units = "in", dpi = 300, limitsize = FALSE)

# Display the plot
#quartz(width=10, height=8)  # Adjust size as needed (for OS use only)


# model 6 -----------------------------------------------------------
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
Q2d_emm_Hu_Arabic_speech_gate <- emmeans(LMM_Hu_Arabic_speech_gate, pairwise ~ Gate|ItemEmotion,
                                       lmer.df = "satterthwaite", 
                                       lmerTest.limit = 622)  # Adjust pbkrtest.limit if needed
summary(Q2d_emm_Hu_Arabic_speech_gate )

summary.Q2d.stats.table <- as.data.frame(summary(Q2d_emm_Hu_Arabic_speech_gate))

contrasts_df_2d <- summary.Q2d.stats.table$contrasts

nice_table(contrasts_df_2d)

# Q2d connected point-----------------------------------------------

# filter data with raw mean or emmeans
# L1_Arabic_speech_gate_mean <- L1_Arabic_speech_gate %>%
#   filter(Gate %in% c("G200", "G400", "G500", "G600", "GFULL")) %>%
#   group_by(Gate, ItemEmotion) %>%
#   summarize(mean_HuScore = mean(HuScore, na.rm = TRUE),
#             sd = sd(HuScore, na.rm = TRUE))

summary_emmeans_Q2d <- as.data.frame(summary(Q2d_emm_Hu_Arabic_speech_gate$emmeans))

# create a new colomn to specify tick position
summary_emmeans_Q2d <- summary_emmeans_Q2d %>%
  mutate(Gate_pos = case_when(
    Gate == "G200" ~ 2,
    Gate == "G400" ~ 4,
    Gate == "G500" ~ 5,
    Gate == "G600" ~ 6,
    Gate == "GFULL" ~ 8  # Double the distance for "GFULL"
  ))


last_points_speech_arb <- data.frame(
  ItemEmotion = c("Anger", "Fear", "Sadness", "Happiness"),
  emmean = c(0.5228, 0.5848, 0.4429, 0.7131),
  Gate = "GFULL"  # Assuming the last point should be labeled at the GFULL gate
)


# Merge last_points_Q2a with L1_Mandarin_VOC_gate_mean to get Gate_pos
last_points_Q2d <- summary_emmeans_Q2d  %>%
  mutate(Gate_pos = case_when(
    Gate == "GFULL" ~ 8.2
  ))

#plot
p_Q2d <-
  ggplot(
    summary_emmeans_Q2d,
    aes(
      x = Gate_pos,
      y = emmean,
      group = ItemEmotion,
      color = ItemEmotion,
      fill = ItemEmotion
    )
  ) +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.1, color = NA) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.05) +
  geom_point(size = 5) +
  geom_path(arrow = arrow(length = unit(1.0, "mm"))) +
  labs(x = "Gate duration") + 
       #y = "HuScore (native speech prosody - Arabic group)", 
      # title = "Figure 2d. HuScore as a function of native speech prosody and gate duration (Arabic)") +
  geom_text(data = last_points_Q2d, 
            aes(x = Gate_pos, label = ItemEmotion), 
            vjust = -1, 
            hjust = 1, 
            check_overlap = TRUE,
            size = 5) + 
  theme(panel.background = element_blank()) +
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(vjust = 1, 
                                     hjust = 0.5, 
                                     size = 16), # Center x-axis label
        axis.title.y = element_blank(), # Center y-axis label
        axis.line = element_line(color = "black", 
                                 size = 1.0),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.ticks.length = unit(1.4,"mm"),
        axis.ticks = element_line(size = 1.0, 
                                  colour = "black"),
        axis.text.x = element_text(size = 16, colour = "black"),
        axis.text.y = element_text(size = 16, colour = "black")) +
  scale_x_continuous(breaks = c(2, 4, 5, 6, 8), 
                     labels = c("G200", "G400", "G500", "G600", "GFULL"),
                     expand = expansion(mult = c(0.15, 0))) +    # add padding only to the start of x-axis
  scale_y_continuous(limits = c(-0.1, 1.0), 
                     breaks = seq(0, 1, by = 0.2),
                     expand = expansion(mult = c(0, 0))) +   # Specify y-axis tick locations# Adjust tick length
  scale_color_manual(values = ItemEmotion_color) +
  scale_fill_manual(values = ItemEmotion_color) 

print(p_Q2d)

ggsave("Q2d_Speech_Hu_Arb.svg", plot = p_Q2d, width = 10, height = 8, units = "in", dpi = 300, limitsize = FALSE)

# figure 1 (1--combine) -----------------------------------------------

combined_figure1 <- (p_Q2a | p_Q2b) / 
  (p_Q2c | p_Q2d) +
  plot_layout(guides = 'collect') &
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2))

print(combined_figure1)

ggsave("Figure2_combine.svg", plot = combined_figure1, width = 7000, height = 5000, units = "px", dpi = 300, limitsize = FALSE)



# model 9 ------------------------------------------------------------
# Q4a LMM model for Hu Score as a function of Speech Familiarity (Mandarin) ---------------------------------------------------------
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
Q4a_emm_Q4a_all_speech_man <- emmeans(LMM_Q4a_all_speech_gate_man, revpairwise ~ Gate|ItemToListener,
                                      lmer.df = "satterthwaite", 
                                      lmerTest.limit = 1500)  # Adjust pbkrtest.limit if needed

summary(Q4a_emm_Q4a_all_speech_man)
summary.Q4a.stats.table <- as.data.frame(summary(Q4a_emm_Q4a_all_speech_man))
contrasts_df_4a <- as.data.frame(summary.Q4a.stats.table$contrasts)
nice_table(contrasts_df_4a)

# Q4a plot  (Fig5) HuScore connected point (using emmeans 20240813) ----------------------------------------------------------------

# Man_speech_gate_mean <- df_Q4a_all_speech_gate_man %>%
#   filter(Gate %in% c("G200", "G400", "G500", "G600", "GFULL")) %>%
#   group_by(Gate,ItemToListener) %>%
#   summarize(mean_HuScore = mean(HuScore, na.rm = TRUE), sd = sd(HuScore, na.rm = TRUE)) %>%
#   mutate(Gate_pos = case_when(
#   Gate == "G200" ~ 2,
#   Gate == "G400" ~ 4,
#   Gate == "G500" ~ 5,
#   Gate == "G600" ~ 6,
#   Gate == "GFULL" ~ 8  # Double the distance for "GFULL"
# ))

# filter data
summary_emmeans_Q4a <- as.data.frame(summary(Q4a_emm_Q4a_all_speech_man$emmeans))

# create a new colomn to specify tick position
summary_emmeans_Q4a <- summary_emmeans_Q4a %>%
  mutate(Gate_pos = case_when(
    Gate == "G200" ~ 2,
    Gate == "G400" ~ 4,
    Gate == "G500" ~ 5,
    Gate == "G600" ~ 6,
    Gate == "GFULL" ~ 8  # Double the distance for "GFULL"
  ))


last_points_Q4a <- summary_emmeans_Q4a %>%
  mutate(Gate_pos = case_when(
    Gate == "GFULL" ~ 8.2
  ))
  

Q4a_legend_labels <- c("L1", "L2", "Foreign")


Speech_color <- c(
  "L1" = "#fc9272",
  "L2" = "#6baed6",
  "Foreign" = "#800080"
)


p_Q4a <- ggplot(summary_emmeans_Q4a, 
                aes(x = Gate_pos, 
                    y = emmean, 
                    group = ItemToListener,
                    color = ItemToListener,
                    fill = ItemToListener)) +
  geom_point(size = 5,shape = 17) +
  geom_path(arrow = arrow(length = unit(0.3, "mm"))) +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.1, color = NA) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.05) +
  labs(x = "Gate duration", 
       y = "HuScore", title = "(4a) Chinese") +
  geom_text(data = last_points_Q4a, 
            aes(x = Gate_pos, label = ItemToListener), 
            vjust = -1, 
            hjust = 1, 
            check_overlap = TRUE,
            size = 5) + 
  theme(panel.background = element_blank()) +
  theme(legend.position = "none") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), # Center y-axis label
        plot.title = element_text(size = 16, hjust = 0),
        axis.line = element_line(color = "black", 
                                 size = 1.0),
        axis.ticks.length = unit(1.4,"mm"),
        axis.ticks = element_line(size = 1.0, 
                                  colour = "black"),
        axis.text.x = element_text(size = 16, colour = "black"),
        axis.text.y = element_text(size = 16, colour = "black")) +
  scale_x_continuous(breaks = c(2, 4, 5, 6, 8), 
                     labels = c("G200", "G400", "G500", "G600", "GFULL"),
                     expand = expansion(mult = c(0.15,0))) +
  scale_y_continuous(limits = c(-0.1, 1.0), 
                     breaks = seq(0, 1, by = 0.2),
                     expand = expansion(mult = c(0, 0))) + 
  scale_color_manual(values = Speech_color) +
  scale_fill_manual(values = Speech_color)

# Print the plot
print(p_Q4a)

# Save the plot as a square image
ggsave("Q5a_Chinese.svg", plot = p_Q4a, width = 10, height = 8, units = "in", dpi = 300, limitsize = FALSE)


# model 10 ------------------------------------------------------------
# Q4b LMM model for Hu Score as a function of Speech Familiarity (Arabic)  ---------------------------------------------------------

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

# Q4b plot (Fig5) HuScore connected point ----------------------------------------------------------------

# arb_speech_gate_mean <- df_Q4b_all_speech_gate_arb %>%
#   filter(Gate %in% c("G200", "G400", "G500", "G600", "GFULL")) %>%
#   group_by(Gate,ItemToListener) %>%
#   summarize(mean_HuScore = mean(HuScore, na.rm = TRUE), sd = sd(HuScore, na.rm = TRUE)) %>%
#   mutate(Gate_pos = case_when(
#     Gate == "G200" ~ 2,
#     Gate == "G400" ~ 4,
#     Gate == "G500" ~ 5,
#     Gate == "G600" ~ 6,
#     Gate == "GFULL" ~ 8  # Double the distance for "GFULL"
#   ))

# filter data
summary_emmeans_Q4b <- as.data.frame(summary(Q4b_emm_all_speech_arb$emmeans))

# create a new colomn to specify tick position
summary_emmeans_Q4b <- summary_emmeans_Q4b %>%
  mutate(Gate_pos = case_when(
    Gate == "G200" ~ 2,
    Gate == "G400" ~ 4,
    Gate == "G500" ~ 5,
    Gate == "G600" ~ 6,
    Gate == "GFULL" ~ 8  # Double the distance for "GFULL"
  ))


Speech_color <- c(
  "L1" = "#fc9272",
  "L2" = "#6baed6",
  "Foreign" = "#800080"
)

last_points_Q4b <- data.frame(
  ItemToListener= c("L1", "L2", "Foreign"),
  mean_HuScore = c(0.559, 0.546, 0.530),
  Gate_pos = c(8.3, 8.3, 8.4),
  Gate = "GFULL"  # Assuming the last point should be labeled at the GFULL gate
)

Q4b_legend_labels <- c("L1", "L2", "Foreign")

# plot
p_Q4b <- ggplot(
  summary_emmeans_Q4b,
  aes(
    x = Gate_pos,
    y = emmean,
    group = ItemToListener,
    color = ItemToListener,
    fill = ItemToListener
  )
) +
  geom_point(size = 5) +
  geom_path(arrow = arrow(length = unit(0.3, "mm"))) +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.1, color = NA) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.05) +
  labs(x = "Gate Interval (ms)", y = "HuScore", title = "(4b) Arab") +
  geom_text(
    data = last_points_Q4b,
    aes(x = Gate_pos, y = mean_HuScore, label = ItemToListener),
    vjust = -1,
    hjust = 1,
    check_overlap = FALSE
  ) +
  theme(panel.background = element_blank()) +
  theme(legend.position = "none") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    # Center y-axis label
    plot.title = element_text(size = 16, hjust = 0),
    axis.line = element_line(color = "black", size = 1.0),
    axis.ticks.length = unit(1.4, "mm"),
    axis.ticks = element_line(size = 1.0, colour = "black"),
    axis.text.x = element_text(size = 16, colour = "black"),
    axis.text.y = element_text(size = 16, colour = "black")
  ) +
  scale_x_continuous(
    breaks = c(2, 4, 5, 6, 8),
    labels = c("G200", "G400", "G500", "G600", "GFULL"),
    expand = expansion(mult = c(0.15, 0))
  ) +
  scale_y_continuous(
    limits = c(-0.1, 1.0),
    breaks = seq(0, 1, by = 0.2),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_color_manual(values = Speech_color) +
  scale_fill_manual(values = Speech_color)

# Print the plot
print(p_Q4b)

# Save the plot as a square image
ggsave("Q5b_Arab.svg", plot = p_Q4b, width = 10, height = 8, units = "in", dpi = 300, limitsize = FALSE)

# model 11 -----------------------------------------------------------
# Q4a-b_combine Hu Score -----------------------------------------------------------

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
Q4a_emm_Q4a_all_speech_man_arb <- emmeans(LMM_Q4a_all_speech_gate_man_arb, revpairwise ~ ListenerLang|ItemLang,
                                          lmer.df = "satterthwaite", 
                                          lmerTest.limit = 1500)  # Adjust pbkrtest.limit if needed

summary(Q4a_emm_Q4a_all_speech_man_arb)
summary.Q4ab.stats.table <- as.data.frame(summary(Q4a_emm_Q4a_all_speech_man_arb))
contrasts_df_4ab <- as.data.frame(summary.Q4ab.stats.table$contrasts)
nice_table(contrasts_df_4ab)

# new EIP analysis ------------------------------------------------------
# Attention, go back to line 83-130 to check how the data is cleaned and handled for followup models of EIPS.
# old Model 12, model 8, model 7, model 2 used the median EIP as DV without adding Gfull duration as covariate
# following models used mean EIP to with Gfull duration added in the model as a covariate
# model 13 (new model relevant to model 2 for Q1b )-----------------------------------------------------------
# Q1b LMM model for EIP (mean EIP) as function of L1 vs. Speech type (Gfull gate duration as covariate)   -------------------------------

# filler  EIP data for LMM model with Gfull duration. 
# Attention, please go back to line 83-130 to check how the data is cleaned and handled for followup models of EIPS.
# Model 12, model 8, model 7, model 2 used the median EIP as DV without adding Gfull duration as covariate  (they are the old models)

df_Q1b_EIP_listener_L1_VOC_duration <- gate_EIP_drop_pleasure %>%
  filter(ItemToListener != "Foreign", ItemToListener != "L2") %>%
  droplevels() %>%
  group_by(ListenerLang, ItemToListener, EIP) %>%
  mutate(ItemType = case_when(ItemType == "utterance" ~ 'Speech',
                              ItemType == "vocalization" ~ "Vocalization",
                              TRUE ~ ItemType)) %>%
  mutate(ListenerLang = case_when(ListenerLang == "Mandarin" ~ "Chinese",
                                  TRUE ~ ListenerLang))

# new LMM model using mean value of EIP as DV, and add duration of Gfull stimuli as a covariate 
Q1_LMM_mean_EIP_L1_VOC_duration  <- lmer(
  EIPtime ~ (ListenerLang + ItemType)^2 + FullDuration_ms +
    (1 | ListenerID) + (1 | ItemEmotion), REML = FALSE,
  data = df_Q1b_EIP_listener_L1_VOC_duration
)

# write LMM results to a table in HTML format
tab_model(Q1_LMM_mean_EIP_L1_VOC_duration, show.df = TRUE)

# f-test
print(anova(Q1_LMM_mean_EIP_L1_VOC_duration))

# Q1b t-test (pairwise comparison) Estimated marginal means ------------------------------------------------------------------
Q1b_emm_EIP_mean_duration <- emmeans(Q1_LMM_mean_EIP_L1_VOC_duration, revpairwise ~ ItemType|ListenerLang, adjust="tukey",
                       lmer.df = "satterthwaite", 
                       lmerTest.limit = 5283)  # Adjust pbkrtest.limit if needed
summary(Q1b_emm_EIP_mean_duration)

summary.Q1b.stats.table_duration <- as.data.frame(summary(Q1b_emm_EIP_mean_duration))

contrasts_df_1b_mean_duration <- summary.Q1b.stats.table_duration$contrasts

nice_table(contrasts_df_1b_mean_duration) # sig for both directions

# (Fig 4)plot emmeans huScore for Q1b ------------------------------------------------------------

summary_emmeans_Q1b <- as.data.frame(summary(Q1b_emm_EIP_mean_duration$emmeans))

nice_table(summary_emmeans_Q1b)

summary_emmeans_Q1b$ListenerLang <- factor(
  summary_emmeans_Q1b$ListenerLang,
  levels = c("Chinese", "Arabic")  # Ensure "Chinese" comes before "Arabic"
)

# filter EMMs Chinese group
summary_emmeans_Q1b_man <- summary_emmeans_Q1b %>%
  filter(summary_emmeans_Q1b$ListenerLang == "Chinese")


# plot EMMs Chinese group
P_emmeans_Q1b_man <- ggplot(summary_emmeans_Q1b_man,
                        aes(x = ItemType, y = emmean, fill = ItemType)) +
  geom_bar(
    stat = "identity",
    position = position_dodge(width = 0.8),
    width = 0.5
  ) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.25,
    position = position_dodge(width = 0.8)
  ) +
  labs(x = "Event Type", y = "Estimated Marginal Mean", title = "Fig4") +
  theme_minimal() +
  scale_y_continuous(
    limits = c(0, 1200),
    breaks = seq(0, 1200, by = 100),
    expand = expansion(mult = c(0, 0.05))  # Add a small expansion at the top
  ) +
  theme(
    text = element_text(family = "Helvetica"),
    legend.position = "none",
    axis.line = element_line(color = "black"),  # Add black axis lines
    axis.ticks = element_line(color = "black"),  # Add black axis ticks
    axis.ticks.length = unit(0.2, "cm"),  # Length of the ticks
    axis.text = element_text(color = "black"),  # Black axis labels
    axis.title = element_text(color = "black"),  # Black axis titles
    panel.grid = element_blank(),  # Remove grid lines for a clean look
    panel.spacing = unit(1, "cm"),  # Adjust spacing between panels
    strip.background = element_blank(),  # Remove default facet strip background
    strip.text = element_text(color = "black", size = 12)  # Customize facet labels
  ) +
  facet_wrap(~ ListenerLang) +  # Facet the plot by ListenerLang
  coord_flip()  # Flip the coordinates to make the bars horizontal

print(P_emmeans_Q1b_man)

ggsave("Q1b_bar_mean_EIP_man.svg", plot = P_emmeans_Q1b_man, width = 10, height = 8, units = "in", dpi = 300, limitsize = FALSE)

# plot EMMs Chinese group

# filter EMMs Arab group
summary_emmeans_Q1b_Arab <- summary_emmeans_Q1b %>%
  filter(summary_emmeans_Q1b$ListenerLang == "Arabic")


# plot EMMs Arab group
P_emmeans_Q1b_Arab <- ggplot(summary_emmeans_Q1b_Arab,
                            aes(x = ItemType, y = emmean, fill = ItemType)) +
  geom_bar_pattern(
    stat = "identity",
    position = position_dodge(width = 0.8),
    width = 0.5
  ) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.25,
    position = position_dodge(width = 0.8)
  ) +
  labs(x = "Event Type", y = "Estimated Marginal Mean", title = "Fig4-Arab") +
  theme_minimal() +
  scale_y_continuous(
    limits = c(0, 1200),
    breaks = seq(0, 1200, by = 100),
    expand = expansion(mult = c(0, 0.05))  # Add a small expansion at the top
  ) +
  theme(
    text = element_text(family = "Helvetica"),
    legend.position = "none",
    axis.line = element_line(color = "black"),  # Add black axis lines
    axis.ticks = element_line(color = "black"),  # Add black axis ticks
    axis.ticks.length = unit(0.2, "cm"),  # Length of the ticks
    axis.text = element_text(color = "black"),  # Black axis labels
    axis.title = element_text(color = "black"),  # Black axis titles
    panel.grid = element_blank(),  # Remove grid lines for a clean look
    panel.spacing = unit(1, "cm"),  # Adjust spacing between panels
    strip.background = element_blank(),  # Remove default facet strip background
    strip.text = element_text(color = "black", size = 12)  # Customize facet labels
  ) +
  coord_flip()  # Flip the coordinates to make the bars horizontal

print(P_emmeans_Q1b_Arab)

ggsave("Q1b_bar_mean_EIP_Arab.svg", plot = P_emmeans_Q1b_Arab, width = 10, height = 8, units = "in", dpi = 300, limitsize = FALSE)


# model 14 (new model relevant for model 7 for Q3a) -----------------------------
# Q3a LMM for EIP (mean EIP) as a function of L1 background and NVV ItemEmotion at Gfull -----------------------------------------------------------
# Vocalizations: Listener (MAND, ARAB) x Emotion ((ANG, FER, SAD, HAP) drop HAP-Pleasure)
# filter mean EIP data for Q3a

df_Q3a_L1_VOC_mean_EIP_duration <- gate_EIP_drop_pleasure  %>%
  filter(ItemToListener != "Foreign", ItemToListener != "L2", ItemType != "utterance") %>%
  droplevels() %>%
  group_by(ListenerLang, ItemToListener, EIP)

# LMM model using median value df (sig better model)
LMM_Q3a_EIP_VOC_L1_duration  <- lmer(
  EIPtime ~ (ListenerLang + ItemEmotion)^2 + FullDuration_ms +
    (1 | ListenerID), REML = FALSE,
  data = df_Q3a_L1_VOC_mean_EIP_duration  
)

# model comparison
LMM_Q3a_EIP_VOC_L1_duration_try  <- lmer(
  EIPtime ~ (ListenerLang + ItemEmotion)^2 + 
    (1 | ListenerID), REML = FALSE,
  data = df_Q3a_L1_VOC_mean_EIP_duration  
)

anova(LMM_Q3a_EIP_VOC_L1_duration, LMM_Q3a_EIP_VOC_L1_duration_try)

# # Generate predictions including the covariate
# # Use base R to select unique rows
# newdata <- df_Q3a_L1_VOC_mean_EIP_duration[ , c("ListenerLang", "ItemEmotion", "FullDuration_s")]
# newdata <- unique(newdata)
# 
# # Predict EIP values including the covariate
# newdata$predicted_EIP <- predict(LMM_Q3a_EIP_VOC_L1_duration, newdata = newdata, re.form = NA)
# 
# # Merge predictions with the original summary data
# Q3a_meanEIP_predict <- df_Q3a_L1_VOC_mean_EIP_duration %>%
#   left_join(newdata, by = c("ListenerLang", "ItemEmotion", "FullDuration_s"))

# write LMM results to a table in HTML format
tab_model(LMM_Q3a_EIP_VOC_L1_duration, show.df = TRUE)

# Perform the F-test
print(anova(LMM_Q3a_EIP_VOC_L1_duration))

# Q3a t-tests -------------------------------------------------------------
Q3a_emm_EIP_VOC_L1_duration <- emmeans(LMM_Q3a_EIP_VOC_L1_duration, pairwise ~ ItemEmotion|ListenerLang, methods ="turkey",
                              lmer.df = "satterthwaite", 
                              lmerTest.limit = 1754)  # Adjust pbkrtest.limit if needed

summary(Q3a_emm_EIP_VOC_L1_duration)
summary.Q3a.stats.table_duration <- as.data.frame(summary(Q3a_emm_EIP_VOC_L1_duration ))
contrasts_df_3a_duration <- as.data.frame(summary.Q3a.stats.table_duration$contrasts)
nice_table(contrasts_df_3a_duration)

# (Fig 2ab)plot emmeans results Q3a ----------------------------------------------------------- 
summary_emmeans_Q3a <- as.data.frame(summary(Q3a_emm_EIP_VOC_L1_duration$emmeans))

# plot EMMs of Chinese group
summary_emmeans_Q3a_Chinese <- summary_emmeans_Q3a %>%
  filter(ListenerLang != "Arabic") %>%
  mutate(ItemEmotion = factor(ItemEmotion, levels = c("Sadness","Happiness", "Fear", "Anger")))

nice_table(summary_emmeans_Q3a)

# plot EMMs Chinese group
emmeans_Q3a_Chinese <- ggplot(summary_emmeans_Q3a_Chinese,
                           aes(x = ItemEmotion, y = emmean, fill = ItemEmotion)) +
  geom_bar(stat = "identity",
                   position = position_dodge(width = 0.8),
                   width = 0.5) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.25,
    position = position_dodge(width = 0.8)
  ) +
  labs(x = "Vocalization", 
       y = "Estimated Marginal Mean", 
       title = "(a) Chinese group") +
  theme_classic() +
  scale_fill_manual(
    values = c(
      "Sadness" = "#440154FF",
      "Happiness" = "#F9C",
      "Fear" = "#31688EFF",
      "Anger" = "#35B779FF"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 1200), 
    breaks = seq(0, 1200, by = 100),
    expand = expansion(mult = c(0, 0.05))  # Add a small expansion at the top
  ) +
  
  theme(
    text = element_text(family = "Helvetica"),
    legend.position = "none",  # Remove the legend
    axis.line = element_line(color = "black"),  # Add black axis lines
    axis.ticks = element_line(color = "black"),  # Add black axis ticks
    axis.ticks.length = unit(0.2, "cm"),  # Length of the ticks
    axis.text = element_text(color = "black"),  # Black axis labels
    axis.title = element_text(color = "black"),  # Black axis titles
    panel.grid.major = element_line(color = "grey80", size = 0.5),  # Remove grid lines if you want a clean look
    panel.grid.minor = element_blank() # Add a border around the plot
  ) +
  coord_flip()  # Flip the coordinates to make the bars horizontal

print(emmeans_Q3a_Chinese)

ggsave("Q3a_bar_mean_EIP_Chinese_v2.svg", plot = emmeans_Q3a_Chinese, width = 10, height = 8, units = "in", dpi = 300, limitsize = FALSE)

# plot EMMs Arab group
summary_emmeans_Q3a_Arab <- summary_emmeans_Q3a %>%
  filter(ListenerLang != "Mandarin") %>%
  mutate(ItemEmotion = factor(ItemEmotion, levels = c("Sadness","Happiness", "Fear", "Anger")))

# plot EMMs Arab group
p_emmeans_Q3a_Arab <- ggplot(summary_emmeans_Q3a_Arab,
                           aes(x = ItemEmotion, y = emmean, fill = ItemEmotion)) +
  geom_bar_pattern(stat = "identity",
                   position = position_dodge(width = 0.8),
                   width = 0.5) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.25,
    position = position_dodge(width = 0.8)
  ) +
  labs(x = element_blank(), 
       y = "Estimated Marginal Mean", 
       title = "(b) Arab group") +
  theme_minimal() +
  scale_fill_manual(
    values = c(
      "Sadness" = "#440154FF",
      "Happiness" = "#F9C",
      "Fear" = "#31688EFF",
      "Anger" = "#35B779FF"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 1200), 
    breaks = seq(0, 1200, by = 100),
    expand = expansion(mult = c(0, 0.05))  # Add a small expansion at the top
  ) +
  
  theme(
    text = element_text(family = "Helvetica"),
    legend.position = "none",  # Remove the legend
    axis.line = element_line(color = "black"),  # Add black axis lines
    axis.ticks = element_line(color = "black"),  # Add black axis ticks
    axis.ticks.length = unit(0.2, "cm"),  # Length of the ticks
    axis.text = element_text(color = "black"),  # Black axis labels
    axis.title = element_text(color = "black"),  # Black axis titles
    panel.grid.major  = element_line(colour = "grey80", size = 0.5),  # Remove grid lines if you want a clean look
    panel.grid.minor  = element_blank()  
  ) +
  coord_flip()  # Flip the coordinates to make the bars horizontal


print(p_emmeans_Q3a_Arab)

ggsave("Q3a_bar_mean_EIP_Arab_v2.svg", plot = p_emmeans_Q3a_Arab, width = 10, height = 8, units = "in", dpi = 300, limitsize = FALSE)

# # Plot pairwise comparisons
# summary_contrasts_Q3a <- as.data.frame(summary(Q3a_emm_EIP_VOC_L1_duration$contrasts))
# Calculate confidence intervals
# summary_contrasts_Q3a$lower.CL <- summary_contrasts_Q3a$estimate - 1.96 * summary_contrasts_Q3a$SE
# summary_contrasts_Q3a$upper.CL <- summary_contrasts_Q3a$estimate + 1.96 * summary_contrasts_Q3a$SE
# ggplot(summary_contrasts_Q3a, aes(x = contrast, y = estimate, color = p.value < 0.05)) +
#   geom_point(size = 3) +
#   geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
#   labs(x = "Contrast", y = "Estimate", title = "Pairwise Comparisons of Item Emotions") +
#   scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black"), labels = c("Significant", "Not Significant")) +
#   facet_wrap(~ ListenerLang) +  # Facet by ListenerLang
#   theme_minimal()


# model 15 (new model relevant for model 8 for Q3b)-----------------------------------------------------------
# Q3b LMM for EIP (mean EIP) of Speech utterance as a function of L1 background vs.ItemEmotion  -----------------------------------------------------------

# clean EIP data for Gfull gate 
df_Q3b_L1_Speech_meanEIP_duration <- gate_EIP_drop_pleasure %>%
  filter(ItemToListener != "Foreign", ItemToListener != "L2", ItemType != "vocalization") %>%
  droplevels() %>%
  group_by(ListenerLang, ItemToListener, EIP)

# LMM model (median)
LMM_Q3b_L1_Speech_EIP_mean_duration  <- lmer(
  EIPtime ~ (ListenerLang + ItemEmotion) ^ 2 + FullDuration_ms +
    (1 | ListenerID),
  REML = FALSE,
  data = df_Q3b_L1_Speech_meanEIP_duration)

# write LMM results to a table in HTML format
tab_model(LMM_Q3b_L1_Speech_EIP_mean_duration, show.df = TRUE)

# f-test
print(anova(LMM_Q3b_L1_Speech_EIP_mean_duration))

# Q3b t-tests -------------------------------------------------------------
Q3b_emm_L1_speech_duration <- emmeans(LMM_Q3b_L1_Speech_EIP_mean_duration, pairwise ~ ListenerLang|ItemEmotion,
                             lmer.df = "satterthwaite", 
                             lmerTest.limit = 1764)  # Adjust pbkrtest.limit if needed

summary.Q3b.stats.table_duration <- summary(Q3b_emm_L1_speech_duration)

summary.Q3b.stats.table_duration <- as.data.frame(summary(Q3b_emm_L1_speech_duration))

contrasts_df_3b_duration <- as.data.frame(summary.Q3b.stats.table_duration$contrast)

nice_table(contrasts_df_3b_duration)

# (Fig 2cd)plot emmeans results Q3b ----------------------------------------------------- 

summary_emmeans_Q3b <- as.data.frame(summary(Q3b_emm_L1_speech_duration$emmeans))

# plot EMMs of Chinese group
summary_emmeans_Q3b_Chinese <- summary_emmeans_Q3b %>%
  filter(ListenerLang != "Arabic") %>%
  mutate(ItemEmotion = factor(ItemEmotion, levels = c("Sadness","Happiness", "Fear", "Anger")))

nice_table(summary_emmeans_Q3b)

# plot EMMs Chinese group
emmeans_Q3b_Chinese <- ggplot(summary_emmeans_Q3b_Chinese,
                           aes(x = ItemEmotion, y = emmean, fill = ItemEmotion)) +
  geom_bar(stat = "identity",
                   position = position_dodge(width = 0.8),
                   width = 0.5) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.25,
    position = position_dodge(width = 0.8)
  ) +
  labs(x = "Native prosody", 
       y = "Estimated Marginal Mean", 
       title = "(c) Chinese group") +
  theme_minimal() +
  scale_fill_manual(
    values = c(
      "Sadness" = "#440154FF",
      "Happiness" = "#F9C",
      "Fear" = "#31688EFF",
      "Anger" = "#35B779FF"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 1200), 
    breaks = seq(0, 1200, by = 100),
    expand = expansion(mult = c(0, 0.05))  # Add a small expansion at the top
  ) +
  theme(
    text = element_text(family = "Helvetica"),
    legend.position = "none",  # Remove the legend
    axis.line = element_line(color = "black"),  # Add black axis lines
    axis.ticks = element_line(color = "black"),  # Add black axis ticks
    axis.ticks.length = unit(0.2, "cm"),  # Length of the ticks
    axis.text = element_text(color = "black"),  # Black axis labels
    axis.title = element_text(color = "black"),  # Black axis titles
    panel.grid.major = element_line(colour = "grey80", size = 0.5),  # Remove grid lines if you want a clean look
    panel.grid.minor= element_blank()  # Add a border around the plot
  ) +
  coord_flip()  # Flip the coordinates to make the bars horizontal

print(emmeans_Q3b_Chinese)

ggsave("Q3b_bar_mean_EIP_Chinese_v2.svg", plot = emmeans_Q3b_Chinese, width = 10, height = 8, units = "in", dpi = 300, limitsize = FALSE)

# plot EMMs Arab group
summary_emmeans_Q3b_Arab <- summary_emmeans_Q3b %>%
  filter(ListenerLang != "Mandarin") %>%
  mutate(ItemEmotion = factor(ItemEmotion, levels = c("Sadness","Happiness", "Fear", "Anger")))

# plot EMMs Arab group
emmeans_Q3b_Arab <- ggplot(summary_emmeans_Q3b_Arab,
                           aes(x = ItemEmotion, y = emmean, fill = ItemEmotion)) +
  geom_bar_pattern(stat = "identity",
                   position = position_dodge(width = 0.8),
                   width = 0.5) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.25,
    position = position_dodge(width = 0.8)
  ) +
  labs(x = element_blank(), 
       y = "Estimated Marginal Mean", 
       title = "(d) Arab group") +
  theme_minimal() +
  scale_fill_manual(
    values = c(
      "Sadness" = "#440154FF",
      "Happiness" = "#F9C",
      "Fear" = "#31688EFF",
      "Anger" = "#35B779FF"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 1200), 
    breaks = seq(0, 1200, by = 100),
    expand = expansion(mult = c(0, 0.05))  # Add a small expansion at the top
  ) +
  coord_flip() + # Flip the coordinates to make the bars horizontal
  theme(
    text = element_text(family = "Helvetica"),
    legend.position = "none",  # Remove the legend
    axis.line = element_line(color = "black"),  # Add black axis lines
    axis.ticks = element_line(color = "black"),  # Add black axis ticks
    axis.ticks.length = unit(0.2, "cm"),  # Length of the ticks
    axis.text = element_text(color = "black"),  # Black axis labels
    axis.title = element_text(color = "black"),  # Black axis titles
    panel.grid.major  = element_line(colour = "grey80", size = 0.5),  # Remove grid lines if you want a clean look
    panel.grid.minor = element_blank()  # Add a border around the plot
  )

print(emmeans_Q3b_Arab)

ggsave("Q3b_bar_mean_EIP_Arab_v2.svg", plot = emmeans_Q3b_Arab, width = 10, height = 8, units = "in", dpi = 300, limitsize = FALSE)


# combine Figure 2 -----------------------------------------------------------
combined_figure2 <- (emmeans_Q3a_Chinese | p_emmeans_Q3a_Arab) / 
  (emmeans_Q3b_Chinese | emmeans_Q3b_Arab) +
  plot_layout(guides = 'collect') &
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2))

print(combined_figure2)

ggsave("Figure2_combine_v2.svg", plot = combined_figure2, width = 7000, height = 5000, units = "px", dpi = 300, limitsize = FALSE)

# model 16 (new model relevant for model 12 for Q4c) ---------------------------------------
# Q4c LMM for EIP (mean EIP) as a function of L1 background, event language collapsed on ItemEmotion  -----------------------------------------------------------
# Listener (MAND, ARAB) x Emotion , Event (L1, L2, Foreign)

# filter EIP data for Q4C
df_Q4c_all_speech_meanEIP_duration <- gate_EIP_drop_pleasure %>%
  filter(ItemToListener != "vocalization", ItemType != "vocalization") %>%
  droplevels() %>%
  group_by(ListenerLang, ItemToListener, ItemEmotion)


# LMM model with median EIP value (new collapsed on emotion types)
LMM_Q4c_all_speech_ListLan_meanEIP_duration  <- lmer(
  EIPtime ~ (ListenerLang + ItemToListener)^2 + FullDuration_ms +
    (1 | ListenerID) + (1 | ItemEmotion), REML = FALSE,
  data = df_Q4c_all_speech_meanEIP_duration 
)

# write LMM results to a table in HTML format
tab_model(LMM_Q4c_all_speech_ListLan_meanEIP_duration, show.df = TRUE)

# f-test
print(anova(LMM_Q4c_all_speech_ListLan_meanEIP_duration))

# Q4c t-tests -------------------------------------------------------------
Q4c_emm_meanEIP_all_speech <- emmeans(LMM_Q4c_all_speech_ListLan_meanEIP_duration, revpairwise ~ ListenerLang|ItemToListener, methods ="turkey",
                                  lmer.df = "satterthwaite", 
                                  lmerTest.limit = 4779)  # Adjust pbkrtest.limit if needed

summary(Q4c_emm_meanEIP_all_speech)

summary.Q4c.stats.table_duration <- as.data.frame(summary(Q4c_emm_meanEIP_all_speech))

contrasts_df_4c_duration <- as.data.frame(summary.Q4c.stats.table_duration$contrasts)

nice_table(contrasts_df_4c_duration)

# try to use pwpm function in the emmeans pkg

pwpm(Q4c_emm_meanEIP_all_speech)

plot_emm_Q4c <- plot(Q4c_emm_meanEIP_all_speech, comparisons = TRUE)

plot_emm_Q4c 

pwpp(Q4c_emm_meanEIP_all_speech) +
  geom_vline(xintercept = 0.05, linetype = 2, color = 'red',show.legend = NA)

# try to use emmip function 
# Define the levels for the factors you want to visualize
mylist <- list(ItemToListener = c("Foreign", "L1", "L2"),
               ListenerLang = c("Arabic", "Mandarin"))

# Create the interaction plot using your model
interaction_plot <- emmip(LMM_Q4c_all_speech_ListLan_meanEIP_duration, 
                          ItemToListener ~ ListenerLang, 
                          at = mylist, 
                          CIs = TRUE)

interaction_plot

# (Figure 5c) plot emmeans results of Q4c --------------------------------------------------

summary_emmeans_Q4c <- as.data.frame(summary(Q4c_emm_meanEIP_all_speech$emmeans))

summary_emmeans_Q4c_man <- summary_emmeans_Q4c %>%
  filter(ListenerLang == "Mandarin")

summary_emmeans_Q4c_man$ItemToListener <- factor(summary_emmeans_Q4c_man$ItemToListener, 
                                                levels = c("Foreign", "L2", "L1"))

summary_emmeans_Q4c_man$ItemToListener_numeric <- as.numeric(summary_emmeans_Q4c_man$ItemToListener)

nice_table(summary_emmeans_Q4c_man)


# plot EMMs Chinese group
P_emmeans_Q4c <- ggplot(summary_emmeans_Q4c_man,
                        aes(ItemToListener_numeric, y = emmean, fill = ItemToListener)) +
#   # Background rectangles with increasing alpha
#   geom_rect(aes(xmin = ItemToListener_numeric - 0.5, xmax = ItemToListener_numeric + 0.5, 
#                 ymin = -Inf, ymax = 200), alpha = 0.05, fill = "grey30") +
#   geom_rect(aes(xmin = ItemToListener_numeric - 0.5, xmax = ItemToListener_numeric + 0.5, 
#                 ymin = 300, ymax = 400), alpha = 0.1, fill = "grey30") +
#   geom_rect(aes(xmin = ItemToListener_numeric - 0.5, xmax = ItemToListener_numeric + 0.5, 
#                 ymin = 400, ymax = 500), alpha = 0.15, fill = "grey30") +
#   geom_rect(aes(xmin = ItemToListener_numeric - 0.5, xmax = ItemToListener_numeric + 0.5, 
#                 ymin = 500, ymax = 600), alpha = 0.2, fill = "grey30") +
#   geom_rect(aes(xmin = ItemToListener_numeric - 0.5, xmax = ItemToListener_numeric + 0.5, 
#                 ymin = 600, ymax = Inf), alpha = 0.25, fill = "grey30") +
#   
#   # Annotations for each shaded area, positioned at the bottom
#   annotate("text", x = 0.6, y = 100, label = "G200", color = "black", size = 4, hjust = 0.5) +
#   annotate("text", x = 0.6, y = 350, label = "G400", color = "black", size = 4, hjust = 0.5) +
#   annotate("text", x = 0.6, y = 450, label = "G500", color = "black", size = 4, hjust = 0.5) +
#   annotate("text", x = 0.6, y = 550, label = "G600", color = "black", size = 4, hjust = 0.5) +
#   annotate("text", x = 0.6, y = 800, label = "Gfull", color = "black", size = 4, hjust = 0.5) +
  
  # Main plot components
  geom_bar(
    stat = "identity",
    position = position_dodge(width = 0.8),
    width = 0.5
    ) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.25,
    position = position_dodge(width = 0.8)
  ) +
  geom_path(aes(group = ItemToListener), size = 0.8) +  # Single geom_path for all paths
  
  coord_flip() +  # Flip the coordinates to make the bars horizontal
  
  # Restore original x-axis labels
  scale_x_continuous(breaks = c(1, 2, 3), labels = c("Foreign", "L2", "L1")) +
  
  # Other plot settings and customization
  theme_minimal() +
  labs(x = "Familiarity", y = "Estimated Marginal Mean", title = "(4c)") +
  scale_y_continuous(
    limits = c(0, 1400),
    breaks = seq(0, 1400, by = 100),
    expand = expansion(mult = c(0, 0.05))
  ) +
  theme(
    legend.position = "none",
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.2, "cm"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    panel.grid = element_blank(),
    panel.spacing = unit(1, "cm"),
    strip.background = element_blank(),
    strip.text = element_text(color = "black", size = 12)
  ) +
  scale_fill_manual(values = c("L1" = "#fc9272", "L2" = "#6baed6", "Foreign" = "#8eFae1"))

print(P_emmeans_Q4c)

ggsave("Q4cd_bar_mean_EIP.svg", plot = P_emmeans_Q4c, width = 10, height = 8, units = "in", dpi = 300, limitsize = FALSE)



