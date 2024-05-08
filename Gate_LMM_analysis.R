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

# set work directory ------------------------------------------------------
setwd('C://Users//hcui8//Dropbox//Trying//Gate_analysis')
# setwd('/Users/hainingcui/Dropbox/Trying/Gate_analysis')

# Hu Score  -------------------------------------------------------------
#import data of all participants
gate_HuScore <- read.csv('combined_HuScore.csv') 

# rename label Happiness_laughter to happiness
gate_HuScore <- gate_HuScore %>%
  mutate(ItemEmotion = case_when(
    ItemEmotion == "Happiness_laughter" ~ "Happiness",
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

# drop Happiness_please level
gate_HuScore <- gate_HuScore %>%
  filter(ItemEmotion != "Happiness_pleasure") %>%
  mutate(ItemEmotion = droplevels(ItemEmotion))

# Check if each column in the gate_HuScore dataframe is a factor
factors_check <- sapply(gate_HuScore, is.factor)
# Print the results
print(factors_check)

print(str(gate_HuScore))

# Rename label type
# gate_HuScore <- gate_HuScore %>%
#   mutate(ItemEmotion2 = case_when(
#     ItemEmotion2 == "1" ~ "Anger",
#     ItemEmotion2 == "2" ~ "Fear",
#     ItemEmotion2 == "3" ~ "Happiness",
#     ItemEmotion2 == "4" ~ "Sadness",
#     TRUE ~ ItemEmotion2 
#   ))


# change VOC 'happiness_laughter', 'happiness_pleasure' to 'Happiness"
# gate_HuScore <- gate_HuScore %>%
#   mutate(ItemEmotion = case_when(
#     ItemEmotion %in% c('Happiness_laughter') ~ 'Happiness_amusement',
#     TRUE ~ ItemEmotion
#   ))

# EIP ---------------------------------------------------------------------
# import EIP data (Yondu created dataframe in excel)
gate_EIP <- read.csv('CCGating2_EIP_ALL PARTICIPANTSjuly2018.csv')

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

# rename "ItemToListener" to "ItemToListener"
gate_EIP <- gate_EIP %>%
  rename(ItemToListener = ItemToListener)

# drop Pleasure level
gate_EIP <- gate_EIP %>%
  filter(ItemEmotion != "Pleasure") %>%
  droplevels()

print(str(gate_EIP))

# summary Hu Score and EIP data ---------------------------------------------------------
summary(gate_HuScore)

#summarize Discrib Stats
summary_gate_HuScore <- gate_HuScore %>%
  group_by(ListenerLang, ItemToListener, Gate, ItemEmotion) %>%
  summarise(
    Mean = mean(HuScore, na.rm = TRUE),
    Max = max(HuScore, na.rm = TRUE),
    Median = median(HuScore, na.rm = TRUE),
    SD = sd(HuScore, na.rm = TRUE),
  ) %>%
  ungroup()  # Remove grouping

# print a HTML table
nice_table(summary_gate_HuScore)

# summary EIP data
summary(gate_EIP)

# summarize Discrib Stats
summary_gate_EIP <- gate_EIP %>%
  group_by(ListenerLang, ItemToListener, ItemEmotion) %>%
  summarise(
    EIP_Mean = mean(EIPtime, na.rm = TRUE),
    EIP_SD = sd(EIPtime, na.rm = TRUE),
    EIP_Max = max(EIPtime, na.rm = TRUE),
    EIP_Median = median(EIPtime, na.rm = TRUE)
  ) %>%
  ungroup()  # Remove grouping

# print out a HTML table
nice_table(summary_gate_EIP)


# # check data normality  -------------------------------------------------

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

# Q1a LMM model for Hu Score as a function of L1 vs. Speech type (Gfulll gate only) ---------------------------------------------------------
# {use data of GFULL condition only}, {run separately on Hu scores and EIP}

# filter data for LMM Q1a, manipulate the data for LMM models to drop L2 and Foreign conditions and keep GFULL
df_Q1a_Gfull_L1_NVV <- gate_HuScore %>%
  filter(ItemToListener != "Foreign", ItemToListener != "L2") %>%
  filter(Gate == "GFULL") %>%
  droplevels() %>%
  group_by(ListenerLang, ItemToListener, Gate)

# add one more variable for ItemEmotion (VOC happiness combined in to happiness)
df_Q1a_Gfull_L1_NVV$ItemEmotion2 <- df_Q1a_Gfull_L1_NVV$ItemEmotion
levels(df_Q1a_Gfull_L1_NVV$ItemEmotion2)[levels(df_Q1a_Gfull_L1_NVV$ItemEmotion2) == "Happiness_laughter"] <- "Happiness"
levels(df_Q1a_Gfull_L1_NVV$ItemEmotion2)[levels(df_Q1a_Gfull_L1_NVV$ItemEmotion2) == "Happiness_pleasure"] <- "Happiness"

# convert to factors
df_Q1a_Gfull_L1_NVV$ItemEmotion2 <- factor(df_Q1a_Gfull_L1_NVV$ItemEmotion2)
df_Q1a_Gfull_L1_NVV$ItemEmotion <- factor(df_Q1a_Gfull_L1_NVV$ItemEmotion)
df_Q1a_Gfull_L1_NVV$ListenerLang <- factor(df_Q1a_Gfull_L1_NVV$ListenerLang)


#summary data
summary_df_Q1a <- df_Q1a_Gfull_L1_NVV %>%
  group_by(ListenerLang, ItemToListener) %>%
  summarise(
    Mean = mean(HuScore, na.rm = TRUE),
    SD = sd(HuScore, na.rm = TRUE),
    #Min = min(HuScore, na.rm = TRUE),
    #Max = max(HuScore, na.rm = TRUE)
    #Median = median(HuScore, na.rm = TRUE)
  ) %>%
  ungroup()  

print(summary_df_Q1a)
nice_table(summary_df_Q1a)

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

# Q1a t-test (pairwise comparison) Estimated marginal means ------------------------------------------------------------------
Q1a_emm_Gfull_Hu <- emmeans(LMM_Q1a_Gfull_Hu_L1_NVV, revpairwise ~ ItemType|ListenerLang, adjust="tukey",
                            #contrast = list("compare VOC to Utter" = c(-1, 1)),
                            lmer.df = "satterthwaite", 
                            lmerTest.limit = 450)  # Adjust pbkrtest.limit if needed

#summary_LMM_Gfull_Hu. Vocalizations should yield higher accuracy and shorter recognition latency than L1 speech
summary(Q1a_emm_Gfull_Hu)
summary.Q1a.stats.table <- as.data.frame(summary(Q1a_emm_Gfull_Hu))
contrasts_df_1a <- summary.Q1a.stats.table$contrasts
nice_table(contrasts_df_1a)

# Line-Plot Q1a Hu scores with Sig (At GFULL by type of speech/event vs. Language Group) --------
# # line plot (summary of mean)
# df_Q1a_plot <- df_Q1a_Gfull_L1_listener_NVV_utter %>%
#   group_by(ItemType, ListenerLang) %>%
#   summarize(mean_HuScore = mean(HuScore, na.rm = TRUE), sd = sd(HuScore, na.rm = TRUE))
# 
# #Create a boxplot with an interaction fill between two fixed terms
# Q1a_custom_colors <- c(
#   "Vocalization.Arabic" = "#8FBC8F",
#   "Utterance.Arabic" = "#2E8B57",
#   "Vocalization.Mandarin" = "#fcb4a5",
#   "Utterance.Mandarin" = "#e6550d"
# )
# 
# #plot line
# p_line_Q1a <- ggplot(df_Q1a_plot, aes(x = interaction(ItemType, ListenerLang), 
#                                                               y = mean_HuScore,  
#                                                               color = interaction(ItemType, ListenerLang))) +
#   geom_line(aes(linetype=ItemType), linewidth=4) +
#   geom_point(aes(shape = ItemType),size = 4) +
#   geom_path(arrow = arrow(length = unit(0.3, "mm"))) +
#   geom_errorbar(aes(ymin=mean_HuScore-sd, ymax=mean_HuScore+sd),width = 0.05) +
#   scale_color_manual(values = Q1a_custom_colors) +
#   labs(x = "ItemType and ListenerLang Interaction", 
#        y = "HuScore", 
#        title = "HuScore as a function of Listener's L1 and type of speech",
#        color = "") +
#   theme(panel.background = element_blank(), 
#         axis.title.x = element_text(vjust = -1.5, hjust = 0.5, size = 12),  # Center x-axis label
#         #axis.title.y = element_text(vjust = 1, hjust = 0.5, size = 12), # Center y-axis label
#         axis.line = element_line(color = "black", size = 0.5),
#         axis.ticks.length = unit(1.4,"mm"),
#         axis.ticks = element_line(size = .5, colour = "black"),
#         legend.position = "none"
#         ) + # Hide all legends
#         scale_y_continuous(breaks = seq(0, 1, by = 0.1)) + # Specify y-axis tick locations# Adjust tick length
#   guides(shape = FALSE)  # This removes the legend for shapes
# 
# 
# print(p_line_Q1a)
# 
# p_line_Q1a +
#   geom_line(data=tibble(x=c(3.2, 3.8), y=c(1, 1)), 
#             aes(x=x, y=y),
#             inherit.aes = FALSE) +
#   geom_text(data=tibble(x=3.5, y=1.04), 
#             aes(x=x, y=y, label="*"), size=6,
#             inherit.aes = FALSE) +
#   geom_line(data=tibble(x=c(1.2, 1.8), y=c(1, 1)), 
#             aes(x=x, y=y),
#             inherit.aes = FALSE) +
#   geom_text(data=tibble(x=1.5, y=1.04), 
#             aes(x=x, y=y, label="*"),size=6,
#             inherit.aes = FALSE)
# 

# Violin-plot Q1a L1 & vocalization -------------------------------------------------
Q1a_custom_colors <- c(
  "Vocalization.Arabic" = "#8FBC8F",
  "Utterance.Arabic" = "#2E8B57",
  "Vocalization.Mandarin" = "#fcb4a5",
  "Utterance.Mandarin" = "#e6550d"
)

#plot box
violin_Q1a_L1_VOC <-
  ggplot(df_Q1a_Gfull_L1_NVV,
         aes(
           x = interaction(ItemType, ListenerLang),
           y = HuScore,
           fill = interaction(ItemType, ListenerLang)
         )) +
  geom_violin(position = position_dodge(width = 1.0), color = NA, trim = FALSE) +
  #geom_jitter(height = 0, width = 0.005) +
  scale_fill_manual(values = Q1a_custom_colors) +
  theme_minimal() +
  labs(title = "FigureQ1a. HuScore(Gfull) as a function of event type and L1 background",
       x = "",
       y = "HuScore",
       fill = "Speech Type divided by L1 background")+
  stat_summary(fun = mean, geom = "point", shape = 20, size = 5, color = "darkred",
               position = position_dodge(width = 1.0)) +
  theme(
    strip.text.x = element_text(size = 11, color = "black", face = "bold"),
    axis.text.x = element_text(angle = 25, hjust = 1),  # Optional: rotate x-axis labels if needed
    panel.spacing = unit(1, "lines"),  # Adjust spacing between panels if necessary
    axis.ticks.length.x = unit(4, "pt"),  # Adjust length of ticks if needed
    axis.text.x.bottom = element_text(margin = margin(1, 0, 1, 0, "lines")), # Adjust margin around text if needed
    legend.position = "none" ,
    plot.title = element_text(hjust = 0.5)  # Center the title
  ) 

print(violin_Q1a_L1_VOC)

violin_Q1a_L1_VOC +
  geom_line(data=tibble(x=c(1.1, 1.70), y=c(1, 1)),
            aes(x=x, y=y), size=1,
            inherit.aes = FALSE) +
  geom_text(data=tibble(x=1.45, y=1.02),
            aes(x=x, y=y, label="*"),size=6,
            inherit.aes = FALSE) +
  geom_line(data=tibble(x=c(3.2, 3.8), y=c(1, 1)),
            aes(x=x, y=y), size=1,
            inherit.aes = FALSE) +
  geom_text(data=tibble(x=3.45, y=1.05),
            aes(x=x, y=y, label="n.g"),size=5,
            inherit.aes = FALSE)

# Violin-plot Q1a vocalization (Vocalization-Emotion ) -------------------------------------------------
# filter data (VOC with all emotion type)
df_Q1a_Gfull_VOC <- gate_HuScore %>%
  filter(ItemToListener != "Foreign", ItemToListener != "L2", ItemToListener != "L1") %>%
  filter(Gate == "GFULL") %>%
  droplevels() %>%
  group_by(ListenerLang, ItemToListener, Gate)


emotion_colors <- c(
  Anger = "#E63946",  # A vivid, slightly desaturated red
  Fear = "#457B9D",  # A soft, desaturated blue
  #Happiness_laughter = "#F4A261", 
  Happiness = "#F4A261",# A warm, muted orange
  #Happiness_pleasure = "#2A9D8F",  # A calming, desaturated teal
  Sadness = "#9C89B8"  # A gentle, muted purple
)

# Function to lighten or darken colors
lighten_color <- function(color, amount = 0.4) adjustcolor(color, amount)
darken_color <- function(color, amount = 0.8) adjustcolor(color, amount)

# Generate variations for each language
emotion_colors_arabic <- setNames(sapply(emotion_colors, lighten_color), paste(names(emotion_colors), "Arabic", sep = "."))
emotion_colors_mandarin <- setNames(sapply(emotion_colors, darken_color), paste(names(emotion_colors), "Mandarin", sep = "."))

# Combine into one vector for use in ggplot
all_colors <- c(emotion_colors_arabic, emotion_colors_mandarin)

#plot box
violin_Q1a_VOC <-
  ggplot(df_Q1a_Gfull_VOC,
         aes(
           x = ListenerLang,
           y = HuScore,
           fill = interaction(ItemEmotion, ListenerLang)
         )) +
  geom_violin(position = position_dodge(width = 0.8), color = NA, trim = FALSE) +
  scale_fill_manual(values = all_colors) +
  theme_minimal() +
  labs(title = "HuScore(Gfull) as a function of vocalization emotion and L1 background",
       x = "",
       y = "HuScore",
       fill = "Speech Type divided by L1 background")+
  facet_wrap(~ItemEmotion, nrow=1) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 5, color = "darkred",
               position = position_dodge(width = 0.3)) +
  theme(
    strip.text.x = element_text(size = 11, color = "black", face = "bold"),
    axis.text.x = element_text(angle = 25, hjust = 1,color = "black", face = "bold"),  # Optional: rotate x-axis labels if needed
    panel.spacing = unit(1, "lines"),  # Adjust spacing between panels if necessary
    axis.ticks.length.x = unit(4, "pt"),  # Adjust length of ticks if needed
    axis.text.x.bottom = element_text(margin = margin(1, 0, 1, 0, "lines")), # Adjust margin around text if needed
    legend.position = "none" ,
    plot.title = element_text(hjust = 0.5)  # Center the title
  ) 

print(violin_Q1a_VOC)

# violin-plot Q1a vocalization (L1-Emotion ) -------------------------------------------------

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

#####
# Q1b LMM model for EIP as function of L1 vs. Speech type (Gfull gate only)   -------------------------------

# fillter  EIP data for Gfull gate 
EIP_listener_L1_VOC_Gfull <- gate_EIP %>%
  filter(ItemToListener != "Foreign", ItemToListener != "L2") %>%
  #filter(EIP == "5") %>%
  droplevels() %>%
  group_by(ListenerLang, ItemToListener, EIP)

# LMM model
LMM_EIP_L1_VOC  <- lmer(
  EIPtime ~ (ListenerLang + ItemType)^2 + ListenerLang:ItemType + 
    (1 | ListenerID) + (1 | ItemEmotion), RMLF = 
  data = EIP_listener_L1_VOC_Gfull
)

# write LMM results to a table in HTML format
tab_model(LMM_EIP_L1_VOC, show.df = TRUE)


# Q1b t-test (pairwise comparison) Estimated marginal means ------------------------------------------------------------------
Q1b_emm_EIP <- emmeans(LMM_EIP_L1_VOC, revpairwise ~ ItemType|ListenerLang, adjust="tukey",
                        lmer.df = "satterthwaite", 
                        lmerTest.limit = 5283)  # Adjust pbkrtest.limit if needed
summary(Q1b_emm_EIP)

summary.Q1b.stats.table <- as.data.frame(summary(Q1b_emm_EIP))

contrasts_df_1b <- summary.Q1b.stats.table$contrasts

nice_table(contrasts_df_1b ) # sig for both directions

# Q1b bar-plot ----------------------------------------------------------------
# Create the boxplot with an interaction fill between two fixed terms
custom_colors_1b <- c(
  "vocalization.Arabic" = "#679267",  # Darker than #8FBC8F
  "utterance.Arabic" = "#20693D",       # Darker than #2E8B57
  "vocalization.Mandarin" = "#e9967a",  # Darker than #fcb4a5
  "utterance.Mandarin" = "#cc4c02"        # Darker than #e6550d
)

# Summary data Q1b
EIP_summary <- EIP_listener_L1_VOC_Gfull %>%
  group_by(ListenerLang, ItemType) %>%
  summarize(
    mean_EIP = mean(EIPtime, na.rm = TRUE),
    sd = sd(EIPtime, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )

print(EIP_summary)

nice_table(EIP_summary)

#plot Q1b
bar_1b_EIP_Gfull <- ggplot(EIP_summary,
                          aes(
                            x = interaction(ItemType, ListenerLang),
                            y = mean_EIP,
                            fill = interaction(ItemType, ListenerLang)
                          )) +
  geom_col() +
  geom_errorbar(aes(ymin=mean_EIP-sd, ymax=mean_EIP+sd),width = 0.2) +
  scale_fill_manual(values = custom_colors_1b) +
  theme_minimal() +
  labs(title = "FigureQ1b. EIP as a function of Speech type and L1 background",
       x = "ItemType divided by L1 background of listeners",
       y = "Mean EIP",
       fill = "Speech Type*L1 Interaction") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(vjust = -1.5, size = 12),
    legend.position = "none"
        )

#quartz(width=10, height=8)  # Adjust size as needed
bar_1b_EIP_Gfull

bar_1b_EIP_Gfull +
  geom_line(data=tibble(x=c(1.25, 1.75), y=c(900, 900)), 
            aes(x=x, y=y),
            inherit.aes = FALSE) +
  geom_text(data=tibble(x=1.5, y=900), 
            aes(x=x, y=y, label="*"), size=6,
            inherit.aes = FALSE) +
  geom_line(data=tibble(x=c(3.25, 3.75), y=c(690, 690)), 
            aes(x=x, y=y),
            inherit.aes = FALSE) +
  geom_text(data=tibble(x=3.5, y=690), 
            aes(x=x, y=y, label="*"),size=6,
            inherit.aes = FALSE)

#####
# Q2a LMM model for HuScore as a function of NVV vs. Gate by Mandarin L1 listeners ----------------------------------------------------
#  Emotion (ANG, FER, SAD, HAP-Amuse, HAP-Pleasure) vs. Duration (G200, G400, G500, G600, GFULL)

# clean, filter data
L1_mandarin_VOC_gate  <- gate_HuScore %>%
  filter(ItemToListener != "Foreign", 
         ItemToListener != "L2", 
         ItemToListener != "L1", 
         ListenerLang != "Arabic") %>%
  droplevels() %>%
  group_by(ListenerLang, ItemToListener)

summary_L1_mandarin_VOC_gate <- L1_mandarin_VOC_gate %>%
  group_by(Gate, ItemEmotion, ListenerLang, ItemType) %>%
  summarize(mean=mean(HuScore, na.rm=TRUE),
            sd=sd(HuScore, na.rm=TRUE))
 
# print out summarized data as a HTML table 
nice_table(summary_L1_mandarin_VOC_gate)

#LMM model 2a
LMM_Hu_mandarin_nvv_gate  <- lmer(
  HuScore ~ (ItemEmotion + Gate)^2 + 
    (1 | Subject), REML = FALSE,
  data = L1_mandarin_VOC_gate
)

# write LMM results to a table in HTML format
tab_model(LMM_Hu_mandarin_nvv_gate, show.df = TRUE)


# Q2a t-test for direction of statistical significance --------

Q2a_emm_mandarin_nvv_gate <- emmeans(LMM_Hu_mandarin_nvv_gate , pairwise ~ Gate|ItemEmotion, methods="turkey",
                        lmer.df = "satterthwaite", 
                        lmerTest.limit = 625)  # Adjust pbkrtest.limit if needed

summary(Q2a_emm_mandarin_nvv_gate)
summary.Q2a.stats.table <- as.data.frame(summary(Q2a_emm_mandarin_nvv_gate))
contrasts_df_2a <- summary.Q2a.stats.table$contrasts
nice_table(contrasts_df_2a)

# Q2a violin-plot faced, VOc, emotion, Mandarin listeners -----------------------------------------------

# Define shades of orange for each gate
emotion_colors <- c(
  Anger = "#E63946",  # A vivid, slightly desaturated red
  Fear = "#457B9D",  # A soft, desaturated blue
  Happiness = "#F4A261",  # A warm, muted orange
  #Happiness_pleasure = "#2A9D8F",  # A calming, desaturated teal
  Sadness = "#9C89B8"  # A gentle, muted purple
)

# Function to lighten or darken colors
lighten_color1 <-
  function(color, amount = 0.2)
    adjustcolor(color, amount)
lighten_color2 <-
  function(color, amount = 0.4)
    adjustcolor(color, amount)
lighten_color3 <-
  function(color, amount = 0.6)
    adjustcolor(color, amount)
lighten_color4 <-
  function(color, amount = 0.8)
    adjustcolor(color, amount)
lighten_color5 <-
  function(color, amount = 1.2)
    adjustcolor(color, amount)

# Generate variations for each language
emotion_colors_G200 <-
  setNames(sapply(emotion_colors, lighten_color1),
           paste(names(emotion_colors), "G200", sep = "."))
emotion_colors_G400 <-
  setNames(sapply(emotion_colors, lighten_color2),
           paste(names(emotion_colors), "G400", sep = "."))
emotion_colors_G500 <-
  setNames(sapply(emotion_colors, lighten_color3),
           paste(names(emotion_colors), "G500", sep = "."))
emotion_colors_G600 <-
  setNames(sapply(emotion_colors, lighten_color4),
           paste(names(emotion_colors), "G600", sep = "."))
emotion_colors_Gfull <-
  setNames(sapply(emotion_colors, lighten_color5),
           paste(names(emotion_colors), "GFULL", sep = "."))

# Combine into one vector for use in ggplot
all_colors <-
  c(
    emotion_colors_G200,
    emotion_colors_G400,
    emotion_colors_G500,
    emotion_colors_G600,
    emotion_colors_Gfull
  )

# Ensure your Gate variable is a factor with the levels in the correct order for the plot
L1_mandarin_VOC_gate$Gate <-
  factor(L1_mandarin_VOC_gate$Gate,
         levels = c("G200", "G400", "G500", "G600", "GFULL"))

# Create the plot
vilion_Q2a_NVV_gate_manda <- ggplot(L1_mandarin_VOC_gate,
                               aes(
                                 x = Gate,
                                 y = HuScore,
                                 fill = interaction(ItemEmotion, Gate)  # Fill boxes with gate color
                               )) +
  geom_violin(position = position_dodge(width = 0.8),
              color = NA,
              trim = FALSE) +
  scale_fill_manual(values = all_colors) +  # Use the defined orange shades
  theme_minimal() +
  labs(title = "FigureQ2a. Hu score as a function of NVV type and gate (Mandarin)",
       x = "Gate",
       y = "Mean Hu score",
       fill = "Gate") +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 20,
    size = 3,
    color = "black",
    position = position_dodge(width = 0.8)
  ) +
  facet_wrap( ~ ItemEmotion, nrow = 1) +  # Facet by emotion
  theme(
    strip.text.x = element_text(
      size = 12,
      color = "black",
      face = "bold"
    ),
    axis.text.x = element_text(angle = 45, hjust = 1),
    # Optional: rotate x-axis labels if needed
    panel.spacing = unit(1, "lines"),
    # Adjust spacing between panels if necessary
    axis.ticks.length.x = unit(4, "pt"),
    # Adjust length of ticks if needed
    axis.text.x.bottom = element_text(margin = margin(1, 0, 1, 0, "lines")),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )

# Display the plot (mac only)
#quartz(width=10, height=8)  # Adjust size as needed
vilion_Q2a_NVV_gate_manda

#####
# Q2b LMM model for Hu Score as a function of NVV vs. Gate by Arabic L1 listeners------------------------------------------

# clean, filler data
L1_Arabic_VOC_gate <- gate_HuScore %>%
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
  data = L1_Arabic_VOC_gate
)

# write LMM results to a table in HTML format
tab_model(LMM_Hu_Arabic_nvv_gate, show.df = TRUE)

# Q2b t-tests -----------------------------------------------------------

LMM_Hu_Arabic_nvv_gate <- emmeans(LMM_Hu_Arabic_nvv_gate , pairwise ~ Gate|ItemEmotion,
                                    lmer.df = "satterthwaite", 
                                    lmerTest.limit = 625)  # Adjust pbkrtest.limit if needed
summary(LMM_Hu_Arabic_nvv_gate)

summary.Q2b.stats.table <- as.data.frame(summary(LMM_Hu_Arabic_nvv_gate))

contrasts_df_2b <- summary.Q2b.stats.table $contrasts

nice_table(contrasts_df_2b )

# Q2b connected-scatter-plot, VOC emotion, Arabic listeners -----------------------------------------------

# scatter line plot 
L1_Arabic_VOC_gate_mean <- L1_Arabic_VOC_gate %>%
  filter(Gate %in% c("G200", "G400", "G500", "G600", "GFULL")) %>%
  group_by(Gate,ItemEmotion) %>%
  summarize(mean_HuScore = mean(HuScore, na.rm = TRUE), sd = sd(HuScore, na.rm = TRUE))


last_points <- data.frame(
  ItemEmotion = c("Anger", "Fear", "Sadness", "Happiness" ),
  mean_HuScore = c(0.75621366, 0.71333102, 0.81856648, 0.56574978),
  Gate = "GFULL"  # Assuming the last point should be labeled at the GFULL gate
)


p_Q2b <- ggplot(L1_Arabic_VOC_gate_mean, aes(x = Gate, y = mean_HuScore, group = ItemEmotion, color = ItemEmotion)) +
  geom_point(size = 4) +
  geom_path(arrow = arrow(length = unit(0.3, "mm"))) +
  #geom_errorbar(aes(ymin=mean_HuScore-sd, ymax=mean_HuScore+sd),width = 0.03) +
  labs(x = "Gate Interval (ms)", y = "HuScore", title = "Figure2b. Connected Scatter Plot of HuScore by Gate and ItemEmotion (Arabic)") +
  geom_text(data = last_points, 
            aes(label = ItemEmotion), 
            vjust = -1, 
            hjust = 1, 
            check_overlap = TRUE) + 
  theme(panel.background = element_blank()) +
  theme(legend.position="none") +
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
  scale_x_discrete(breaks = c("G200", "G400", "G500", "G600", "GFULL")) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2))  # Specify y-axis tick locations# Adjust tick length

print(p_Q2b)

#####
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


# Q2c pairwise comparison t-tests ---------------------------------

LMM_Hu_Mandarin_speech_gate <- emmeans(LMM_Hu_Mandarin_speech_gate , pairwise ~ Gate|ItemEmotion,
                                  lmer.df = "satterthwaite", REML = FALSE,
                                  lmerTest.limit = 622)  # Adjust pbkrtest.limit if needed
summary(LMM_Hu_Mandarin_speech_gate)

summary.Q2c.stats.table <- as.data.frame(summary(LMM_Hu_Mandarin_speech_gate))

contrasts_df_2c <- summary.Q2c.stats.table$contrasts

nice_table(contrasts_df_2c)

# Q2c plot faced by gates ()-----------------------------------------------

# scatter line plot 
L1_Mandarin_speech_gate_mean <- L1_Mandarin_speech_gate %>%
  filter(Gate %in% c("G200", "G400", "G500", "G600", "GFULL")) %>%
  group_by(Gate, ItemEmotion) %>%
  summarize(mean_HuScore = mean(HuScore, na.rm = TRUE),
            sd = sd(HuScore, na.rm = TRUE))


last_points_speech_Man <- data.frame(
  ItemEmotion = c("Anger", "Fear", "Sadness", "Happiness"),
  mean_HuScore = c(0.602, 0.558, 0.657, 0.588),
  Gate = "GFULL"  # Assuming the last point should be labeled at the GFULL gate
)


p_Q2c <-
  ggplot(
    L1_Mandarin_speech_gate_mean ,
    aes(
      x = Gate,
      y = mean_HuScore,
      group = ItemEmotion,
      color = ItemEmotion
    )
  ) +
  geom_point(size = 4) +
  geom_path(arrow = arrow(length = unit(0.3, "mm"))) +
  #geom_errorbar(aes(ymin=mean_HuScore-sd, ymax=mean_HuScore+sd),width = 0.03) +
  labs(x = "Gate Interval (ms)", y = "HuScore", title = "Figure2c. HuScore as a function of Speech Emotion and Gate duration (Mandarin listeners)") +
  geom_text(
    data = last_points_speech_Man ,
    aes(label = ItemEmotion),
    vjust = -1,
    hjust = 1.2,
    check_overlap = FALSE,
    
  ) +
  theme(panel.background = element_blank()) +
  theme(legend.position = "none") +
  theme(
    axis.title.x = element_text(
      vjust = -1.5,
      hjust = 0.5,
      size = 12
    ),
    # Center x-axis label
    axis.title.y = element_text(
      vjust = 1,
      hjust = 0.5,
      size = 12
    ),
    # Center y-axis label
    axis.line = element_line(color = "black",
                             size = 0.),
    axis.ticks.length = unit(1.4, "mm"),
    axis.ticks = element_line(size = .5,
                              colour = "black")
  ) +
  scale_x_discrete(breaks = c("G200", "G400", "G500", "G600", "GFULL")) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1))  # Specify y-axis tick locations# Adjust tick length

print(p_Q2c)

# Display the plot
#quartz(width=10, height=8)  # Adjust size as needed

#####
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

# Q2d pairwise comparison t-tests ---------------------------------------

LMM_Hu_Arabic_speech_gate <- emmeans(LMM_Hu_Arabic_speech_gate , pairwise ~ Gate|ItemEmotion,
                                       lmer.df = "satterthwaite", 
                                       lmerTest.limit = 622)  # Adjust pbkrtest.limit if needed
summary(LMM_Hu_Arabic_speech_gate)
summary.Q2d.stats.table <- as.data.frame(summary(LMM_Hu_Arabic_speech_gate))
contrasts_df_2d <- summary.Q2d.stats.table$contrasts
nice_table(contrasts_df_2d)


# Q2d plot faced by gates ()-----------------------------------------------

# Define shades of orange for each gate

gate_colors_Areb_sp <- c(
  "G200" = "#deebf7",  # lightest blue
  "G400" = "#9ecae1",
  "G500" = "#3182bd",
  "G600" = "#2171b5",
  "GFULL" = "#08519c"  # darkest blue
)


# Ensure your Gate variable is a factor with the levels in the correct order for the plot
L1_Arabic_speech_gate $Gate <- factor(L1_Arabic_speech_gate $Gate, levels = c("G200", "G400", "G500", "G600", "GFULL"))

# Create the plot
box_P_Sp_gate_Arb <- ggplot(L1_Arabic_speech_gate,
                            aes(
                              x = Gate,
                              y = HuScore,
                              fill = Gate  # Fill boxes with gate color
                            )) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = gate_colors_Areb_sp) +  # Use the defined orange shades
  theme_minimal() +
  labs(title = "Figure2d. Hu score as a function of Speech type and gate (Arabic)",
       x = "Gate",
       y = "Mean Hu score",
       fill = "Gate") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "black",
               position = position_dodge(width = 0.8)) +
  facet_wrap(~ItemEmotion) +  # Facet by emotion
  theme(strip.text.x = element_text(size = 12, color = "black", face = "bold"))

# Display the plot
#quartz(width=10, height=8)  # Adjust size as needed

box_P_Sp_gate_Arb

#####
# Q3a LMM for EIP of NVV as a function of L1 background vs. ItemEmotion  -----------------------------------------------------------
# Vocalizations: Listener (MAND, ARAB) x Emotion (ANG, FER, SAD, HAP-Amuse, HAP-Pleasure) at Gfull

# filter EIP data for Q3a
df_Q3a_L1_VOC_EIP <- gate_EIP %>%
  filter(ItemToListener != "Foreign", ItemToListener != "L2", ItemType != "utterance") %>%
  #filter(EIP == "5") %>%
  droplevels() %>%
  group_by(ListenerLang, ItemToListener, EIP)

# the DV is named EIPtime
summary_df_Q3a <- df_Q3a_L1_VOC_EIP  %>%
  group_by(ListenerLang, ItemToListener,ItemEmotion) %>%
  summarise(
    Mean = mean(EIPtime, na.rm = TRUE),
    Min = min(EIPtime, na.rm = TRUE),
    Max = max(EIPtime, na.rm = TRUE),
    SD = sd(EIPtime, na.rm = TRUE),
    Median = median(EIPtime, na.rm = TRUE)
  ) %>%
  ungroup()  # Remove grouping

# Display the summary table
print(summary_df_Q3a )

nice_table(summary_df_Q3a) 

# LMM model
LMM_Q3a_EIP_VOC_L1  <- lmer(
  EIPtime ~ (ListenerLang + ItemEmotion)^2 + ListenerLang:ItemEmotion + 
    (1 | ListenerID), REML = FALSE,
  data = df_Q3a_L1_VOC_EIP 
)

# write LMM results to a table in HTML format
tab_model(LMM_Q3a_EIP_VOC_L1, show.df = TRUE)

# Q3a t-tests -------------------------------------------------------------

Q3a_emm_EIP_L1_Nvv <- emmeans(LMM_Q3a_EIP_VOC_L1, pairwise ~ ItemEmotion, methods ="turkey",
                                     lmer.df = "satterthwaite", 
                                     lmerTest.limit = 622)  # Adjust pbkrtest.limit if needed

summary(Q3a_emm_EIP_L1_Nvv)
summary.Q3a.stats.table <- as.data.frame(summary(Q3a_emm_EIP_L1_Nvv))
contrasts_df_3a <- as.data.frame(summary.Q3a.stats.table$contrasts)
nice_table(contrasts_df_3a)

# Q3a plot ---------------------------------------------------------------

#Assign names to the colors for the respective emotions

names(emotion_colors) <- c('Anger', 'Fear', 'Happiness', 'Sadness')

emotion_colors <- c(
  'Anger' = "#ef8a62",      # Warm terracotta
  'Fear' = "#67a9cf",      # Soft blue
  'Happiness' = "#f7fcb9", # Light yellow
  'Sadness' = "#998ec3"  # Lavender
)

# create the boxplot
Q3a_bar_NVV_L1 <- ggplot(summary_df_Q3a,
                               aes(
                                 x = ItemEmotion,
                                 y = Mean,
                                 fill = ItemEmotion  # Use ItemEmotion for fill
                               )) +
  geom_col(position = position_dodge(width = 0.8)) +
  #geom_jitter(position = position_jitter(width = 0.2), alpha = 0.5) + 
  scale_fill_manual(values = emotion_colors) +
  theme_minimal() +
  labs(title = "Figure3a. EIP as a function of NVV emotion and L1 background",
       x = "NVV Emotion",
       y = "Mean EIP",
       fill = "NVV Emotion") +
  #stat_summary(fun = mean, shape = 20, size = 0.3, color = "darkred",
               #position = position_dodge(width = 0.8)) +
  facet_wrap(~factor(ListenerLang)) +  # Add faceting for 'Gate'
  theme(strip.text.x = element_text(size = 12, color = "black", face = "bold"))

#quartz(width=10, height=8) 
Q3a_bar_NVV_L1

#####
# Q3b LMM for EIP of Speech utterance as a function of L1 background vs. ItemEmotion  -----------------------------------------------------------

# clean EIP data for Gfull gate 
df_Q3b_L1_Speech_EIP <- gate_EIP %>%
  filter(ItemToListener != "Foreign", ItemToListener != "L2", ItemType != "vocalization") %>%
  droplevels() %>%
  group_by(ListenerLang, ItemToListener, EIP)

# summarize filtered data
summary_df_Q3b <- df_Q3b_L1_Speech_EIP  %>%
  group_by(ListenerLang, ItemToListener,ItemEmotion) %>%
  summarise(
    Mean = mean(EIPtime, na.rm = TRUE),
    Min = min(EIPtime, na.rm = TRUE),
    Max = max(EIPtime, na.rm = TRUE),
    SD = sd(EIPtime, na.rm = TRUE),
    Median = median(EIPtime, na.rm = TRUE)
  ) %>%
  ungroup()  # Remove grouping

# printout a table in HTML format
nice_table(summary_df_Q3b)

# LMM model
LMM_Q3b_L1_Speech_EIP  <- lmer(
  EIPtime ~ (ListenerLang + ItemEmotion) ^ 2 + 
  (1 | ListenerID),
  REML = FALSE,
  data = df_Q3b_L1_Speech_EIP
)

# write LMM results to a table in HTML format
tab_model(LMM_Q3b_L1_Speech_EIP, show.df = TRUE)


# Q3b t-tests -------------------------------------------------------------
Q3b_emm_L1_speech <- emmeans(LMM_Q3b_L1_Speech_EIP, pairwise ~ ItemEmotion|ListenerLang,
                                lmer.df = "satterthwaite", 
                                lmerTest.limit = 1346)  # Adjust pbkrtest.limit if needed

summary.Q3b.stats.table <- summary(Q3b_emm_L1_speech)
summary.Q3b.stats.table <- as.data.frame(summary(Q3b_emm_L1_speech))
contrasts_df_3b <- as.data.frame(summary.Q3b.stats.table$contrast)
nice_table(contrasts_df_3b)

# Q3b plot ---------------------------------------------------------------
#Assign names to the colors for the respective emotions
names(emotion_colors) <- c('Anger', 'Fear', 'Happiness', 'Sadness')

emotion_colors <- c(
  'Anger' = "#ef8a62",      # Warm terracotta
  'Fear' = "#67a9cf",      # Soft blue
  'Happiness' = "#f7fcb9", # Light yellow
  'Sadness' = "#998ec3",   # Lavender
)

# Now create the plot with these colors
bar_L1_speech_EIP <- ggplot(summary_df_Q3b,
                             aes(
                               x = ItemEmotion,
                               y = Mean,
                               fill = ItemEmotion  # Use ItemEmotion for fill
                             )) +
  geom_col(position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = emotion_colors) +
  theme_minimal() +
  labs(title = "Figure3b. EIP as a function of Speech type and L1 background at Gfull",
       x = "NVV Emotion",
       y = "Mean EIP",
       fill = "Speech Emotion") +
  #stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "darkred",
               #position = position_dodge(width = 0.8)) +
  facet_wrap(~factor(ListenerLang)) +  # Add faceting for 'Gate'
  theme(strip.text.x = element_text(size = 12, color = "black", face = "bold"))

# quartz(width=10, height=8) 
bar_L1_speech_EIP 

#####
# Q4a LMM model for Hu Score as a function of L1 vs. Speech type (Mandarin) ---------------------------------------------------------

# manipulate the data for LMM models to drop L2 and Foreign conditions and keep GFULL
df_Q4a_all_speech_gate_man <- gate_HuScore %>%
  filter(ItemToListener != "Vocalization") %>%
  filter(ListenerLang == "Mandarin") %>%
  droplevels() %>%
  group_by(ItemToListener, Gate, ItemEmotion)

summary_df_Q4a <- df_Q4a_all_speech_gate_man %>%
  group_by(Gate, ItemEmotion, ListenerLang, ItemType) %>%
  summarize(mean=mean(HuScore, na.rm=TRUE),
            sd=sd(HuScore, na.rm=TRUE))

nice_table(summary_df_Q4a)

# LMM model of gate, emotion, speech-Hu Score
LMM_Q4a_all_speech_gate_man  <- lmer(
  HuScore ~ (ItemToListener + ItemEmotion + Gate)^3 +  
    (1 | Subject),REML = FALSE,
  data = df_Q4a_all_speech_gate_man 
)

# write LMM results to a table in HTML format
tab_model(LMM_Q4a_all_speech_gate_man, show.df = TRUE)

# Q4a t-test (pairwise comparison) Estimated marginal means ------------------------------------------------------------------
Q4a_emm_Q4a_all_speech_man <- emmeans(LMM_Q4a_all_speech_gate_man, revpairwise ~ ItemToListener|ItemEmotion|Gate,
                        lmer.df = "satterthwaite", 
                        lmerTest.limit = 1500)  # Adjust pbkrtest.limit if needed

summary(Q4a_emm_Q4a_all_speech_man)
summary.Q4a.stats.table <- as.data.frame(summary(Q4a_emm_Q4a_all_speech_man))
contrasts_df_4a <- as.data.frame(summary.Q4a.stats.table$contrasts)
nice_table(contrasts_df_4a)

# Q4a plot ----------------------------------------------------------------
emotion_colors <- c(
  'L1' = "#ef8a62",      # Warm terracotta
  'L2' = "#67a9cf",      # Soft blue
  'Foreign' = "#f7fcb9"# Light yellow
  #'Sadness' = "#998ec3",   # Lavender
)

# Now create the plot with these colors
box_P4a <- ggplot(df_Q4a_all_speech_gate_man,
                             aes(
                               x = ItemToListener,
                               y = HuScore,
                               fill = ItemToListener  # Use ItemEmotion for fill
                             )) +
  geom_col(position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = emotion_colors) +
  theme_minimal() +
  labs(title = "Figure4a. Hu score as a function of Speech type, Langauge type, and gate (Mandarin)",
       x = "Speech Emotion",
       y = "Mean Hu Score",
       fill = "Speech Emotion") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "darkred",
               position = position_dodge(width = 0.8)) +
  facet_grid(ItemEmotion ~ Gate) +  # Use facet_grid for two-way faceting
  theme(strip.text.x = element_text(size = 12, color = "black", face = "bold"),
        strip.text.y = element_text(size = 12, color = "black", face = "bold"),
        panel.spacing.x = unit(3, "lines"),  # Adjust horizontal spacing
        panel.spacing.y = unit(3, "lines"))  # Adjust vertical spacing

#quartz(width=17, height=8) 

box_P4a 

#####
# Q4b LMM model for Hu Score as a function of L1 vs. Speech type  ---------------------------------------------------------

# fillter data for LMM models to VOC condition
df_Q4b_all_speech_gate_arb <- gate_HuScore %>%
  filter(ItemToListener != "Vocalization") %>%
  filter(ListenerLang == "Arabic") %>%
  droplevels() %>%
  group_by(ItemToListener, Gate, ItemEmotion)


summary_df_Q4b <- df_Q4a_all_speech_gate_arb %>%
  group_by(ItemEmotion, Gate, ListenerLang, ItemType) %>%
  summarize(mean=mean(HuScore, na.rm=TRUE),
            sd=sd(HuScore, na.rm=TRUE))

nice_table(summary_df_Q4b)

# LMM model of gate, emotion, speech-Hu Score
LMM_Q4b_all_speech_gate_arb  <- lmer(
  HuScore ~ (ItemToListener + ItemEmotion + Gate)^3  + 
    (1 | Subject), REML = FALSE,
  data = df_Q4b_all_speech_gate_arb 
)

# write LMM results to a table in HTML format
tab_model(LMM_Q4b_all_speech_gate_arb, show.df = TRUE)

# Q4b t-test (pairwise comparison) Estimated marginal means ------------------------------------------------------------------
Q4b_emm_all_speech_arb <- emmeans(LMM_Q4b_all_speech_gate_arb, revpairwise ~ ItemToListener|ItemEmotion|Gate,
                      lmer.df = "satterthwaite", 
                      lmerTest.limit = 1500)  # Adjust pbkrtest.limit if needed

summary(Q4b_emm_all_speech_arb)
summary.Q4b.stats.table <- as.data.frame(summary(Q4b_emm_all_speech_arb))
contrasts_df_4b <- as.data.frame(summary.Q4b.stats.table$contrasts)
nice_table(contrasts_df_4b)

# Q4b plot ----------------------------------------------------------------
emotion_colors <- c(
  'L1' = "#ef8a62",      # Warm terracotta
  'L2' = "#67a9cf",      # Soft blue
  'Foreign' = "#f7fcb9"# Light yellow
  #'Sadness' = "#998ec3"   # Lavender
  
)

# Now create the plot with these colors
box_P4b <- ggplot(df_Q4b_all_speech_gate_arb,
                  aes(
                    x = ItemToListener,
                    y = HuScore,
                    fill = ItemToListener  # Use ItemEmotion for fill
                  )) +
  geom_col(position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = emotion_colors) +
  theme_minimal() +
  labs(title = "Figure4b. Hu score as a function of Speech type, Langauge type, and gate (Arabic)",
       x = "Item language",
       y = "Mean Hu Score",
       fill = "Speech Emotion") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "darkred",
               position = position_dodge(width = 0.8)) +
  facet_grid(ItemEmotion ~ Gate) +  # Use facet_grid for two-way faceting
  theme(strip.text.x = element_text(size = 12, color = "black", face = "bold"),
        strip.text.y = element_text(size = 12, color = "black", face = "bold"),
        panel.spacing.x = unit(3, "lines"),  # Adjust horizontal spacing
        panel.spacing.y = unit(3, "lines"))  # Adjust vertical spacing

#quartz(width=17, height=8) 
box_P4b 


#####
# Q4c LMM for EIP of speech as a function of L1 background, event language and ItemEmotion  -----------------------------------------------------------
# Listener (MAND, ARAB) x Emotion , Event (L1, L2, Foreign) , emotion (ANG, FER, SAD, HAP)

# filter EIP data for Q3a
df_Q4c_all_speech_EIP <- gate_EIP %>%
  filter(ItemToListener != "vocalization", ItemType != "vocalization") %>%
  droplevels() %>%
  group_by(ListenerLang, ItemToListener, EIP)

# the DV is named EIPtime
summary_df_Q4c <- df_Q4c_all_speech_EIP  %>%
  group_by(ListenerLang, ItemToListener,ItemEmotion) %>%
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

# LMM model
LMM_Q4c_all_speech_EIP  <- lmer(
  EIPtime ~ (ListenerLang + ItemToListener + ItemEmotion)^3 +
    (1 | ListenerID), REML = FALSE,
  data = df_Q4c_all_speech_EIP 
)

# write LMM results to a table in HTML format
tab_model(LMM_Q4c_all_speech_EIP, show.df = TRUE)

# Q4c t-tests -------------------------------------------------------------
Q4c_emm_EIP_all_speech <- emmeans(LMM_Q4c_all_speech_EIP, revpairwise ~ ItemToListener|ListenerLang|ItemEmotion, methods ="turkey",
                              lmer.df = "satterthwaite", 
                              lmerTest.limit = 4779)  # Adjust pbkrtest.limit if needed

summary(Q4c_emm_EIP_all_speech )
summary.Q4c.stats.table <- as.data.frame(summary(Q4c_emm_EIP_all_speech ))
contrasts_df_4c <- as.data.frame(summary.Q4c.stats.table$contrasts)
nice_table(contrasts_df_4c)

# Q4c plot ----------------------------------------------------------------
emotion_colors <- c(
  'L1' = "#ef8a62",      # Warm terracotta
  'L2' = "#67a9cf",      # Soft blue
  'Foreign' = "#f7fcb9"# Light yellow
  #'Sadness' = "#998ec3"   # Lavender
  
)

# Now create the plot with these colors
box_P4c <- ggplot(summary_df_Q4c,
                  aes(
                    x = ItemToListener,
                    y = Mean,
                    fill = ItemToListener  # Use ItemEmotion for fill
                  )) +
  geom_col(position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = emotion_colors) +
  theme_minimal() +
  labs(title = "Figure4c. EIP as a function of Speech type, L1 background, Emotion",
       x = "Item language",
       y = "EIP",
       fill = "Speech Emotion") +
  #stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "darkred",
               #position = position_dodge(width = 0.8)) +
  facet_grid(ItemEmotion ~ ListenerLang) +  # Use facet_grid for two-way faceting
  theme(strip.text.x = element_text(size = 12, color = "black", face = "bold"),
        strip.text.y = element_text(size = 12, color = "black", face = "bold"),
        panel.spacing.x = unit(3, "lines"),  # Adjust horizontal spacing
        panel.spacing.y = unit(3, "lines"))  # Adjust vertical spacing

#quartz(width=17, height=8) 
box_P4c 

#####
# Q6  Hu Score at G200 NVV and speech 
# Q6a LMM model for Hu Score as a function of all Speech type (G200 gate only) ---------------------------------------------------------
# {use data of G200 condition only}

# filter data for LMM Q1a, manipulate the data for LMM models to drop L2 and Foreign conditions and keep GFULL
df_Q6_speech_NVV_G200 <- gate_HuScore %>%
  #filter(ItemToListener != "Foreign", ItemToListener != "L2") %>%
  filter(Gate == "G200") %>%
  droplevels() %>%
  group_by(ListenerLang, ItemToListener, Gate, ItemEmotion)

#summary data
summary_df_Q6 <- df_Q6_speech_NVV_G200 %>%
  group_by(ListenerLang, ItemToListener, Gate, ItemEmotion) %>%
  summarise(
    Mean = mean(HuScore, na.rm = TRUE),
    SD = sd(HuScore, na.rm = TRUE),
    #Min = min(HuScore, na.rm = TRUE),
    #Max = max(HuScore, na.rm = TRUE)
    #Median = median(HuScore, na.rm = TRUE)
  ) %>%
  ungroup()  

print(summary_df_Q6)
nice_table(summary_df_Q6)

# first LMM model of Gfull gate-Hu Score
# MODEL 1:	Perceived (MAND, ARAB) x Event (VOC, L1) as fixed factors
# Subject and item emotion as random factors
LMM_Q6_speech_NVV_G200  <- lmer(
  HuScore ~ (ListenerLang + ItemType + ItemEmotion)^3  + 
    (1 | Subject), REML = FALSE,
  data = df_Q6_speech_NVV_G200
)

summary(LMM_Q6_speech_NVV_G200)

# write LMM_Q1a results to a table in HTML format
tab_model(LMM_Q6_speech_NVV_G200, show.df = TRUE)

# Q1a t-test (pairwise comparison) Estimated marginal means ------------------------------------------------------------------
Q6_emm_speech_all_hu <- emmeans(LMM_Q6_speech_NVV_G200, revpairwise ~ ItemEmotion|ItemType|ListenerLang, adjust="tukey",
                            #contrast = list("compare VOC to Utter" = c(-1, 1)),
                            lmer.df = "satterthwaite", 
                            lmerTest.limit = 800)  # Adjust pbkrtest.limit if needed

#summary_LMM_Gfull_Hu. Vocalizations should yield higher accuracy and shorter recognition latency than L1 speech
summary(Q6_emm_speech_all_hu)
summary.Q6.stats.table <- as.data.frame(summary(Q6_emm_speech_all_hu ))
contrasts_df_6 <- summary.Q6.stats.table$contrasts
nice_table(contrasts_df_6)


