#install.packages("readxl")
#install.packages("lme4")
#install.packages("plyr")
#install.packages('smplot2')
#install.packages("rempsyc")
#install.packages("flextable")
#install.packages("broom")
#install.packages("report")
#install.packages("effectsize")

# Import libs -------------------------------------------------------------
library(readxl)
library(tidyverse)
library(lmerTest)
library(Matrix)
library(ggpubr)
library(ggplot2)
library(lme4)
library(dplyr)
library(rempsyc)
library(flextable)
library(broom)
library(report)
library(effectsize)

# set path --------------------------------------------------------------
#setwd('/Users/hainingcui/Dropbox/Trying/EEG_KNOC_Analysis/KNOC_HN_Beh_ERP_corrrelation_analysis_v2') #for OS
setwd('C://Users//hcui8//Dropbox//Trying//EEG_KNOC_Analysis//KNOC_HN_Beh_ERP_corrrelation_analysis_v2') #for windows

file_path <- "C://Users//hcui8//Dropbox//Trying//EEG_KNOC_Analysis//KNOC_HN_Beh_ERP_corrrelation_analysis_v2"
#file_path <- '/Users/hainingcui/Dropbox/Trying/EEG_KNOC_Analysis/KNOC_HN_Beh_ERP_corrrelation_analysis_v2'

# #load pre-EEG rating score ----------------------------------------------
pre_EEG_scores <- read_xlsx('KNOC_preEEG_results.xlsx')

pre_EEG_hit_rate <- cbind(pre_EEG_scores[,1:2], pre_EEG_scores[,6:8], pre_EEG_scores[,9:33])

summary(pre_EEG_hit_rate)
  
pre_EEG_certainty <- cbind(pre_EEG_scores[,1:2], pre_EEG_scores[,6:8], pre_EEG_scores[, 64:88])
pre_EEG_peer <- cbind(pre_EEG_scores[,1:2], pre_EEG_scores[,6:8], pre_EEG_scores[, 90:114])


# summarize postEEG rating scores -----------------------------------------
postEEG_ratings <- read.csv("KNOC_post_rating_hit_ForCorrelate.csv")
colnames(postEEG_ratings)[1] <- "Subject"
colnames(postEEG_ratings)[3] <- "true_weak_post_hit"

# rank of ID knowledge differences
postEEG_ratings <- postEEG_ratings %>%
  #mutate(perform_totoal = true_strong_post_hit + true_weak_hit) %>%
  mutate(perform_post_rank = ifelse(true_strong_post_hit >= 0.9 & true_weak_post_hit >= 0.5 ,"High",
                                    ifelse(true_strong_post_hit < 0.9 & true_weak_post_hit < 0.5 ,"low", "medium")))

print(postEEG_ratings)

#post hit mean per sub copied from excel
post_hit <- read.delim("clipboard")
# Perform t-test
post_hit_t_test_result <- t.test(post_hit$High_true, post_hit$Low_true)


#post confidence rating mean per sub copied from excel

post_condidence <-read.delim("clipboard")

# Perform t-test
post_confidence_t_test_result <- t.test(post_condidence$High_true, post_condidence$low_true)

# # load N400 data, clean, summarize mean ---------------------------------

All_clean_N400 <- read.csv('clean_data_N400_v2.csv')
colnames(All_clean_N400)[8] <- "MemoryStrength" 
colnames(All_clean_N400)[2] <- "Activation_N400" 
All_clean_N400$MemoryStrength[All_clean_N400$MemoryStrength == "H"] <- "Strong"
All_clean_N400$MemoryStrength[All_clean_N400$MemoryStrength == "L"] <- "Weak"
All_clean_N400$TruthValue[All_clean_N400$TruthValue == "FALSE"] <- "False"
All_clean_N400$TruthValue[All_clean_N400$TruthValue == "TRUE"] <- "True"
All_clean_N400$TruthValue <- factor(All_clean_N400$TruthValue, levels = c("False", "True"))
All_clean_N400$MemoryStrength <- factor(All_clean_N400$MemoryStrength, levels = c("Strong", "Weak"))


# summarize the mean and sum of 'Activation_N400' by 'Subjects' 'TruthValue', and 'MemoryStrength'
summary_mean_N400 <- All_clean_N400 %>%
  group_by(Subject, TruthValue, MemoryStrength) %>%
  summarize(mean_activation_N400 = mean(Activation_N400))

# N400 condition amplitude differences
summary_mean_N400 <- summary_mean_N400 %>% 
  pivot_wider(
    names_from = c(TruthValue, MemoryStrength),
    values_from = c(mean_activation_N400)
  ) 

summary_mean_N400 <- summary_mean_N400 %>% 
  mutate(
    N400_TS_FS = True_Strong - False_Strong,
    N400_FW_TW = False_Weak - True_Weak,
    N400_FS_TS = False_Strong - True_Strong,
    N400_TW_FW = True_Weak - False_Weak
   
  )

# load LPC data, clean, summarize mean ----------------------------------

All_clean_LPC <- read.csv('clean_data_LPC_v2.csv')
colnames(All_clean_LPC)[8] <- "MemoryStrength" 
colnames(All_clean_LPC)[2] <- "Activation_LPC" 
All_clean_LPC$MemoryStrength[All_clean_LPC$MemoryStrength == "H"] <- "Strong"
All_clean_LPC$MemoryStrength[All_clean_LPC$MemoryStrength == "L"] <- "Weak"
All_clean_LPC$TruthValue[All_clean_LPC$TruthValue == "FALSE"] <- "False"
All_clean_LPC$TruthValue[All_clean_LPC$TruthValue == "TRUE"] <- "True"
All_clean_LPC$TruthValue <- factor(All_clean_LPC$TruthValue, levels = c("False", "True"))
All_clean_LPC$MemoryStrength <- factor(All_clean_LPC$MemoryStrength, levels = c("Strong", "Weak"))


# summarize the mean and sum of 'Activation_LPC' by 'Subjects'
summary_mean_LPC <- All_clean_LPC %>%
  group_by(Subject, TruthValue, MemoryStrength) %>%
  summarize(mean_activation_LPC = mean(Activation_LPC))

# LPC condition amplitude differences
summary_mean_LPC <- summary_mean_LPC %>% 
  pivot_wider(
    names_from = c(TruthValue, MemoryStrength),
    values_from = c(mean_activation_LPC)
  ) 

summary_mean_LPC <- summary_mean_LPC %>% 
  mutate(
    LPC_TS_FS = True_Strong - False_Strong,
    LPC_FW_TW = False_Weak - True_Weak,
    LPC_FS_TS = False_Strong - True_Strong,
    LPC_TW_FW = True_Weak - False_Weak
  )


# online behavior rating, clean data, and checking missing val, and add ambiguity --------
online_ratings <- read.delim("KNOC_EEG_beha_online_neutreal_all2.txt")
online_ratings$TV[online_ratings$TV == "FF"] <- "False"
online_ratings$TV[online_ratings$TV == "TT"] <- "True"
online_ratings$KT[online_ratings$KT == "H"] <- "Strong"
online_ratings$KT[online_ratings$KT == "L"] <- "Weak"
online_ratings$TV <- factor(online_ratings$TV, levels = c("False", "True"))
online_ratings$KT <- factor(online_ratings$KT, levels = c("Strong", "Weak"))
print(is_factor <- is.factor(online_ratings$KT))
print(is_factor <- is.factor(online_ratings$TV))
colnames(online_ratings)[23] <- "TruthValue" 
colnames(online_ratings)[25] <- "MemoryStrength" 

# summary of mean and sd ratings scores 
online_ratings_discribtive <- online_ratings %>%
  group_by(TruthValue,MemoryStrength) %>%
  summarise(mean_rating = mean(Recoded_Response),
            sd_rating = sd(Recoded_Response)
  )

summary_online_rating <- online_ratings %>%
  group_by(Subject, TruthValue, MemoryStrength) %>%
  add_count() %>%
  summarize(mean_rating = mean(Recoded_Response, na.rm = TRUE),
            count = n())

# # create the violinplot with mean and SD ------------------------------------
# graph_rating <- ggplot(online_ratings,
#                 aes(x=TruthValue, y=Recoded_Response, fill=interaction(TruthValue, MemoryStrength))) +
#   geom_violin(trim = TRUE)+
#   facet_grid(~MemoryStrength) +
#   theme_classic() +
#   geom_line(data = online_ratings_summary, aes(y = mean_rating, group = MemoryStrength))
#
# graph_rating <- graph_rating +stat_summary(fun=mean, geom="point",
#                      shape=15, color="black",
#                      fill="black")
#
# print(graph_rating)

# # perform LMM on the online ratings scales --------------------------------
# # Set "false" as the reference level for "TruthValue"
# online_ratings$TruthValue <- relevel(online_ratings$TruthValue, ref = "False")
# 
# # Set "Weak" as the reference level for "MemoryStrength"
# online_ratings$MemoryStrength <- relevel(online_ratings$MemoryStrength, ref = "Weak")
# 
# # perform LMMs 
# lmm <- lmer(Recoded_Response ~ 1 + TruthValue + MemoryStrength +(1|Subject) + (1|Item), data = online_ratings, REML=F)
# 
# lmm1 <- lmer(Recoded_Response ~1 +  TruthValue:MemoryStrength + (1|Subject) + (1|Item), data = online_ratings,REML=F)
# 
# lmm2 <- lmer(Recoded_Response ~ 1 + TruthValue + (1|Subject) + (1|Item), data = online_ratings, REML=F)
# 
# lmm3 <- lmer(Recoded_Response ~1 +  MemoryStrength + (1|Subject) + (1|Item), data = online_ratings,REML=F)
# 
# # Get the summary output as a character vector
# summary_lmm <- capture.output(summary(lmm), anova(lmm))
# print(summary_lmm)
# summary_lmm1 <- capture.output(summary(lmm1), anova(lmm1))
# print(summary_lmm1)
# summary_lmm2 <- capture.output(summary(lmm2), anova(lmm2))
# summary_lmm3 <- capture.output(summary(lmm3), anova(lmm3))
# write.csv(summary_lmm,"summary_LMM_beh_main.csv")
# write.csv(summary_lmm1,"summary_LMM_beh_interaction.csv")
# 
# # simple effect t-test (pairwise comparison) Estimated marginal meanson level of Truth value collapsed on Memory strength (knowledge type) ----------------------------------------------
# library(emmeans)
# (p_emm1 <- emmeans(lmm,~KT:TV, pbkrtest.limit = 4578) %>% pairs(adjust="Tukey", side = "="))
# 
# ## Effect size
# (emm1 <- emmeans(lmm,~TV:KT, pbkrtest.limit = 4578))
# #(eff1 <- eff_size(emm1, sigma = sigma(lmm), edf = df.residual(lmm)))
# 
# ## Output
# CI_emm1 <- confint(p_emm1)
# p_emm1<-as.data.frame(p_emm1)
# #eff1<-as.data.frame(eff1)
# 
# output1 <- data.frame(p_emm1$contrast,round(p_emm1$estimate,1),round(CI_emm1$lower.CL,1),
#                     round(CI_emm1$upper.CL,1),round(p_emm1$p.value,3))
# 
# names(output1) <- c("Effect","EMMeans","95% CI (Lower)","95% CI (Upper)","p-value")
# output1
# # correct p value
# output1$`p-value`<-ifelse(output1$`p-value`<0.001,"<0.001",round(output1$`p-value`,3))
# 
# write.csv(output1, "postHoc_beh_results_P_effectSize.csv")
# write.csv(p_emm1, "postHoc_beh_t-test_results.csv")

# # add ambiguity level based on the re-coded response (1&5 least- --------

# Create a function to determine ambiguity
classify_ambiguity <- function(value) {
  if (value %in% c(1, 5)) {
    return("0")
  } else if (value %in% c(2, 4, 3)) {
    return("1")
  } 
  else {
    return("Undefined") # You can modify this according to your needs
  }
}

# add the 'Ambiguity" column using mutate
online_ratings_amb <- online_ratings %>%
  mutate(Ambiguity = sapply(Recoded_Response,classify_ambiguity))

# transform Ambiguity to numerical 
online_ratings_amb <- transform(online_ratings_amb, Ambiguity = as.numeric(Ambiguity))
class(online_ratings_amb $Ambiguity)

# Create a table with all combinations of Subject, Ambiguity, TruthValue, and MemoryStrength
all_combinations <- expand.grid(
  Subject = unique(online_ratings_amb$Subject),
  Ambiguity = unique(online_ratings_amb$Ambiguity),
  TruthValue = unique(online_ratings_amb$TruthValue),
  MemoryStrength = unique(online_ratings_amb$MemoryStrength)
)

# Perform left join to keep all combinations and summarize data with amb
summary_online_rating_amb <- all_combinations %>%
  left_join(online_ratings_amb, by = c("Subject", "Ambiguity", "TruthValue", "MemoryStrength")) %>%
  group_by(Subject, Ambiguity, TruthValue, MemoryStrength) %>%
  summarize(mean_rating = mean(Recoded_Response, na.rm = TRUE),
            count = sum(!is.na(Recoded_Response)))

# # rename subject columns 
# summary_mean_beha$Subject <- gsub("sub0", "", as.character((summary_mean_beha$Subject)))
# summary_mean_beha$Subject <- gsub("sub", "", as.character((summary_mean_beha$Subject)))
# summary_mean_beha$Subject <- as.numeric(summary_mean_beha$Subject)

#write_excel_csv(summary_mean_beha, file="summary_mean_KNOC_beha_ambi.csv")


# subset strong-memory-least ambiguity subgroups ------------------------

# function for selecting ambiguity based on memory strength subgroups
subset_data_strong <- function(data_frame, mem_strength, ambiguity_val) {
  subsetted <- data_frame %>%
    filter(MemoryStrength == mem_strength & Ambiguity == ambiguity_val & (TruthValue == "True" | TruthValue == "False"))
  return(subsetted)
}

# Applying the function to your data
filtered_subset_S_Umb <- subset_data_strong(summary_online_rating_amb, "Strong", 0)
filtered_subset_W_Umb <- subset_data_strong(summary_online_rating_amb,"Weak", 0)

# regroup ambiguity using median split
rename_ambiguity_level_median <- function(data_frame, count_col, ambiguity_col) {
  median_count <- median(data_frame[[count_col]])
  
  if (median_count > 0) {
    data_frame[[ambiguity_col]] <- ifelse(data_frame[[count_col]] > median_count, 0, 1)
  } else {
    print("Count column has no positive values.")
  }
  
  return(data_frame)
}

# function for for regroup ambiguity using portion of 36 items
rename_ambiguity_level_portion <- function(data_frame, count_col, ambiguity_col, total_items = 36, threshold_percent = 50) {
  total_count <- sum(data_frame[[count_col]])
  
  if (total_count > 0) {
    portion_threshold <- total_count * (threshold_percent / 100) / total_items
    data_frame[[ambiguity_col]] <- ifelse(data_frame[[count_col]] > portion_threshold, 0, 1)
  } else {
    print("Count column has no positive values.")
  }
  
  return(data_frame)
}

# regroup based on portion 50% chance level threshold
filtered_subset_S_Umb_re <- rename_ambiguity_level_portion(filtered_subset_S_Umb, "count", "Ambiguity")
filtered_subset_W_Umb_re <- rename_ambiguity_level_portion(filtered_subset_W_Umb, "count", "Ambiguity")

# write_excel_csv(filtered_subset_S_Umb_re, file="KNOC_beha_ambi_subLevel_S.csv")
# write_excel_csv(filtered_subset_W_Umb_re, file="KNOC_beha_ambi_subLevel_W.csv")


# import subgroup data
regroup_strong_amb <- read.delim("clipboard")
# rename subject columns 
regroup_strong_amb$Subject <- gsub("sub0", "", as.character((regroup_strong_amb$Subject)))
regroup_strong_amb$Subject <- gsub("sub", "", as.character((regroup_strong_amb$Subject)))
# transform subject to numerical 
regroup_strong_amb<- transform(regroup_strong_amb, Subject = as.numeric(Subject))
class(regroup_strong_amb$Subject)

regroup_weak_amb <- read.delim("clipboard")
regroup_weak_amb$Subject <- gsub("sub0", "", as.character((regroup_weak_amb$Subject)))
regroup_weak_amb$Subject <- gsub("sub", "", as.character((regroup_weak_amb$Subject)))
regroup_weak_amb<- transform(regroup_weak_amb, Subject = as.numeric(Subject))
class(regroup_weak_amb$Subject)

# #correlation between Ambiguous (binary categorical) and LPC, N400 mean activation subject level * none-sig--------

cor_N400_amb_S <- cor.test(regroup_strong_amb$Ambiguity, summary_mean_N400$N400_TS_FS)
cor_LPC_amb_S <- cor.test(regroup_strong_amb$Ambiguity, summary_mean_LPC$LPC_TS_FS)
cor_LPC_amb_S2 <- cor.test(regroup_strong_amb$Ambiguity, summary_mean_LPC$LPC_FS_TS)

cor_N400_amb_W <- cor.test(regroup_weak_amb$Ambiguity, summary_mean_N400$N400_FW_TW)
cor_LPC_amb_W <- cor.test(regroup_weak_amb$Ambiguity, summary_mean_LPC$LPC_FW_TW)
cor_LPC_amb_W2 <- cor.test(regroup_weak_amb$Ambiguity, summary_mean_LPC$LPC_TW_FW)

# #correlation between N400 and LPC mean amplitude subject level *sig --------
cor_N400_LPC_TS <- cor.test(summary_mean_N400$True_Strong, summary_mean_LPC$True_Strong)
cor_N400_LPC_FS <- cor.test(summary_mean_N400$False_Strong, summary_mean_LPC$False_Strong)
cor_N400_LPC_TW <- cor.test(summary_mean_N400$True_Weak, summary_mean_LPC$True_Weak)
cor_N400_LPC_FW <- cor.test(summary_mean_N400$False_Weak, summary_mean_LPC$False_Weak)



# #correlation between N400 and LPC mean amplitude subject level *sig --------
cor_N400_LPC_TS_FS <- cor.test(summary_mean_N400$N400_TS_FS, summary_mean_LPC$LPC_TS_FS)

cor_N400_LPC_TW_FW <- cor.test(summary_mean_N400$N400_TW_FW, summary_mean_LPC$LPC_TW_FW)

# #correlation between N400 and LPC mean amplitude item level *sig --------

#co_N400_LPC_TS_item <- cor.test(All_clean_LPC$Activation_LPC, All_clean_N400)


# # # merge amb_strong and N400 ERP_mean ----------------------------------------------------
# summary_mean_N400 <- list(summary_mean_N400, regroup_strong_amb) %>%
#   reduce(full_join, by= c("Subject"))
# summary_mean_N400  <- data.frame(summary_mean_N400)
# 
# summary_mean_N400 <- list(summary_mean_N400, regroup_strong_amb) %>%
#   reduce(function(x, y) full_join(x, y[, c("Subject", "Ambiguity")], by = "Subject"))
# summary_mean_N400 <- data.frame(summary_mean_N400)
# 
# summary_mean_N400_amb <- pivot_longer(summary_mean_N400, cols = starts_with("False_Strong") | starts_with("True_Strong"), 
#                                       names_to = "Condition", values_to = "Amplitude")
# LMM_amb_N400 <- lmer(Amplitude ~ (Condition+Ambiguity)^2 + (1|Subject), data = summary_mean_N400_amb)
# summary(LMM_amb_N400)
# anova(LMM_amb_N400)
# summary(LMM_amb_N400)$coefficients

# # merge amb_strong and LPC ERP_mean for LMM model *none-sig ----------------------------------------------------
summary_mean_LPC <- list(summary_mean_LPC, regroup_strong_amb) %>%
  reduce(full_join, by= c("Subject"))
summary_mean_LPC  <- data.frame(summary_mean_LPC)

summary_mean_LPC_amb <- pivot_longer(summary_mean_LPC, cols = starts_with("False_Strong") | starts_with("True_Strong"), 
                                      names_to = "Condition", values_to = "Amplitude")

summary_mean_LPC_amb <- transform(summary_mean_LPC_amb, Ambiguity = as.character(Ambiguity))


LMM_amb_LPC_strong <- lmer(Amplitude ~ (Condition+Ambiguity)^2 + (1|Subject), data = summary_mean_LPC_amb)
summary(LMM_amb_LPC_strong)
anova(LMM_amb_LPC_strong)
summary(LMM_amb_LPC_strong)$coefficients

# # merge amb_weak and LPC ERP_mean for LMM model *none-sig ----------------------------------------------------
summary_mean_LPC_weak_amb <- list(summary_mean_LPC, regroup_weak_amb) %>%
  reduce(function(x, y) full_join(x, y[, c("Subject", "Ambiguity")], by = "Subject"))
summary_mean_LPC_weak_amb  <- data.frame(summary_mean_LPC_weak_amb)

summary_mean_LPC_weak_amb  <- pivot_longer(summary_mean_LPC_weak_amb , cols = starts_with("False_Weak") | starts_with("True_Weak"), 
                                     names_to = "Condition", values_to = "Amplitude")
summary_mean_LPC_weak_amb <- transform(summary_mean_LPC_weak_amb, Ambiguity = as.character(Ambiguity))

LMM_amb_LPC_weak <- lmer(Amplitude ~ (Condition+Ambiguity)^2 + (1|Subject), data = summary_mean_LPC_weak_amb)

summary(LMM_amb_LPC_weak)
anova(LMM_amb_LPC_weak)
summary(LMM_amb_LPC_weak)$coefficients

# # merge_online_beha and ERP_mean ----------------------------------------------------
# summary_mean_list <- list(summary_mean_beha, summary_mean_N400, summary_mean_LPC) %>% 
#   reduce(full_join, by= c("Subject", "MemoryStrength", "TruthValue"))  
# summary_mean_all <- data.frame(summary_mean_list)
# 
# # merge with post rating score 
# summary_mean_all<- list(summary_mean_all,postEEG_ratings) %>%
#   reduce(full_join, by= c("Subject"))
# summary_mean_all <- data.frame(summary_mean_all)
# 
# summary_mean_all <- summary_mean_all %>% 
#   pivot_wider(
#     names_from = c(TruthValue, MemoryStrength),
#     values_from = c(mean_rating,mean_activation_N400,mean_activation_LPC)
#   ) 
# 
# summary_mean_all <- summary_mean_all %>% 
#   mutate(
#     rate_TS_FS = mean_rating_True_Strong - mean_rating_False_Strong,
#     N400_TS_FS = mean_activation_N400_True_Strong - mean_activation_N400_False_Strong,
#     LPC_TS_FS = mean_activation_LPC_True_Strong - mean_activation_LPC_False_Strong,
#     #
#     rate_TS_TW = mean_rating_True_Strong - mean_rating_True_Weak,
#     N400_TS_TW = mean_activation_N400_True_Strong - mean_activation_N400_True_Weak,
#     LPC_TS_TW = mean_activation_LPC_True_Strong - mean_activation_LPC_True_Weak,
#     #
#     rate_FS_FW = mean_rating_False_Strong - mean_rating_False_Weak,
#     N400_FS_FW = mean_activation_N400_False_Strong - mean_activation_N400_False_Weak,
#     LPC_FS_FW = mean_activation_LPC_False_Strong - mean_activation_LPC_False_Weak
#   ) %>%
#   mutate(
#     perfrom_online_rank = ifelse(mean_rating_True_Strong > 4 & mean_rating_True_Weak > 4, "high",
#                                       #ifelse(mean_rating_False_Strong > 3 & mean_rating_False_Weak > 3, "low",
#                                              "low"))
# 
# ggplot(subset(summary_mean_all, perfrom_online_rank %in% c("high", "low")), aes(x = perfrom_online_rank, y = mean_activation_N400_True_Weak))+
#   geom_col(aes(fill = perfrom_online_rank), width = 0.7) +
#   scale_fill_viridis_d()
# 


# # perform correlation test between online rating scores and ERP activation for TS-FS conditions --------
# correlate_Beha_N400_TS_FS <- corr.test(summary_mean_all$rate_TS_FS, summary_mean_all$N400_TS_FS, adjust = 'none')
# correlate_Beha_LPC_TS_FS <- corr.test(summary_mean_all$rate_TS_FS, summary_mean_all$LPC_TS_FS, adjust = 'none')
# 
# 
# correlate_Beha_N400_TS_FS_high <- cor.test(formula = ~ rate_TS_FS + N400_TS_FS, 
#                                                   data = summary_mean_all,
#                                                   subset = perfrom_online_rank == "high")
# 
# correlate_Beha_N400_TS_FS_low <- cor.test(formula = ~ rate_TS_FS + N400_TS_FS, 
#                                            data = summary_mean_all,
#                                            subset = perfrom_online_rank == "low")
# 
# correlate_Beha_LPC_TS_FS_high <- cor.test(formula = ~ rate_TS_FS + LPC_TS_FS, 
#                                       data = summary_mean_all,
#                                       subset = perfrom_online_rank == "high")
# 
# correlate_Beha_LPC_TS_FS_low <- cor.test(formula = ~ rate_TS_FS + LPC_TS_FS, 
#                                           data = summary_mean_all,
#                                           subset = perfrom_online_rank == "low")
# 
# # perform correlation between online rating scores and ERP activation for TS-TW ---------------------------------------------
# 
# correlate_Beha_N400_TS_TW <- corr.test(summary_mean_all$rate_TS_TW, summary_mean_all$N400_TS_TW, adjust = 'none')
# correlate_Beha_LPC_TS_TW <- corr.test(summary_mean_all$rate_TS_TW, summary_mean_all$LPC_TS_TW, adjust = 'none')
# 
# # perform correlation TS-TW of N400 effect
# correlate_Beha_N400_TS_TW_high <- cor.test(formula = ~ rate_TS_TW + N400_TS_TW, 
#                                            data = summary_mean_all,
#                                            subset = perfrom_online_rank == "high")
# 
# correlate_Beha_N400_TS_TW_low <- cor.test(formula = ~ rate_TS_TW + N400_TS_TW, 
#                                           data = summary_mean_all,
#                                           subset = perfrom_online_rank == "low")
# 
# # perform correlation TS-TW of LPC effect
# correlate_Beha_LPC_TS_TW_high <- cor.test(formula = ~ rate_TS_TW + LPC_TS_TW,   # significant
#                                           data = summary_mean_all,
#                                           subset = perfrom_online_rank == "high")
# 
# # plot significant correlation TS_FS high performance LPC --------------------------------------------------------
# ggplot(subset(summary_mean_all, perfrom_online_rank %in% c("high"))) + 
#   geom_point(mapping = aes(x = rate_TS_TW, y = LPC_TS_TW, color = perfrom_online_rank )) +
#   geom_smooth(mapping = aes(x = rate_TS_TW, y = LPC_TS_TW))
# 
# ggplot(subset(summary_mean_all, perfrom_online_rank %in% c("high"))) + 
#   geom_point(mapping = aes(x = rate_TS_TW, y = LPC_TS_TW, color = "blue" )) 
# 
# correlate_Beha_LPC_TS_TW_low <- cor.test(formula = ~ rate_TS_TW + LPC_TS_TW,   
#                                           data = summary_mean_all,
#                                           subset = perfrom_online_rank == "low")
# 
# # perform correlation TS rating and LPC response
# correlate_Beha_LPC_TS_high <- cor.test(formula = ~ mean_rating_True_Strong + mean_activation_LPC_True_Strong,   
#                                           data = summary_mean_all,
#                                           subset = perfrom_online_rank == "high")
# 
# correlate_Beha_LPC_TS_low <- cor.test(formula = ~ mean_rating_True_Strong + mean_activation_LPC_True_Strong,  
#                                        data = summary_mean_all,
#                                        subset = perfrom_online_rank == "low")
# 
# # # perform correlation FS-FW ---------------------------------------------
# 
# correlate_Beha_N400_FS_FW <- corr.test(summary_mean_all$rate_FS_FW, summary_mean_all$N400_FS_FW, adjust = 'none')
# correlate_Beha_LPC_FS_FW <- corr.test(summary_mean_all$rate_FS_FW, summary_mean_all$LPC_FS_FW, adjust = 'none')
# 
# # perform correlation test between post rating hit rate and ERP response differences
# correlation_Post_N400 <- corr.test(postEEG_ratings$post_hit_TS_TW,summary_mean_all$N400_TS_TW, adjust = 'none')
# correlation_Post_LPC <- corr.test(postEEG_ratings$post_hit_TS_TW,summary_mean_all$LPC_TS_TW, adjust = 'none')
# 
# correlation_post_N400_TS <- corr.test(postEEG_ratings$true_strong_post_hit, summary_mean_all$mean_activation_N400_True_Strong, adjust = 'none')
# correlation_post_N400_TW <- corr.test(postEEG_ratings$true_weak_hit, summary_mean_all$mean_activation_N400_True_Weak, adjust = 'none')
# 
# correlation_post_LPC_TS <- corr.test(postEEG_ratings$true_strong_post_hit, summary_mean_all$mean_activation_LPC_True_Strong, adjust = 'none')
# correlation_post_LPC_TW <- corr.test(postEEG_ratings$true_weak_hit, summary_mean_all$mean_activation_LPC_True_Weak, adjust = 'none')
# 
# # perform correlation test on the postEEG ratings and averaged ERP response --------
# # 
# # corr.test(factor_data[, 1:4], Behav_results[,3:6], adjust = 'none')
# # 
# # cor(factor_data[, 1:4], IAT_Dscore$Dscore)




