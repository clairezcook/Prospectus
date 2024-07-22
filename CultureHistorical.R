library(ggplot2)
library(data.table)
library(dplyr)
library(plyr)
library(readxl)
library(gtools)
library(ggpubr)
library(tidyr)
colors <- c("Low Fe" = "gray30", "Low Fe, Mn" = "black", "Replete" = "white","Low Mn" = "gray80")


setwd("~/Library/CloudStorage/OneDrive-UniversityofGeorgia/Lab/prelimMIXO/CoultureCounter")
Sizes <- read_excel("CoulterCounter.xlsx", sheet="Sheet2")
colnames(Sizes)[10] = "Stdev"

#### Size ####
data <- data.frame (species = Sizes$Culture, treatment = Sizes$Media, result = Sizes$Average)


df_aov <- data %>%
  dplyr::group_by(species) %>%
  tidyr::nest() %>%
  rowwise() %>% 
  dplyr::mutate(aov_results = list(aov(result ~ treatment, data = data)), 
                emm = list(emmeans::emmeans(aov_results, "treatment", type= "response")), 
                cld = list(multcomp::cld(emm, Letters = LETTERS, reverse = TRUE))) %>% 
  dplyr::select(-data, -aov_results, -emm) %>% 
  unnest(cols = c(cld)) %>%
  dplyr::mutate(cld = trimws(.group)) %>% 
  dplyr::select(-.group)

order <- c("Replete", "Low Mn", "Low Fe", "Low Fe, Mn")
size <- ggplot(df_aov, aes(x= species, y = emmean, fill = factor(treatment, levels =order)))+
  geom_bar(position = 'dodge', stat = 'identity', color="black")+
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), position = position_dodge(.9), width = 0.2) + 
  geom_text(aes(label = cld, y = upper.CL), vjust = -0.5, position = position_dodge(0.9),size = 3)+
  labs(x="Culture", y="Diameter (µm)", fill="Media Type", title="b)")+
  theme_bw() + scale_fill_manual(values=colors)

#### Growth Rates ####
setwd("~/Library/CloudStorage/OneDrive-UniversityofGeorgia/Lab/prelimMIXO/Growth/ChelexedGS_Raw")
GrowthRates <- read_excel("GrowthRates.xlsx", sheet = "newer")
colnames(GrowthRates)[4]='Rate'
order <- c("Replete", "Low Mn", "Low Fe", "Low Fe, Mn")

GrowthRates <- subset(GrowthRates, Transfer %in% c(4, 5, 8, 9, 10, 14))

data <- data.frame(species = GrowthRates$Culture, treatment = GrowthRates$Media, result = GrowthRates$Rate)

df_aov <- data %>%
  dplyr::group_by(species) %>%
  tidyr::nest() %>%
  rowwise() %>% 
  dplyr::mutate(aov_results = list(aov(result ~ treatment, data = data)), 
                emm = list(emmeans::emmeans(aov_results, "treatment", type= "response")), 
                cld = list(multcomp::cld(emm, Letters = LETTERS, reverse = TRUE))) %>% 
  dplyr::select(-data, -aov_results, -emm) %>% 
  unnest(cols = c(cld)) %>%
  dplyr::mutate(cld = trimws(.group)) %>% 
  dplyr::select(-.group)

mixo <- ggplot(df_aov, aes(x= species, y = emmean, fill = factor(treatment, levels=order)))+
  geom_bar(position = 'dodge', stat = 'identity', color="black")+
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), position = position_dodge(.9), width = 0.2) + 
  geom_text(aes(label = cld, y = upper.CL), vjust = -0.5, position = position_dodge(0.9),size = 3)+
  labs(x="Culture", y="Growth Rate (1/day)", fill="Media Treatment", title="a)")+
  theme_bw()+  scale_fill_manual(values=colors)
mixo

#### Fv/Fm ####
colors <- c("Low Fe" = "gray30", "Low Fe, Mn" = "black", "Replete" = "white","Low Mn" = "gray80")

setwd("~/Desktop/OneDrive/Lab/prelimMIXO/FIRe")
fvfm <- read_excel("HistoricalFvFm.xlsx")

# Perform ANOVA and post-hoc tests
data <-  data.frame (species = fvfm$Culture, treatment = fvfm$Media, result = fvfm$`Fv/Fm`)
df_aov <- data %>%
  dplyr::group_by(species) %>%
  tidyr::nest() %>%
  rowwise() %>% 
  dplyr::mutate(aov_results = list(aov(result ~ treatment, data = data)), 
                emm = list(emmeans::emmeans(aov_results, "treatment", type= "response")), 
                cld = list(multcomp::cld(emm, Letters = LETTERS, reverse = TRUE))) %>% 
  dplyr::select(-data, -aov_results, -emm) %>% 
  unnest(cols = c(cld)) %>%
  dplyr::mutate(cld = trimws(.group)) %>% 
  dplyr::select(-.group)

# Plot the combined data
fvfm <- ggplot(df_aov, aes(x = species, y = emmean, fill = factor(treatment, levels = order))) +
  geom_bar(position = 'dodge', stat = 'identity', color = "black") +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), position = position_dodge(0.9), width = 0.2) +
  geom_text(aes(label = cld, y = upper.CL), vjust = -0.5, position = position_dodge(0.9), size = 3) +
  labs(x = "Culture", y = "Fv/Fm", fill = "Media Treatment", title="c)") +
  theme_bw() + scale_fill_manual(values = colors)

print(fvfm)



library(gridExtra)
combined_plot <- grid.arrange(mixo, size, fvfm)

                              