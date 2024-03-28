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
ggplot(df_aov, aes(x= species, y = emmean, fill = factor(treatment, levels =order)))+
  geom_bar(position = 'dodge', stat = 'identity', color="black")+
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), position = position_dodge(.9), width = 0.2) + 
  geom_text(aes(label = cld, y = upper.CL), vjust = -0.5, position = position_dodge(0.9),size = 3)+
  labs(x="Culture", y="Diameter (µm)", fill="Media Type")+
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
  labs(x="Culture", y="Growth Rate (1/day)", fill="Media Treatment")+
  theme_bw()+  scale_fill_manual(values=colors)
mixo

#### Fv/Fm ####
## GC (From GC June file)
order <- c('Replete', 'Low Mn', 'Low Fe', 'Low Fe, Mn')
gc_fv <- read_excel("~/Desktop/OneDrive/Lab/prelimMIXO/FIRe/JuneGC/JuneGC.xlsx")
colnames(gc_fv)[6]='PAR'
colnames(gc_fv)[12]='FvFm'


data <- data.frame(treatment = gc_fv$Media, result = gc_fv$FvFm)

df_aov <- data %>%
  tidyr::nest() %>%
  rowwise() %>% 
  dplyr::mutate(aov_results = list(aov(result ~ treatment, data = data)), 
                emm = list(emmeans::emmeans(aov_results, "treatment", type= "response")), 
                cld = list(multcomp::cld(emm, Letters = LETTERS, reverse = TRUE))) %>% 
  dplyr::select(-data, -aov_results, -emm) %>% 
  unnest(cols = c(cld)) %>%
  dplyr::mutate(cld = trimws(.group)) %>% 
  dplyr::select(-.group)

gcfv <- ggplot(df_aov, aes(x=factor(treatment, levels=order), y = emmean, fill = factor(treatment, levels =order)))+
  geom_bar(position = 'dodge', stat = 'identity', color="black")+
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), position = position_dodge(.9), width = 0.2) + 
  geom_text(aes(label = cld, y = upper.CL), vjust = -0.5, position = position_dodge(0.9),size = 3)+
  labs(y="Fv/Fm", x="",fill="Media Type", title="G. Cryophilia Photosynthetic Efficiency (PSII)")+
  theme_bw()+ scale_fill_manual(values=colors)+scale_y_continuous(limits=c(0,0.6))
gcfv

## MA
ma_fv <- read_excel("~/Desktop/OneDrive/Lab/prelimMIXO/FIRe/JuneMA/JuneMA.xlsx")
colnames(ma_fv)[6]='PAR'
colnames(ma_fv)[11]='FvFm'
ma_fv <- ma_fv[-4,]
data <- data.frame(treatment = ma_fv$Media, result = ma_fv$FvFm)

df_aov <- data %>%
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
mafv <- ggplot(df_aov, aes(x=factor(treatment, levels=order), y = emmean, fill = factor(treatment, levels=order)))+
  geom_bar(position = 'dodge', stat = 'identity', color="black")+
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), position = position_dodge(.9), width = 0.2) + 
  geom_text(aes(label = cld, y = upper.CL), vjust = -0.5, position = position_dodge(0.9),size = 3)+
  labs(y="Fv/Fm", x="",fill="Media Type", title="M. Antarctica Photosynthetic Efficiency (PSII)")+
  theme_bw() + scale_fill_manual(values=colors) + scale_y_continuous(limits=c(-0.04,0.25))
mafv

## PT (from PT_ETR.R in ETR folder)
setwd("~/Desktop/OneDrive/Lab/prelimMIXO/FIRe/ETR")
order <- c('Replete', 'Low Mn', 'Low Fe', 'Low Fe, Mn')
ptfv <- read_excel("PT_ETR.xlsx", sheet="Fv")
colnames(ptfv)[5]='PAR'
colnames(ptfv)[11]='FvFm'

data <- data.frame(treatment = ptfv$Media, result = ptfv$FvFm)

df_aov <- data %>%
  tidyr::nest() %>%
  rowwise() %>% 
  dplyr::mutate(aov_results = list(aov(result ~ treatment, data = data)), 
                emm = list(emmeans::emmeans(aov_results, "treatment", type= "response")), 
                cld = list(multcomp::cld(emm, Letters = LETTERS, reverse = TRUE))) %>% 
  dplyr::select(-data, -aov_results, -emm) %>% 
  unnest(cols = c(cld)) %>%
  dplyr::mutate(cld = trimws(.group)) %>% 
  dplyr::select(-.group)

ptfv <- ggplot(df_aov, aes(x=factor(treatment, levels=order), y = emmean, fill = factor(treatment, levels =order)))+
  geom_bar(position = 'dodge', stat = 'identity', color="black")+
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), position = position_dodge(.9), width = 0.2) + 
  geom_text(aes(label = cld, y = upper.CL), vjust = -0.5, position = position_dodge(0.9),size = 3)+
  labs(y="Fv/Fm", x="Media Type",fill="Media Type", title="P. tychotreta Photosynthetic Efficiency (PSII)")+
  theme_bw() + scale_fill_manual(values=colors) + scale_y_continuous(limits=c(0,0.6))
ptfv

grid.arrange(gcfv, mafv, ptfv)

