library(ggplot2)
library(data.table)
library(dplyr)
library(readxl)
library(gtools)
library(grid)
library(readxl)
library(arsenal)
library(ggpmisc)
library(gridExtra)
library(caret)
library(tidyr)
library(stringr)
library(patchwork)


#### Station 9,10 only surface ####
setwd("~/Desktop/OneDrive/Cruises/PUPCYCLE/ReanayzedJan2024/Stations")
lysotracker <- read_excel("pup_stations11jan24.xlsx", 
                          sheet="OSM") %>%
  filter(Depth=="SUR") %>%
  mutate(upwellstatus = ifelse(Station == 9, "New", ifelse(Station == 10, "Aged", NA)))
setwd("~/Desktop/OneDrive/Cruises/PUPCYCLE/MicroscopeCounts")
flp <- read_excel("Stn9&10.xlsx", sheet="Sheet2") %>%
  filter(Depth=="SUR") %>%
  mutate(upwellstatus = ifelse(Station == 9, "New", ifelse(Station == 10, "Aged", NA)))%>%
  mutate(propmixo=subpercentmixo*100)
order <- c("New", "Aged")
lysotracker$upwellstatus <- as.factor(lysotracker$upwellstatus)
data <- data.frame (treatment = lysotracker$upwellstatus, result = lysotracker$percent)

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
df_aov$lower.CL[df_aov$lower.CL < 0] <- 0

percentplot <- ggplot(df_aov, aes(x= factor(treatment, levels=order), y = emmean))+
  geom_bar(position = 'dodge', stat = 'identity')+
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), position = position_dodge(.9), width = 0.2) + 
  geom_text(aes(label = cld, y = upper.CL), vjust = -0.5, position = position_dodge(0.9),size = 3)+
  labs(x="", y="Proportion of Mixo (LysoTracker)", fill="Timepoint")+
  theme_bw()+theme(
    text=element_text(size=16))
percentplot

data <- data.frame (treatment = lysotracker$upwellstatus, result = lysotracker$nanoeuk)

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
df_aov$lower.CL[df_aov$lower.CL < 0] <- 0

nanoeukplot <- ggplot(df_aov, aes(x= factor(treatment, levels=order), y = emmean))+
  geom_bar(position = 'dodge', stat = 'identity')+
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), position = position_dodge(.9), width = 0.2) + 
  geom_text(aes(label = cld, y = upper.CL), vjust = -0.5, position = position_dodge(0.9),size = 3)+
  labs(x="",y="Phototrophs (cells/mL)", fill="Timepoint")+
  theme_bw()+theme(
    text=element_text(size=16))+
  ggtitle("a)")
nanoeukplot

data <- data.frame (treatment = lysotracker$upwellstatus, result = lysotracker$heteros)

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
df_aov$lower.CL[df_aov$lower.CL < 0] <- 0

heterosplot <- ggplot(df_aov, aes(x= factor(treatment, levels=order), y = emmean))+
  geom_bar(position = 'dodge', stat = 'identity')+
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), position = position_dodge(.9), width = 0.2) + 
  geom_text(aes(label = cld, y = upper.CL), vjust = -0.5, position = position_dodge(0.9),size = 3)+
  labs(x="", y="Heterotrophs (cells/mL)", fill="Timepoint")+
  theme_bw()+theme(
    text=element_text(size=16))+
  ggtitle("b)")
heterosplot

data <- data.frame (treatment = flp$upwellstatus, result = flp$propmixo)

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
df_aov$lower.CL[df_aov$lower.CL < 0] <- 0

flpplot <- ggplot(df_aov, aes(x= factor(treatment, levels=order), y = emmean))+
  geom_bar(position = 'dodge', stat = 'identity')+
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), position = position_dodge(.9), width = 0.2) + 
  geom_text(aes(label = cld, y = upper.CL), vjust = -0.5, position = position_dodge(0.9),size = 3)+
  labs(x="", y="Proportion of Grazing Mixo (%)", fill="Timepoint")+
  theme_bw()+theme(
    text=element_text(size=16))+
  ggtitle("d)")
flpplot

data <- data.frame (treatment = lysotracker$upwellstatus, result = lysotracker$mixo)

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
df_aov$lower.CL[df_aov$lower.CL < 0] <- 0

mixoplot <- ggplot(df_aov, aes(x= factor(treatment, levels=order), y = emmean))+
  geom_bar(position = 'dodge', stat = 'identity')+
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), position = position_dodge(.9), width = 0.2) + 
  geom_text(aes(label = cld, y = upper.CL), vjust = -0.5, position = position_dodge(0.9),size = 3)+
  labs(x="", y="Mixotrophs (cells/mL)", fill="Timepoint")+
  theme_bw()+theme(
    text=element_text(size=16)) +
  ggtitle("c)")
mixoplot

grid.arrange(nanoeukplot, heterosplot, mixoplot, flpplot, ratio, ncol=3)
