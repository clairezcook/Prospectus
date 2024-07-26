library(ggplot2)
library(data.table)
library(dplyr)
library(plyr)
library(readxl)
library(gtools)
library(ggpubr)
library(tidyr)



bacteria <- read_excel("~/Desktop/OneDrive/Cruises/PUPCYCLE/ReanayzedJan2024/bacteria_cubis.xlsx", 
                       sheet="calculated") %>%
  mutate(Treatment = case_when(
    Treatment == "control" ~ "Control",
    Treatment == "iron" ~ "Iron Addition",
    Treatment == "dfb" ~ "DFB (iron chelator)",
    TRUE ~ Treatment  # Keep other values unchanged
  )) %>%
  mutate(Days =case_when(
    Timepoint == "T2" ~ 7, 
    Timepoint == "T3" ~11,
    Timepoint=="T1" ~ 2,
    Timepoint=="T0"~0
  ))
nut <- read_excel("~/Desktop/OneDrive - University of Georgia/Cruises/PUPCYCLE/pup_nutincubations.xlsx")
colnames(nut)[5]="PO4"
colnames(nut)[7]="NO2"
colnames(nut)[8]="SiO2"
colnames(nut)[9]="NO3"
nut <- nut %>%
  group_by(Timepoint, Treatment) %>%
  #dplyr::summarise(avPO = mean(PO4), sdPO = sd(PO4), 
  # avSi = mean(SiO2), sdSi=sd(SiO2), 
  # avNO = mean(NO3), sdNO = sd(NO3))%>%
  # mutate(mixo=percentmixo*100) %>%
  mutate(Treatment = case_when(
    Treatment=="control" ~ "Control", 
    Treatment =="dfb" ~"DFB (iron chelator)", 
    Treatment =="iron"~"Iron Addition"
  ))

bacnut <- merge(bacteria, nut, by =c("Timepoint", "Treatment",
                                     "Replicate")) %>%
  mutate(preynutratio = sub/`NO3`, 
         nutpreyratio = `NO3`/sub)

ggplot(bacnut, aes(x=Days, y=nutpreyratio, fill=Treatment)) + 
  geom_point() +
  geom_smooth(se=FALSE, color="black") +
  facet_grid(~factor(Treatment, levels=c("Control", "Iron Addition", "DFB (iron chelator)")), scales="free") +
  theme_bw() + 
  theme(text = element_text(size = 22), legend.position = 'none') +
  scale_x_continuous(breaks=c(0,2,7,11)) + 
  labs(x="Time (days)", y="Inorganic Nitrogen : Bacteria Ratio") +
  #geom_hline(yintercept = 2.86e-5, linetype="dashed", color = "green") +
  #geom_hline(yintercept = 5.71e-5, linetype="dashed", color = "green") +
  #geom_hline(yintercept = 5.26e-6, linetype="dashed", color = "red") +
  #geom_hline(yintercept = 2e-5, linetype="dashed", color = "red")
  geom_hline(yintercept = 5.26e-06, color='#CA3542', linetype=2) +
  geom_hline(yintercept = 2.86e-05, color='#27647B', linetype=2) +
  geom_ribbon(aes(ymin = -Inf, ymax =  5.26e-06, fill = "Above Top Hline"), alpha = 0.2) +
  geom_ribbon(aes(ymin = 2.86e-05, ymax = Inf, fill = "Below Bottom Hline"), alpha = 0.2)

#### Station 9/10 ####
#### Nut:Prey ratio ####
setwd("~/Desktop/OneDrive/Cruises/PUPCYCLE")
nuts <- read_excel("pup_stnnutrients.xlsx", sheet="calc")
colnames(nuts)[3]="Replicate"
setwd("~/Desktop/OneDrive/Cruises/PUPCYCLE/MicroscopeCounts")
flp <- read_excel("Stn9&10.xlsx", sheet="Sheet2")
lysonuts <- merge(flp, nuts) %>%
  unique()

nutpreyratio <-lysonuts%>%
  filter(Depth=="SUR")%>%
  mutate(nutpreyratio = (NO3/bacteria)) %>%
  mutate(upwellstatus = ifelse(Station == 9, "New", ifelse(Station == 10, "Aged", NA)))


order <- c("New", "Aged")

data <- data.frame (treatment = nutpreyratio$upwellstatus, result = nutpreyratio$nutpreyratio)

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

ratio <- ggplot(df_aov, aes(x= factor(treatment, levels=order), y = emmean))+
  geom_bar(position = 'dodge', stat = 'identity')+
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), position = position_dodge(.9), width = 0.2) + 
  geom_text(aes(label = cld, y = upper.CL), vjust = -0.5, position = position_dodge(0.9),size = 3)+
  labs(x="", y="Inorganic Nitrogen : Bacteria Ratio", fill="Timepoint")+
  theme_bw()+theme(
    text=element_text(size=16))+
  geom_hline(yintercept = .0000005, color='#CA3542', linetype=2) +
  geom_hline(yintercept = .000007, color='#27647B', linetype=2) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = .0000005, fill = '#CA3542', alpha = 0.2) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = .000007, ymax = Inf, fill = '#27647B', alpha = 0.2) +
  ggtitle("e)")

ratio

