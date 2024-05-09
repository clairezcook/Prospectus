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
  geom_hline(yintercept = .000001, color='#CA3542', linetype=2) +
  geom_hline(yintercept = .00004, color='#27647B', linetype=2) +
  geom_ribbon(aes(ymin = -Inf, ymax = .000001, fill = "Above Top Hline"), alpha = 0.2) +
  geom_ribbon(aes(ymin = .00004, ymax = Inf, fill = "Below Bottom Hline"), alpha = 0.2)
