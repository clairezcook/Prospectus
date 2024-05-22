library(ggplot2)
library(dplyr)
library(tidyr)
library(emmeans)
library(multcomp)
library(readxl)
library(stringr)

lyso <- read_excel("~/Desktop/OneDrive/Cruises/PUPCYCLE/ReanayzedJan2024/Cubitainers/cubitainerlysotracker.xlsx", 
                   sheet = "sizefrac") %>%
  group_by(Timepoint, Treatment) %>%
  na.omit()%>%
  mutate(Timepoint = case_when(
    Timepoint == "T0" ~ 0,
    Timepoint == "T1" ~ 2,
    Timepoint == "T2" ~ 7,
    Timepoint == "T3" ~ 11)) %>%
  mutate(Treatment=case_when(
    Treatment == "iron" ~ "Iron Addition", 
    Treatment == "dfb" ~ "DFB (iron chelator)", 
    Treatment=="control" ~"Control"
  )) %>%
  group_by(Timepoint, Treatment) %>%
  mutate(concsmall=concall-conclarge) %>%
  mutate(percentsmall=concsmall/(smallnanoeuk+concsmall)) %>%
  mutate(proplarge=percentlarge*100)

flp <- read_excel("~/Desktop/OneDrive - University of Georgia/Cruises/PUPCYCLE/PUP_microscopecubi.xlsx") %>%
  mutate(Timepoint=as.numeric(str_replace(Timepoint, "T", "")))%>%
  group_by(CubiTimepoint, Replicate, CubiTreatment, Timepoint) %>%
  dplyr::summarise(percentmixo = mean(percentmixo, na.rm = TRUE)) %>%
  ungroup()%>%
  pivot_wider(id_cols=c(CubiTimepoint, CubiTreatment, Replicate),
              names_from=Timepoint, 
              values_from=percentmixo)%>%
  mutate(diff=(`1`-`0`))%>%
  na.omit()%>%
  ungroup()%>%
  group_by(CubiTimepoint, CubiTreatment)%>%
  mutate(Timepoint = case_when(
    CubiTimepoint == "T0" ~ 0,
    CubiTimepoint == "T1" ~ 2,
    CubiTimepoint == "T2" ~ 7,
    CubiTimepoint == "T3" ~ 11)) %>%
  mutate(CubiTreatment=case_when(
    CubiTreatment == "fe" ~ "Iron Addition", 
    CubiTreatment == "dfb" ~ "DFB (iron chelator)", 
    CubiTreatment=="control" ~"Control"
  )) %>%
  mutate(percentmixo=diff/100)
colnames(flp)[2]="Treatment"

order <- c("Control", "Iron Addition", "DFB (iron chelator)")
custom_colors <- c("Control" = "#1B264F",
                   "Iron Addition" = "#FFF07C",
                   "DFB (iron chelator)" = "#758E4F")
barwithletters <- function(timepoint, treatment, result, y_axis_label) {
  data <- data.frame(timepoint = as.factor(timepoint),
                     treatment = as.factor(treatment),
                     result = result)
  
  df_aov <- data %>%
    dplyr::group_by(timepoint) %>%
    tidyr::nest() %>%
    rowwise() %>% 
    dplyr::mutate(aov_results = list(aov(result ~ treatment, data = data)), 
                  emm = list(emmeans::emmeans(aov_results, "treatment", type= "response")), 
                  cld = list(multcomp::cld(emm, Letters = LETTERS, reverse = TRUE))) %>% 
    dplyr::select(-data, -aov_results, -emm) %>% 
    unnest(cols = c(cld)) %>%
    dplyr::mutate(cld = trimws(.group)) %>% 
    dplyr::select(-.group)
  
  hetero <- ggplot(df_aov, aes(x= treatment, y = emmean, fill = factor(timepoint, levels=(order))))+
    geom_bar(position = 'dodge', stat = 'identity') +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), position = position_dodge(.9), width = 0.2) + 
    geom_text(aes(label = cld, y = upper.CL), vjust = -0.5, position = position_dodge(0.9),size = 3)+
    labs(x="Timepoint (days)", y=y_axis_label, fill="Treatment")+
    theme_bw()+theme(text = element_text(size=15)) +
    scale_fill_manual(values = custom_colors)   +
    facet_grid(~factor(timepoint, levels=order))+
    ggtitle("b)")
  return(list(plot = hetero, df_aov = df_aov))
}

flpgraph <- barwithletters(flp$Treatment, flp$Timepoint, flp$diff,  "Proportion of Grazing Phototrophs FLP (%)")
flpplot <- flpgraph$plot
print(flpplot)

data <- data.frame(species = lyso$Timepoint, treatment = lyso$Treatment, result = lyso$proplarge)
custom_colors <- c("Control" = "#1B264F",
                   "Iron Addition" = "#FFF07C",
                   "DFB (iron chelator)" = "#758E4F")

df_aov <- data %>%
  dplyr::group_by(treatment) %>%
  tidyr::nest() %>%
  rowwise() %>% 
  dplyr::mutate(aov_results = list(aov(result ~ species, data = data)), 
                emm = list(emmeans::emmeans(aov_results, "species", type= "response")), 
                cld = list(multcomp::cld(emm, Letters = LETTERS, reverse = TRUE))) %>% 
  dplyr::select(-data, -aov_results, -emm) %>% 
  unnest(cols = c(cld)) %>%
  dplyr::mutate(cld = trimws(.group)) %>% 
  dplyr::select(-.group)


percent <- ggplot(df_aov, aes(x = species, y = emmean, fill = factor(treatment, levels=(order)))) +
  geom_bar(position = 'dodge', stat = 'identity') +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), position = position_dodge(3.5), width = 0.2) + 
  geom_text(aes(label = cld, y = upper.CL), vjust = -0.5, position = position_dodge(3.5), size = 3) +
  labs(x = "Timepoint (days)", y = "Mixotrophic Phototrophs LysoTracker (%)", fill = "Treatment") +
  theme_bw() +
  theme(text = element_text(size = 15), legend.position = 'none', 
        plot.title.position = "plot", plot.title = element_text(hjust = 0)) +
  scale_fill_manual(values = custom_colors) + 
  scale_x_continuous(breaks=c(7,11)) +
  ggtitle("        c)")

percent

nut <- read_excel("~/Desktop/OneDrive - University of Georgia/Cruises/PUPCYCLE/pup_nutincubations.xlsx")%>%
  mutate(Days =case_when(
    Timepoint == "T2" ~ 7, 
    Timepoint == "T3" ~11,
    Timepoint=="T1" ~ 2,
    Timepoint=="T0"~0)) %>%
  mutate(Treatment = case_when(
    Treatment == "control" ~ "Control",
    Treatment == "iron" ~ "Iron Addition",
    Treatment == "dfb" ~ "DFB (iron chelator)",
    TRUE ~ Treatment  # Keep other values unchanged
  ))

setwd("~/Desktop/OneDrive - University of Georgia/Cruises/PUPCYCLE/ReanayzedJan2024/Cubitainers")
cubitainerlyso <- read_excel("cubitainerlysotracker.xlsx"
                             ,sheet="sizefrac"
) %>%
  mutate(Treatment = case_when(
    Treatment == "control" ~ "Control",
    Treatment == "iron" ~ "Iron Addition",
    Treatment == "dfb" ~ "DFB (iron chelator)",
    TRUE ~ Treatment  # Keep other values unchanged
  )) %>%
  mutate(Timepoint =case_when(
    Timepoint == "T2" ~ 7, 
    Timepoint == "T3" ~11,
    Timepoint=="T1" ~ 2,
    Timepoint=="T0"~0
    
  ))
coeff <- 700
nutanano <- ggplot()+
  geom_point(data=cubitainerlyso, aes(x=Timepoint, y=nanoeuk), color="black") + 
  geom_smooth(data=cubitainerlyso, aes(x=Timepoint, y=nanoeuk), se=FALSE, colour="black")+
  geom_point(data=nut, aes(x=Days, y=NO3*coeff), color="gray80")+
  geom_smooth(data=nut, aes(x=Days, y=NO3*coeff), color="gray80", se=FALSE, linetype="dashed" )+
  facet_grid(~factor(Treatment, levels=c("Control", "Iron Addition", "DFB (iron chelator)")), scales="free") +
  scale_y_continuous(name="Phototrophic Nanoeuks (cells/mL)", 
                     labels = scales::label_scientific(style = "plain"),
                     sec.axis=sec_axis(trans=~./coeff, name="Nitrate Concentration (µM)")) +
  theme_bw()+theme(axis.title.y.right= element_text(color="gray80"), 
                   text=element_text(size=17))+
  scale_fill_manual(values=c("#9EC1A3", "#40798C", "#1B264F"))+
  scale_x_continuous(breaks=c(0,2,7,11)) +
  labs(x="Timepoint (days)") + 
  ggtitle("a)")
nutanano
library(gridExtra)

grid.arrange(
  nutanano,  
  arrangeGrob(flpplot, percent, ncol = 2),  
  nrow = 2,  
  heights = c(1, 1)  
)
