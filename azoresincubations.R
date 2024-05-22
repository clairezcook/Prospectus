library(ggplot2)
library(data.table)
library(dplyr)
library(plyr)
library(readxl)
library(gtools)
library(ggpubr)
library(tidyverse)
library(rstatix)
library(multcomp)
library(emmeans)
library(multcompView)
library(ggtext)
library(tidyverse)
library(lubridate)
library(reshape2)
#library(MBA)
library(mgcv)
library(ggplot2)
library(data.table)
library(dplyr)
library(plyr)
library(readxl)
library(gtools)
library(ggpubr)
library(tidyr)
library(marmap)
library(mapdata)
library(sf)
library(marmap)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)

df <- read_excel("~/Desktop/OneDrive/Cruises/azores/NutrientIncubations_6Dec23/Incubations_heterotrophs_6Dec23.xlsx")

# Calculate % mixotrophy 

first_non_na <- function(x) {
  first_value <- na.omit(x)[1]
  ifelse(is.na(first_value), NA, first_value)
}


calculated_df <- df %>%
  group_by(Incubation, Treatment, Replicate, Lyso) %>%
  mutate(percentmixo = lysotracker_concentration / nanoeuk_concentration) %>%
  ungroup() %>%
  group_by(Incubation, Treatment, Replicate) %>%
  filter(all(c("Yes", "No") %in% Lyso)) %>%
  mutate(subpercent = percentmixo[Lyso == "Yes"] - percentmixo[Lyso == "No"]) %>%
  mutate(subhetero = heteros_concentration[Lyso == "Yes"] - heteros_concentration[Lyso == "No"]) %>%
  mutate(submixo=lysotracker_concentration[Lyso == "Yes"] - lysotracker_concentration[Lyso == "No"])%>%
  #dplyr::select(Incubation, Station_nickname, Latitude, Treatment, Replicate, Lyso)%>%
  mutate(Nano = ifelse(Lyso == "No", nanoeuk_concentration, NA),
         Pico = ifelse(Lyso == "No", picoeuks_concentration, NA),
         Percent = ifelse(Lyso == "Yes", subpercent, NA),
         Hetero = ifelse(Lyso == "Yes", subhetero, NA), 
         Syn = ifelse(Lyso == "No", syn_concentration, NA), 
         Pro = ifelse(Lyso == "No", pro_concentration, NA), 
         Mixo=ifelse(Lyso=="Yes", submixo,NA)) %>%
  ungroup()%>%
  group_by(Incubation, Treatment, Replicate) %>%
  summarise_all(first_non_na)%>%
  dplyr::select(-c(Lyso, syn_concentration, percentmixo, subpercent, subhetero,
                   nanoeuk_concentration, lysotracker_concentration, pro_concentration, 
                   picoeuks_concentration, heteros_concentration)) %>%
  mutate(Limitation =case_when(
    Incubation == 1 ~ "Co-limitation",
    Incubation == 2 ~ "N limitation",
    Incubation == 3 ~ "NS",
    Incubation == 4 ~ "N serial P",
    TRUE ~ "Other"))
order <- c("Control", "N", "NP", "P")
stnorder <- c("N limitation", "NS", "N serial P", "Co-limitation")

data <- data.frame (species = calculated_df$Limitation, treatment = calculated_df$Treatment, result = calculated_df$Percent)
custom_colors <- c("Control" = "#1B264F",
                   "N" = "#FFF07C",
                   "NP" = "#758E4F",
                   "P" = "#EF8354")
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

percent <- ggplot(df_aov, aes(x= factor(species, levels=stnorder), y = emmean, fill = treatment))+
  geom_bar(position = 'dodge', stat = 'identity')+
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), position = position_dodge(.9), width = 0.2) + 
  geom_text(aes(label = cld, y = upper.CL), vjust = -0.5, position = position_dodge(0.9),size = 3)+
  labs(x="Resulting Nutrient Limitation", 
       y=label_wrap_gen(width=30)("Proportion Mixotrophic Phototrophs"), 
       fill="Nutrient")+
  theme_bw() +theme(text = element_text(size = 16), 
                    axis.title.y=element_text(vjust=0.5, 
                                              margin = margin(t = 0, r = 10, b = 0, l = 0),
                                              lineheight = 0.9))+
  scale_fill_manual(values = custom_colors) +
  ggtitle("b)")
percent

data <- data.frame (species = calculated_df$Limitation, treatment = calculated_df$Treatment, result = calculated_df$Nano)

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

nano <- ggplot(df_aov, aes(x= factor(species, levels=stnorder), y = emmean, fill = treatment))+
  geom_bar(position = 'dodge', stat = 'identity')+
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), position = position_dodge(.9), width = 0.2) + 
  geom_text(aes(label = cld, y = upper.CL), vjust = -0.5, position = position_dodge(0.9),size = 3)+
  labs(x="", 
       y=label_wrap_gen(width=30)("Photosynthetic Nanoeukaryotes (cells/mL)"), 
       fill="Nutrient")+
  theme_bw()+theme(text = element_text(size = 16), axis.title.y=element_text(vjust=0.5, 
                                                                             margin = margin(t = 0, r = 10, b = 0, l = 0),
                                                                             lineheight = 0.9))+
  scale_fill_manual(values = custom_colors) +
  ggtitle("a)")
nano

#### Map ####
az <- read_excel("~/Desktop/OneDrive/Cruises/Azores/StationData.xlsx", sheet="Sheet2")
nutstations <- az %>%
  filter(Station %in% c("1", "4", "15", "5.5"))

# Get bathymetric data
azoresbathy <- getNOAA.bathy(lon1=-40, lon2=0, lat1=30, lat2=50, resolution = 4)
bat_xyz <- as.xyz(azoresbathy)

# Import country data
country <- ne_countries(scale = "medium", returnclass = "sf")
azplot <- ggplot() + 
  geom_sf(data = country) +
  geom_tile(data = bat_xyz, aes(x = V1, y = V2, fill = V3)) +
  geom_sf(data = country) +
  coord_sf(xlim = c(-32, -7), 
           ylim = c(35, 50)) +
  labs(x = "Longitude", y = "Latitude", fill = "Depth (m)") +
  theme_minimal() +
  geom_point(data=az, aes(x=Long, y=Lat), colour ="white", size=2) + 
  geom_point(data=az, aes(x=Long, y=Lat), colour ="black") + 
  theme(legend.position = 'none') +
  geom_point(data = nutstations, aes(x = Long, y = Lat), colour = "red", size = 2)+
  geom_rect(data = nutstations, aes(xmin = Long + 0.5, xmax = Long + 5, ymin = Lat - 0.5, ymax = Lat + 0.5),
            fill = "white", alpha = 1)+
  geom_text(data = nutstations, aes(x = Long + 0.5, y = Lat, label = NutType),
            hjust = 0, vjust = 0.5, color = "black", size = 3, 
            position = position_nudge(x = 0.05))+
  ggtitle("c)")

azplot

library(gridExtra)

# Combine the two bar graphs into a single plot
combined_plots <- arrangeGrob(nano, percent, ncol = 1)

# Arrange the map and the combined bar graphs horizontally
final_plot <- grid.arrange(combined_plots, azplot, ncol = 2, widths = c(1, 1))

# Print or save the final plot
final_plot

