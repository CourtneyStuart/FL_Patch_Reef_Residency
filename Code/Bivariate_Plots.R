# associated GitHub repo: 
# https://github.com/CourtneyStuart/FL_Patch_Reef_Residency

#### LIBRARIES ####
# install packages (only need to do this once)
# install.packages(c("easypackages", "tidyverse", "ggplot2",
#                    "conflicted", "here", "dplyr", "cowplot", 
#                    "PNWColors", "patchwork"))

# load packages
library(easypackages)
libraries("tidyverse", "ggplot2", "conflicted", "here",
          "dplyr", "cowplot", "PNWColors", "patchwork")
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("distinct", "dplyr")

# set seed to ensure reproducibility
set.seed(7)   

#### DIRECTORIES ####
# set working directory and relative project path
setwd("E:/Data/Florida/FL_Patch_Reef_Residency/")
# set_here() # set if using here() in this location for the first time
here::i_am(".here")
here::here() 

# read in the calibration data
# gray snapper (Lutjanus griseus (lg))
lg_train = read.csv(here("Data", "Model_Data", "Subadult_Gray_Snapper_Patch_Reef_Data_Training.csv"))

# bluestriped grunt (Haemulon sciurus (hs))
hs_train = read.csv(here("Data", "Model_Data", "Subadult_Bluestriped_Grunt_Patch_Reef_Data_Training.csv"))

# create a new column "Count" where values represent abundance as integers 
# (whole numbers), rather than averages across buddy diver pairs. 
lg_train = lg_train %>%
  mutate(Count = round(lg_train$Abundance, digits = 0)) %>%
  relocate(Count, .after = Abundance)

hs_train = hs_train %>%
  mutate(Count = round(hs_train$Abundance, digits = 0)) %>%
  relocate(Count, .after = Abundance)

train = rbind(lg_train, hs_train)

# bivariate plots
# palette for plotting
my_pal = pnw_palette("Bay",8)
my_pal


#### DEPTH ####
depth_plot = 
  ggplot(train, aes(x = Depth, y = Count)) +
  # points for each species
  geom_point(aes(color = Species), size = 2) +
  # smooth line with confidence intervals
  geom_smooth(aes(color = Species, fill = Species), method = "loess", se = TRUE, alpha = 0.5) +
  # custom colors and labels for points, lines, and shading
  scale_color_manual(values = c("LUT GRIS" = my_pal[5], "HAE SCIU" = my_pal[1]),
                     labels = c("LUT GRIS" = expression(italic("Lutjanus griseus")),
                                "HAE SCIU" = expression(italic("Haemulon sciurus")))) +
  scale_fill_manual(values = c("LUT GRIS" = my_pal[5], "HAE SCIU" = my_pal[1]),
                    labels = c("LUT GRIS" = expression(italic("Lutjanus griseus")),
                               "HAE SCIU" = expression(italic("Haemulon sciurus")))) +
  # axis labels and theme
  labs(x = "Depth (m below surface)",
       y = "Abundance") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(face = "italic")) +
  guides(color = guide_legend(override.aes = list(
    shape = 16, size = 3, linetype = 1, alpha = 0.5)))  

depth_plot  

#### SLOPE ####
slope_plot =
  ggplot(train, aes(x = Slope, y = Count)) +
  # points for each species
  geom_point(aes(color = Species), size = 2) +
  # smooth line with confidence intervals
  geom_smooth(aes(color = Species, fill = Species), method = "loess", se = TRUE, alpha = 0.5) +
  # custom colors and labels for points, lines, and shading
  scale_color_manual(values = c("LUT GRIS" = my_pal[5], "HAE SCIU" = my_pal[1]),
                     labels = c("LUT GRIS" = expression(italic("Lutjanus griseus")),
                                "HAE SCIU" = expression(italic("Haemulon sciurus")))) +
  scale_fill_manual(values = c("LUT GRIS" = my_pal[5], "HAE SCIU" = my_pal[1]),
                    labels = c("LUT GRIS" = expression(italic("Lutjanus griseus")),
                               "HAE SCIU" = expression(italic("Haemulon sciurus")))) +
  # axis labels and theme
  labs(x = "Slope (degrees)",
       y = "Abundance") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(face = "italic")) +
  guides(color = guide_legend(override.aes = list(
    shape = 16, size = 3, linetype = 1, alpha = 0.5)))  

slope_plot

#### BPI ####
bpi_plot = 
  ggplot(train, aes(x = BPI_Broad, y = Count)) +
  # points for each species
  geom_point(aes(color = Species), size = 2) +
  # smooth line with confidence intervals
  geom_smooth(aes(color = Species, fill = Species), method = "loess", se = TRUE, alpha = 0.5) +
  # custom colors and labels for points, lines, and shading
  scale_color_manual(values = c("LUT GRIS" = my_pal[5], "HAE SCIU" = my_pal[1]),
                     labels = c("LUT GRIS" = expression(italic("Lutjanus griseus")),
                                "HAE SCIU" = expression(italic("Haemulon sciurus")))) +
  scale_fill_manual(values = c("LUT GRIS" = my_pal[5], "HAE SCIU" = my_pal[1]),
                    labels = c("LUT GRIS" = expression(italic("Lutjanus griseus")),
                               "HAE SCIU" = expression(italic("Haemulon sciurus")))) +
  # axis labels and theme
  labs(x = "Broad-scale bathymetric position index",
       y = "Abundance") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(face = "italic")) +
  guides(color = guide_legend(override.aes = list(
    shape = 16, size = 3, linetype = 1, alpha = 0.5))) 
bpi_plot 

#### PATCH REEF AREA ####
area_plot =
  ggplot(train, aes(x = Patch_Area, y = Count)) +
  # points for each species
  geom_point(aes(color = Species), size = 2) +
  # smooth line with confidence intervals
  geom_smooth(aes(color = Species, fill = Species), method = "loess", se = TRUE, alpha = 0.5) +
  # custom colors and labels for points, lines, and shading
  scale_color_manual(values = c("LUT GRIS" = my_pal[5], "HAE SCIU" = my_pal[1]),
                     labels = c("LUT GRIS" = expression(italic("Lutjanus griseus")),
                                "HAE SCIU" = expression(italic("Haemulon sciurus")))) +
  scale_fill_manual(values = c("LUT GRIS" = my_pal[5], "HAE SCIU" = my_pal[1]),
                    labels = c("LUT GRIS" = expression(italic("Lutjanus griseus")),
                               "HAE SCIU" = expression(italic("Haemulon sciurus")))) +
  # axis labels and theme
  labs(x = "Patch reef area (sq m)",
       y = "Abundance") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(face = "italic")) +
  guides(color = guide_legend(override.aes = list(
    shape = 16, size = 3, linetype = 1, alpha = 0.5)))  

area_plot

#### PA RATIO ####
PA_ratio_plot =
  ggplot(train, aes(x = PA_Ratio, y = Count)) +
  # points for each species
  geom_point(aes(color = Species), size = 2) +
  # smooth line with confidence intervals
  geom_smooth(aes(color = Species, fill = Species), method = "loess", se = TRUE, alpha = 0.5) +
  # custom colors and labels for points, lines, and shading
  scale_color_manual(values = c("LUT GRIS" = my_pal[5], "HAE SCIU" = my_pal[1]),
                     labels = c("LUT GRIS" = expression(italic("Lutjanus griseus")),
                                "HAE SCIU" = expression(italic("Haemulon sciurus")))) +
  scale_fill_manual(values = c("LUT GRIS" = my_pal[5], "HAE SCIU" = my_pal[1]),
                    labels = c("LUT GRIS" = expression(italic("Lutjanus griseus")),
                               "HAE SCIU" = expression(italic("Haemulon sciurus")))) +
  # axis labels and theme
  labs(x = "Perimeter-to-area ratio",
       y = "Abundance") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(face = "italic")) +
  guides(color = guide_legend(override.aes = list(
    shape = 16, size = 3, linetype = 1, alpha = 0.5)))  

PA_ratio_plot

#### PREDATOR DENSITY ####
pred_plot =  ggplot(train, aes(x = Pred_Density, y = Count)) +
  # points for each species
  geom_point(aes(color = Species), size = 2) +
  # smooth line with confidence intervals
  geom_smooth(aes(color = Species, fill = Species), method = "loess", se = TRUE, alpha = 0.5) +
  # custom colors and labels for points, lines, and shading
  scale_color_manual(values = c("LUT GRIS" = my_pal[5], "HAE SCIU" = my_pal[1]),
                     labels = c("LUT GRIS" = expression(italic("Lutjanus griseus")),
                                "HAE SCIU" = expression(italic("Haemulon sciurus")))) +
  scale_fill_manual(values = c("LUT GRIS" = my_pal[5], "HAE SCIU" = my_pal[1]),
                    labels = c("LUT GRIS" = expression(italic("Lutjanus griseus")),
                               "HAE SCIU" = expression(italic("Haemulon sciurus")))) +
  # axis labels and theme
  labs(x = "Predator density (abundance/sq m)",
       y = "Abundance") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(face = "italic")) +
  guides(color = guide_legend(override.aes = list(
    shape = 16, size = 3, linetype = 1, alpha = 0.5)))  

#### SEAGRASS AREA ####
seagrass_plot = 
  ggplot(train, aes(x = Area_SG, y = Count)) +
  # points for each species
  geom_point(aes(color = Species), size = 2) +
  # smooth line with confidence intervals
  geom_smooth(aes(color = Species, fill = Species), method = "loess", se = TRUE, alpha = 0.5) +
  # custom colors and labels for points, lines, and shading
  scale_color_manual(values = c("LUT GRIS" = my_pal[5], "HAE SCIU" = my_pal[1]),
                     labels = c("LUT GRIS" = expression(italic("Lutjanus griseus")),
                                "HAE SCIU" = expression(italic("Haemulon sciurus")))) +
  scale_fill_manual(values = c("LUT GRIS" = my_pal[5], "HAE SCIU" = my_pal[1]),
                    labels = c("LUT GRIS" = expression(italic("Lutjanus griseus")),
                               "HAE SCIU" = expression(italic("Haemulon sciurus")))) +
  # axis labels and theme
  labs(x = "Area of seagrass in a 500m buffer (sq m)",
       y = "Abundance") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(face = "italic")) +
  guides(color = guide_legend(override.aes = list(
    shape = 16, size = 3, linetype = 1, alpha = 0.5)))  

seagrass_plot 

#### CORAL REEF AREA ####
coral_plot = 
  ggplot(train, aes(x = Area_CRHB, y = Count)) +
  # points for each species
  geom_point(aes(color = Species), size = 2) +
  # smooth line with confidence intervals
  geom_smooth(aes(color = Species, fill = Species), method = "loess", se = TRUE, alpha = 0.5) +
  # custom colors and labels for points, lines, and shading
  scale_color_manual(values = c("LUT GRIS" = my_pal[5], "HAE SCIU" = my_pal[1]),
                     labels = c("LUT GRIS" = expression(italic("Lutjanus griseus")),
                                "HAE SCIU" = expression(italic("Haemulon sciurus")))) +
  scale_fill_manual(values = c("LUT GRIS" = my_pal[5], "HAE SCIU" = my_pal[1]),
                    labels = c("LUT GRIS" = expression(italic("Lutjanus griseus")),
                               "HAE SCIU" = expression(italic("Haemulon sciurus")))) +
  # axis labels and theme
  labs(x = "Area of coral/hardbottom in a 500m buffer (sq m)",
       y = "Abundance") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(face = "italic")) +
  guides(color = guide_legend(override.aes = list(
    shape = 16, size = 3, linetype = 1, alpha = 0.5)))  

coral_plot 

#### NEIGHBOUR PATCH DISTANCE ####
neighbour_plot = 
  ggplot(train, aes(x = Patch_Neigh_Dist, y = Count)) +
  # points for each species
  geom_point(aes(color = Species), size = 2) +
  # smooth line with confidence intervals
  geom_smooth(aes(color = Species, fill = Species), method = "loess", se = TRUE, alpha = 0.5) +
  # custom colors and labels for points, lines, and shading
  scale_color_manual(values = c("LUT GRIS" = my_pal[5], "HAE SCIU" = my_pal[1]),
                     labels = c("LUT GRIS" = expression(italic("Lutjanus griseus")),
                                "HAE SCIU" = expression(italic("Haemulon sciurus")))) +
  scale_fill_manual(values = c("LUT GRIS" = my_pal[5], "HAE SCIU" = my_pal[1]),
                    labels = c("LUT GRIS" = expression(italic("Lutjanus griseus")),
                               "HAE SCIU" = expression(italic("Haemulon sciurus")))) +
  # axis labels and theme
  labs(x = "Distance to nearest neighbouring patch reef (m)",
       y = "Abundance") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(face = "italic")) +
  guides(color = guide_legend(override.aes = list(
    shape = 16, size = 3, linetype = 1, alpha = 0.5)))  

neighbour_plot

#### OFFSHORE REEF DISTANCE ####
reef_plot = 
  ggplot(train, aes(x = Reef_Dist, y = Count)) +
  # points for each species
  geom_point(aes(color = Species), size = 2) +
  # smooth line with confidence intervals
  geom_smooth(aes(color = Species, fill = Species), method = "loess", se = TRUE, alpha = 0.5) +
  # custom colors and labels for points, lines, and shading
  scale_color_manual(values = c("LUT GRIS" = my_pal[5], "HAE SCIU" = my_pal[1]),
                     labels = c("LUT GRIS" = expression(italic("Lutjanus griseus")),
                                "HAE SCIU" = expression(italic("Haemulon sciurus")))) +
  scale_fill_manual(values = c("LUT GRIS" = my_pal[5], "HAE SCIU" = my_pal[1]),
                    labels = c("LUT GRIS" = expression(italic("Lutjanus griseus")),
                               "HAE SCIU" = expression(italic("Haemulon sciurus")))) +
  # axis labels and theme
  labs(x = "Distance to nearest point along Florida Reef Tract (m)",
       y = "Abundance") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(face = "italic")) +
  guides(color = guide_legend(override.aes = list(
    shape = 16, size = 3, linetype = 1, alpha = 0.5)))  

reef_plot 

#### MANGROVE DISTANCE ####
mangrove_plot = 
  ggplot(train, aes(x = Mangrove_Dist, y = Count)) +
  # points for each species
  geom_point(aes(color = Species), size = 2) +
  # smooth line with confidence intervals
  geom_smooth(aes(color = Species, fill = Species), method = "loess", se = TRUE, alpha = 0.5) +
  # custom colors and labels for points, lines, and shading
  scale_color_manual(values = c("LUT GRIS" = my_pal[5], "HAE SCIU" = my_pal[1]),
                     labels = c("LUT GRIS" = expression(italic("Lutjanus griseus")),
                                "HAE SCIU" = expression(italic("Haemulon sciurus")))) +
  scale_fill_manual(values = c("LUT GRIS" = my_pal[5], "HAE SCIU" = my_pal[1]),
                    labels = c("LUT GRIS" = expression(italic("Lutjanus griseus")),
                               "HAE SCIU" = expression(italic("Haemulon sciurus")))) +
  # axis labels and theme
  labs(x = "Distance to nearest mangrove patch (m)",
       y = "Abundance") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(face = "italic")) +
  guides(color = guide_legend(override.aes = list(
    shape = 16, size = 3, linetype = 1, alpha = 0.5)))  
mangrove_plot 

#### WINTER TEMP ####
win_temp_plot = 
  ggplot(train, aes(x = Mean_Win_Temp, y = Count)) +
  # points for each species
  geom_point(aes(color = Species), size = 2) +
  # smooth line with confidence intervals
  geom_smooth(aes(color = Species, fill = Species), method = "loess", se = TRUE, alpha = 0.5) +
  # custom colors and labels for points, lines, and shading
  scale_color_manual(values = c("LUT GRIS" = my_pal[5], "HAE SCIU" = my_pal[1]),
                     labels = c("LUT GRIS" = expression(italic("Lutjanus griseus")),
                                "HAE SCIU" = expression(italic("Haemulon sciurus")))) +
  scale_fill_manual(values = c("LUT GRIS" = my_pal[5], "HAE SCIU" = my_pal[1]),
                    labels = c("LUT GRIS" = expression(italic("Lutjanus griseus")),
                               "HAE SCIU" = expression(italic("Haemulon sciurus")))) +
  # axis labels and theme
  labs(x = "Mean winter temperature (degrees C)",
       y = "Abundance") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(face = "italic")) +
  guides(color = guide_legend(override.aes = list(
    shape = 16, size = 3, linetype = 1, alpha = 0.5)))  

win_temp_plot

#### WINTER SAL ####
win_sal_plot =
  ggplot(train, aes(x = Mean_Win_Sal, y = Count)) +
  # points for each species
  geom_point(aes(color = Species), size = 2) +
  # smooth line with confidence intervals
  geom_smooth(aes(color = Species, fill = Species), method = "loess", se = TRUE, alpha = 0.5) +
  # custom colors and labels for points, lines, and shading
  scale_color_manual(values = c("LUT GRIS" = my_pal[5], "HAE SCIU" = my_pal[1]),
                     labels = c("LUT GRIS" = expression(italic("Lutjanus griseus")),
                                "HAE SCIU" = expression(italic("Haemulon sciurus")))) +
  scale_fill_manual(values = c("LUT GRIS" = my_pal[5], "HAE SCIU" = my_pal[1]),
                    labels = c("LUT GRIS" = expression(italic("Lutjanus griseus")),
                               "HAE SCIU" = expression(italic("Haemulon sciurus")))) +
  # axis labels and theme
  labs(x = "Mean winter salinity (practical salinity units)",
       y = "Abundance") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(face = "italic")) +
  guides(color = guide_legend(override.aes = list(
    shape = 16, size = 3, linetype = 1, alpha = 0.5)))  

win_sal_plot

#### SUMMER TEMP ####
sum_temp_plot = 
  ggplot(train, aes(x = Mean_Sum_Temp, y = Count)) +
  # points for each species
  geom_point(aes(color = Species), size = 2) +
  # smooth line with confidence intervals
  geom_smooth(aes(color = Species, fill = Species), method = "loess", se = TRUE, alpha = 0.5) +
  # custom colors and labels for points, lines, and shading
  scale_color_manual(values = c("LUT GRIS" = my_pal[5], "HAE SCIU" = my_pal[1]),
                     labels = c("LUT GRIS" = expression(italic("Lutjanus griseus")),
                                "HAE SCIU" = expression(italic("Haemulon sciurus")))) +
  scale_fill_manual(values = c("LUT GRIS" = my_pal[5], "HAE SCIU" = my_pal[1]),
                    labels = c("LUT GRIS" = expression(italic("Lutjanus griseus")),
                               "HAE SCIU" = expression(italic("Haemulon sciurus")))) +
  # axis labels and theme
  labs(x = "Mean summer temperature (degrees C)",
       y = "Abundance") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(face = "italic")) +
  guides(color = guide_legend(override.aes = list(
    shape = 16, size = 3, linetype = 1, alpha = 0.5)))  

sum_temp_plot

#### SUMMER SAL ####
sum_sal_plot =
  ggplot(train, aes(x = Mean_Sum_Sal, y = Count)) +
  # points for each species
  geom_point(aes(color = Species), size = 2) +
  # smooth line with confidence intervals
  geom_smooth(aes(color = Species, fill = Species), method = "loess", se = TRUE, alpha = 0.5) +
  # custom colors and labels for points, lines, and shading
  scale_color_manual(values = c("LUT GRIS" = my_pal[5], "HAE SCIU" = my_pal[1]),
                     labels = c("LUT GRIS" = expression(italic("Lutjanus griseus")),
                                "HAE SCIU" = expression(italic("Haemulon sciurus")))) +
  scale_fill_manual(values = c("LUT GRIS" = my_pal[5], "HAE SCIU" = my_pal[1]),
                    labels = c("LUT GRIS" = expression(italic("Lutjanus griseus")),
                               "HAE SCIU" = expression(italic("Haemulon sciurus")))) +
  # axis labels and theme
  labs(x = "Mean summer salinity (practical salinity units)",
       y = "Abundance") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(face = "italic")) +
  guides(color = guide_legend(override.aes = list(
    shape = 16, size = 3, linetype = 1, alpha = 0.5)))  

sum_sal_plot

#### DEPTH VS. SUMMER TEMP ####
depth_sum_temp_plot =
  ggplot(train, aes(x = Depth, y = Mean_Sum_Temp)) +
  geom_point(color = "black", size = 2) + 
  geom_smooth(method = "loess", color = "blue", fill = "gray40") +  
  labs(x = "Depth (m below surface)", y = "Mean summer temperature (degrees C)") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

depth_sum_temp_plot

#### DEPTH VS. SUMMER SAL ####
depth_sum_sal_plot =
  ggplot(train, aes(x = Depth, y = Mean_Sum_Sal)) +
  geom_point(color = "black", size = 2) + 
  geom_smooth(method = "loess", color = "blue", fill = "gray40") +  
  labs(x = "Depth (m below surface)", y = "Mean summer salinity (practical salinity units)") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

depth_sum_sal_plot

#### DEPTH VS. WINTER TEMP ####
depth_win_temp_plot =
  ggplot(train, aes(x = Depth, y = Mean_Win_Temp)) +
  geom_point(color = "black", size = 2) + 
  geom_smooth(method = "loess", color = "blue", fill = "gray40") +  
  labs(x = "Depth (m below surface)", y = "Mean winter temperature (degrees C)") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

depth_win_temp_plot

#### DEPTH VS. WINTER SAL ####
depth_win_sal_plot =
  ggplot(train, aes(x = Depth, y = Mean_Win_Sal)) +
  geom_point(color = "black", size = 2) + 
  geom_smooth(method = "loess", color = "blue", fill = "gray40") +  
  labs(x = "Depth (m below surface)", y = "Mean winter salinity (practical salinity units)") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

depth_win_sal_plot

#### MULTI-PANEL PLOTS ####
require(patchwork)

# arrange in rows, collect legend, label rows as A, B, C
reefscape_position =
 (neighbour_plot + mangrove_plot + reef_plot) +
  plot_layout(ncol = 1, guides = "collect") +  
  plot_annotation(tag_levels = 'A') &
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

reefscape_position

ggsave(
  filename = here("Figures", "Supplementary_Figure_3.png"),
  plot = reefscape_position,
  width = 6, height = 8,  
  dpi = 450)

habitat_structure = 
  (depth_plot + slope_plot +
   bpi_plot + area_plot +
   PA_ratio_plot + pred_plot +
   coral_plot + seagrass_plot) +
  plot_layout(ncol = 2, guides = "collect") +  
  plot_annotation(tag_levels = 'A') &
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

habitat_structure

ggsave(
  filename = here("Figures", "Supplementary_Figure_7.png"),
  plot = habitat_structure,
  width = 8, height = 10,  
  dpi = 450)

environmental_conditions =
  (sum_temp_plot + win_temp_plot + sum_sal_plot + win_sal_plot) +
  plot_layout(ncol = 2, guides = "collect") +  
  plot_annotation(tag_levels = 'A') &
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

environmental_conditions  

ggsave(
  filename = here("Figures", "Supplementary_Figure_4.png"),
  plot = environmental_conditions,
  width = 8, height = 8,  
  dpi = 450)


depth_environment =
  (depth_sum_temp_plot + depth_win_temp_plot + depth_sum_sal_plot + depth_win_sal_plot) +
  plot_layout(ncol = 2, guides = "collect") +  
  plot_annotation(tag_levels = 'A') 

depth_environment

ggsave(
  filename = here("Figures", "Supplementary_Figure_8.png"),
  plot = depth_environment,
  width = 8, height = 8,  
  dpi = 450)
