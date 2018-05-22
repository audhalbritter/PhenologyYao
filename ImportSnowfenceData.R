library("tidyverse")
library("readxl")
library("broom")
library("lubridate")
library("ggpubr")
library("cowplot")

pn <- . %>% print(n = Inf)

### Soil CNP
cnp <- read_excel(path = "Data/soil data_CNP_2015.xlsx", sheet = 1)

cnp <- cnp %>% 
  rename(treatment = snow, depth = `depth(cm)`, total_C_g_kg = `total C（g/kg）`, organic_C_g_kg = `organic C（g/kg）`, total_N_g_kg = `total N（g/kg）`, available_N_mg_kg = `available N（mg/kg）`, total_P_percent = `total P(%)`, available_P_mg_kg = `available P（mg/kg）`) %>% 
  gather(key = variable, value = value, total_C_g_kg, organic_C_g_kg, total_N_g_kg, available_N_mg_kg, total_P_percent, available_P_mg_kg) %>% 
  mutate(newT = case_when(
    depth == "0-10" & treatment == "SH" ~ "1-10-S",
    depth == "10-20" & treatment == "SH" ~ "10-20-S",
    depth == "0-10" & treatment == "CK" ~ "1-10-C",
    depth == "10-20" & treatment == "CK" ~ "10-20-C"
  )) %>% 
  mutate(treatment = plyr::mapvalues(treatment, c("CK", "SH"), c("Control", "Snow")))

cnp %>% 
  ggplot(aes(x = newT, y = value, fill = treatment)) +
  geom_boxplot() +
  facet_wrap( ~ variable, scales = "free_y") +
  stat_compare_means()


cnpPlot <- ggplot(cnp, aes(x = treatment, y = value, fill = treatment)) +
  geom_boxplot() +
  geom_jitter(color = "grey", width = 0.3) +
  labs(x = "", y = "") +
  scale_fill_manual(values = c("grey", "blue")) +
  facet_grid(variable ~ depth, scales = "free_y") +
  stat_compare_means()

ggsave(cnpPlot, filename = "Figures/CNP.jpg", width = 6, height = 8, dpi = 300)

dfCNP <- cnp %>% 
  group_by(variable, depth) %>%
  do(fit = lm(value ~ snow, data = .))
  
tidy(dfCNP, fit) %>% 
  filter(p.value < 0.05, term != "(Intercept)")



### Ingrowth core
# 2015: Auger, soil sample
rootBiomassSpring <- read_excel(path = "Data/RootCN_2015_Auger_RootBiomass.xlsx", sheet = 1)
rootBiomassSpring <- rootBiomassSpring %>% 
  rename(treatment = Treatment, plot = `Plot#`, mass = `mass(mg)`, total_N_g_kg = `total N(g/kg)`, total_Cg_kg = `total carbon(g/kg)`, depth = `Depthe(cm)`) %>% 
  select(-X__1, -X__2, -X__3) %>% 
  gather(key = variable, value = value, total_Cg_kg, total_N_g_kg) %>% 
  mutate(year = 2016) %>% 
  mutate(newT = case_when(
    depth == "0-10" & treatment == "S" ~ "0-10-S",
    depth == "10-20" & treatment == "S" ~ "10-20-S",
    depth == "0-10" & treatment == "C" ~ "0-10-C",
    depth == "10-20" & treatment == "C" ~ "10-20-C"
  ))

RootBiomassPlot <- ggplot(rootBiomassSpring, aes(x = treatment, y = value, fill = treatment)) +
  geom_boxplot() +
  geom_jitter(color = "grey", width = 0.3) +
  labs(x = "", y = "Total C or N in g/kg") +
  scale_fill_manual(values = c("grey", "blue")) +
  facet_grid(variable ~ depth, scales = "free_y") +
  stat_compare_means()

ggsave(RootBiomassPlot, filename = "Figures/RootBiomass.jpg", width = 6, height = 4, dpi = 300)


ggplot(rootBiomassSpring, aes(x = newT, y = value, fill = treatment)) +
  geom_boxplot() +
  labs(x = "", y = "Total C or N in g/kg") +
  facet_wrap( ~ variable, scales = "free_y")

dfingrowth <- ingrowth %>% 
  group_by(variable, depth) %>% 
  do(fit = lm(value ~ treatment, data = .))

tidy(dfingrowth, fit) %>% pn


# 2016 root growth and traits
rootGrowth <- read_excel(path = "Data/root CN_2016_Ingrowth_RootGrowth.xls", sheet = 1)
rootGrowth <- rootGrowth %>% 
  rename(plot = `Plot#`, total_C_g_kg = `total C(g/kg)`, total_N_g_kg = `total N(g/kg)`) %>% 
  mutate(depth = plyr::mapvalues(depth, c("0~10", "10~20"), c("0-10", "10-20"))) %>% 
  gather(key = variable, value = value, total_C_g_kg, total_N_g_kg) %>% 
  mutate(year = 2016) %>% 
  mutate(newT = case_when(
    depth == "0-10" & treatment == "S" ~ "0-10-S",
    depth == "10-20" & treatment == "S" ~ "10-20-S",
    depth == "0-10" & treatment == "C" ~ "0-10-C",
    depth == "10-20" & treatment == "C" ~ "10-20-C"
  ))

ggplot(rootGrowth, aes( x = newT, y = value, fill = treatment)) +
  geom_boxplot() +
  labs(x = "", y = "Total C or N in g/kg") +
  facet_wrap( ~ variable, scales = "free_y")

rootGrowthPlot <- ggplot(rootGrowth, aes(x = treatment, y = value, fill = treatment)) +
  geom_boxplot() +
  geom_jitter(color = "grey", width = 0.3) +
  labs(x = "", y = "Total C or N in g/kg") +
  scale_fill_manual(values = c("grey", "blue")) +
  facet_grid(variable ~ depth, scales = "free_y") +
  stat_compare_means()

ggsave(rootGrowthPlot, filename = "Figures/rootGrowth.jpg", width = 6, height = 4, dpi = 300)

### Root traits 2016
roottrait <- read_excel(path = "Data/Root-trait data_2016.xlsx", sheet = 1)
roottrait <- roottrait %>% 
  rename(Length_cm = `Length(cm)`, RootVolume_cm3 = `RootVolume(cm³)`, MeasuredWeight_g = `Measured Weight(g)`, TotalWeight_g = `Weight(g)`, TotalLength_cm = Length, CoreVolume_cm3 = `soil vlumn(cm³)`) %>% 
  gather(key = Variable, value = Value, -Year, -ImageFileName, -Site, -Treatment, -Plot, -Hole, -Depth, -Class, -Sort, -Length_cm, -SurfArea, -RootVolume_cm3, -MeasuredWeight_g, -CoreVolume_cm3) %>% 
  mutate(newT = case_when(
    Depth == "0-10" & Treatment == "Snow" ~ "0-10-S",
    Depth == "10-20" & Treatment == "Snow" ~ "10-20-S",
    Depth == "0-10" & Treatment == "Control" ~ "0-10-C",
    Depth == "10-20" & Treatment == "Control" ~ "10-20-C"
  ))


roottrait %>% 
  filter(Variable %in% c("TotalWeight", "TotalLength")) %>% 
  ggplot(aes(x = newT, y = Value, fill = Treatment)) +
  geom_boxplot() +
  facet_grid(Variable ~ Year, scales = "free_y")

dfroottrait <- roottrait %>% 
  group_by(Year, Depth, Variable) %>% 
  do(fit = lm(Value ~ Treatment, data = .))

tidy(dfroottrait, fit) %>%
  filter(p.value < 0.05, term != "(Intercept)")


RootTraits15 <- roottrait %>% 
  filter(Year == 2015) %>% 
  ggplot(aes(x = Treatment, y = Value, fill = Treatment)) +
  geom_boxplot() +
  geom_jitter(color = "grey", width = 0.3) +
  labs(x = "", y = "") +
  scale_fill_manual(values = c("grey", "blue")) +
  facet_grid(Variable ~ Depth, scales = "free_y") +
  stat_compare_means()

ggsave(RootTraits15, filename = "Figures/RootTraits15.jpg", width = 6, height = 8, dpi = 300)

RootTraits16 <- roottrait %>% 
  filter(Year == 2016) %>% 
  ggplot(aes(x = Treatment, y = Value, fill = Treatment)) +
  geom_boxplot() +
  geom_jitter(color = "grey", width = 0.3) +
  labs(x = "", y = "") +
  scale_fill_manual(values = c("grey", "blue")) +
  facet_grid(Variable ~ Depth, scales = "free_y") +
  stat_compare_means()

ggsave(RootTraits16, filename = "Figures/RootTraits16.jpg", width = 6, height = 8, dpi = 300)

### Cover
# shrubs 2015
shrub15 <- read_excel(path = "Data/201507_shrub data.xlsx", sheet = 1)
shrub15 <- shrub15 %>% 
  rename(plot = `Plot#`, plot_area = `Plot area`, cover_percent = `total cover`) %>% 
  rowwise() %>% 
  mutate(mean_height_cm = mean(c(H1, H2, H3, H3, H4, H5, H6, H7, H8, H9, H10, H11, H12, H13, H14, H15, H16, H17, H18))) %>% 
  select(plot, cover_percent, mean_height_cm) %>% 
  mutate(treatment = factor(substr(plot, 5, 5))) %>%
  mutate(year = 2015)

# 2016
shrub16 <- read_excel(path = "Data/201607_shrub data.xlsx", sheet = 1)
shrub <- shrub16 %>% 
  rename(plot = `Plot#`, year = date, plot_area = `plot#`, cover_percent = `total cover`) %>% 
  rowwise() %>% 
  mutate(mean_height_cm = mean(c(H1, H2, H3, H3, H4, H5, H6, H7, H8, H9, H10))) %>% 
  select(plot, year, cover_percent, mean_height_cm) %>% 
  mutate(treatment = factor(substr(plot, 5, 5))) %>%
  mutate(year = year(ymd(year))) %>% 
  bind_rows(shrub15) %>% 
  gather(key = variable, value = value, mean_height_cm, cover_percent) %>% 
  mutate(treatment = plyr::mapvalues(treatment, c("S", "C"), c("Snow", "Control")))

ShrubPlot <- ggplot(shrub, aes(x = treatment, y = value, fill = treatment)) +
  geom_boxplot() +
  geom_jitter(color = "grey", width = 0.3) +
  labs(x = "", y = "") +
  scale_fill_manual(values = c("grey", "blue")) +
  facet_grid(variable ~ year, scales = "free_y") +
  stat_compare_means()
ggsave(ShrubPlot, filename = "Figures/ShrubPlot.jpg", width = 6, height = 6, dpi = 300)

dfshrub <- shrub %>% 
  group_by(year, variable) %>% 
  do(fit = lm(mean_height ~ treatment, data = .))

tidy(dfshrub, fit) %>%
  filter(p.value < 0.05, term != "(Intercept)")



community15 <- read_excel(path = "Data/201507_community data.xlsx", sheet = 2)
community15 %>% 
  rename(plot = `Plot#`, species = `Latin name`, cover = `cover%`) %>% 
  mutate(treatment = factor(substr(plot, 5, 5))) %>%
  filter(`species name` == "litter") %>% 
  ggplot(aes(x = treatment, cover)) +
    geom_jitter()
  
community16 <- read_excel(path = "Data/201607_community data.xlsx", sheet = 2)
community16 %>% 
  rename(site = `Site name`, plot = `Plot no.`, species = `Latine name`) %>% 
  mutate(treatment = factor(substr(plot, 5, 5))) %>%
  filter(species == "litter") %>% 
  ggplot(aes(x = treatment, cover)) +
  geom_boxplot()


# New Shoot Growth
newShoot <- read_excel(path = "Data/shrub new-shoot data_2017.xlsx", sheet = 1)
newShoot <- newShoot %>% 
  mutate(Treatment = plyr::mapvalues(Treatment, c("Snowfence", "CK"), c("Snow", "Control")))

NewShootsPlot <- ggplot(newShoot, aes(x = Treatment, y = New, fill = Treatment)) +
  geom_boxplot() +
  geom_jitter(color = "grey", width = 0.3) +
  labs(x = "", y = "Length new shoots") +
  scale_fill_manual(values = c("grey", "blue")) +
  stat_compare_means()
  
ggsave(NewShootsPlot, filename = "Figures/NewShoots.jpg", width = 7, height = 6, dpi = 300)

summary(lm(New ~ Treatment, data = newShoot))


### Biomass
AGB <- read_excel(path = "Data/AGB_2016.xlsx", sheet = 1)
AGBPlot <- ggplot(AGB, aes(x = Treatment, y = `AGB(g/m2)`, fill = Treatment)) +
  geom_boxplot() +
  geom_jitter(color = "grey", width = 0.3) +
  labs(x = "", y = "AGB g/m2") +
  scale_fill_manual(values = c("grey", "blue")) +
  stat_compare_means()

ggsave(AGBPlot, filename = "Figures/AGB.jpg", width = 7, height = 6, dpi = 300)

