library("tidyverse")
library("readxl")
library("broom")

pn <- . %>% print(n = Inf)

### Soil CNP
cnp <- read_excel(path = "Data/soil data_CNP_2015.xlsx", sheet = 1)

cnp <- cnp %>% 
  rename(depth = `depth(cm)`, total_C = `total C（g/kg）`, organic_C = `organic C（g/kg）`, total_N = `total N（g/kg）`, available_N = `available N（mg/kg）`, total_P = `total P(%)`, available_P = `available P（mg/kg）`) %>% 
  gather(key = variable, value = value, total_C, organic_C, total_N, available_N, total_P, available_P) %>% 
  mutate(newT = case_when(
    snow == "SH" & depth == "0-10" ~ "SH-1-10",
    snow == "SH" & depth == "10-20" ~ "SH-10-20",
    snow == "CK" & depth == "0-10" ~ "CK-1-10",
    snow == "CK" & depth == "10-20" ~ "CK-10-20"
  ))

cnp %>% 
  ggplot(aes(x = newT, y = value, fill = snow)) +
  geom_boxplot() +
  facet_wrap( ~ variable, scales = "free_y")

dfCNP <- cnp %>% 
  group_by(variable, depth) %>%
  do(fit = lm(value ~ snow, data = .))
  
tidy(dfCNP, fit) %>% 
  filter(p.value < 0.05, term != "(Intercept)")



### Ingrowth core
# 2016
ingrowth16 <- read_excel(path = "Data/root CN_2016_ingrowth core.xls", sheet = 1)
ingrowth16 <- ingrowth16 %>% 
  rename(plot = `Plot#`, total_C = `total C(g/kg)`, total_N = `total N(g/kg)`) %>% 
  mutate(depth = plyr::mapvalues(depth, c("0~10", "10~20"), c("0-10", "10-20"))) %>% 
  gather(key = variable, value = value, total_C, total_N) %>% 
  mutate(year = 2016) %>% 
  mutate(newT = case_when(
    treatment == "S" & depth == "0-10" ~ "S-1-10",
    treatment == "S" & depth == "10-20" ~ "S-10-20",
    treatment == "C" & depth == "0-10" ~ "C-1-10",
    treatment == "C" & depth == "10-20" ~ "C-10-20"
  ))


# 2015 and bind rows
ingrowth15 <- read_excel(path = "Data/RootCN_2015_using augar.xlsx", sheet = 1)
ingrowth <- ingrowth15 %>% 
  rename(treatment = Treatment, plot = `Plot#`, mass = `mass(mg)`, total_N = `total N(g/kg)`, total_C = `total carbon(g/kg)`, depth = `Depthe(cm)`) %>% 
  select(-X__1, -X__2, -X__3) %>% 
  gather(key = variable, value = value, total_C, total_N, mass) %>% 
  mutate(year = 2015) %>% 
  mutate(newT = case_when(
    treatment == "S" & depth == "0-10" ~ "S-1-10",
    treatment == "S" & depth == "10-20" ~ "S-10-20",
    treatment == "C" & depth == "0-10" ~ "C-1-10",
    treatment == "C" & depth == "10-20" ~ "C-10-20"
  )) %>% 
  bind_rows(ingrowth16)


ggplot(ingrowth, aes( x = newT, y = value, fill = treatment)) +
  geom_boxplot() +
  labs(x = "", y = "Total C or N in g/kg") +
  facet_grid(year ~ variable, scales = "free_y")

dfingrowth <- ingrowth %>% 
  group_by(variable, depth, year) %>% 
  do(fit = lm(value ~ treatment, data = .))

tidy(dfingrowth, fit) %>% pn





### Root traits
roottrait <- read_excel(path = "Data/Root-trait data_2016.xlsx", sheet = 1)
roottrait <- roottrait %>% 
  rename(Length = `Length(cm)`, RootVolume = `RootVolume(cm³)`, MeasuredWeight = `Measured Weight(g)`, Weight = `Weight(g)`, Length2 = Length, SomeVolume = `土壤体积(cm³)`) %>% 
  gather(key = Variable, value = Value, -Year, -ImageFileName, -Site, -Treatment, -Plot, -Hole, -Depth, -Class, -Sort)

roottrait %>% 
  filter(Year == 2015) %>% 
  ggplot(aes(x = Treatment, y = Value)) +
         geom_boxplot() +
         facet_grid(Variable ~ Depth, scales = "free_y")

 roottrait %>% 
  filter(Year == 2016) %>% 
  ggplot(aes(x = Treatment, y = Value)) +
  geom_boxplot() +
  facet_grid(Variable ~ Depth, scales = "free_y")   

dfroottrait <- roottrait %>% 
  group_by(Year, Depth, Variable) %>% 
  do(fit = lm(Value ~ Treatment, data = .))

tidy(dfroottrait, fit) %>% 
  filter(p.value < 0.05, term != "(Intercept)")
