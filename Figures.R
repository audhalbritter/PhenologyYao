#### FIGURES

#make figures
##figure 1: first, peak and end of bud, flower and seed (unit:doy). no riep seeds
##figure 2: duration of bud, flower and seed (unit:days)
##figure 3:bf and fs (unit:days),only peak gaps,no first and end
pheno.long %>%
  filter(pheno.stage != "r"& pheno.stage != "sr") %>%
  #filter(pheno.var %in% c("first", "peak", "end"), pheno.unit == "doy") %>% 
  #filter(pheno.var %in% c("duration"), pheno.unit == "days") %>% 
  filter(pheno.var %in% c("peak"), pheno.unit == "days") %>% 
  ggplot(aes(x = treatment, y = value)) +
  geom_boxplot() +
  facet_grid(pheno.stage ~ pheno.var)+
  #labs(x = "treatment", y = "Day of the year")+
  labs(x = "treatment", y = "Duration in days")
  




# Making Figures for each species
##figure 5 bud(peak) - duration
##figure 4 flower(peak) - duration
##figure 6 seed(peak) - duration
##figure 7 
pheno.long %>% 
  #filter(functionalGroup == "forb") %>% 
  #filter(flTime == "early") %>% #needn't
  filter(pheno.stage %in% c("f"), pheno.var %in% c("first", "peak", "duration")) %>% 
  #filter(pheno.stage %in% c("b"), pheno.var %in% c("first", "peak", "duration")) %>% 
  #filter(pheno.stage %in% c("s"), pheno.var %in% c("first", "peak", "duration")) %>% 
  #filter(pheno.var %in% "duration") %>% 
  group_by(species, treatment, pheno.var, pheno.stage) %>% 
  summarise(mean = mean(value)) %>% 
  spread(key = treatment, value = mean) %>% 
  na.omit() %>% 
  gather(key = treatment, value = value, -species, -pheno.var) %>% 
  spread(key = pheno.var, value = value) %>% 
  ggplot(aes(y = species, x = peak, color = treatment)) + geom_point() +
  labs(y = "", x = "Day of the year") +
  geom_segment(aes(x=first, xend=(first+duration), y=species, yend=species),size=1)


#### Trait data plots
PhenologicalStages <- pheno.long %>% 
  filter(pheno.stage != "r"& pheno.stage != "sr") %>%
  filter(pheno.var == "peak", pheno.unit == "doy") %>% 
  #filter(functionalGroup %in% c("forb", "graminoid")) %>% 
  #filter(flTime == c("early", "late")) %>%
  ggplot(aes(x = treatment, y = value, color = FunctionalTypes)) +
  geom_boxplot() +
  facet_grid(~pheno.stage)

PhenologicalStages + theme_grey(base_size = 20) + theme(legend.title=element_blank()) + ggtitle("DOY")
PhenologicalDuration + theme_grey(base_size = 20) + theme(legend.title=element_blank()) + ggtitle("Days")
PhenologicalFirstEnd + theme_grey(base_size = 20) + theme(legend.title=element_blank()) + ggtitle("Days")
