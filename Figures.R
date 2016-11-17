#### FIGURES


pheno.long %>% 
  filter(pheno.var %in% c("peak"), pheno.unit == "days") %>% 
  ggplot(aes(x = treatment, y = value)) +
  geom_boxplot() +
  facet_grid(pheno.stage ~ pheno.var)



# Making Figures for each species
pheno.long %>% 
  #filter(functionalGroup == "forb") %>% 
  #filter(flTime == "early") %>%
  filter(pheno.stage %in% c("f"), pheno.var %in% c("first", "duration")) %>% 
  #filter(pheno.stage %in% c("bf", "fs", "sr")) %>% 
  #filter(pheno.var %in% "duration") %>% 
  group_by(species, treatment, pheno.var) %>% 
  summarise(mean = mean(value)) %>% 
  spread(key = treatment, value = mean) %>% 
  na.omit() %>% 
  gather(key = treatment, value = value, -species, -pheno.var) %>% 
  spread(key = pheno.var, value = value) %>% 
  ggplot(aes(y = species, x = first, color = treatment)) + geom_point() +
  labs(y = "", x = "Day of the year") +
  geom_segment(aes(x=first, xend=(first+duration), y=species, yend=species),size=1)

PhenologicalStages <- pheno.long %>% 
  filter(treatment %in% c("OTC", "Control","Warm","Local")) %>% 
  filter(origSite %in% c("H", "A")) %>% 
  filter(functionalGroup %in% c("forb", "graminoid")) %>% 
  #filter(flTime == c("early", "late")) %>%
  #filter(pheno.stage %in% c("b", "f", "s"), pheno.var == "first") %>% 
  #filter(pheno.stage %in% c("bf", "fs", "sr")) %>% 
  filter(pheno.var %in% "duration") %>% 
  #group_by(species, treatment) %>% 
  #summarise(mean = mean(value)) %>% 
  ggplot(aes(x = treatment, y = value, color = functionalGroup)) +
  geom_boxplot() +
  facet_grid(origSite~pheno.stage)

PhenologicalStages + theme_grey(base_size = 20) + theme(legend.title=element_blank()) + ggtitle("DOY")
PhenologicalDuration + theme_grey(base_size = 20) + theme(legend.title=element_blank()) + ggtitle("Days")
PhenologicalFirstEnd + theme_grey(base_size = 20) + theme(legend.title=element_blank()) + ggtitle("Days")
