#### FIGURES

#make figures
##figure 1: first, peak and end of bud, flower and seed (unit:doy). no riep seeds
##figure 2: duration of bud, flower and seed (unit:days)
##figure 3:bf and fs (unit:days),only peak gaps,no first and end
pheno.long %>%
  filter(pheno.stage != "Ripe"& pheno.stage != "SeedRipe") %>%
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
PhenoFlower <- pheno.long %>% 
  #filter(functionalGroup == "forb") %>% 
  #filter(flTime == "early") %>% #needn't
  filter(pheno.stage %in% c("Flower"), pheno.var %in% c("first", "peak", "duration")) %>% 
  #filter(pheno.stage %in% c("b"), pheno.var %in% c("first", "peak", "duration")) %>% 
  #filter(pheno.stage %in% c("s"), pheno.var %in% c("first", "peak", "duration")) %>% 
  #filter(pheno.var %in% "duration") %>% 
  mutate(species = factor(species)) %>% 
  group_by(species, treatment, pheno.var, pheno.stage) %>% 
  summarise(mean = mean(value)) %>% 
  spread(key = treatment, value = mean) %>% 
  na.omit() %>% 
  gather(key = treatment, value = value, -species, -pheno.var, -pheno.stage) %>% 
  spread(key = pheno.var, value = value) %>% 
  mutate(sp.num = as.numeric(species)) %>% 
  #mutate(sp.num = ifelse(sp.num == 3, 5, sp.num)) %>% 
  mutate(sp.num = ifelse(treatment == "Snow", sp.num + 0.2, sp.num)) %>% 
  ggplot(aes(y = peak, x = sp.num, color = treatment)) + geom_point(size = 2) +
  #scale_x_discrete(limits = c(1,  2,  3,  4,  6,  7,  8,  9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22), labels = c("Agrostis sinorupestris", "Aletris pauciflora", "Androsace minor",  "Aster asteroides", "Cyananthus incanus",  "Deyeuxia pulchella",  "Euphorbia sp", "Galearis spathulata",  "Gentiana crassula", "Juncus leucanthus", "Juncus leucomelas", "Kobresia sp", "Pedicularis rhodotricha", "Polygonum macrophyllum", "Polygonum viviparum", "Potentilla stenophylla", "Rhodiola fastigiata", "Rhodiola yunnanensis", "Salix souliei", "Tanacetum tatsienense")) +
  scale_x_discrete(limits = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), labels = c("Androsace minor",  "Cyananthus incanus",  "Deyeuxia pulchella",  "Galearis spathulata",  "Gentiana crassula", "Juncus leucanthus", "Juncus leucomelas", "Kobresia sp", "Pedicularis rhodotricha", "Polygonum macrophyllum", "Polygonum viviparum", "Potentilla stenophylla", "Rhodiola fastigiata", "Rhodiola yunnanensis", "Salix souliei", "Tanacetum tatsienense")) +
  labs(x = "", y = "Day of the year") +
  ggtitle("Peak and duration of flowering") +
  geom_segment(aes(y=first, yend=(first+duration), x=sp.num, xend=sp.num), size = 1.2) +
  scale_color_manual(values = c("grey", "blue")) +
  coord_flip() + theme_bw(base_size = 14)
ggsave(PhenoFlower, filename = "Figures/PhenoFlower.jpg", width = 6, height = 8, dpi = 300)


##Make figures for Duration of peak FlowerSeed
PhenoSeed <- pheno.long %>% 
  filter(pheno.stage %in% c("Seed"), pheno.var %in% c("first", "peak", "duration")) %>% 
  mutate(species = factor(species)) %>% 
  group_by(species, treatment, pheno.var, pheno.stage) %>% 
  summarise(mean = mean(value)) %>% 
  spread(key = treatment, value = mean) %>% 
  na.omit() %>% 
  gather(key = treatment, value = value, -species, -pheno.var, -pheno.stage) %>% 
  spread(key = pheno.var, value = value) %>% 
  mutate(sp.num = as.numeric(species)) %>% 
  mutate(sp.num = ifelse(treatment == "Snow", sp.num + 0.2, sp.num)) %>% 
  ggplot(aes(y = peak, x = sp.num, color = treatment)) + geom_point(size = 2) +
  scale_x_discrete(limits = c(1,  2,  3,  4,  6,  7,  8,  9, 10, 11, 12, 13, 15, 16, 17), labels = c("Androsace minor",  "Cyananthus incanus",  "Deyeuxia pulchella",  "Euphorbia NA", "Galearis spathulata", "Gentiana crassula", "Juncus leucanthus", "Juncus leucomelas", "Kobresia NA", "Pedicularis rhodotricha", "Polygonum macrophyllum", "Polygonum viviparum", "Potentilla stenophylla", "Rhodiola fastigiata", "Rhodiola yunnanensis", "Salix souliei", "Tanacetum tatsienense")) +
  labs(x = "", y = "Day of the year") +
  ggtitle("Peak and duration of flowering") +
  geom_segment(aes(y=first, yend=(first+duration), x=sp.num, xend=sp.num), size = 1.2) +
  scale_color_manual(values = c("grey", "blue")) +
  coord_flip() + theme_bw(base_size = 14)
ggsave(PhenoFlower, filename = "Figures/PhenoFlower.jpg", width = 6, height = 8, dpi = 300)


PhenoSeed <- pheno.long %>% 
  filter(pheno.stage %in% c("Seed"), pheno.var %in% c("peak", "duration")) %>% 
  mutate(species = factor(species)) %>% 
  group_by(species, treatment, pheno.var, pheno.stage) %>% 
  summarise(mean = mean(value)) %>% 
  spread(key = treatment, value = mean) %>% 
  na.omit() %>% 
  gather(key = treatment, value = value, -species, -pheno.var, -pheno.stage) %>% 
  spread(key = pheno.stage, value = value) %>% 
  mutate(sp.num = as.numeric(species)) %>% 
  #mutate(sp.num = ifelse(sp.num == 3, 5, sp.num)) %>% 
  mutate(sp.num = ifelse(treatment == "Snow", sp.num + 0.2, sp.num)) %>% 
  ggplot(aes(y = Seed, x = sp.num, color = treatment)) + geom_point(size = 2) +
  scale_x_discrete(limits = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), labels = c("Androsace minor",  "Cyananthus incanus",  "Deyeuxia pulchella",  "Galearis spathulata",  "Gentiana crassula", "Juncus leucanthus", "Juncus leucomelas", "Kobresia sp", "Pedicularis rhodotricha", "Polygonum macrophyllum", "Polygonum viviparum", "Potentilla stenophylla", "Rhodiola fastigiata", "Rhodiola yunnanensis", "Salix souliei", "Tanacetum tatsienense")) +
  labs(x = "", y = "Days") +
  ggtitle("Duration of Peak FlowerSeed") +
  geom_segment(aes(y=Flower, yend=(Flower+FlowerSeed), x=sp.num, xend=sp.num), size = 1.2) +
  scale_color_manual(values = c("grey", "blue")) +
  coord_flip() + theme_bw(base_size = 14)
ggsave(PhenoSeed, filename = "Figures/PhenoSeed.jpg", width = 6, height = 8, dpi = 300) 

pheno.long %>% 
  filter(pheno.unit == "doy", pheno.stage %in% c("Flower"), pheno.var == "peak") %>% 
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~ species)

dfPheno <- pheno.long %>% 
  filter(pheno.unit == "doy", pheno.stage %in% c("Flower", "Seed"), pheno.var %in% c("peak", "first")) %>% 
  group_by(species, pheno.var, pheno.stage) %>% 
  do(fit = lm(value ~ treatment, data = .))


tidy(dfPheno, fit) %>% 
  filter(p.value <= 0.05, term != "(Intercept)") %>% pn


#### Trait data plots
PhenologicalStages <- pheno.long %>% 
  filter(pheno.stage != "Ripe"& pheno.stage != "SeedRipe") %>%
  filter(pheno.var == "peak", pheno.unit == "doy") %>% 
  #filter(functionalGroup %in% c("forb", "graminoid")) %>% 
  #filter(flTime == c("early", "late")) %>%
  ggplot(aes(x = treatment, y = value, color = FloweringClassfication)) +
  geom_boxplot() +
  facet_grid(~pheno.stage)

PhenologicalStages + theme_grey(base_size = 20) + theme(legend.title=element_blank()) + ggtitle("DOY")
PhenologicalDuration + theme_grey(base_size = 20) + theme(legend.title=element_blank()) + ggtitle("Days")
PhenologicalFirstEnd + theme_grey(base_size = 20) + theme(legend.title=element_blank()) + ggtitle("Days")
