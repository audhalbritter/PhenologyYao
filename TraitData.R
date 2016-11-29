# Trait data
trait <- read_excel("plant functional groups-Snow Fence.xlsx", col_names = TRUE)
trait <- trait[,c(1,3)]
head(trait)





### Flowering time

pheno.long %>% 
  filter(treatment == "Control", pheno.stage == "Flower", pheno.var == "first") %>% 
  ggplot(aes(x = value)) + geom_histogram()

flowertime <- read_excel("FloweringClassification.xlsx", col_names = TRUE)
flowertime <- flowertime[,1:2]
head(flowertime)
