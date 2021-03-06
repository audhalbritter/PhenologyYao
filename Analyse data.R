########################################
### ANALYSE PHENOLOGY DATA ###
########################################

#### LIBRARIES ####
library("lme4")
library("tidyr")
library("dplyr")

### DATA
head(pheno.long)
pheno.long %>% 
  mutate(block = substring(plot, nchar(plot), nchar(plot))) %>% 
  mutate(treatment = substring(plot, 1, 2)) %>% 
  group_by(pheno.stage, pheno.var, treatment) %>% 
  ggplot(aes(x = treatment, y = value)) +
  geom_boxplot() +
  facet_grid(pheno.stage ~ pheno.var, scales = "free_y")


#### Analyse Models
dat <- pheno.long %>% filter(pheno.stage == "Flower", pheno.var == "peak") %>% mutate(treatment = factor(treatment), plot = factor(plot), species = factor(species))
hist(dat$value)

fit.glmm1 <- glmer(value ~ treatment + (1|plot) + (1|species), data = dat, family = "poisson")
fit.glmm2 <- glmer(value ~ 1 + (1|plot) + (1|species), data = dat, family = "poisson")
modsel(list(fit.glmm, fit.glmm2), 1000)

# Mixed Effects Model including plot and species as random effects
fit.glmm1 <- glmer(value ~ treatment * species + (1|plot), data = dat, family = "poisson")
fit.glmm2 <- glmer(value ~ treatment + species + (1|plot), data = dat, family = "poisson")
fit.glmm3 <- glmer(value ~ treatment + (1|plot), data = dat, family = "poisson")
fit.glmm4 <- glmer(value ~ species + (1|plot), data = dat, family = "poisson")
fit.glmm5 <- glmer(value ~ 1 + (1|plot), data = dat, family = "poisson")

summary(fit.glmm1)
overdisp_fun(fit.glmm1)
modsel(list(fit.glmm, fit.glmm2, fit.glmm3, fit.glmm4, fit.glmm5), 1000)

# backtransform the data
newdat <- expand.grid(
  treatment=c("Control", "Snow")
  , value = 0
)
newdat$value <- predict(fit.glmm, newdat, re.form = NA, type="response")
newdat


### Overdispersion
dat$observation <- 1:nrow(dat)

fit.glmm.od <- glmer(value ~ treatment * species + (1|plot) + (1|observation), data = dat, family = "poisson")
fit.glmm.od2 <- glmer(value ~ 1 + (1|plot) + (1|species) + (1|observation), data = dat, family = "poisson")
summary(fit.glmm.od)
overdisp_fun(fit.glmm.od)
modsel(list(fit.glmm.od, fit.glmm.od2), 1000)

# backtransform the data
newdat <- expand.grid(
  treatment=c("Control", "Snow")
  , value = 0
)
newdat$value <- predict(fit.glmm.od, newdat, re.form = NA, type="response")
newdat


#### FUNCTIONS

### Test overdispersion
# compare the residual deviance to the residual degrees of freedom
# these are assumed to be the same.

overdisp_fun <- function(model) {
  ## number of variance parameters in 
  ##   an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m)*(nrow(m)+1)/2
  }
  model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
  rdf <- nrow(model.frame(model))-model.df
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}



#function for QAICc. NB, phi is the scaling parameter from the quasi-family model. If using e.g. a poisson family, phi=1 and QAICc returns AICc, or AIC if QAICc=FALSE.
QAICc <- function(mod, scale, QAICc = TRUE) {
  ll <- as.numeric(logLik(mod))
  df <- attr(logLik(mod), "df")
  n <- length(resid(mod))
  if (QAICc)
    qaic = as.numeric(-2 * ll/scale + 2 * df + 2 * df * (df + 1)/(n - df - 1))
  else qaic = as.numeric(-2 * ll/scale + 2 * df)
  qaic
}

# Model selection
modsel <- function(mods,x){	
  phi=1
  dd <- data.frame(Model=1:length(mods), K=1, QAIC=1)
  for(j in 1:length(mods)){
    dd$K[j] = attr(logLik(mods[[j]]),"df")
    dd$QAIC[j] = QAICc(mods[[j]],phi)
  }
  dd$delta.i <- dd$QAIC - min(dd$QAIC)
  dd <- subset(dd,dd$delta.i<x)
  dd$re.lik <- round(exp(-0.5*dd$delta.i),3)
  sum.aic <- sum(exp(-0.5*dd$delta.i))
  wi <- numeric(0)
  for (i in 1:length(dd$Model)){wi[i] <- round(exp(-0.5*dd$delta.i[i])/sum.aic,3)}; dd$wi<-wi
  print(dds <- dd[order(dd$QAIC), ])
  assign("mstable",dd,envir=.GlobalEnv)
}



