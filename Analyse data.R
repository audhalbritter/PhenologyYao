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
dat <- pheno.long %>% 
  mutate(block = substring(plot, nchar(plot), nchar(plot))) %>% 
  mutate(treatment = substring(plot, 1, 2)) %>% 
  filter(pheno.stage == "f", pheno.var == "peak")


# fit simple regression/anova
fit.lm <- lm(value ~ treatment, data = dat)
anova(fit.lm)
hist(dat$value)
par(mfrow=c(1,1))
plot(fit.lm)


# log transform
fit <- lm(log(value) ~ treatment, data = dat)
anova(fit)
hist(log(dat$value))


# GLM for count data
fit.glm <- glm(value ~ treatment, data = dat, family = "poisson")
summary(fit.glm)
plot(fit.glm)



# Mixed Effects Model including block
fit.glmm <- glmer(value ~ treatment + (1|block), data = dat, family = "poisson")
summary(fit.glmm)

# Mixed Effects Model including species
fit.glmm <- glmer(value ~ treatment + species + (1|block), data = dat, family = "poisson")
summary(fit.glmm)
plot(fit.glmm)

# backtransform the data
newdat <- expand.grid(
  treatment=c("GC", "SH")
  , value = 0
)
mm <- model.matrix(terms(fit.glmm), newdat)
newdat$value <- predict(fit.glmm, newdat, re.form = NA, type="response")

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
overdisp_fun(fit.glmm)


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

# Test if treatment is important using model selection
fit.glmm1 <- glmer(value ~ treatment + (1|block) + (1|species), data = dat, family = "poisson")
fit.glmm2 <- glmer(value ~ 1 + (1|block) + (1|species), data = dat, family = "poisson")

modsel(list(fit.glmm1, fit.glmm2), 1000)


