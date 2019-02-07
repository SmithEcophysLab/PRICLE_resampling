# Script to analyzing community composition data from the PRICLE before and after treatments

## Load packages
library(lme4)
library(car)
library(emmeans)

## read in data
diversity = read.csv('../data/diversity_data.csv')
cover_sum = read.csv('../data/cover_pft_data.csv')

## create factors
diversity$nitrogenfac = as.factor(diversity$nitrogen)
diversity$precipitationfac = as.factor(diversity$precipitation)
diversity$blockfac = as.factor(diversity$block)
diversity$yearfac = as.factor(diversity$year)
diversity$plotfac = as.factor(diversity$plot)
cover_sum$nitrogenfac = as.factor(cover_sum$nitrogen)
cover_sum$precipitationfac = as.factor(cover_sum$precipitation)
cover_sum$blockfac = as.factor(cover_sum$block)
cover_sum$yearfac = as.factor(cover_sum$year)
cover_sum$plotfac = as.factor(cover_sum$plot)


## fit mixed effects models
### Diversity
D_lmer = lmer(log10(D) ~ precipitationfac * nitrogenfac * yearfac + (1|blockfac) + (1|blockfac : precipitationfac) + (1|blockfac : nitrogenfac) + (1|plotfac), data = diversity)

### Evenness
E_lmer = lmer(log10(E) ~ precipitationfac * nitrogenfac * yearfac + (1|blockfac) + (1|blockfac : precipitationfac) + (1|blockfac : nitrogenfac) + (1|plotfac), data = diversity)

### Richness
R_lmer = lmer(log10(R) ~ precipitationfac * nitrogenfac * yearfac + (1|blockfac) + (1|blockfac : precipitationfac) + (1|blockfac : nitrogenfac) + (1|plotfac), data = diversity)

### Clonal forbs
cf_lmer = lmer(log10(Sum + 1) ~ precipitationfac * nitrogenfac * yearfac + (1|blockfac) + (1|blockfac : precipitationfac) + (1|blockfac : nitrogenfac) + (1|plotfac), data = subset(cover_sum, pft == 'cf'))

### C4 grasses
c4g_lmer = lmer(log10(Sum + 1) ~ precipitationfac * nitrogenfac * yearfac + (1|blockfac) + (1|blockfac : precipitationfac) + (1|blockfac : nitrogenfac) + (1|plotfac), data = subset(cover_sum, pft == 'c4g'))

### Nitrogen fixers
nf_lmer = lmer(log10(Sum + 1) ~ precipitationfac * nitrogenfac * yearfac + (1|blockfac) + (1|blockfac : precipitationfac) + (1|blockfac : nitrogenfac) + (1|plotfac), data = subset(cover_sum, pft == 'nf'))

### Other forbs
of_lmer = lmer(log10(Sum + 1) ~ precipitationfac * nitrogenfac * yearfac + (1|blockfac) + (1|blockfac : precipitationfac) + (1|blockfac : nitrogenfac) + (1|plotfac), data = subset(cover_sum, pft == 'of'))

### C3 grasses
c3g_lmer = lmer(log10(Sum + 1) ~ precipitationfac * nitrogenfac * yearfac + (1|blockfac) + (1|blockfac : precipitationfac) + (1|blockfac : nitrogenfac) + (1|plotfac), data = subset(cover_sum, pft == 'c3g'))


## examine output
### Diversity
Anova(D_lmer)
cld(lsmeans(D_lmer, ~ yearfac))
cld(lsmeans(D_lmer, ~ precipitationfac * nitrogenfac))
cld(lsmeans(D_lmer, ~ precipitationfac * nitrogenfac * yearfac))
cld(lsmeans(D_lmer, ~precipitationfac * nitrogenfac, at = list(yearfac = '2012'))) 
cld(lsmeans(D_lmer, ~precipitationfac * nitrogenfac, at = list(yearfac = '2013'))) 
cld(lsmeans(D_lmer, ~precipitationfac * nitrogenfac, at = list(yearfac = '2014'))) 
cld(lsmeans(D_lmer, ~precipitationfac * nitrogenfac, at = list(yearfac = '2018')))

### Evenness
Anova(E_lmer)
cld(lsmeans(E_lmer, ~ yearfac))
cld(lsmeans(E_lmer, ~ precipitationfac * nitrogenfac))
cld(lsmeans(E_lmer, ~ precipitationfac * nitrogenfac * yearfac))
cld(lsmeans(E_lmer, ~precipitationfac * nitrogenfac, at = list(yearfac = '2012'))) 
cld(lsmeans(E_lmer, ~precipitationfac * nitrogenfac, at = list(yearfac = '2013'))) 
cld(lsmeans(E_lmer, ~precipitationfac * nitrogenfac, at = list(yearfac = '2014'))) 
cld(lsmeans(E_lmer, ~precipitationfac * nitrogenfac, at = list(yearfac = '2018')))

### Richness
Anova(R_lmer)
cld(lsmeans(R_lmer, ~ yearfac))
cld(lsmeans(R_lmer, ~ precipitationfac * nitrogenfac))
cld(lsmeans(R_lmer, ~ precipitationfac * nitrogenfac * yearfac))
cld(lsmeans(R_lmer, ~precipitationfac * nitrogenfac, at = list(yearfac = '2012'))) 
cld(lsmeans(R_lmer, ~precipitationfac * nitrogenfac, at = list(yearfac = '2013'))) 
cld(lsmeans(R_lmer, ~precipitationfac * nitrogenfac, at = list(yearfac = '2014'))) 
cld(lsmeans(R_lmer, ~precipitationfac * nitrogenfac, at = list(yearfac = '2018')))

### Clonal Forb Cover
Anova(cf_lmer)
cld(lsmeans(cf_lmer, ~ yearfac))
cld(lsmeans(cf_lmer, ~ precipitationfac * nitrogenfac))
cld(lsmeans(cf_lmer, ~ precipitationfac * nitrogenfac * yearfac))
cld(lsmeans(cf_lmer, ~precipitationfac * nitrogenfac, at = list(yearfac = '2012'))) 
cld(lsmeans(cf_lmer, ~precipitationfac * nitrogenfac, at = list(yearfac = '2013'))) 
cld(lsmeans(cf_lmer, ~precipitationfac * nitrogenfac, at = list(yearfac = '2014'))) 
cld(lsmeans(cf_lmer, ~precipitationfac * nitrogenfac, at = list(yearfac = '2018')))

### C4 Grass Cover
Anova(c4g_lmer)
cld(lsmeans(c4g_lmer, ~ yearfac))
cld(lsmeans(c4g_lmer, ~ precipitationfac * nitrogenfac))
cld(lsmeans(c4g_lmer, ~ precipitationfac * nitrogenfac * yearfac))
cld(lsmeans(c4g_lmer, ~precipitationfac * nitrogenfac, at = list(yearfac = '2012'))) 
cld(lsmeans(c4g_lmer, ~precipitationfac * nitrogenfac, at = list(yearfac = '2013'))) 
cld(lsmeans(c4g_lmer, ~precipitationfac * nitrogenfac, at = list(yearfac = '2014'))) 
cld(lsmeans(c4g_lmer, ~precipitationfac * nitrogenfac, at = list(yearfac = '2018')))

### Nitrogen Fixer Cover
Anova(nf_lmer)
cld(lsmeans(nf_lmer, ~ yearfac))
cld(lsmeans(nf_lmer, ~ precipitationfac * nitrogenfac))
cld(lsmeans(nf_lmer, ~ precipitationfac * nitrogenfac * yearfac))
cld(lsmeans(nf_lmer, ~precipitationfac * nitrogenfac, at = list(yearfac = '2012'))) 
cld(lsmeans(nf_lmer, ~precipitationfac * nitrogenfac, at = list(yearfac = '2013'))) 
cld(lsmeans(nf_lmer, ~precipitationfac * nitrogenfac, at = list(yearfac = '2014'))) 
cld(lsmeans(nf_lmer, ~precipitationfac * nitrogenfac, at = list(yearfac = '2018')))

### Other Forb Cover
Anova(of_lmer)
cld(lsmeans(of_lmer, ~ yearfac))
cld(lsmeans(of_lmer, ~ precipitationfac * nitrogenfac))
cld(lsmeans(of_lmer, ~ precipitationfac * nitrogenfac * yearfac))
cld(lsmeans(of_lmer, ~precipitationfac * nitrogenfac, at = list(yearfac = '2012'))) 
cld(lsmeans(of_lmer, ~precipitationfac * nitrogenfac, at = list(yearfac = '2013'))) 
cld(lsmeans(of_lmer, ~precipitationfac * nitrogenfac, at = list(yearfac = '2014'))) 
cld(lsmeans(of_lmer, ~precipitationfac * nitrogenfac, at = list(yearfac = '2018')))

### C3 Grass Cover
Anova(c3g_lmer)
cld(lsmeans(c3g_lmer, ~ yearfac))
cld(lsmeans(c3g_lmer, ~ precipitationfac * nitrogenfac))
cld(lsmeans(c3g_lmer, ~ precipitationfac * nitrogenfac * yearfac))
cld(lsmeans(c3g_lmer, ~precipitationfac * nitrogenfac, at = list(yearfac = '2012'))) 
cld(lsmeans(c3g_lmer, ~precipitationfac * nitrogenfac, at = list(yearfac = '2013'))) 
cld(lsmeans(c3g_lmer, ~precipitationfac * nitrogenfac, at = list(yearfac = '2014'))) 
cld(lsmeans(c3g_lmer, ~precipitationfac * nitrogenfac, at = list(yearfac = '2018')))


