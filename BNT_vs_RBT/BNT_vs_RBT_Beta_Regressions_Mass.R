###############################################################################
#                                                                      March 19
#
#   Beta Regression fit to the diet data - looking at changes with turbidity
#
#  Notes:
#  * Prior version - 'BNT_vs_RBT_Beta_Regressions.R'
#
#  To Do:
#  * Clean up the plotting/extracting results part
#  * Add in other covariates
#    -- Season or Year effects?
#  * Add in model selection, looks like it will work with loo/waic!
#  * seperate analysis of fish size vs number of taxa in diet?
#
###############################################################################
library(rstan)
library(brms)
library(ggplot2)
library(dplyr)
library(foodbase)
library(tidybayes)
library(modelr)

rstan_options(auto_write = TRUE)
options (mc.cores=parallel::detectCores()) # Run on multiple cores

d.tmp = readDB(gear = "FishGut", type = "Sample", updater = TRUE)

d.1 = d.tmp[d.tmp$SpeciesID %in% c("RBT", "BNT"),]

#-----------------------------------------------------------------------------#
dat = sampspec(samp = d.1)

# cut down some of the columns in samples (below is the full set)
keep.col = c("PITTagID",
             "TripID",
             # "Region",
             # "Reach",
             "Date",
             "Time",
             "RiverMile",
             "Site",
             "SpeciesID",
             "ForkLength",
             "Weight")#,
# "FlagStrange",
# "FlagDelete",
# "EntererSample",
# "Processor",
# "ProcessDate",
# "ProcessTime",
# "QAQC",
# "Checker",
# "EntererSpecimen",
# "Notes"

ltl.samps = dat$Samples[,names(dat$Samples) %in% keep.col]

#--------------------------------------
# get total mass
spec = dat$Biomass

spec$TotalMass = rowSums(spec[,3:23])

spec.ltl = spec[,c(1:2,24)]

diet = left_join(ltl.samps, spec.ltl, by = "PITTagID")

# maybe we should fix this in the data base?
names(diet)[7] = "FishSpecies"
names(diet)[10] = "SpeciesID"
#-----------------------------------------------------------------------------#
# cut down to some of the common prey (exclude "NEMA" - the mass regression sees way high)
keep = c("CHIL", "SIML", "CHIP", "CHIA", "SIMA", "SIMP", "HEMA", "GAMM", "HYMA",
         "COLE", "NZMS")

# taxa that show up in > 1% of samples
# keep = c("TAMA", "MUSC", "TRIL", "DIPT", "MITE", "TRIA", "FORM", "NZMS", "COLE",
#          "HYMA", "HEMA", "GAMM", "SIMA", "SIMP", "CHIA", "CHIP", "SIML", "CHIL", "SPID")

diet.2 = diet[which(diet$SpeciesID %in% keep),]

# diet.2 = diet

# cut out the zeros. b/c they won't effect the proportions
diet.3 = diet.2[which(diet.2$TotalMass > 0),]

diet.3$prey = as.character(diet.3$SpeciesID)

#-----------------------------------------------------------------------------#
# add in the turbidity & other covariates...


#-----------------------------------------------------------------------------#
# format for beta

id = unique(diet.3$PITTagID)

out = list()

for(i in 1:length(id)){
  sub = diet.3[diet.3$PITTagID == id[i],]
  
  total = sum(sub$TotalMass)
  
  out[[i]] = data.frame(id = sub$PITTagID,
                        species = sub$FishSpecies,
                        forklength = sub$ForkLength,
                        prey = sub$prey,
                        # turb = sub$ts.mean,
                        prop = sub$TotalMass / total)
}

all = do.call(rbind, out)

all$prop = ifelse(all$prop == 1, .99, all$prop)

# theres a bad length in the data
all$forklength = ifelse(all$forklength >1000, NA, all$forklength)

all$forklength.2 = all$forklength / mean(all$forklength, na.rm = T)

all = all[!is.na(all$forklength),]

#-----------------------------------------------------------------------------#
# quick plots
windows(record = T, xpos = 25)

p = ggplot(all, aes(x = prey, y = prop)) +
  geom_boxplot(aes(color = species, fill = species), alpha = .5)
p 

p = ggplot(all, aes(x = forklength, y = prop)) +
    geom_point() +
    lims(x = c(0, 500)) +
    facet_wrap(~ prey)
p
#-----------------------------------------------------------------------------#
# bnt = all[all$species == "BNT",]
# rbt = all[all$species == "RBT",]

# ni = 1000
# nt = 1
# nb = 500
# nc = 3

# t1 = proc.time()

fit.1 = brm(prop ~ prey,
            data = bnt, family = "Beta", chains = 3)
# chains = nc, iter = ni, warmup = nb, thin = nt)
# control = list(max_treedepth = 12, adapt_delta = .9))
# t2 = proc.time()
fit.2 = brm(prop ~ prey,
            data = rbt, family = "Beta", chains = 3)
# chains = nc, iter = ni, warmup = nb, thin = nt)
# control = list(max_treedepth = 12, adapt_delta = .9))
# t2 = proc.time()

fit.1 = brm(prop ~ 1,
            data = all, family = "Beta", chains = 3)

fit.2 = brm(prop ~ species,
            data = all, family = "Beta", chains = 3)

fit.3 = brm(prop ~ prey,
            data = all, family = "Beta", chains = 3)

fit.4 = brm(prop ~ species + prey,
            data = all, family = "Beta", chains = 3)

fit.5 = brm(prop ~ species * prey,
            data = all, family = "Beta", chains = 3, iter = 500)

fit.6 = brm(prop ~ species * prey + forklength.2,
            data = all, family = "Beta", chains = 3, iter = 500)

fit.7 = brm(prop ~ species * prey * forklength.2,
            data = all, family = "Beta", chains = 3, iter = 500)

waic(fit.1, fit.2, fit.3, fit.4, fit.5)

waic(fit.6, fit.7)

#-----------------------------------------------------------------------------#
effects = data_grid(all, species, prey) %>% add_fitted_draws(fit.5)

# sort based on RBT estimate
tmp = filter(effects, species == "RBT") %>%
      group_by(prey) %>%
      summarise(mean = mean(.value)) %>%
      arrange(mean)

effects$prey2 = factor(effects$prey, 
                       levels = tmp$prey, 
                       ordered = TRUE)


p = ggplot(effects, aes(x = prey2, y = .value)) +
    stat_pointinterval(aes(color = species), position = position_dodge(width = .5)) +
    coord_flip()
p

#-----------------------------------------------------------------------------#
effects = data_grid(all, species, prey, forklength.2) %>% add_fitted_draws(fit.7)

# sort based on RBT estimate
tmp = filter(effects, species == "RBT") %>%
      group_by(prey) %>%
      summarise(mean = mean(.value)) %>%
      arrange(mean)

pred.in = effects[effects$species == "RBT",]
pred.in = effects[effects$species == "BNT",]

p = ggplot(pred.in, aes(x = forklength.2, y = .value)) +
    stat_lineribbon(aes(y = .value)) +
    facet_wrap(~ prey) +
    labs(title = "BNT")
    # coord_flip()
p


plot(marginal_effects(fit.6), rug = TRUE, ask = FALSE)
#-----------------------------------------------------------------------------#


#-----------------------------------------------------------------------------#
# End