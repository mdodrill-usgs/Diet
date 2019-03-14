###############################################################################
#                                                                      March 19
#
#   Beta Regression fit to the diet data - looking at changes with turbidity
#
#  Notes:
#  * Prior version - U:\Desktop\FB_DOWN\Analysis\DIET_Turb_Prop\brms_Beta_v1
#
#  To Do:
#  * Clean up the plotting/extracting results part
#  * Add in other covariates
#  * Add in model selection, looks like it will work with loo/waic!
#
###############################################################################
library(rstan)
library(brms)
library(ggplot2)
# library(reshape2)
library(dplyr)
library(foodbase)

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
# get total counts
spec = dat$Specimens

spec$TotalCount = rowSums(spec[,3:23])

spec.ltl = spec[,c(1:2,24)]

diet = left_join(ltl.samps, spec.ltl, by = "PITTagID")

# maybe we should fix this in the data base?
names(diet)[7] = "FishSpecies"
names(diet)[10] = "SpeciesID"
#-----------------------------------------------------------------------------#
# cut down to some of the common prey 
keep = c("CHIL", "SIML", "CHIP", "CHIA", "SIMA", "SIMP", "HEMA", "GAMM", "HYMA",
         "COLE", "NEMA", "NZMS")

diet.2 = diet[which(diet$SpeciesID %in% keep),]

# cut out the zeros. b/c they won't effect the proportions
diet.3 = diet.2[which(diet.2$TotalCount >= 1 ),]

diet.3$prey = as.character(diet.3$SpeciesID)

#-----------------------------------------------------------------------------#
# add in the turbidity & other covariates...


#-----------------------------------------------------------------------------#
# format for beta

id = unique(diet.3$PITTagID)

out = list()

for(i in 1:length(id)){
  sub = diet.3[diet.3$PITTagID == id[i],]
  
  total = sum(sub$TotalCount)
  
  out[[i]] = data.frame(id = sub$PITTagID,
                        species = sub$FishSpecies,
                        prey = sub$prey,
                        # turb = sub$ts.mean,
                        prop = sub$TotalCount / total)
}

all = do.call(rbind, out)

all$prop = ifelse(all$prop == 1, .99, all$prop)

#-----------------------------------------------------------------------------#
# quick plots

p = ggplot(all, aes(x = prey, y = prop)) +
    geom_boxplot(aes(color = species, fill = species), alpha = .5)
p 

#-----------------------------------------------------------------------------#
bnt = all[all$species == "BNT",]
rbt = all[all$species == "RBT",]

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

library(tidybayes)
library(modelr)


bnt %>%
  data_grid(prey) %>%
  add_fitted_draws(fit.1) %>%
  head(10)


# bnt %>%
#   data_grid(prey) %>%
#   add_fitted_draws(fit.1) %>%
#   ggplot(aes(x = .value, y = prey)) +
#   stat_pointintervalh(.width = c(.66, .95))
b.out = bnt %>%
  data_grid(prey) %>%
  add_fitted_draws(fit.1)

r.out = rbt %>%
  data_grid(prey) %>%
  add_fitted_draws(fit.2)

  
all.out = rbind(b.out, r.out)  
all.out$sp = c(rep("BNT", nrow(b.out)), rep("RBT", nrow(r.out)))

# 
# p = ggplot(all.out, aes(x = value, y = prey)) +
#   stat_pointintervalh()
# p  

 all.out %>% ggplot(aes(x = .value, y = prey)) +
  stat_pointintervalh(.width = c(.66, .95), aes(color = sp), position = "dodge")
#-----------------------------------------------------------------------------#
# End