###############################################################################
#                                                                      April 19
#                     Gastric Lavage - Beta Regression
#
#  Notes:
#  * Small model selection section
#
#  To Do:
#  * 
#
###############################################################################
library(rstan)
library(brms)
library(ggplot2)
library(dplyr)
library(foodbase)
library(ggthemes)
library(tidybayes)
library(modelr)

rstan_options(auto_write = TRUE)
options (mc.cores=parallel::detectCores()) # Run on multiple cores

d.tmp = readDB(gear = "FishGut", type = "Sample", updater = TRUE)

d.1 = d.tmp[d.tmp$FishSpeciesID %in% c("RBT", "BNT"),]

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
             "FishSpeciesID",
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
# get mass
spec = dat$Biomass

spec$TotalMass = rowSums(spec[,3:23])

spec.ltl = spec[,c(1:2,24)]

diet = left_join(ltl.samps, spec.ltl, by = "PITTagID")

# Deal with the NAs !!!!!!!!!!!!!!

#-----------------------------------------------------------------------------#
# only the gastric lavage samples, start with 'G'
diet2 = diet[which(substr(diet$PITTagID, 1, 1) == "G"),]

# only sample with mass, cut down, b/c this won't effect the proportions
diet3 = diet2[which(diet2$TotalMass > 0),]

diet3 = droplevels(diet3)

diet3$SpeciesID = as.character(diet3$SpeciesID)

# code in the days of the week
dow = weekdays(diet3$Date)

diet3$dow = factor(dow, levels = c("Friday", "Saturday", "Sunday", "Monday"),
                   ordered = TRUE)

# add another group for 'other'
key = c('OLIG', 'QUAG', 'DAPH', 'NZMS', 'CHIA', 'GAMM', 'CHIP', 'CHIL')

diet3$Prey = ifelse(!diet3$SpeciesID %in% key, "Other", diet3$SpeciesID)

#-----------------------------------------------------------------------------#
# format for beta

id = unique(diet3$PITTagID)

out = list()

for(i in 1:length(id)){
  sub = diet3[diet3$PITTagID == id[i],]
  
  total = sum(sub$TotalMass)
  
  out[[i]] = data.frame(id = sub$PITTagID,
                        species = sub$FishSpeciesID,
                        forklength = sub$ForkLength,
                        dow = sub$dow,
                        prey = sub$Prey,
                        # turb = sub$ts.mean,
                        prop = sub$TotalMass / total)
}

all = do.call(rbind, out)

all$prop = ifelse(all$prop == 1, .99, all$prop)

all$flow = ifelse(all$dow %in% c("Friday", "Monday"), "Weekday", "Weekend")

#-----------------------------------------------------------------------------#

# quick plots
windows(record = T, xpos = 25)

p = ggplot(all, aes(x = prey, y = prop)) +
  geom_boxplot(aes(color = dow, fill = dow), alpha = .5)
p 

p = ggplot(all, aes(x = prey, y = prop)) +
  geom_boxplot(aes(color = flow, fill = dow), alpha = .5)
p 


#-----------------------------------------------------------------------------#
# fit some models and compare

fit.1 = brm(prop ~ 1,
            data = all, family = "Beta", chains = 3)

fit.2 = brm(prop ~ prey,
            data = all, family = "Beta", chains = 3)

fit.3 = brm(prop ~ flow,
            data = all, family = "Beta", chains = 3)

fit.4 = brm(prop ~ flow + prey,
            data = all, family = "Beta", chains = 3)

# Winner, for now
fit.5 = brm(prop ~ flow * prey,
            data = all, family = "Beta", chains = 3)

fit.6 = brm(prop ~ dow + prey,
            data = all, family = "Beta", chains = 3)

fit.7 = brm(prop ~ dow * prey,
            data = all, family = "Beta", chains = 3)


waic(fit.1, fit.2, fit.3, fit.4, fit.5, fit.6, fit.7)
loo(fit.1, fit.2, fit.3, fit.4, fit.5, fit.6, fit.7)
#-----------------------------------------------------------------------------#
# look at model predictions

effects = data_grid(all, flow, prey) %>% add_fitted_draws(fit.5)

# sort based on precents of prey
tmp = filter(effects, flow == "Weekend") %>%
  group_by(prey) %>%
  summarise(mean = mean(.value)) %>%
  arrange(mean)

effects$prey2 = factor(effects$prey, 
                       levels = tmp$prey, 
                       ordered = TRUE)



p = ggplot(effects, aes(x = prey2, y = .value)) +
    stat_pointinterval(aes(color = flow), position = position_dodge(width = .5)) +
    labs(x = "Prey Type", y = "Proportion of Diet by Mass",
         title = "Lees Ferry - Rainbow Trout Diet Lavage \n (preliminary data - do not cite)",
         color = "") +
    coord_flip() +
    theme_base() +
    theme(legend.position = c(.85,.92))
p


#-----------------------------------------------------------------------------#
# end