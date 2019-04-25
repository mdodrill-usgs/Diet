###############################################################################
#                                                                      April 19
#                     Gastric Lavage
#   
#
#  Notes:
#  * 
#
#  To Do:
#  * 
#
###############################################################################
# library(rstan)
# library(brms)
library(ggplot2)
# library(reshape2)
library(dplyr)
library(foodbase)
library(ggthemes)

# rstan_options(auto_write = TRUE)
# options (mc.cores=parallel::detectCores()) # Run on multiple cores

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
# get mass
spec = dat$Biomass

spec$TotalMass = rowSums(spec[,3:23])

spec.ltl = spec[,c(1:2,24)]

diet = left_join(ltl.samps, spec.ltl, by = "PITTagID")

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
# Grouped proportions by day and prey type

tots = group_by(diet3, dow, Prey) %>%
  summarise(tot.mass = sum(TotalMass))

prop = group_by(tots, dow) %>%
       summarise(all.mass = sum(tot.mass))

test = left_join(tots, prop, by = "dow")

test$prop = test$tot.mass / test$all.mass

#--------------------------------------
# plot
windows(xpos = 25, record = TRUE)


p = ggplot(test, aes(x = dow, y = prop)) +
    geom_col(aes(fill = Prey)) +
    labs(x = "", y = "Proportion of Diet by Mass",
         title = "Lees Ferry - Rainbow Trout Diet Lavage (preliminary data - do not cite)") +
    theme_base()

p

#-----------------------------------------------------------------------------#
# Proportions by day & site & prey
diet3b = diet3[which(!is.na(diet3$RiverMile)),]

diet3b$site = as.factor(diet3b$RiverMile)

tots = group_by(diet3b, dow, site, Prey) %>%
  summarise(tot.mass = sum(TotalMass))

prop = group_by(tots, site, dow) %>%
  summarise(all.mass = sum(tot.mass))

test = left_join(tots, prop, by = c("dow", "site"))

test$prop = test$tot.mass / test$all.mass

#--------------------------------------
# plot
windows(xpos = 25, record = TRUE)


p = ggplot(test, aes(x = dow, y = prop)) +
  geom_col(aes(fill = Prey)) +
  labs(x = "", y = "Proportion of Diet by Mass",
       title = "Lees Ferry - Rainbow Trout Diet Lavage (preliminary data - do not cite)") +
  facet_wrap(~ site) +
  theme_base() +
  theme(legend.position = c(.8,.27))

p

#-----------------------------------------------------------------------------#
# Total prey mass by day and prey type

tots = group_by(diet3, dow, Prey) %>%
       summarise(tot.mass = sum(TotalMass))

p = ggplot(tots, aes(x = dow, y = tot.mass/1000)) +
  geom_col(aes(fill = Prey)) +
  labs(x = "", y = "Total Mass in Fish Guts (g) \n(caution - not adjusted for body size)",
       title = "Lees Ferry - Rainbow Trout Diet Lavage (preliminary data - do not cite)") +
  theme_base()

p


#-----------------------------------------------------------------------------#


