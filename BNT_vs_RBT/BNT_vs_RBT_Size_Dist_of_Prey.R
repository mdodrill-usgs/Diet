###############################################################################
#                                                                      Feb 2019 
#                Quick look at the size distribution of prey
#                             RBT vs BNT 
#
# Notes: 
# *
#  
###############################################################################
# library(devtools)
# install_github(repo = 'jmuehlbauer-usgs/R-packages', subdir = 'foodbase')
library(dplyr)
library(foodbase)
library(ggplot2)
library(tidyr)

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


specs = dat$Specimens

samp.sum = dplyr::select(specs, -SpeciesID) %>%
           group_by(PITTagID) %>%
           summarise_all(.funs = sum)

diet = left_join(ltl.samps, samp.sum, by = "PITTagID")
#-----------------------------------------------------------------------------#

ltl.d = dplyr::select(diet, SpeciesID, starts_with("B")) %>%
        group_by(SpeciesID) %>%
        summarise_all(.funs = sum)

ltl.d.2 = gather(ltl.d, size, count, -SpeciesID) 

ltl.d.2$size.2 = factor(ltl.d.2$size, levels = unique(ltl.d.2$size), ordered = T)

# got to be a better way...
r.sum = sum(ltl.d.2[ltl.d.2$SpeciesID == "RBT",]$count)
b.sum = sum(ltl.d.2[ltl.d.2$SpeciesID == "BNT",]$count)
ltl.d.2$prop = ltl.d.2$count / rep(c(b.sum, r.sum),21)

p = ggplot(ltl.d.2, aes(x = size.2, y = prop, group = SpeciesID)) +
    geom_point(aes(color = SpeciesID)) +
    geom_line(aes(color = SpeciesID))
p

#-----------------------------------------------------------------------------#
# End 