###############################################################################
#                                                                      April 19
#                 Gut Fullness - Data Prep for Hurdle Models
#  
#  Notes:
#  * Added:
#    - mean drift mass,
#    - river temps
#    - rbt density
#    - time since dark
#  * includes empty tummies 
#  * This version is fitting ... 
#    - using (1|no.trip.site) as random effect
#                     
#
#  To Do:
#  * 
#
###############################################################################
# library(devtools)
# install_github(repo = 'jmuehlbauer-usgs/R-packages', subdir = 'foodbase')
library(dplyr)
library(foodbase)
library(suncalc)
library(arm)

# d.tmp = readDB(gear = "FishGut", type = "Sample", updater = TRUE)
d.tmp = readDB(gear = "FishGut", type = "Sample", updater = FALSE)

d.1 = d.tmp[d.tmp$FishSpeciesID %in% c("RBT"),]

NO_trips = c('GC20120419','GC20120705','GC20120913','GC20130110',
             'GC20130404','GC20130625','GC20130912','GC20140109',
             'GC20140403','GC20140626','GC20140911','GC20150108')

d.2 = d.1[which(d.1$TripID %in% NO_trips),]

#-----------------------------------------------------------------------------#
dat = sampspec(samp = d.2)

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

spec.ltl.2 = group_by(spec.ltl, PITTagID) %>%
             summarise(est.mass.mg = sum(TotalMass, na.rm = TRUE))

spec.ltl.2$est.mass.g = spec.ltl.2$est.mass.mg * .001

diet = left_join(ltl.samps, spec.ltl.2, by = "PITTagID")

# add in the No site
# these are based roughly on the excel file "Jan2016_Site Location List for Boatman.xlsx"
diet$no.site = ifelse(diet$RiverMile <= 0, 'I',
                           ifelse(diet$RiverMile >= 17.22 & diet$RiverMile <= 20.58, 'II', 
                                  ifelse(diet$RiverMile >= 37.57 & diet$RiverMile <= 42.11, 'III',
                                         ifelse(diet$RiverMile > 59 & diet$RiverMile < 62.5, 'IVa',
                                                ifelse(diet$RiverMile > 63 & diet$RiverMile < 65.6, 'IVb',
                                                       ifelse(diet$RiverMile > 65.6, 'downstream', 'other'))))))

# drop samples outside of sites (or NA, i.e., no river mile)
# Morgan is looking into this

diet.2 = diet[which(diet$no.site %in% c("I", "II", "III", "IVa", "IVb")),]

# change the name for matching below
names(diet.2)[which(names(diet.2) == "TripID")] = "no.trip"

#-----------------------------------------------------------------------------#
# rbt abundance from Josh
tmp.d = read.table(file = "U:/Desktop/FB_Git/Diet/NO_Fullness/Data/Global_mr.csv", header = T, sep = ",")

trips = unique(diet.2$no.trip)

trip.key = data.frame(trip.id = trips,
                      month = month.abb[as.numeric(substr(trips, 7, 8))],
                      year = substr(trips, 5, 6))

trip.key$month = ifelse(trip.key$month == "Jun", "Jul", paste(trip.key$month))

trip.key$josh = paste(trip.key$year, trip.key$month, sep = "-")

tmp.d2 = tbl_df(data.frame(site = tmp.d$reach,
                           trip = trip.key[match(tmp.d$trip, trip.key$josh),1],
                           N = as.numeric(tmp.d$MLE)))
trout = tmp.d2[which(!is.na(tmp.d2$trip)),]

trout$density = ifelse(trout$site == "IVb", trout$N / 5750, trout$N / 4000)

names(trout)[1:2] = c("no.site", "no.trip")

# write this out, so I can use it in the selection model
# write.table(trout, file = "RBT_Density.txt", row.names = F, sep = "\t")
#-----------------------------------------------------------------------------#
# Temperature and Turbidity 
setwd('U:/Desktop/FB_Git/Diet/NO_Fullness/Data')
d.turb = read.table(file = "NO_Turb_ts_summary.csv", header = T, sep = ",")
names(d.turb)[1:2] = c("no.trip", "no.site")  # so I can merge below

d.temp = read.table(file = "NO_Temp_ts_summary.csv", header = T, sep = ",")
names(d.temp)[1:2] = c("no.trip", "no.site")  # so I can merge below

#-----------------------------------------------------------------------------#
# Bring in the drift data!

# drift.tmp = readDB(gear = "Drift", type = "Sample", updater = TRUE)
drift.tmp = readDB(gear = "Drift", type = "Sample", updater = FALSE)

drift.tmp.2 = drift.tmp[which(drift.tmp$TripID %in% NO_trips),]
# all gear type 4 for NO trips

drift.dat = sampspec(samp = drift.tmp.2, species = "Big9")

#--------------------------------------
# cut down the sample table
keep.col.2 = c("BarcodeID",
             "TripID",
             # "Region",
             # "Reach",
             "Date",
             "SampleNumber",
             "RiverMile",
             "DepthTotal",
             "DepthSample",
             "DepthIntegrated",
             # "GearID",
             "TimeDay", 
             "TimeBegin", 
             "TimeElapsed",
             "Volume")
# "Notes")

ltl.samp = drift.dat$Samples[,which(names(drift.dat$Samples) %in% keep.col.2)] 

ltl.samp$no.site = ifelse(ltl.samp$RiverMile <= 0, 'I',
                      ifelse(ltl.samp$RiverMile >= 17.22 & ltl.samp$RiverMile <= 20.58, 'II', 
                             ifelse(ltl.samp$RiverMile >= 37.57 & ltl.samp$RiverMile <= 42.11, 'III',
                                    ifelse(ltl.samp$RiverMile > 59 & ltl.samp$RiverMile < 62.5, 'IVa',
                                           ifelse(ltl.samp$RiverMile > 63 & ltl.samp$RiverMile < 65.6, 'IVb',
                                                  ifelse(ltl.samp$RiverMile > 65.6, 'downstream', 'other'))))))

# Only data for the NO sites
ltl.samp.2 = ltl.samp[which(ltl.samp$no.site %in% c("I", "II", "III", "IVa", "IVb")),]

#--------------------------------------
# only the total mass

# counts...
# drift.dat$Specimens$CountTotal = rowSums(drift.dat$Specimens[,3:23])
# mass...
drift.dat$Biomass$MassTotal = rowSums(drift.dat$Specimens[,3:23])
ltl.specs = drift.dat$Biomass[,c(1,2,24)]

#--------------------------------------
# merge the sample table with the specimen counts

no.drift.dat = left_join(ltl.samp.2, ltl.specs, by = "BarcodeID")

no.drift.dat$SpeciesID = as.character(no.drift.dat$SpeciesID)

#--------------------------------------
# should it be, sum across taxa for a sample, then average? (this gives the same avg mass as below)

# get the avg. mass per trip, site, & taxa
t.dat = dplyr::select(no.drift.dat, no.trip = TripID, no.site,
                      taxa = SpeciesID, mass = MassTotal) %>%
  filter(no.site %in% c("I", "II", "III", "IVa", "IVb")) %>%
  group_by(no.trip, no.site, taxa) %>%
  summarise(avg.mass = mean(mass))

# sum across taxa
dat3 = group_by(t.dat, no.trip, no.site) %>%
  summarise(drift.mass = sum(avg.mass))

#-----------------------------------------------------------------------------#
# put the pieces together

# add in drift
diet.3 = left_join(diet.2, dat3, by = c("no.trip", "no.site"))

# add in trout numbers
diet.4 = left_join(diet.3, trout, by = c("no.trip", "no.site"))

# add in the turbidity
diet.5 = left_join(diet.4, d.turb, by = c("no.trip", "no.site"))

# add in temperatures
diet.6 = left_join(diet.5, d.temp, by = c("no.trip", "no.site"))

#-----------------------------------------------------------------------------#    # need to check this section
# Add the time since sunset piece...

# Lees Ferry -- Lat: 36.865545, Long: -111.589680
my.lat = c(36.865545)
my.long = c(-111.589680)

sunset = getSunlightTimes(date = diet.6$Date, lat = my.lat, lon = my.long, tz = "MST")

# sunset.time = substr(sunset$sunset, 12, 16)
sunset.time = sunset$sunset

# tmp.tss = difftime(strptime(diet.6$Time, "%H:%M"), strptime(sunset.time, "%H:%M"), units = "mins")
tmp.tss = difftime(strptime(paste(diet.6$Date, diet.6$Time), "%Y-%m-%d %H:%M"),
                   strptime(sunset$sunset, "%Y-%m-%d %H:%M"), units = "mins")

# fix bad tss, because 1). fish sample taken after midnight and the date (in the
# data) doesn't change and 2). fish sample taken before sunset. Correct these by
# 1). changing the date and recalculating, or 2). assuming 1 min after sunset.
out = vector()

for(i in 1:length(tmp.tss)){
  if(tmp.tss[i] <= 0 & tmp.tss[i] <= -150){  # when fish sample is taken past midnight
    out[i] = difftime(strptime(paste(diet.6$Date[i] + 1, diet.6$Time[i]), "%Y-%m-%d %H:%M"),
                      strptime(sunset.time[i], "%Y-%m-%d %H:%M"), units = "mins")
  } else {
    if(tmp.tss[i] <= 0){ # when fish sample is taken before sunset
      out[i] = 1
    } else {
      out[i] = tmp.tss[i]      
    }
  }
}


diet.6$tss = out

dat.in = diet.6
#-----------------------------------------------------------------------------#
# There are some missing fish weights, so predict weight, given length
# email from Eldo, where Korman provides the a & b parms, don't handle the
# sigma, these are from a log ~ log regression
a = -11.1584485245053
b = 2.9485978095191
# sigma = 0.153933682398055

# tl = seq(10,400,10)
# pred.wt = exp(a + b*log(tl))

dat.in$Weight = ifelse(is.na(dat.in$Weight),
                       exp(a + b*log(dat.in$ForkLength)),
                           dat.in$Weight)
#-----------------------------------------------------------------------------#
# there is a bad fork length - take it out
dat.in = dat.in[-172,]

#-----------------------------------------------------------------------------#
dat.in$no.trip.site = as.factor(paste(dat.in$no.trip, dat.in$no.site))

# scale and center
dat.in$ts.mean.2 = rescale(log(dat.in$ts.mean))
dat.in$ts.mean.3 = rescale(dat.in$ts.mean)
dat.in$drift.mass.2 = rescale(dat.in$drift.mass)
dat.in$density.2 = rescale(dat.in$density)
dat.in$tss.2 = rescale(dat.in$tss)
dat.in$mean.temp.2 = rescale(dat.in$mean.temp)

rm(list=setdiff(ls(), "dat.in"))
#-----------------------------------------------------------------------------#
# End