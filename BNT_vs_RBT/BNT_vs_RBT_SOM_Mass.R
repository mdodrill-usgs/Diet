###############################################################################
#                                                                      March 19
#
#     Exploring the use of SOM & hierarchical clustering for the diet data
#
#  Notes:
#  * 
#
#  To Do:
#  * 
#
###############################################################################
# library(ggplot2)
library(dplyr)
library(foodbase)
library(tidyr)
library(kohonen)

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
             "Weight")

ltl.samps = dat$Samples[,names(dat$Samples) %in% keep.col]

#--------------------------------------
# get total mass
spec = dat$Biomass

spec$TotalMass = rowSums(spec[,3:23])

spec.ltl = spec[,c(1:2,24)]

diet = left_join(ltl.samps, spec.ltl, by = "PITTagID")

#-----------------------------------------------------------------------------#
# cut down to some of the common prey (exclude "NEMA" - the mass regression sees way high)
keep = c("CHIL", "SIML", "CHIP", "CHIA", "SIMA", "SIMP", "HEMA", "GAMM", "HYMA",
         "COLE", "NZMS")

# fish.code = c("FEGG", "CYP", "SUC",	"FISH",	"BHS",	"FMS", "HBC",	"GSF", "SMB",
#                "STB",	"RBT",	"FHM",	"SPD",	"BNT")

# cant add fish in b/c they don't have mass estimates...
# diet$SpeciesID = ifelse(diet$SpeciesID %in% fish.code, "FISH", as.character(diet$SpeciesID))

# taxa that show up in > 1% of samples
keep = c("TAMA", "MUSC", "TRIL", "DIPT", "MITE", "TRIA", "FORM", "NZMS", "COLE",
         "HYMA", "HEMA", "GAMM", "SIMA", "SIMP", "CHIA", "CHIP", "SIML", "CHIL", "SPID")

diet.2 = diet[which(diet$SpeciesID %in% keep),]
# diet.2 = diet

# diet.2[is.na(diet.2)] <- 0
#-----------------------------------------------------------------------------#
set.seed(12345)

ltl = diet.2[,c("PITTagID", "FishSpeciesID", "SpeciesID", "TotalMass")]

test = spread(ltl, SpeciesID, TotalMass)

# take out any columns with no data (all 0)
# test2 = test[,which(colSums(test[,3:ncol(test)]) != 0)]

dat.in = as.matrix(test[,3:ncol(test)])

fish.sp = as.factor(test[,2])


som.fit <- som(scale(dat.in), grid = somgrid(6, 6, "hexagonal"),
               rlen = 1000, dist.fcts = "euclidean")

summary(som.fit)

plot(som.fit, type = c("codes"))
plot(som.fit, type = c("changes"))
plot(som.fit, type = c("counts"))
plot(som.fit, type = c("dist.neighbours"))
plot(som.fit, type = c("mapping"), col = c("red", "blue")[as.integer(fish.sp)])
plot(som.fit, type = c("quality"))


tmp = hclust(dist(som.fit$codes[[1]]), method = "ward.D")

plot(tmp)

test2 = cutree(hclust(dist(som.fit$codes[[1]])),4)

add.cluster.boundaries(som.fit, test2)


xyf.wines <- xyf(scale(dat.in), fish.sp, grid = somgrid(14, 14, "hexagonal"), rlen = 1000)
summary(xyf.wines)

plot(xyf.wines, type = "codes", shape = "straight")
plot(xyf.wines, type = "changes")
plot(xyf.wines, type = c("counts"))
plot(xyf.wines, type = c("dist.neighbours"))
plot(xyf.wines, type = c("mapping"))
plot(xyf.wines, type = c("quality"))


test = cutree(hclust(dist(xyf.wines$codes[[2]])),2)

add.cluster.boundaries(xyf.wines, test)

tmp = hclust(dist(xyf.wines$codes[[2]]))

plot(tmp)
#-----------------------------------------------------------------------------#


xyf.fit <- xyf(X = scale(dat.in),
               Y = classvec2classmat(fish.sp),
               grid = somgrid(14, 14, "hexagonal"),
               rlen = 1000,
               user.weights = .99)

plot(xyf.fit, "codes")
plot(xyf.fit, "mapping", col = c("red", "blue")[as.integer(fish.sp)])
plot(xyf.fit, type = c("dist.neighbours"))

pred = predict(xyf.fit)$unit.preditions



tmp = hclust(dist(xyf.fit$codes[[2]]))
plot(tmp)



test = cutree(hclust(dist(xyf.fit$codes[[2]])),2)
add.cluster.boundaries(xyf.fit, test)


data("nir")
attach(nir)


nir.xyf2 <- xyf(X = spectra,
                Y = classvec2classmat(temperature),
                grid = somgrid(6, 6, "hexagonal"))

plot(nir.xyf2, "codes")

#-----------------------------------------------------------------------------#

# http://blog.schochastics.net/post/soms-and-ggplot/

# check out geom_hex