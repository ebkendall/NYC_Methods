rm(list=ls())

# load("~/Documents/Research/EmmettKendall/NullStreetTest/combinedMatchingSetup12.dat")
# load("~/Documents/Research/EmmettKendall/NullStreetTest/sim_orig_12.dat")
# load("~/Documents/Research/EmmettKendall/NullStreetTest/indexList_MAIN.RData")

#Buffer of 1200
# load("~/Desktop/Research/UF/2021/NYC_Methods/Simulation/Results/Flat_area/combinedMatchingSetup12.dat")
# load("~/Desktop/Research/UF/2021/NYC_Methods/Simulation/Results/Investigation/sim_orig_12.dat")

# Buffer of 400
load("~/Desktop/Research/UF/2021/NYC_Methods/Simulation/Results/Flat_area/combinedMatchingSetup4.dat")
load("~/Desktop/Research/UF/2021/NYC_Methods/Simulation/Results/Investigation/sim_orig_4.dat")


load("~/Desktop/Research/UF/2021/NYC_Methods/Simulation/Data/indexList_MAIN.RData")
load("~/Desktop/Research/UF/2021/NYC_Methods/Simulation/Data/totalStreetBuffInfo_ORIG.RData")

samp = sample(1 : nrow(combinedMatchingSetup), 10000, replace=FALSE)

## plot statistic as a function of area
AREA = (combinedMatchingSetup$area1 + combinedMatchingSetup$area2)[samp]
TEST = combinedMatchingSetup$tStat_area[samp]

## plot null statistics
plot(AREA, TEST, ylim=c(0,10))

## plot the original statistics to see how they compare
points(sim_orig$area1 + sim_orig$area2, sim_orig$tStats_area, col=2)

## Just look at the data for the ones with big statistics
## It appears something weird is going on with street length or area length
wBig = which(combinedMatchingSetup$tStat_area > 10)
combinedMatchingSetup[wBig[101:115],]

## Look at correlation between counts and both streets and areas
cor(combinedMatchingSetup$count1, combinedMatchingSetup$streets1)
cor(combinedMatchingSetup$count1, combinedMatchingSetup$area1)

## Do the same, but just for the big streets (association flips!)
cor(combinedMatchingSetup$count1[wBig], combinedMatchingSetup$streets1[wBig])
cor(combinedMatchingSetup$count1[wBig], combinedMatchingSetup$area1[wBig])

## Look at bordering data
sim_orig[1:15,]

cbind(sim_orig$area1, sim_orig$area2)
sort(sim_orig$area1 / sim_orig$area2)



wBig = which(combinedMatchingSetup$tStat_area > 2)
sort(combinedMatchingSetup$area1[wBig] / combinedMatchingSetup$area2[wBig])[(1:455)*100]

sort(combinedMatchingSetup$area1[-wBig] / combinedMatchingSetup$area2[-wBig])[(1:348)*1000]



## Matching using just total area without making any fixes
pval = rep(NA, nrow(sim_orig))

for (ii in 1 : nrow(sim_orig)) {
  if (ii %in% indexList_MAIN) {
    ## find matches
    area_temp = sim_orig$area1[ii] + sim_orig$area2[ii]
    stat_temp = sim_orig$tStats_area[ii]

    dist_temp = abs(area_temp - (combinedMatchingSetup$area1 + combinedMatchingSetup$area2))

    w500 = order(dist_temp)[1:500]

    null_dist = combinedMatchingSetup$tStat_area[w500]
    pval[ii] = mean(null_dist > stat_temp)
  }
}

hist(pval, xlim=c(0,1))
mean(pval < 0.05, na.rm=TRUE)
mean(pval < 0.15, na.rm=TRUE)




## Matching using just total area but after removing irregular matching locations
wMatchOk = which((combinedMatchingSetup$area1 / combinedMatchingSetup$area2) > 0.5 &
                   (combinedMatchingSetup$area1 / combinedMatchingSetup$area2) < 2 &
                   (combinedMatchingSetup$streets1 / combinedMatchingSetup$streets2) > 0.5 &
                   (combinedMatchingSetup$streets1 / combinedMatchingSetup$streets2) < 2)
combinedMatchingSetupFix = combinedMatchingSetup[wMatchOk,]


pval = rep(NA, nrow(sim_orig))

for (ii in 1 : nrow(sim_orig)) {
  if (ii %in% indexList_MAIN) {
    ## find matches
    area_temp = sim_orig$area1[ii] + sim_orig$area2[ii]
    stat_temp = sim_orig$tStats_area[ii]

    dist_temp = abs(area_temp - (combinedMatchingSetupFix$area1 + combinedMatchingSetupFix$area2))

    w500 = order(dist_temp)[1:500]

    null_dist = combinedMatchingSetupFix$tStat_area[w500]
    pval[ii] = mean(null_dist > stat_temp)
  }
}

hist(pval, xlim=c(0,1))
mean(pval < 0.05, na.rm=TRUE)
mean(pval < 0.15, na.rm=TRUE)
min(pval, na.rm=TRUE)





## Matching using bivariate quantity
wMatchOk = which((combinedMatchingSetup$area1 / combinedMatchingSetup$area2) > 0.5 &
                   (combinedMatchingSetup$area1 / combinedMatchingSetup$area2) < 2 &
                   (combinedMatchingSetup$streets1 / combinedMatchingSetup$streets2) > 0.5 &
                   (combinedMatchingSetup$streets1 / combinedMatchingSetup$streets2) < 2)
combinedMatchingSetupFix = combinedMatchingSetup[wMatchOk,]

combinedMatchingSetupFix$ratioArea = combinedMatchingSetupFix$area1 /
  combinedMatchingSetupFix$area2
combinedMatchingSetupFix$ratioArea[which(combinedMatchingSetupFix$ratioArea < 1)] =
  1/combinedMatchingSetupFix$ratioArea[which(combinedMatchingSetupFix$ratioArea < 1)]


v1 = sd(combinedMatchingSetupFix$area1 + combinedMatchingSetupFix$area2, na.rm=TRUE)^2
v2 = sd(combinedMatchingSetupFix$ratioArea, na.rm=TRUE)^2

pval = rep(NA, nrow(sim_orig))

for (ii in 1 : nrow(sim_orig)) {
  if (ii %in% indexList_MAIN) {
    ## find matches
    area_temp = sim_orig$area1[ii] + sim_orig$area2[ii]
    ratio_temp = max(sim_orig$area1[ii] / sim_orig$area2[ii],
                     sim_orig$area2[ii] / sim_orig$area1[ii])
    stat_temp = sim_orig$tStats_area[ii]

    dist_temp = sqrt(((area_temp - (combinedMatchingSetupFix$area1 + combinedMatchingSetupFix$area2))^2/v1) +
      ((ratio_temp - combinedMatchingSetupFix$ratioArea)^2 / v2))

    w500 = order(dist_temp)[1:500]

    null_dist = combinedMatchingSetupFix$tStat_area[w500]
    pval[ii] = mean(null_dist > stat_temp)
  }
}

hist(pval, xlim=c(0,1))
mean(pval < 0.05, na.rm=TRUE)
mean(pval < 0.15, na.rm=TRUE)
min(pval, na.rm=TRUE)






## Matching using bivariate quantity but using less matches
wMatchOk = which((combinedMatchingSetup$area1 / combinedMatchingSetup$area2) > 0.5 &
                   (combinedMatchingSetup$area1 / combinedMatchingSetup$area2) < 2 &
                   (combinedMatchingSetup$streets1 / combinedMatchingSetup$streets2) > 0.5 &
                   (combinedMatchingSetup$streets1 / combinedMatchingSetup$streets2) < 2)
combinedMatchingSetupFix = combinedMatchingSetup[wMatchOk,]

combinedMatchingSetupFix$ratioArea = combinedMatchingSetupFix$area1 /
  combinedMatchingSetupFix$area2
combinedMatchingSetupFix$ratioArea[which(combinedMatchingSetupFix$ratioArea < 1)] =
  1/combinedMatchingSetupFix$ratioArea[which(combinedMatchingSetupFix$ratioArea < 1)]


v1 = sd(combinedMatchingSetupFix$area1 + combinedMatchingSetupFix$area2, na.rm=TRUE)^2
v2 = sd(combinedMatchingSetupFix$ratioArea, na.rm=TRUE)^2

pval = rep(NA, nrow(sim_orig))

for (ii in 1 : nrow(sim_orig)) {
  if (ii %in% indexList_MAIN) {
    ## find matches
    area_temp = sim_orig$area1[ii] + sim_orig$area2[ii]
    ratio_temp = max(sim_orig$area1[ii] / sim_orig$area2[ii],
                     sim_orig$area2[ii] / sim_orig$area1[ii])
    stat_temp = sim_orig$tStats_area[ii]

    dist_temp = sqrt(((area_temp - (combinedMatchingSetupFix$area1 + combinedMatchingSetupFix$area2))^2/v1) +
                       ((ratio_temp - combinedMatchingSetupFix$ratioArea)^2 / v2))

    w50 = order(dist_temp)[1:50]

    null_dist = combinedMatchingSetupFix$tStat_area[w50]
    pval[ii] = mean(null_dist > stat_temp)
  }
}

hist(pval, xlim=c(0,1))
mean(pval < 0.05, na.rm=TRUE)
mean(pval < 0.15, na.rm=TRUE)
min(pval, na.rm=TRUE)






## Matching using being within 10% on both ratio and total area
wMatchOk = which((combinedMatchingSetup$area1 / combinedMatchingSetup$area2) > 0.5 &
                   (combinedMatchingSetup$area1 / combinedMatchingSetup$area2) < 2 &
                   (combinedMatchingSetup$streets1 / combinedMatchingSetup$streets2) > 0.5 &
                   (combinedMatchingSetup$streets1 / combinedMatchingSetup$streets2) < 2)
combinedMatchingSetupFix = combinedMatchingSetup[wMatchOk,]

combinedMatchingSetupFix$ratioArea = combinedMatchingSetupFix$area1 /
  combinedMatchingSetupFix$area2
combinedMatchingSetupFix$ratioArea[which(combinedMatchingSetupFix$ratioArea < 1)] =
  1/combinedMatchingSetupFix$ratioArea[which(combinedMatchingSetupFix$ratioArea < 1)]

pval = rep(NA, nrow(sim_orig))

for (ii in 1 : nrow(sim_orig)) {
  if (ii %in% indexList_MAIN) {
    ## find matches
    area_temp = sim_orig$area1[ii] + sim_orig$area2[ii]
    ratio_temp = max(sim_orig$area1[ii] / sim_orig$area2[ii],
                     sim_orig$area2[ii] / sim_orig$area1[ii])
    stat_temp = sim_orig$tStats_area[ii]

    wArea = which((combinedMatchingSetupFix$area1 + combinedMatchingSetupFix$area2) > 0.9*area_temp &
                    (combinedMatchingSetupFix$area1 + combinedMatchingSetupFix$area2) < 1.1*area_temp)
    wRatio = which(combinedMatchingSetupFix$ratioArea > 0.9*ratio_temp &
                     combinedMatchingSetupFix$ratioArea < 1.1*ratio_temp)
    wIntersection = intersect(wArea, wRatio)

    null_dist = combinedMatchingSetupFix$tStat_area[wIntersection]
    pval[ii] = mean(null_dist > stat_temp)
  }
}

hist(pval, xlim=c(0,1))
mean(pval < 0.05, na.rm=TRUE)
mean(pval < 0.15, na.rm=TRUE)
min(pval, na.rm=TRUE)








## There seems to still be a problem where some null streets are not as badly imbalanced
## as those we have already removed, but there is still some issue. Maybe they don't
## Cross water instantly, but do at some point. Look at
## combinedMatchingSetupFix[70467,]
## or
## combinedMatchingSetupFix[47225,]
## For examples where this seems to occur. They have a big discrepancy in street length
## but not area and their counts are imbalanced because of this leading to big test statistics
## and a skewed null distribution that has bigger values than we want
## I will now remove these as well in an ad-hoc fashion, though we may want to try and remove
## these when constructing the null streets in the first place as you mentioned in our meeting

wMatchOk = which((combinedMatchingSetup$area1 / combinedMatchingSetup$area2) > 0.5 &
                   (combinedMatchingSetup$area1 / combinedMatchingSetup$area2) < 2 &
                   (combinedMatchingSetup$streets1 / combinedMatchingSetup$streets2) > 0.5 &
                   (combinedMatchingSetup$streets1 / combinedMatchingSetup$streets2) < 2)
combinedMatchingSetupFix = combinedMatchingSetup[wMatchOk,]

## Create ratios of area and streets
combinedMatchingSetupFix$ratioArea = combinedMatchingSetupFix$area1 /
  combinedMatchingSetupFix$area2
combinedMatchingSetupFix$ratioArea[which(combinedMatchingSetupFix$ratioArea < 1)] =
  1/combinedMatchingSetupFix$ratioArea[which(combinedMatchingSetupFix$ratioArea < 1)]

combinedMatchingSetupFix$ratioStreet = combinedMatchingSetupFix$streets1 /
  combinedMatchingSetupFix$streets2
combinedMatchingSetupFix$ratioStreet[which(combinedMatchingSetupFix$ratioStreet < 1)] =
  1/combinedMatchingSetupFix$ratioStreet[which(combinedMatchingSetupFix$ratioStreet < 1)]

## Now remove data points where these ratios are much different
wRatioOk = which(combinedMatchingSetupFix$ratioArea / combinedMatchingSetupFix$ratioStreet < 1.4 &
                   combinedMatchingSetupFix$ratioArea / combinedMatchingSetupFix$ratioStreet > 1/1.4)
combinedMatchingSetupFix2 = combinedMatchingSetupFix[wRatioOk,]

pval = rep(NA, nrow(sim_orig))

for (ii in 1 : nrow(sim_orig)) {
  if (ii %in% indexList_MAIN) {
    ## find matches
    area_temp = sim_orig$area1[ii] + sim_orig$area2[ii]
    ratio_temp = max(sim_orig$area1[ii] / sim_orig$area2[ii],
                     sim_orig$area2[ii] / sim_orig$area1[ii])
    stat_temp = sim_orig$tStats_area[ii]

    dist_temp = sqrt(((area_temp - (combinedMatchingSetupFix2$area1 + combinedMatchingSetupFix2$area2))^2/v1) +
                       ((ratio_temp - combinedMatchingSetupFix2$ratioArea)^2 / v2))

    w50 = order(dist_temp)[1:50]

    null_dist = combinedMatchingSetupFix2$tStat_area[w50]
    pval[ii] = mean(null_dist > stat_temp)
  }
}

hist(pval, xlim=c(0,1))
mean(pval < 0.05, na.rm=TRUE)
mean(pval < 0.15, na.rm=TRUE)
min(pval, na.rm=TRUE)




# --------------------------------------------------------------------------
# Emmett's Attempt
# --------------------------------------------------------------------------
# Figure out the max ratio of area and the max ratio of streets and get rid of 
# all null streets that are above that threshold

# Initial clean of combinedMatchingSetup for outliers
wMatchOk = which((combinedMatchingSetup$area1 / combinedMatchingSetup$area2) > 0.5 &
                   (combinedMatchingSetup$area1 / combinedMatchingSetup$area2) < 2 &
                   (combinedMatchingSetup$streets1 / combinedMatchingSetup$streets2) > 0.5 &
                   (combinedMatchingSetup$streets1 / combinedMatchingSetup$streets2) < 2)
combinedMatchingSetupFix = combinedMatchingSetup[wMatchOk,]

# First grab the ratio of streets and areas from the original borders
tempS1 = tempS2 = rep(NA, 164)
for(ii in indexList_MAIN) {
  tempS1[ii] = totalStreetBuffInfo_ORIG[[12]][[ii]]$streetLength1
  tempS2[ii] = totalStreetBuffInfo_ORIG[[12]][[ii]]$streetLength2
}
sim_orig$streets1 = tempS1; sim_orig$streets2 = tempS2

sim_orig$ratioArea = sim_orig$area1 /
  sim_orig$area2
sim_orig$ratioArea[which(sim_orig$ratioArea < 1)] =
  1/sim_orig$ratioArea[which(sim_orig$ratioArea < 1)]

sim_orig$ratioStreet = sim_orig$streets1 /
  sim_orig$streets2
sim_orig$ratioStreet[which(sim_orig$ratioStreet < 1)] =
  1/sim_orig$ratioStreet[which(sim_orig$ratioStreet < 1)]

sim_orig$ratio_ratio = sim_orig$ratioArea / 
  sim_orig$ratioStreet
sim_orig$ratio_ratio[which(sim_orig$ratio_ratio < 1)] =
  1/sim_orig$ratio_ratio[which(sim_orig$ratio_ratio < 1)]

# Doing the same ratio idea but for the null streets
combinedMatchingSetupFix$ratioArea = combinedMatchingSetupFix$area1 /
  combinedMatchingSetupFix$area2
combinedMatchingSetupFix$ratioArea[which(combinedMatchingSetupFix$ratioArea < 1)] =
  1/combinedMatchingSetupFix$ratioArea[which(combinedMatchingSetupFix$ratioArea < 1)]

combinedMatchingSetupFix$ratioStreet = combinedMatchingSetupFix$streets1 /
  combinedMatchingSetupFix$streets2
combinedMatchingSetupFix$ratioStreet[which(combinedMatchingSetupFix$ratioStreet < 1)] =
  1/combinedMatchingSetupFix$ratioStreet[which(combinedMatchingSetupFix$ratioStreet < 1)]

combinedMatchingSetupFix$ratio_ratio = combinedMatchingSetupFix$ratioArea / 
  combinedMatchingSetupFix$ratioStreet
combinedMatchingSetupFix$ratio_ratio[which(combinedMatchingSetupFix$ratio_ratio < 1)] =
  1/combinedMatchingSetupFix$ratio_ratio[which(combinedMatchingSetupFix$ratio_ratio < 1)]

wRatioOk = which(combinedMatchingSetupFix$ratioArea / combinedMatchingSetupFix$ratioStreet < 1.4 &
                   combinedMatchingSetupFix$ratioArea / combinedMatchingSetupFix$ratioStreet > 1/1.4)
combinedMatchingSetupFix = combinedMatchingSetupFix[wRatioOk,]

pval = rep(NA, nrow(sim_orig))

for (ii in 1 : nrow(sim_orig)) {
  if (ii %in% indexList_MAIN) {
    ## find matches
    stat_temp = sim_orig$tStats_area[ii]
    match_base = sim_orig$ratio_ratio[ii]
    
    match_ind = which(combinedMatchingSetupFix$ratio_ratio < 1.2 * match_base &
                      combinedMatchingSetupFix$ratio_ratio > 0.8 * match_base)
    
    print(paste0("There are ", length(match_ind), " many matches for ", ii))
    
    null_dist = combinedMatchingSetupFix$tStat_area[match_ind]
    pval[ii] = mean(null_dist > stat_temp)
  }
}

hist(pval, xlim=c(0,1))
mean(pval < 0.05, na.rm=TRUE)
mean(pval < 0.15, na.rm=TRUE)
min(pval, na.rm=TRUE)

