## --------------------------------------------------------------------------
## The goal of this file is to test the new matching method on all buffer
## widths and see if we get more reasonable histograms and p-values
## --------------------------------------------------------------------------

## Formatting the data
library("sp")
library("sf")
library("rgeos")
library("raster")

surface_type = c("uniform", "hotspot", "cov_r")
folder_type <- c("Flat_area", "HotSpot_area", "Random_area")

trialNum = as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID')) # 1-100
set.seed(trialNum)

for (s_name in 1:3) {

  Dir = paste0('~/Desktop/Research/UF/2021/NYC_Methods/Simulation/Results/Investigation/',
                surface_type[s_name])
  print(Dir)

  for (k in 2:13) {

    load("../Data/totalStreetBuffInfo_ORIG.RData")
    load(paste0("../Data/gridPointValues_", surface_type[s_name],
                "_", trialNum, ".rda"))

    gridPointValues = NULL
    if(s_name == 1) {gridPointValues = gridPointValues_uniform}
    if(s_name == 2) {gridPointValues = gridPointValues_hotspot}
    if(s_name == 3) {gridPointValues = gridPointValues_cov_r}

    sim_orig <- data.frame("count1" = rep(NA,164), "count2" = rep(NA,164),
                           "tStats" = rep(NA,164), "tStats_area" = rep(NA,164),
                           "pVals_naive" = rep(NA,164), "area1" = rep(NA,164),
                           "area2" = rep(NA,164))
    load("../Data/gridWithin.rda")
    load("../Data/indexList_MAIN.RData")
    gridCoords = gridWithin@coords
    colnames(gridCoords) = c("x", "y")

    print(paste0("Finding points in original borders for k = ", k))
    for (i in indexList_MAIN) {

      print(paste0("index ", i, " of 164"))
      poly1 = totalStreetBuffInfo_ORIG[[k]][[i]]$buffer@polygons[[1]]
      poly2 = totalStreetBuffInfo_ORIG[[k]][[i]]$buffer@polygons[[2]]

      area1 = poly1@area
      area2 = poly2@area

      p1 = point.in.polygon(gridCoords[,1], gridCoords[,2],
                            poly1@Polygons[[1]]@coords[,1], poly1@Polygons[[1]]@coords[,2])
      p2 = point.in.polygon(gridCoords[,1], gridCoords[,2],
                            poly2@Polygons[[1]]@coords[,1], poly2@Polygons[[1]]@coords[,2])
      ind1 <- which(p1 > 0)
      ind2 <- which(p2 > 0)

      s1 = totalStreetBuffInfo_ORIG[[k]][[i]]$streetLength1
      s2 = totalStreetBuffInfo_ORIG[[k]][[i]]$streetLength2

      gridVals_ind_1 = gridWithin$index[ind1]
      gridVals_ind_2 = gridWithin$index[ind2]

      gridValues1 = gridPointValues[gridVals_ind_1]
      gridValues2 = gridPointValues[gridVals_ind_2]

      arr1 <- sum(gridValues1)
      arr2 <- sum(gridValues2)

      count1 = count2 = 0

      #count on one side of boundary
      if(arr1 > 0) {count1 = rpois(1, arr1)}
      else {count1 = rpois(1, 1)} #assume there is at least 1

      #count on the other side of the boundary
      if(arr2 > 0) {count2 = rpois(1, arr2)}
      else {count2 = rpois(1, 1)} #assume there exists at least 1

      t1 = count1
      t2 = count2

      print("Calculating test stats")

      vals = c(t1,s1,t2,s2)
      if(sum(vals == 0) > 0) {
        if(vals[2] == 0 | vals[4] == 0) {
          vals = vals+1
        } else {
          vals[1] = vals[1] + 1
          vals[3] = vals[3] + 1
        }

      }

      tStat = tStat_a = pval = 0

      # Want division to be large / small (streets)
      if ((vals[1]/vals[2]) > (vals[3]/vals[4])) {
        tStat = (vals[1]/vals[2]) / (vals[3]/vals[4])
      } else {
        tStat = (vals[3]/vals[4]) / (vals[1]/vals[2])
      }

      # Want division to be large / small (area)
      if ((vals[1]/area1) > (vals[3]/area2)) {
        tStat_a = (vals[1]/area1) / (vals[3]/area2)
      } else {
        tStat_a = (vals[3]/area2) / (vals[1]/area1)
      }

      n = count1 + count2
      p = 0.5
      pval = 0

      if (count1 <= n/2) {
        pval = pbinom(count1, n, p) + 1 - pbinom(count2, n, p)
      } else {
        pval = pbinom(count2, n, p) + 1 - pbinom(count1, n, p)
      }

      sim_orig[i,] = c(t1, t2, tStat, tStat_a, pval, area1, area2)

    }

    save(sim_orig, file = paste0(Dir, '/sim_orig_', k, '.dat'))
  }
}


## --------------------------- Plot --------------------------------------------
pdf('~/Desktop/Research/UF/2021/NYC_Methods/Simulation/Results/Plots/pValDistr150.pdf')
par(mfrow=c(2,3))

load("~/Desktop/Research/UF/2021/NYC_Methods/Simulation/Data/indexList_MAIN.RData")
load("~/Desktop/Research/UF/2021/NYC_Methods/Simulation/Data/totalStreetBuffInfo_ORIG.RData")

for (k in 2:13) {

  print(k)

  for (s_name in 1:3) {

    Dir = paste0('~/Desktop/Research/UF/2021/NYC_Methods/Simulation/Results/Investigation/',
                  surface_type[s_name])
    print(Dir)


    load(paste0("~/Desktop/Research/UF/2021/NYC_Methods/Simulation/Results/", folder_type[s_name], "/combinedMatchingSetup", k, ".dat"))
    load(paste0(Dir, "/sim_orig_",k,".dat"))

    # We did this in the combination step
    # wMatchOk = which((combinedMatchingSetup$area1 / combinedMatchingSetup$area2) > 0.5 &
    #                    (combinedMatchingSetup$area1 / combinedMatchingSetup$area2) < 2 &
    #                    (combinedMatchingSetup$streets1 / combinedMatchingSetup$streets2) > 0.5 &
    #                    (combinedMatchingSetup$streets1 / combinedMatchingSetup$streets2) < 2)
    # combinedMatchingSetupFix = combinedMatchingSetup[wMatchOk,]
    #
    # ## Create ratios of area and streets
    # combinedMatchingSetupFix$ratioArea = combinedMatchingSetupFix$area1 /
    #   combinedMatchingSetupFix$area2
    # combinedMatchingSetupFix$ratioArea[which(combinedMatchingSetupFix$ratioArea < 1)] =
    #   1/combinedMatchingSetupFix$ratioArea[which(combinedMatchingSetupFix$ratioArea < 1)]
    #
    # combinedMatchingSetupFix$ratioStreet = combinedMatchingSetupFix$streets1 /
    #   combinedMatchingSetupFix$streets2
    # combinedMatchingSetupFix$ratioStreet[which(combinedMatchingSetupFix$ratioStreet < 1)] =
    #   1/combinedMatchingSetupFix$ratioStreet[which(combinedMatchingSetupFix$ratioStreet < 1)]

    ## Now remove data points where these ratios are much different
    wRatioOk = which(combinedMatchingSetupFix$ratioArea / combinedMatchingSetupFix$ratioStreet < 1.4 &
                       combinedMatchingSetupFix$ratioArea / combinedMatchingSetupFix$ratioStreet > 1/1.4)
    combinedMatchingSetupFix2 = combinedMatchingSetupFix[wRatioOk,]

    v1 = sd(combinedMatchingSetupFix2$area1 + combinedMatchingSetupFix2$area2, na.rm=TRUE)^2
    v2 = sd(combinedMatchingSetupFix2$ratioArea, na.rm=TRUE)^2

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

        w50 = order(dist_temp)[1:150]

        null_dist = combinedMatchingSetupFix2$tStat_area[w50]
        pval[ii] = mean(null_dist > stat_temp)
      }
    }

    hist(pval, main = paste0(folder_type[s_name], ": pVal for B_", k*100),
         xlab = paste0("Perc. < 0.05 is ",  round(mean(pval < 0.05, na.rm=TRUE), 4)),
         xlim=c(0,1))
    # mean(pval < 0.15, na.rm=TRUE)
    # min(pval, na.rm=TRUE)

  }
}
dev.off()
