## --------------------------------------------------------------------------
## The goal of this file is to test the new matching method on all buffer
## widths and see if we get more reasonable histograms and p-values
## --------------------------------------------------------------------------

## **** This file is directly based on trial_joey_method.r ****

## Formatting the data
library("sp")
library("sf")
library("rgeos")
library("raster")

set.seed(11)

load("../Data/totalStreetBuffInfo_ORIG.RData")
load("../Data/tree_df_coord.RData")
load("../Data/indexList_MAIN.RData")

for (k in 2:13) {

    sim_orig <- data.frame("count1" = rep(NA,164), "count2" = rep(NA,164),
                           "tStats" = rep(NA,164), "tStats_area" = rep(NA,164),
                           "pVals_naive" = rep(NA,164), "area1" = rep(NA,164),
                           "area2" = rep(NA,164))

    gridCoords = tree_df_coord

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

      t1 <- length(which(p1 > 0)) # now I need to access their index
      t2 <- length(which(p2 > 0))

      s1 = totalStreetBuffInfo_ORIG[[k]][[i]]$streetLength1
      s2 = totalStreetBuffInfo_ORIG[[k]][[i]]$streetLength2

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

      n = t1 + t2
      p = 0.5
      pval = 0

      if (t1 <= n/2) {
        pval = pbinom(t1, n, p) + 1 - pbinom(t2, n, p)
      } else {
        pval = pbinom(t2, n, p) + 1 - pbinom(t1, n, p)
      }

      sim_orig[i,] = c(t1, t2, tStat, tStat_a, pval, area1, area2)

    }

    save(sim_orig, file = paste0('../Output/combination/sim_orig_', k, '.dat'))
}





## --------------------------- Plot --------------------------------------------
p_val_df <- vector(mode = "list", length = 13)

print("Now onto plotting everything")

pdf('../Output/Plots/pValDistr_tree.pdf')
par(mfrow=c(2,2))

for (k in 2:13) {

    print(k)

    load(paste0("../Output/combination/combinedMatchingSetup", k, ".dat"))
    load(paste0("../Output/combination/sim_orig_", k, ".dat"))

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

    hist(pval, main = paste0("Trees: pVal for B_", k*100),
         xlab = paste0("Perc. < 0.05 is ",  round(mean(pval < 0.05, na.rm=TRUE), 4)),
         xlim=c(0,1))

    p_val_df[[k]] = pval
}
dev.off()

save(p_val_df, file = paste0("../Output/combination/p_val_df.dat"))
