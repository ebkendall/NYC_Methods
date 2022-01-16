##############################
#### LOADING LIB AND DATA ####
##############################
package.install = function(pack) {
  local({r <- getOption("repos");r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)})

  # name of package to install / load
  pack = pack

  if (pack %in% rownames(installed.packages())) {
    library(pack, character.only=T)
  } else {
    if (pack %in% rownames(installed.packages(lib.loc='/blue/jantonelli/emmett.kendall/Packages/R_4_0'))) {
      library(pack, lib.loc='/blue/jantonelli/emmett.kendall/Packages/R_4_0', character.only=T)
    } else {
      install.packages(pack, lib='/blue/jantonelli/emmett.kendall/Packages/R_4_0')
      library(pack, lib.loc='/blue/jantonelli/emmett.kendall/Packages/R_4_0', character.only=T)
    }
  }
}

package.install("sp")
package.install("sf")
package.install("rgeos")
package.install("raster")

k = as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID')) # 2-13
set.seed(k)


##############################################################################################################
############################################### Workflow #####################################################
##############################################################################################################
# 1) streetLengthInfo_null.R : provides all information about each null street (buffer and street length)    #
# 2) nullStreetsWithData.R   : does point.in.polygon with any spatial data you want                          #
# 3) combinePreMatch.R       : puts null street data in dataframe for easy manipulation                      #
# 4) matchingCriteria.R      : uses combinePreMatch.R and nullStreetsWithData.R to create null distribution  #
##############################################################################################################


load(paste0("Output/nullStr_point_data_", k, "_1.dat"))
nullStr_point_data = nullStr_point_data[nullStr_point_data$precinct != -1, ]
combinedMatchingSetup <- nullStr_point_data
print("Loading all data")
for(i in 2:77) {
  print(i)
  load(paste0("Output/nullStr_point_data_", k, "_", i, ".dat"))
  nullStr_point_data = nullStr_point_data[nullStr_point_data$precinct != -1, ]
  combinedMatchingSetup = rbind(combinedMatchingSetup, nullStr_point_data)
}

# Filter out the streets that do not have any streets because those are not relevant
combinedMatchingSetup = combinedMatchingSetup[combinedMatchingSetup$streets1 != 0, ]
combinedMatchingSetup = combinedMatchingSetup[combinedMatchingSetup$streets2 != 0, ]
combinedMatchingSetup = combinedMatchingSetup[!is.na(combinedMatchingSetup$tStat), ]

save(combinedMatchingSetup, file = paste0("Summary_total/combinedMatchingSetup", k, ".dat"))

###################################################################
# DATA I WILL TEST EVERYTHING WITH : combinedMatchingSetup
###################################################################

load("../Data/indexList_MAIN.RData")               # indexList_MAIN
load("../Data/totalStreetBuffInfo_ORIG.RData")     # totalStreetBuffInfo_ORIG
# load("../Data/gridPointValues_hotspot.rda")
load("../Data/gridPointValues_uniform.rda")
load("../Data/gridPointValues_cov_r.rda")

# gridPointValues = gridPointValues_hotspot
# gridPointValues = gridPointValues_uniform
gridPointValues = gridPointValues_cov_r

# This dataframe contains all matches
myMatchDF = vector(mode = "list", length = 164)
print("Getting Matches")
for (index in 1:164) {
  print(index)
  if(!is.null(totalStreetBuffInfo_ORIG[[k]][[index]])) {

    pureBASE = totalStreetBuffInfo_ORIG[[k]][[index]]$buffer@polygons[[1]]@area +
               totalStreetBuffInfo_ORIG[[k]][[index]]$buffer@polygons[[2]]@area

    #print(paste0("index: ", index, " pureBase: ", pureBASE))
    percCompare = abs(0.1 * pureBASE)
    temp = combinedMatchingSetup$area1 + combinedMatchingSetup$area2
    temp = abs(temp - pureBASE)

    temp2 = which(temp <= percCompare)
    #temp3 = which(temp <= 50)

    if (length(temp2) >= 1000) { #| length(temp3) >= 1000
      myMatchDF[[index]] = list("indexInCombo_perc" = combinedMatchingSetup[temp2,],
                                #"indexInCombo_50" = temp3,
                                "indexInCombo_lastResort" = NA)
    } else {
      temp4 = data.frame("index" = 1:length(temp), "num" = temp)
      temp4 = temp4[order(temp4$num),]
      temp4 = temp4$index[1:1000]
      myMatchDF[[index]] = list("indexInCombo_perc" = combinedMatchingSetup[temp2,],
                                #"indexInCombo_50" = temp3,
                                "indexInCombo_lastResort" = combinedMatchingSetup[temp4,])
    }

  }
}

save(myMatchDF, file = paste0("Summary_total/myMatchDF_area_", k, ".dat"))

###################################################################
###### Obtaining the empirical distribution for TREES #############
###################################################################
sim_orig <- data.frame("count1" = rep(NA,164), "count2" = rep(NA,164),
                       "tStats" = rep(NA,164), "tStats_area" = rep(NA,164),
                       "pVals_naive" = rep(NA,164), "area1" = rep(NA,164),
                        "area2" = rep(NA,164))
load("../Data/gridWithin.rda")
gridCoords = gridWithin@coords
colnames(gridCoords) = c("x", "y")

print("Finding points in original borders")
for (i in indexList_MAIN) {

  print(i)
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

  # plot(streetLengthInfo_null[[1]][[1]]$buffer)
  # points(gridCoords$x, gridCoords$y)
  # points(gridCoords$x[ind1], gridCoords$y[ind1], col = "red")
  # points(gridCoords$x[ind2], gridCoords$y[ind2], col = "blue")
  s1 = totalStreetBuffInfo_ORIG[[k]][[i]]$streetLength1
  s2 = totalStreetBuffInfo_ORIG[[k]][[i]]$streetLength2

  gridVals_ind_1 = gridWithin$index[ind1]
  gridVals_ind_2 = gridWithin$index[ind2]

  gridValues1 = gridPointValues[gridVals_ind_1]
  gridValues2 = gridPointValues[gridVals_ind_2]

  arr1 <- sum(gridValues1)
  arr2 <- sum(gridValues2)
  #print(paste0("arr1: ", arr1, " arr2: ", arr2))
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

p_vals_orig <- rep(NA, 164)
print("Getting the original P-values")
for(i in indexList_MAIN) {

  lstComp = NULL
  if(is.na(myMatchDF[[i]]$indexInCombo_lastResort)) {
    lstComp = myMatchDF[[i]]$indexInCombo_perc$tStat_area
  } else {
    lstComp = myMatchDF[[i]]$indexInCombo_lastResort$tStat_area
  }

  lstComp = lstComp[!is.na(lstComp)]

  p_vals_orig[i] = length(which(lstComp > sim_orig$tStats_area[i])) / length(lstComp)

}

sim_orig$pVals_area = p_vals_orig

save(sim_orig, file = paste0("Summary_total/sim_orig_area_", k, ".dat"))
