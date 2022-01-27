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

load("../Data/gridWithin_prec.rda")    # gridWithin_prec

group = as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID')) # 1-100
set.seed(group)

file_names <- c(paste0("../Data/Surfaces/gridPointValues_hotspot_", group,".rda"),
                paste0("../Data/Surfaces/gridPointValues_uniform_", group,".rda"),
                paste0("../Data/Surfaces/gridPointValues_cov_r_", group,".rda"))

save_names <- c("HotSpot/", "Uniform/", "Random/")

for (file in 1:3) {

    # Loading and initializing variables
    load(file_names[file])
    gridPointValues = NULL

    if (file == 1) {gridPointValues = gridPointValues_hotspot}
    else if (file == 2) {gridPointValues = gridPointValues_uniform}
    else if (file == 3) {gridPointValues = gridPointValues_cov_r}
    else {print("Incorrect input to start")}

    # Iterate through each buffer width
    for (index in 2:13) {

      for (k in 1:77) {

          print(paste0(save_names[file], " Buffer: ", index, ", Precinct: ", k))

          l = 1000 # arbitrary start length

          nullStr_point_data <- data.frame("precinct" = rep(-1,l), "indigo" = rep(-1,l), "juliet" = rep(-1,l),
                                           "count1" = rep(-1,l), "count2" = rep(-1,l),
                                           "streets1" = rep(-1,l), "streets2" = rep(-1,l),
                                           "area1" = rep(-1,l), "area2" = rep(-1,l), "tStat_area" = rep(NA,l), "naivePVal" = rep(-1,l),
                                           "totLength" = rep(-1,l), "splitProper" = rep(T,l), "tStat" = rep(NA,l))
          rowNum = 1

          load(paste0("../Data/OutputStrInfo/strInfo_", index, "_", k, ".dat")) # contains the buffer object

          print(paste0("Total length: ", length(streetLengthInfo_null)))
          for(i in 1:length(streetLengthInfo_null)) {
            print(paste0("i ", i))
            for(j in 1:length(streetLengthInfo_null[[i]])) {
              if(!is.null(streetLengthInfo_null[[i]][[j]])) {
                if(length(streetLengthInfo_null[[i]][[j]]$buffer@polygons) > 1){
                  poly1 = streetLengthInfo_null[[i]][[j]]$buffer@polygons[[1]]
                  poly2 = streetLengthInfo_null[[i]][[j]]$buffer@polygons[[2]]

                  area1 = poly1@area
                  area2 = poly2@area

                  s1 = streetLengthInfo_null[[i]][[j]]$streetLength1
                  s2 = streetLengthInfo_null[[i]][[j]]$streetLength2

                  gridVals_ind_1 = streetLengthInfo_null[[i]][[j]]$pointIndex[[1]]
                  gridVals_ind_2 = streetLengthInfo_null[[i]][[j]]$pointIndex[[2]]

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

                  vals = c(t1,s1,t2,s2)
                  if(sum(vals == 0) > 0) {
        			  if(vals[2] == 0 | vals[4] == 0) {
        				  vals = vals+1
        			  } else {
        				  vals[1] = vals[1] + 1
        				  vals[3] = vals[3] + 1
        			  }
        		  }

                  tStat = tStat_a = 0

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

                  nullStr_point_data[rowNum,] = c(k, i, j, t1, t2,
                                              streetLengthInfo_null[[i]][[j]]$streetLength1,
                                              streetLengthInfo_null[[i]][[j]]$streetLength2,
                                              area1, area2, tStat_a, pval,
                                              streetLengthInfo_null[[i]][[j]]$totalLength, T, tStat)
                  rowNum = rowNum + 1
                }
              }
            }
          }

          save(nullStr_point_data, file=paste0("../Output/", save_names[file],
                "nullData", group, "_at_" ,index, "_", k,".dat", sep=''))
      }
      #remember to remove the -1

    }


}
