library("sp")
library("sf")
library("rgeos")
library("raster")

load("../Data/indexList_MAIN.RData")

surface_type = c("hotspot", "uniform", "cov_r")
save_type = c("HotSpot/", "Uniform/", "Random/")
folder_type <- c("HotSpot_combine", "Uniform_combine", "Random_combine")

set.seed(20)

global_null = vector(mode = "list", length = 3)

for (s_name in 1:3) {
    # Run this for the different surface types
    global_null[[s_name]] = vector(mode = "list", length = 13)

    for (k in 2:13) {
        # We need a global test for each buffer width
        global_null[[s_name]][[k]] = data.frame("max_t_stat" = c(),
                                                "prec_loc" = c())

        for (trialNum in 1:100) {
            # This is the repetition to get the null distribution
            
            temp_max = rep(NA, length(trialNum))
            temp_loc = rep(NA, length(trialNum))
            myInd = 1

            load(paste0("../Output/", folder_type[s_name], "/combinedMatchingSetup", trialNum, ".dat"))

            load(paste0('../Output/sim_orig/', save_type[s_name], "/sim_master_", trialNum,".dat"))

            ## Now remove data points where these ratios are much different
            wRatioOk = which(comboInfo[[k]]$ratioArea / comboInfo[[k]]$ratioStreet < 1.4 &
                            comboInfo[[k]]$ratioArea / comboInfo[[k]]$ratioStreet > 1/1.4)
            combinedMatchingSetupFix2 = comboInfo[[k]][wRatioOk,]

            v1 = sd(combinedMatchingSetupFix2$area1 + combinedMatchingSetupFix2$area2, na.rm=TRUE)^2
            v2 = sd(combinedMatchingSetupFix2$ratioArea, na.rm=TRUE)^2

            for(ii in indexList_MAIN) {
                area_temp = sim_master[[k]]$area1[ii] + sim_master[[k]]$area2[ii]
                ratio_temp = max(sim_master[[k]]$area1[ii] / sim_master[[k]]$area2[ii],
                                sim_master[[k]]$area2[ii] / sim_master[[k]]$area1[ii])
                stat_temp = sim_master[[k]]$tStats_area[ii]

                dist_temp = sqrt(((area_temp - (combinedMatchingSetupFix2$area1 + combinedMatchingSetupFix2$area2))^2/v1) +
                                    ((ratio_temp - combinedMatchingSetupFix2$ratioArea)^2 / v2))


                w50 = order(dist_temp)[1:150]

                null_dist = combinedMatchingSetupFix2$tStat_area[w50]
                rand_ind = sample(c(1:150), 1)

                temp_loc[myInd] = ii
                temp_max[myInd] = null_dist[rand_ind]
                myInd = myInd + 1
            }

            global_null[[s_name]][[k]]$max_t_stat[trialNum] = max(temp_max)
            global_null[[s_name]][[k]]$prec_loc[trialNum] = 
                                temp_loc[which(temp_max == max(temp_max))]
        }

    }

}

global_p_val <- vector(mode = "list", length = 3)

for (s_name in 1:3) {

    global_p_val[[s_name]] = rep(NA, 13)

    for(k in 2:13) {
        load(paste0("../Output/sim_orig/"))
    }

}

# ------------------------------------------------------------------------------
# ######################
# ## Global Test Res. ##
# ######################
# {
#   empiricalDistribution_new_2 = vector(mode = "list", length = 13) 
#   {
#     set.seed(1750)
#     #fix a buffer
#     for(i in 2:13) {
#       estimatedDistr = c(1)
#       whereDistr= c(1)
#       print(i)
#       for (k in 1:5000) {
#         estimate = c(0)
#         where = c(0)
#         q = 1
#         for (j in c(1:23, 26, 27, 29, 33:164)) {
          
#           if(!is.na(ALL_PVAL_BY_WIDTH_PAIR[[i*10]][[j]])) {
#             num = length(ALL_P_VALS_NEW[[i]][[j]]$info$mean)
#             myNum = sample(c(1:num), 1)
#             temp = ALL_P_VALS_NEW[[i]][[j]]$info$mean[myNum] / ALL_P_VALS_NEW[[i]][[j]]$info$stdError[myNum]
#             temp = abs(temp)
#             estimate[q] = temp
#             where[q] = j
#             q = q+1
#             #print(paste0("estimate ", estimate, " myNum ", myNum))
#           }
#         }
        
#         estimatedDistr[k] = max(estimate)
#         whereDistr[k] = where[which(estimate == max(estimate))]
        
#       }
#       empiricalDistribution_new_2[[i]] = data.frame("estDistr" = estimatedDistr, "loc" = whereDistr)
#     }
    
#   }

#   pValMeanSE = vector(mode = "list", length = 13)
#   {
#     for (i in seq(20, 130, 10)) {
#       print(i)
#       temp = arimaOffense(i)
      
#       pValMeanSE[[i/10]] = temp
#     }
#   }
  
#   estDist_new_2 = c(-1,1)
#   {
#     for(i in 2:13) {
#       estimate = c(0)
#       where = c(1)
#       q=1
#       for(j in c(1:23, 26, 27, 29, 33:164)) {
#         if(!is.na(pValMeanSE[[i]]$p[j])) {
          
#           temp = pValMeanSE[[i]]$m[j] / pValMeanSE[[i]]$s[j]
#           temp = abs(temp)
#           estimate[q] = temp
#           where[q] = j
#           q = q + 1
#           #print(paste0("estimate ", estimate, " myNum ", myNum))
#         }
#       }
#       #estDist_new_2[i] = max(estimate)
#       #print(estimate)
#       #print(which(estimate == max(estimate)))
#       print(where[which(estimate == max(estimate))])
#     }
#   }
  
#   globalPvals_new_2 = c(-1,1)
#   {
#     for (i in 2:13) {
#       globalPvals_new_2[i] = sum(empiricalDistribution_new_2[[i]]$estDistr > estDist_new_2[i]) / 5000
      
#     }
#     globalPvals_new_2 = globalPvals_new_2[-1]
#     globalPvals_new_2 = data.frame("p" = globalPvals_new_2)
#   }
  
#   empDist_new_2 = data.frame("2" = c(1:5000), "3" = c(1:5000), "4" = c(1:5000), "5" = c(1:5000), "6" = c(1:5000),
#                        "7" = c(1:5000), "8" = c(1:5000), "9" = c(1:5000), "10" = c(1:5000),
#                        "11" = c(1:5000), "12" = c(1:5000), "13" = c(1:5000))
#   for (i in 1:12) {
#     empDist_new_2[,i] = empiricalDistribution_new_2[[i+1]]$estDistr
#   }
#   empDistT_new_2 = data.frame("2" = c(1), "3" = c(1), "4" = c(1), "5" = c(1), "6" = c(1),
#                         "7" = c(1), "8" = c(1), "9" = c(1), "10" = c(1),
#                         "11" = c(1), "12" = c(1), "13" = c(1))
#   for (i in 1:12) {
#     empDistT_new_2[1,i] = estDist_new_2[i+1]
#   }
  
#   ggplot(empDist_new_2, aes(x=empDist_new_2$X4)) + 
#     geom_histogram(color="black", fill="white") + 
#     geom_vline(data=empDistT_new_2, aes(xintercept=empDistT_new_2$X4, color="red"),
#                linetype="dashed")
  
#   ggplot(ALL_P_VALS_NEW_ADJUST[[2]], aes(x=ALL_P_VALS_NEW_ADJUST[[2]]$adjPval)) + 
#     geom_histogram(color="black", fill="white") + 
#     ggtitle("Histogram of Adjusted P-Values at Buffer 200") +
#     xlab("Adjusted P-Values") + 
#     ylab("Frequency") + 
#     geom_vline(data=empDistT, aes(xintercept=empDistT$X12, color="red"),
#                linetype="dashed")
  
#   unadjPVal200 = data.frame("p" = unlist(ALL_PVAL_BY_WIDTH_PAIR[[20]]))
# }