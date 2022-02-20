load("../Data/indexList_MAIN.RData")

surface_type = c("hotspot", "uniform", "cov_r")
save_type = c("HotSpot/", "Uniform/", "Random/")
folder_type <- c("HotSpot_combine", "Uniform_combine", "Random_combine")

n_matches = 50
trialNum = as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID')) # 1-100
set.seed(trialNum)

# Step Outline -----------------------------------------------------------------
# Step 1: save all the matches in the global_null list for each surface
# Step 2: iterate through and grab the max
# (next steps are in ...h_globalPlots.r)
# Step 3: get a p-value for each surface and each iterate for each buffer width
# Step 4: look at the distribution of p-values across all 100 iterates
# ------------------------------------------------------------------------------

# Step 1 -----------------------------------------------------------------------

for (s_name in 1:3) {
    # Run this for the different surface types
    global_null = vector(mode = "list", length = 13)

    load(paste0("../Output/", folder_type[s_name], "/combinedMatchingSetup", trialNum, ".dat"))

    load(paste0('../Output/sim_orig/', save_type[s_name], "/sim_master_", trialNum,".dat"))

    for (k in 2:13) {
        # We need a global test for each buffer width
        print(paste0(s_name, " ", k))

        global_null[[k]] = matrix(nrow = max(indexList_MAIN), ncol = n_matches)

        ## Now remove data points where these ratios are much different
        # wRatioOk = which(comboInfo[[k]]$ratioArea / comboInfo[[k]]$ratioStreet < 1.4 &
        #                 comboInfo[[k]]$ratioArea / comboInfo[[k]]$ratioStreet > 1/1.4)
        # combinedMatchingSetupFix2 = comboInfo[[k]][wRatioOk,]
        combinedMatchingSetupFix2 = comboInfo[[k]]

        v1 = sd(combinedMatchingSetupFix2$area1 + combinedMatchingSetupFix2$area2, na.rm=TRUE)^2
        v2 = sd(combinedMatchingSetupFix2$ratioArea, na.rm=TRUE)^2

        for(ii in indexList_MAIN) {
            area_temp = sim_master[[k]]$area1[ii] + sim_master[[k]]$area2[ii]
            ratio_temp = max(sim_master[[k]]$area1[ii] / sim_master[[k]]$area2[ii],
                            sim_master[[k]]$area2[ii] / sim_master[[k]]$area1[ii])
            stat_temp = sim_master[[k]]$tStats_area[ii]

            dist_temp = sqrt(((area_temp - (combinedMatchingSetupFix2$area1 + combinedMatchingSetupFix2$area2))^2/v1) +
                                ((ratio_temp - combinedMatchingSetupFix2$ratioArea)^2 / v2))

            w50 = order(dist_temp)[1:n_matches]

            null_dist = combinedMatchingSetupFix2$tStat_area[w50]

            global_null[[k]][ii,] = null_dist
        }

    }

    save(global_null, file = paste0("../Output/Global/", save_type[s_name],
                                    "global_null_", trialNum, ".dat"))
}

# Step 2 -----------------------------------------------------------------------

for (s_name in 1:3) {

    load(paste0("../Output/Global/", save_type[s_name], "global_null_", trialNum, ".dat"))
    global_t_stat <- vector(mode = "list", length = 13)

    for(k in 2:13) {

        print(k)
        global_t_stat[[k]] = data.frame("max_t_stat" = rep(NA, n_matches),
                                        "max_loc" = rep(NA, n_matches))
        
        for (rep in 1:n_matches) {
        # This is the repetition to get the null distribution
            temp_loc = temp_max = c()
            myInd = 1
            for(ii in indexList_MAIN) {
                rand_ind = sample(c(1:n_matches), 1)

                temp_loc[myInd] = ii
                temp_max[myInd] = global_null[[k]][ii, rand_ind]
                myInd = myInd + 1
            }
            print(paste0(max(temp_max, na.rm = T), " ", which(temp_max == max(temp_max, na.rm = T))))
            global_t_stat[[k]][rep, 1] = max(temp_max, na.rm = T)
            global_t_stat[[k]][rep, 2] = 
                                temp_loc[which(temp_max == max(temp_max, na.rm = T))[1]]
        }

    }

    save(global_t_stat, file = paste0("../Output/Global/", save_type[s_name],
                                    "global_t_stat_", trialNum, ".dat"))
}