library("sp")
library("sf")
library("rgeos")
library("raster")

surface_type = c("hotspot", "uniform", "cov_r")
save_type = c("HotSpot/", "Uniform/", "Random/")
folder_type <- c("HotSpot_combine", "Uniform_combine", "Random_combine")

match_count <- seq(10, 500, by = 10)

# trialNum = as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID')) # 1-100
# set.seed(trialNum)
load("../Data/indexList_MAIN.RData")

## --------------------------- Plot --------------------------------------------
# perc_pval_match <- vector(mode = "list", length = 3) # Order: HotSpot, Uniform, Random
# perc_pval_match[[1]] = perc_pval_match[[2]] = perc_pval_match[[3]] = vector(mode = "list", length = 13)

# p_val_df <- vector(mode = "list", length = 3) # Order: HotSpot, Uniform, Random
# p_val_df[[1]] = p_val_df[[2]] = p_val_df[[3]] = vector(mode = "list", length = 13)


# for (k in 2:13) {

#   print(k)

#   for (s_name in 1:3) {

#     load(paste0("../Output/", folder_type[s_name], "/combinedMatchingSetup", trialNum, ".dat"))

#     Dir = paste0('../Output/sim_orig/', save_type[s_name])
#     print(Dir)

#     load(paste0(Dir, "/sim_master_", trialNum,".dat"))

#     ## Now remove data points where these ratios are much different
#     # wRatioOk = which(comboInfo[[k]]$ratioArea / comboInfo[[k]]$ratioStreet < 1.4 &
#     #                    comboInfo[[k]]$ratioArea / comboInfo[[k]]$ratioStreet > 1/1.4)
#     # combinedMatchingSetupFix2 = comboInfo[[k]][wRatioOk,]

#     combinedMatchingSetupFix2 = comboInfo[[k]]
#     v1 = sd(combinedMatchingSetupFix2$area1 + combinedMatchingSetupFix2$area2, na.rm=TRUE)^2
#     v2 = sd(combinedMatchingSetupFix2$ratioArea, na.rm=TRUE)^2

#     row_num = 1
#     perc_pval_match[[s_name]][[k]] = data.frame("num_match" = match_count,
#                                                 "perc_pval_less_05" = rep(NA, length(match_count)))
#     p_val_df[[s_name]][[k]] = matrix(nrow = length(match_count), ncol = nrow(sim_master[[k]]))

#     for(j in match_count) {
#       print(j)
#       pval = rep(NA, nrow(sim_master[[k]]))

#       for (ii in 1 : nrow(sim_master[[k]])) {
#         if (ii %in% indexList_MAIN) {
#           ## find matches
#           area_temp = sim_master[[k]]$area1[ii] + sim_master[[k]]$area2[ii]
#           ratio_temp = max(sim_master[[k]]$area1[ii] / sim_master[[k]]$area2[ii],
#                            sim_master[[k]]$area2[ii] / sim_master[[k]]$area1[ii])
#           stat_temp = sim_master[[k]]$tStats_area[ii]

#           dist_temp = sqrt(((area_temp - (combinedMatchingSetupFix2$area1 + combinedMatchingSetupFix2$area2))^2/v1) +
#                              ((ratio_temp - combinedMatchingSetupFix2$ratioArea)^2 / v2))


#           w50 = order(dist_temp)[1:j]

#           null_dist = combinedMatchingSetupFix2$tStat_area[w50]

#           test = density(null_dist, bw = "ucv")
#           xx = test$x
#           yy = test$y
#           dx <- xx[2L] - xx[1L]
#           C <- sum(yy) * dx
          
#           p.unscaled <- sum(yy[xx >= stat_temp]) * dx
#           p.scaled <- p.unscaled / C
          
          
#           pval[ii] = p.scaled
#         }
#       }

#       perc_pval = mean(pval < 0.05, na.rm=TRUE)
#       perc_pval_match[[s_name]][[k]]$perc_pval_less_05[row_num] = perc_pval
#       p_val_df[[s_name]][[k]][row_num, ] = pval
#       row_num = row_num + 1
#     }
#   }
# }

# save(p_val_df, file = paste0("../Output/sim_orig/p_vals_match_rel/p_val_df_", trialNum, ".dat"))
# save(perc_pval_match, file = paste0("../Output/sim_orig/p_vals_match_rel/perc_pval_match_", trialNum, ".dat"))

# ---------------------------------------------------------------
# ------- Plotting everything
# ---------------------------------------------------------------

load("../Output/sim_orig/p_vals_match_rel/perc_pval_match_1.dat")
final_plot = perc_pval_match
plot_mat = perc_pval_match

for (i in 2:100) {
    load(paste0("../Output/sim_orig/p_vals_match_rel/perc_pval_match_", i, ".dat"))
    for(j in 1:3) {
        for(k in 2:13) {
            final_plot[[j]][[k]] = rbind(final_plot[[j]][[k]], perc_pval_match[[j]][[k]])
            plot_mat[[j]][[k]] = cbind(plot_mat[[j]][[k]], perc_pval_match[[j]][[k]]$perc_pval_less_05)
        }
    }
}

folder_type = c("HotSpot", "Uniform", "Random")

pdf('../Output/Plots/pVal_num_match_kernel_moreFilt.pdf')
par(mfrow=c(3,1))
for (i in 2:13) {
  for(k in 1:3) {
    print(paste0(k, " ", i))
    pval = final_plot[[k]][[i]]
    temp = cbind(plot_mat[[k]][[i]][,1], rowMeans(plot_mat[[k]][[i]][,-1]))
    # print(t(temp))
    plot(pval$num_match, pval$perc_pval_less_05, main = paste0(folder_type[k], ": pVal for B", i*100),
         xaxt="none", xlab = "Perc. < 0.05 is ")
    axis(1, seq(10,500,20), las=2)
    abline(h=0.05, col = "red")
    lines(temp[,1], temp[,2], col = "purple", lwd = 2)
  }
}
dev.off()

pdf('../Output/Plots/pVal_num_match_kernel_moreFilt_2.pdf')
par(mfrow=c(3,1))
for (i in 2:13) {
  for(k in 1:3) {
    pval = plot_mat[[k]][[i]]
    plot(pval[,1], pval[,2], main = paste0(folder_type[k], ": pVal for B", i*100),
         xaxt="none", xlab = "Perc. < 0.05 is ")
    axis(1, seq(10,500,20), las=2)
    for(w in 1:100) {
        abline(lm(pval[,w+1] ~ pval[,1]), col = w)
        # lines(pval[,1], pval[,w+1], col = w)
    }
  }
}
dev.off()
