library("sp")
library("sf")
library("rgeos")
library("raster")

match_count <- seq(10, 500, by = 10)
load("../Data/indexList_MAIN.RData")

set.seed(10)

## --------------------------- Plot --------------------------------------------
perc_pval_match <- vector(mode = "list", length = 13)

p_val_df <- vector(mode = "list", length = 13)

for (k in 2:13) {

    print(k)

    load(paste0("../Output/combination/combinedMatchingSetup", k, ".dat"))

    Dir = paste0('../Output/combination/')

    load(paste0(Dir, "sim_orig_", k,".dat"))

    ## Now remove data points where these ratios are much different
    wRatioOk = which(combinedMatchingSetupFix$ratioArea / combinedMatchingSetupFix$ratioStreet < 1.4 &
                       combinedMatchingSetupFix$ratioArea / combinedMatchingSetupFix$ratioStreet > 1/1.4)
    combinedMatchingSetupFix2 = combinedMatchingSetupFix[wRatioOk,]

    v1 = sd(combinedMatchingSetupFix2$area1 + combinedMatchingSetupFix2$area2, na.rm=TRUE)^2
    v2 = sd(combinedMatchingSetupFix2$ratioArea, na.rm=TRUE)^2

    row_num = 1
    perc_pval_match[[k]] = data.frame("num_match" = match_count,
                                      "perc_pval_less_05" = rep(NA, length(match_count)))
    p_val_df[[k]] = matrix(nrow = length(match_count), ncol = nrow(sim_orig))

    for(j in match_count) {
      print(j)
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


          w50 = order(dist_temp)[1:j]

          null_dist = combinedMatchingSetupFix2$tStat_area[w50]
          pval[ii] = mean(null_dist > stat_temp)
        }
      }

      perc_pval = mean(pval < 0.05, na.rm=TRUE)
      perc_pval_match[[k]]$perc_pval_less_05[row_num] = perc_pval
      p_val_df[[k]][row_num, ] = pval
      row_num = row_num + 1
    }
}

save(p_val_df, file = paste0("../Output/combination/p_val_df.dat"))
save(perc_pval_match, file = paste0("../Output/combination/perc_pval_match.dat"))

# ---------------------------------------------------------------
# ------- Plotting everything
# ---------------------------------------------------------------

load("../Output/combination/perc_pval_match.dat")
final_plot = perc_pval_match
plot_mat = perc_pval_match

pdf('../Output/Plots/pVal_num_match.pdf')
par(mfrow=c(3,1))
for (i in 2:13) {
    print(i)
    pval = final_plot[[i]]
    temp = plot_mat[[i]]
    # print(t(temp))
    plot(pval$num_match, pval$perc_pval_less_05, main = paste0("pVal for B", i*100),
         xaxt="none", xlab = "Perc. < 0.05 is ")
    axis(1, seq(10,500,20), las=2)
    abline(h=0.05, col = "red")
    lines(temp[,1], temp[,2], col = "purple", lwd = 2)
}
dev.off()

# pdf('../Output/Plots/pVal_num_match2.pdf')
# par(mfrow=c(3,1))
# for (i in 2:13) {
#   for(k in 1:3) {
#     pval = plot_mat[[k]][[i]]
#     plot(pval[,1], pval[,2], main = paste0(folder_type[k], ": pVal for B", i*100),
#          xaxt="none", xlab = "Perc. < 0.05 is ")
#     axis(1, seq(10,500,20), las=2)
#     for(w in 1:100) {
#         abline(lm(pval[,w+1] ~ pval[,1]), col = w)
#         # lines(pval[,1], pval[,w+1], col = w)
#     }
#   }
# }
# dev.off()
