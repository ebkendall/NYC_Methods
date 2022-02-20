load("../Data/indexList_MAIN.RData")

args = commandArgs(TRUE)
n_matches = as.numeric(args[1])

save_type = c("HotSpot/", "Uniform/", "Random/") 

# Step 3 -----------------------------------------------------------------------

p_val_df = vector(mode = "list", length = 13)

for(i in 2:13) {p_val_df[[i]] = data.frame("HotSpot" = rep(NA,100),
                                           "Uniform" = rep(NA,100),
                                           "Random"  = rep(NA,100))}

for (trialNum in 1:100) {
    print(trialNum)

    for (s_name in 1:3) {
        
        load(paste0('../Output/sim_orig/', save_type[s_name], "sim_master_", trialNum,".dat"))
        load(paste0('../Output/Global/', save_type[s_name], "global_t_stat_", trialNum,".dat"))
        
        for(k in 2:13) {
            t_stat = max(sim_master[[k]]$tStats_area, na.rm = T)
            loc = which(sim_master[[k]]$tStats_area == t_stat)

            p_val = mean(global_t_stat[[k]]$max_t_stat > t_stat)
            p_val_df[[k]][trialNum, s_name] = p_val
        }

    }
}

pdf(paste0("../Output/Plots/pValGlobal_", n_matches, ".pdf"))
par(mfrow=c(2,3))
for (i in 2:13) {
  for(k in 1:3) {
    print(paste0(i, " ", k))
    pval = p_val_df[[i]][,k]
    hist(pval, breaks = sqrt(length(pval)), main = paste0(save_type[k], ": pVal for B", i*100),
         xlab = paste0("Perc. < 0.05 is ",  round(mean(pval < 0.05, na.rm=TRUE), 4)),
         xlim=c(0,1))
  }
}
dev.off()