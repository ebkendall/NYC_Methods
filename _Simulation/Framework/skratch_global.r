args = commandArgs(TRUE)

n_matches = as.numeric(args[1])
trialNum  = as.numeric(args[2])

save_type = c("HotSpot/", "Uniform/", "Random/") 

pdf("../Output/Plots/global_t_stat_dist_150.pdf")
par(mfrow = c(3,2))
for (s_name in 1:3) {
  
  load(paste0('../Output/sim_orig/', save_type[s_name], "sim_master_", trialNum,".dat"))
  load(paste0('../Output/Global/', save_type[s_name], "global_t_stat_", trialNum,".dat"))
  
  for(k in 2:13) {
    t_stat = max(sim_master[[k]]$tStats_area, na.rm = T)
    hist(global_t_stat[[k]]$max_t_stat, breaks = sqrt(n_matches),
         main = "Histogram of global test stats", 
         xlab = paste0(save_type[s_name], " B ", k * 100),
         ylab = paste0("Obs. test stat: ", round(t_stat, 3)))
    abline(v = t_stat, col = "red")
    
    test = density(global_t_stat[[k]]$max_t_stat, bw = "ucv")
    plot(test, main = "Kernel Dens. of global test stats", 
         xlab = paste0(save_type[s_name], ", B ", k * 100),
         ylab = paste0("Obs. test stat: ", round(t_stat, 3)))
    abline(v = t_stat, col = "red")
  }  
}

dev.off()
