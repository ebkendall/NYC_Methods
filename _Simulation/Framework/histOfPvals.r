load("../Output/sim_orig/p_val_df_1.dat")
final_hist = p_val_df

for (i in 1:100) {
    load(paste0("../Output/sim_orig/p_val_df_", i, ".dat"))
    for(j in 1:3) {
        for(k in 2:13) {
            final_hist[[j]][[k]] = c(final_hist[[j]][[k]], p_val_df[[j]][[k]])
        }
    }
}

folder_type = c("HotSpot", "Uniform", "Random")

pdf("../Output/Plots/pValHistTotal.pdf")
par(mfrow=c(2,3))
for (i in 2:13) {
  for(k in 1:3) {
    pval = final_hist[[k]][[i]]
    hist(pval, main = paste0(folder_type[k], ": pVal for B", i*100),
         xlab = paste0("Perc. < 0.05 is ",  round(mean(pval < 0.05, na.rm=TRUE), 4)),
         xlim=c(0,1))
  }
}
dev.off()
