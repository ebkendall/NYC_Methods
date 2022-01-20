load("Data/indexList_MAIN.RData")
temp = vector(mode = "list", length = 3)
dir = c("Results/Flat_area/", "Results/HotSpot_area/", "Results/Random_area/")
for(i in 1:3) {
	temp[[i]] = vector(mode = "list", length = 13)
	for(j in 2:13) {
	  	load(paste0(dir[i], "sim_orig_area_", j, ".dat"))
	  	temp[[i]][[j]] = sim_orig
	}
}

pVal_mat = matrix(nrow = 12, ncol = 3)
for(i in 1:3) {
	for(j in 2:13) {
	  #pVals[,j-1] = temp[[j]]$pVals
	  t = temp[[i]][[j]]$pVals_area[indexList_MAIN]
	  l = length(which(t < 0.05))
	  pVal_mat[j-1,i] = l / length(t)
	}
}
pVal_mat = round(pVal_mat, digits = 3)
print(pVal_mat)


pdf("simulationHistograms_area.pdf")
label = c("; Flat Surface", "; HotSpots", "; Random Noise")
par(mfrow=c(2, 3))
for(i in 2:13) {
	for(j in 1:3) {
		title = paste0("B: ", i*100, label[j])
		subtitle = paste0("Proportion < 0.05: ", pVal_mat[i-1,j])

		hist(temp[[j]][[i]]$pVals_area[indexList_MAIN],
			main = title,
			sub = subtitle,
			xlab = "p-values",
			col.sub = "red")
	}
}
dev.off()
