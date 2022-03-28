load("../Trial1/sim_orig/p_vals_match_rel/p_val_df_1.dat")
final_hist = p_val_df

for(j in 1:3) {
  for (k in 2:13) {
    final_hist[[j]][[k]] = final_hist[[j]][[k]][15,]
  }
}

for (i in 2:100) {
    load(paste0("../Trial1/sim_orig/p_vals_match_rel/p_val_df_", i, ".dat"))
    for(j in 1:3) {
        for(k in 2:13) {
            final_hist[[j]][[k]] = c(final_hist[[j]][[k]], p_val_df[[j]][[k]][15,])
        }
    }
}

folder_type = c("HotSpot", "Uniform", "Random")

pdf("../Trial1/Plots/pValHistTotal_2.pdf")
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

# ------------------------------------------------------------------------------
args <- commandArgs(TRUE)
buff = as.integer(args[1])

library(rgeos)

load('../Data/indexList_MAIN.RData')
load("../Data/nycSub.RData")
load("../Data/ind_prec_df.rda")
load("../Data/gridWithin_prec.rda")
load("../Data/totalStreetBuffInfo_ORIG.RData")
load('../Output_noWater/sim_orig/Uniform/sim_master_10.dat')

sim_orig = sim_master[[buff]]

plotBorderBufferArr <- function(ind164, buf) {
  
  prec1 = ind_prec_df$prec1[ind164]
  prec2 = ind_prec_df$prec2[ind164]
  
  area1 = totalStreetBuffInfo_ORIG[[2]][[ind164]]$buffer@polygons[[1]]@area / 100000
  area2 = totalStreetBuffInfo_ORIG[[2]][[ind164]]$buffer@polygons[[2]]@area / 100000
  
  tStat = sim_orig$tStats_area[ind164]
  
  
  prec_ind1 = which(nycSub$Precinct == prec1)
  prec_ind2 = which(nycSub$Precinct == prec2)
  nyc_small = nycSub[c(prec_ind1, prec_ind2), ]

  tempOverlap = gIntersection(totalStreetBuffInfo_ORIG[[buf]][[ind164]]$buffer, nyc_small,
                              byid = T)
  
  if(area1 + area2 < 10) {
    xlabel = paste0("*** Relative Area: ", round(area1 + area2, digits = 1),
                    " *** ", " Plot Order: ", length(tempOverlap@plotOrder),
                    "\n tStat = ",
                    round(tStat, digits = 4))
  } else {
    xlabel = paste0("Relative Area: ", round(area1 + area2, digits = 1), 
                    " Plot Order: ", length(tempOverlap@plotOrder),
                    "\n tStat = ",
                    round(tStat, digits = 4))
  }
  
  # Determining which buffer goes with which side
  poly_order = tempOverlap@plotOrder[1:2]
  
  int_1 = gIntersection(tempOverlap[poly_order[1], ], nyc_small[1, ])
  int_2 = gIntersection(tempOverlap[poly_order[1], ], nyc_small[2, ])
  
  if(is.null(int_1)) {
    int_1 = 0
  } else {
    int_1 = int_1@polygons[[1]]@area
  }
  
  if(is.null(int_2)) {
    int_2 = 0
  } else {
    int_2 = int_2@polygons[[1]]@area
  }
  
  if(int_1 < int_2) {
    temp = poly_order[1]
    poly_order[1] = poly_order[2]
    poly_order[2] = temp
  }
  
  plot(tempOverlap, lwd = 0.5, asp = 1, main = paste0("NYC Precincts: ind ", ind164),
       xlab = xlabel, ylab = paste0(round(tempOverlap@polygons[[poly_order[1]]]@area / 10000, 2),
                                    "   ", round(tempOverlap@polygons[[poly_order[2]]]@area / 10000, 2)))
  
  plot(nyc_small[1,], col = "red", add = T)
  plot(nyc_small[2,], col = "blue", add = T)
  plot(tempOverlap, border = "green", add = T)
  
  plot(tempOverlap[poly_order[1], ], col = "red", add = T)
  plot(tempOverlap[poly_order[2], ], col = "blue", add = T)
  
  # plot(totalStreetBuffInfo_ORIG[[buf]][[ind164]]$buffer, border = "red",  add = T)

}


plotOrder_borderLength = data.frame("ind" = rep(NA,164),
                                    "length" = rep(NA, 164))
plotOrder_tStat_area = data.frame("ind" = rep(NA, 164),
                                  "area" = rep(NA, 164))
for(i in indexList_MAIN) {
  plotOrder_borderLength$ind[i] = i
  plotOrder_tStat_area$ind[i] = i
  
  plotOrder_tStat_area$area[i] = sim_orig$tStats_area[i]
  
  area1 = totalStreetBuffInfo_ORIG[[2]][[i]]$buffer@polygons[[1]]@area / 100000
  area2 = totalStreetBuffInfo_ORIG[[2]][[i]]$buffer@polygons[[2]]@area / 100000
  
  plotOrder_borderLength$length[i] = area1 + area2
}

plotOrder_borderLength = na.omit(plotOrder_borderLength[order(plotOrder_borderLength$length), ])
plotOrder_tStat_area = na.omit(plotOrder_tStat_area[order(plotOrder_tStat_area$area, decreasing = T), ])

pdf(paste0("Pictures/obsBorders_tstat_", buff, ".pdf"))
par(mfrow=c(2,2))
for(i in plotOrder_tStat_area$ind) {
  print(i)
  plotBorderBufferArr(i, buff)
}
dev.off()

pdf(paste0("Pictures/obsBorders_borderLength_", buff, ".pdf"))
par(mfrow=c(2,2))
for(i in plotOrder_borderLength$ind) {
  print(i)
  plotBorderBufferArr(i, buff)
}
dev.off()


# 93, 43, 99, 129, 151, 138, 41, 39, 52, 86, 71, 110, 63, 7, 87, 130, 158, 
# 82, 83, 159, 65, 47, 5, 160, 163, 161, 67, 133, 155, 112, 157, 128, 12, 17,
# 147, 134, 156, 140, 58, 162, 164