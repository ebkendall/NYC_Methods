##############################
#### LOADING LIB AND DATA ####
##############################
# package.install = function(pack) {
#   local({r <- getOption("repos");r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)})
#
#   # name of package to install / load
#   pack = pack
#
#   if (pack %in% rownames(installed.packages())) {
#     library(pack, character.only=T)
#   } else {
#     if (pack %in% rownames(installed.packages(lib.loc='/blue/jantonelli/emmett.kendall/Packages/R_4_0'))) {
#       library(pack, lib.loc='/blue/jantonelli/emmett.kendall/Packages/R_4_0', character.only=T)
#     } else {
#       install.packages(pack, lib='/blue/jantonelli/emmett.kendall/Packages/R_4_0')
#       library(pack, lib.loc='/blue/jantonelli/emmett.kendall/Packages/R_4_0', character.only=T)
#     }
#   }
# }

# package.install("mvtnorm")
# package.install("rgeos") # has the gBuffer package
# package.install("tidyverse")

library(mvtnorm); library(rgeos); library(tidyverse)

load("../Data/gridPointValues_length.rda") # gridPointValues_length
load("../Data/gridWithin.rda")             # gridWithin

# k = as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID')) # 1-100
for (k in 1:100) {
    print(k)
    set.seed(k)

    # initialize grid point values to 1
    # the index here corresponds to the index from the gridWithin_prec index
    gridPointValues_uniform = gridPointValues_hotspot = rep(1, gridPointValues_length)

    # Flat   -----------------------------------------------------------------------
    save(gridPointValues_uniform, file = paste0("../Data/Surfaces/gridPointValues_uniform_", k, ".rda"))

    # HOT SPOT BIAS ----------------------------------------------------------------
    hotSpotCenters = sample(c(1:gridPointValues_length), 200) #choosing 200 hotspot centers

    #create buffer around hotspots
    hotSpotPolys = vector(mode = "list", length = length(hotSpotCenters))
    for (i in 1:length(hotSpotCenters)) {
      ind = hotSpotCenters[i]
      hotSpotPolys[[i]] = gBuffer(gridWithin[ind,], byid = T, width = 4000)
    }

    #adjust weight of each coordinate point
    for (i in 1:length(hotSpotCenters)) {
      hotSpotAffect = point.in.polygon(gridWithin@coords[,1], gridWithin@coords[,2],
                                       hotSpotPolys[[i]]@polygons[[1]]@Polygons[[1]]@coords[,1],
                                       hotSpotPolys[[i]]@polygons[[1]]@Polygons[[1]]@coords[,2])
      gridPointValues_hotspot[which(hotSpotAffect > 0)] = gridPointValues_hotspot[which(hotSpotAffect > 0)] + 1
    }

    save(gridPointValues_hotspot, file = paste0("../Data/Surfaces/gridPointValues_hotspot_", k, ".rda"))

    # RANDOM -----------------------------------------------------------------------
    # randomly giving each grid point a value
    gridPointValues_cov_r = round(runif(gridPointValues_length) * 10)

    save(gridPointValues_cov_r, file = paste0("../Data/Surfaces/gridPointValues_cov_r_", k, ".rda"))

}
