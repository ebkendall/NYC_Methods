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
# 
# package.install("mvtnorm")
# package.install("rgeos") # has the gBuffer package
# package.install("tidyverse")
# library(RColorBrewer) #aesthetic colors for plots
# library(rgdal)

library(mvtnorm); library(rgeos); library(tidyverse)

load("~/Desktop/Research/UF/2021/NYC_Methods/Simulation/Data/nycSub.RData")
set.seed(100)

#create data frame of grid points
uniform_DF <- makegrid(nycSub, n = 2000000)

#convert grid to spatial coordinates
uniform_DF_SP <- SpatialPoints(uniform_DF,proj4string = CRS(proj4string(nycSub)))

#filter grid to only be within NYC boundaries
gridWithin <- SpatialPixels(uniform_DF_SP[nycSub,])
gridWithin = as(gridWithin, "SpatialPoints")
gridWithin$index = c(1:length(gridWithin))

save(gridWithin, file = "Data/gridWithin.rda")

gridPointValues_length = length(gridWithin)
save(gridPointValues_length, file = "Data/gridPointValues_length.rda")

gridWithin_prec <- vector(mode = "list", length = 77)
for (i in 1:77) {
  print(i)
  temp = point.in.polygon(gridWithin@coords[,1], gridWithin@coords[,2],
                          nycSub@polygons[[i]]@Polygons[[1]]@coords[,1],
                          nycSub@polygons[[i]]@Polygons[[1]]@coords[,2])
  gridWithin_prec[[i]] <- gridWithin[which(temp > 0), ]
}

save(gridWithin_prec, file = "Data/gridWithin_prec.rda")
