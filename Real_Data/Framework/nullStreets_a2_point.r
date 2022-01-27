##############################
#### LOADING LIB AND DATA ####
##############################
package.install = function(pack) {
  local({r <- getOption("repos");r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)})

  # name of package to install / load
  pack = pack

  if (pack %in% rownames(installed.packages())) {
    library(pack, character.only=T)
  } else {
    if (pack %in% rownames(installed.packages(lib.loc='/blue/jantonelli/emmett.kendall/Packages/R_4_0'))) {
      library(pack, lib.loc='/blue/jantonelli/emmett.kendall/Packages/R_4_0', character.only=T)
    } else {
      install.packages(pack, lib='/blue/jantonelli/emmett.kendall/Packages/R_4_0')
      library(pack, lib.loc='/blue/jantonelli/emmett.kendall/Packages/R_4_0', character.only=T)
    }
  }
}

package.install("sp")
package.install("sf")
package.install("rgeos")
package.install("raster")

load("gridWithin_prec.rda")

for (index in 2:13) {

  k = as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID')) # 1-77
  load(paste0("OutputStrInfo/strInfo_", index, "_", k, ".dat")) # contains the buffer object

  print(paste0("Total length: ", length(streetLengthInfo_null)))
  for(i in 1:length(streetLengthInfo_null)) {
    print(paste0("i ", i))
    for(j in 1:length(streetLengthInfo_null[[i]])) {
      if(!is.null(streetLengthInfo_null[[i]][[j]])) {
        if(length(streetLengthInfo_null[[i]][[j]]$buffer@polygons) > 1) {
          poly1 = streetLengthInfo_null[[i]][[j]]$buffer@polygons[[1]]
          poly2 = streetLengthInfo_null[[i]][[j]]$buffer@polygons[[2]]

          p1 = point.in.polygon(gridWithin_prec[[k]]@coords[,1], gridWithin_prec[[k]]@coords[,2],
                                poly1@Polygons[[1]]@coords[,1], poly1@Polygons[[1]]@coords[,2])
          p2 = point.in.polygon(gridWithin_prec[[k]]@coords[,1], gridWithin_prec[[k]]@coords[,2],
                                poly2@Polygons[[1]]@coords[,1], poly2@Polygons[[1]]@coords[,2])
          ind1 <- which(p1 > 0) # now I need to access their "count values"
          ind2 <- which(p2 > 0) # now I need to access their "count values"

          gridVals_ind_1 = gridWithin_prec[[k]]$index[ind1]
          gridVals_ind_2 = gridWithin_prec[[k]]$index[ind2]

          streetLengthInfo_null[[i]][[j]]$pointIndex <- list("index1" = gridVals_ind_1,
                                                             "index2" = gridVals_ind_2)

        } else {
            # indicates and improper split
            streetLengthInfo_null[[i]][[j]]$pointIndex <- NA
        }
      }
    }
  }

  print(paste0("Finished strInfo_", index, "_", k, ".dat"))
  save(streetLengthInfo_null, file=paste0("OutputStrInfo_new/strInfo_", index, "_", k, ".dat", sep=''))

}
