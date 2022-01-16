# TRENDS FROM REAL DATA --------------------------------------------------------------------------------

# Test runs of the function ----------------------------------------------------------------------------
resultsHotSpot_orig = vector(mode = "list", length = 8)
resultsRandom_orig = vector(mode = "list", length = 8)    
resultsCorr_orig = vector(mode = "list", length = 8)    
resultsFlat_orig = vector(mode = "list", length = 8)    
gridPointValues_flat <- rep(10, 35130)
flat_test = simulationTest(plottingDF, gridPointValues_flat, 5)

for(i in 2:8) {
  # resultsHotSpot_orig[[i]] = simulationTest(plottingDF, gridPointValues, i)
  # print(paste0("a ", i))
  # resultsRandom_orig[[i]] = simulationTest(plottingDF, gridPointValues_cov_r, i)
  # print(paste0("b ", i))
  # resultsCorr_orig[[i]] = simulationTest(coordDF, gridPointValues_cov_c, i)
  # print(paste0("c ", i))
  
}    

pValsHotSpot_orig <- matrix(data = NA, nrow = 164, ncol = 8)
pValsRandom_orig <- matrix(data = NA, nrow = 164, ncol = 8)
pValsCorr_orig <- matrix(data = NA, nrow = 164, ncol = 8)
for(i in 2:8) {
  for(j in 1:164) {
    if(!is.na(resultsHotSpot_orig[[i]][[j]])) {
      pValsHotSpot_orig[j, i] = resultsHotSpot_orig[[i]][[j]]$pValue
    }
    if(!is.na(resultsRandom_orig[[i]][[j]])) {
      pValsRandom_orig[j, i] = resultsRandom_orig[[i]][[j]]$pValue
    }
    if(!is.na(resultsCorr_orig[[i]][[j]])) {
      pValsCorr_orig[j, i] = resultsCorr_orig[[i]][[j]]$pValue
    }
  }
}

hist(pValsRandom_orig[,3])


pValFlatTest = c(1)
for(i in 1:164) {
  if(!is.na(flat_test[[i]])) {
    pValFlatTest[i] = flat_test[[i]]$pValue
  } else {pValFlatTest[i] = NA}
}



# --------------------------- Correlated ---------------------------
# assign correlation to the uniform_DF
# we have to make the grid less dense 
rmnvAnder <-  function(mean,sigma){
  drop(mean + chol(sigma)%*%rnorm(length(mean)))
}
#create data frame of grid points        1000000
uniform_DF_cov_c <- makegrid(nycSub, n = 20000)

#convert grid to spatial coordinates
uniform_DF_SP_cov_c <- SpatialPoints(uniform_DF_cov_c,proj4string = CRS(proj4string(nycSub)))

#filter grid to only be within NYC boundaries
gridWithin_cov_c <- SpatialPixels(uniform_DF_SP_cov_c[nycSub,])
gridWithin_cov_c = as(gridWithin_cov_c, "SpatialPoints")

coordDF <- data.frame(Longitude = gridWithin_cov_c@coords[,1], Latitude = gridWithin_cov_c@coords[,2])
corrStructure <- dist(coordDF)
corrStructure <- as.matrix(corrStructure)
rho = 2500
corrStructure <- -1 * corrStructure / rho
corrStructure <- exp(corrStructure)

#spatialCorr = cov.sp(coords=as.matrix(coordDF), sp.type="matern", sp.par=c(2,2), smoothness=10)

gridPointValues_cov_c = rmnvAnder(rep(0, nrow(coordDF)), corrStructure) # takes 1 minute to produce

#forcing everything to be positive
gridPointValues_cov_c = gridPointValues_cov_c + abs(min(gridPointValues_cov_c))
gridPointValues_cov_c = as.numeric(round(gridPointValues_cov_c))
