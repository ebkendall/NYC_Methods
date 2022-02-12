for (k in 2:13) {

    load(paste0("../Output/null_info/treeData_at_", k, "_1.dat"))
    nullStr_point_data = nullStr_point_data[nullStr_point_data$precinct != -1, ]
    combinedMatchingSetup <- nullStr_point_data

    for(i in 2:77) {
      print(paste0("../Output/null_info/treeData_at_", k, "_", i, ".dat"))
      load(paste0("../Output/null_info/treeData_at_", k, "_", i, ".dat"))
      nullStr_point_data = nullStr_point_data[nullStr_point_data$precinct != -1, ]
      combinedMatchingSetup = rbind(combinedMatchingSetup, nullStr_point_data)
    }

    # Filter out the streets that do not have any streets because those are not relevant
    combinedMatchingSetup = combinedMatchingSetup[combinedMatchingSetup$streets1 != 0, ]
    combinedMatchingSetup = combinedMatchingSetup[combinedMatchingSetup$streets2 != 0, ]
    combinedMatchingSetup = combinedMatchingSetup[!is.na(combinedMatchingSetup$tStat), ]

    wMatchOk = which((combinedMatchingSetup$area1 / combinedMatchingSetup$area2) > 0.5 &
                       (combinedMatchingSetup$area1 / combinedMatchingSetup$area2) < 2 &
                       (combinedMatchingSetup$streets1 / combinedMatchingSetup$streets2) > 0.5 &
                       (combinedMatchingSetup$streets1 / combinedMatchingSetup$streets2) < 2)
    combinedMatchingSetupFix = combinedMatchingSetup[wMatchOk,]

    ## Create ratios of area and streets
    combinedMatchingSetupFix$ratioArea = combinedMatchingSetupFix$area1 /
      combinedMatchingSetupFix$area2
    combinedMatchingSetupFix$ratioArea[which(combinedMatchingSetupFix$ratioArea < 1)] =
      1/combinedMatchingSetupFix$ratioArea[which(combinedMatchingSetupFix$ratioArea < 1)]

    combinedMatchingSetupFix$ratioStreet = combinedMatchingSetupFix$streets1 /
      combinedMatchingSetupFix$streets2
    combinedMatchingSetupFix$ratioStreet[which(combinedMatchingSetupFix$ratioStreet < 1)] =
      1/combinedMatchingSetupFix$ratioStreet[which(combinedMatchingSetupFix$ratioStreet < 1)]

    save(combinedMatchingSetupFix, file = paste0("../Output/combination/combinedMatchingSetup", k, ".dat"))
}
