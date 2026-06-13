#
#Article:		Impacts of groundwater level management on geochemical processes at an agricultural site with shallow groundwater
#Journal:		Environmental Earth Sciences
#Authors:		Jörg Steidl, Ottfried Dietrich, Christoph Merz
#Corresponding: jsteidl@zalf.de
#
## Drivers of Primary component ###############################################
create_PC_drivers_plot <- function(object, directory, rf_model, rf_data, predictor, Component,
                                   assessment = "purity",    
                                   xlab = "Increase in node purity", 
                                   ylab = "Variable", plot = true)
{
  Component <- strsplit(as.character(Component), "~", fixed=TRUE)
  if (length(Component) > 1) 
    element <- Component[[2]]  
  
  if (assessment == "purity") {
    cat("\t\tIncrease in node purity\n")
    filename <- paste0(directory, "/Ly_PrincipalComponent_Driver_", object, "_Purity_", Component, ".pdf")
  }
  else {
    xlab <- "Mean Decrease in Accuracy"
    cat("\t\tMean Decrease in Accuracy\n")
    filename <- paste0(directory, "/Ly_PrincipalComponent_Driver_", object, "_Model_", Component, ".pdf")
  }
  if (!plot)
    cat(paste0("\tAusgabe für ", object, " - ", Component, " in die Datei '", filename, "'\n"))
  else
    cat(paste0("\tAusgabe für ", object, " - ", Component, "\n"))
  if (!plot)
    pdf(filename)
  pp <- list()
  
  rf <- list(
    predictor=predictor,
    rf_model = rf_model,
    rf_data = rf_data
  )

  pp <- Charts_RandomForests_Results(object, directory, rf, Component,
                                           assessment = assessment, xlab = xlab, ylab = ylab)
  return(grid.arrange(grobs = pp, ncol = 1))
}

analyse_PCs_drivers <- function(Object, pca_result, data, ResultFolder = "", assessment = "purity", 
                                plot = TRUE, scale = FALSE, rank = FALSE)
{
  folder = ResultFolder
  
  if (dir.exists(folder) == FALSE) {
    cat("Das Zielverzeichnis ", folder, " existiert nicht!")
    return(list())
  }
  
  rf_models_pcs <- list()
  feature_importances <- list()  
  gts <- list()
  
  pca <- as.data.frame(pca_result$eig[,2]/100)
  pca[ "pc" ] <- rownames(pca)
  names(pca) <- c("pc.ve", "pc")
  pca$pc <- as.numeric(gsub("[^0-9]", "", pca$pc))
  
  rf_quality <- NULL
  rf_base_name <- as.character(Object)
  
  for(i in 1:length(eigen_greater_than_one(pca_result))) {
    pc_data <- pca_result$ind$coord[, i]
    colnames(data) <- gsub(" ", "_", colnames(data))
    data_with_pc <- cbind(data, PC = pc_data)
    
    if (assessment == "purity") {
      rf <- analyse_RandomForest_Robust(data_with_pc, 'PC')
    } else {
      rf <- analyse_RandomForest_ModelValidation(data_with_pc, 'PC')
    }
    
    importance_result <- importance(rf$rf_model, type = 1) 
    importance_df <- data.frame(
      Feature = rownames(importance_result),
      Importance = importance_result[, 1]
    )
    feature_importances[[i]] <- importance_df
    
    rf_quality <- rbind(
      rf_quality, 
      c(
        Object, 
        i, 
        round(pca[pca$pc == i, "pc.ve"]*100, 2), 
        round(rf$rf_model$mse[length(rf$rf_model$mse)], 6), 
        round(rf$rf_model$rsq[length(rf$rf_model$rsq)] * 100, 2)
      )
    )
    
    rf_models_pcs[[i]] <- rf
    assign(paste0("rf_", rf_base_name, "_pc", i), rf, envir = .GlobalEnv)
    
    gt <- create_PC_drivers_plot(
      Object, 
      folder, 
      rf$rf_model, 
      rf$rf_data, 
      rf$predictor, 
      paste0("PC",i), 
      assessment = assessment, 
      ylab = "", 
      plot = plot
    )
    
    gts[[i]] <- gt
  }
  
  rf_quality <- as.data.frame(rf_quality)
  colnames(rf_quality) <- c("Object", "PC", "PC % Var explained", "RF Mean of squared residuals", "RF % Var explained")
  
  rf_models_pcs <<- rf_models_pcs
  
  return(list(
    gts = gts, 
    rf_quality = rf_quality, 
    importances = feature_importances  
  ))
}

plot_PCs_drivers_ <- function(Object, pca_result, data, assessment = "purity",
                              ResultDirectory = "", scale = FALSE, rank = FALSE)
{
  folder = paste0(ResultDirectory, Object)
  if (dir.exists(folder) == FALSE) {
    cat("Das Zielverzeichnis ", folder, " existiert nicht!")
    return(list())
  }
  
  if (dir.exists(folder) == FALSE) 
  {
    cat(paste("\tZielverzeichnis:: ", folder, "wurde angelegt!\n"))
    dir.create(folder)
  }
  else {
    cat("\tZielverzeichnis: ", folder, "\n")
  }

  rfgt <- analyse_PCs_drivers(Object, pca_result, data, ResultFolder = folder, assessment = assessment, 
                             plot = TRUE, scale = scale, rank = rank)

  if (assessment == "purity") Typ <- "Purity"
  else Typ <- "Model"

  if (assessment == "purity") 
    cat("\t\tIncrease in node purity\n")
  else
    cat("\t\tMean Decrease in Accuracy\n")

  for (pc in 1:length(rfgt$importances)) {
    importance_df <- rfgt$importances[[pc]]
    names(importance_df) <- c("Feature", "Importance")
    importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), ]
    
    filename <- paste0(folder, "/", Object, "_FeatureImportance_", pc, ".xlsx")
    cat(paste0("Die Datei '", filename, "' wird geschrieben.\n"))
    
    save_excel(filename, importance_df)
  }
  
  filename <- paste0(folder, "/", Object, "_Drivers_PrincipalComponent_", Typ)
  if (scale == TRUE)
    filename <- paste0(filename, "_Scaled")
  if (rank == TRUE)
    filename <- paste0(filename, "_Ranked")
  filename <- paste0(filename, ".pdf")
  cat(paste0("\tAusgabe für ", Object, " in die Datei '", filename, "'\n"))
  
  pdf(filename, width = 16.5, height = 11.7)
    do.call(grid.arrange, c(rfgt$gts, ncol = length(rfgt$gts)))
  dev.off()
 
  correlations(Object, folder, data, filename = paste0("/", Object, "_correlation RF"))
  
  filename <- paste0(folder, "/", Object, "_Drivers_PrincipalComponent_", Typ)
  if (scale == TRUE)
    filename <- paste0(filename, "_Scaled")
  if (rank == TRUE)
    filename <- paste0(filename, "_Ranked")
  
  for(i in 1:length(rfgt$gts)) {
    png(paste0(filename, "_",i,".png"), width = 8, height = 11.7, units = "in", res = 300)
    grid.arrange(grobs = rfgt$gts[i], ncol = 1)
    dev.off()
  }
  
  return(rfgt$rf_quality)
}

plot_OptimizedPCs_drivers <- function(Object, pca_result, data, assessment = "purity",
                                      ResultDirectory = "", scale = FALSE, rank = FALSE)
{
  folder = paste0(ResultDirectory, Object)
  if (!dir.exists(folder)) {
    cat("Das Zielverzeichnis ", folder, " existiert nicht!")
    return(list())
  }
  
  folderdriver <- paste0(folder,"\\Driver")
  if (!dir.exists(folderdriver)) {
    dir.create(folderdriver)
    return(list())
  }
  
  
  if (!dir.exists(folder)) {
    cat(paste("\tZielverzeichnis:: ", folder, "wurde angelegt!\n"))
    dir.create(folder)
  } else {
    cat("\tZielverzeichnis: ", folder, "\n")
  }

  rfgt <- optimizedRandomForestsModelsRecursiveFeatureElimination(Object, pca_result, data, ResultFolder = folder, assessment = assessment, 
                                                                  plot = TRUE, scale = scale, rank = rank)
  
  a <<- rfgt
  assign(paste0("rf_",Object), a, envir = .GlobalEnv)

  
  Depth = 0
  Lysimeter = 0
  numeric_parts <- unlist(regmatches(Object, gregexpr("\\d+", Object)))
  if (length(numeric_parts)>0) {
    Lysimeter <- as.numeric(numeric_parts[1])
    Depth <- as.numeric(numeric_parts[2])
  }

  # List to store plots for each PC
  all_plots <- list()
  all_small_plots <- list()
  
  for (pc in names(rfgt$importances)) {
    Title <- ""
    if (!is.na(Depth)) {
      if (Depth > 0) {
          Title = paste(Depth,"cm")
      }
    }
    if (Lysimeter == 0) {
      Title = paste0("Lysimeters ", Title)
    } else { 
      Title = paste0("Lysimeter ", Lysimeter, ": ", Title)
    }
    xlab = "Feature importance"
    ylab = ""    
    
    oob_r_squared <- rfgt$models[[pc]]$finalModel$rsq[length(rfgt$models[[pc]]$finalModel$rsq)]
    oob_r_squared_formatted <- paste0(" (r²=", format(round(oob_r_squared, 2), nsmall = 2),", n=", nrow(rfgt$models[[pc]]$trainingData), ")")
    
    Title_small <- paste0(Title, "\nPC",gsub("DIM\\.", "", toupper(pc)), "\n", paste0(" (r²=", format(round(oob_r_squared, 2), nsmall = 2),")"))
    
    Title = paste0(Title, " PC",gsub("DIM\\.", "", toupper(pc)), oob_r_squared_formatted)
    print(Title)
    importance_df <- rfgt$importances[[pc]]
    names(importance_df) <- c("Feature", "Importance")
    importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), ]
    importance_df$Index <- 1:nrow(importance_df)
    
    filename <- paste0(folder, "/", Object, "_Optimized_FeatureImportance_", pc, ".xlsx")
    cat(paste0("Die Datei '", filename, "' wird geschrieben.\n"))
    save_excel(filename, importance_df)

    p <- ggplot(importance_df, aes(x = Importance, y = reorder(Feature, Importance))) +
      geom_bar(stat = 'identity') +
      ggtitle(Title) +
      scale_fill_discrete(name="Variable Group") +
      ylab(ylab) +
      xlab(xlab) +
      theme_classic() + 
      theme(plot.title = element_text(size = 11),  # Größere Titel-Schriftgröße
            axis.title = element_text(size = 10),  # Achsentitel-Schriftgröße
            axis.text.y = element_text(size = 9), # Y-Achsenbeschriftung
            axis.text.x = element_text(size = 9)) +  # X-Achsenbeschriftung)
      theme(
        plot.title = element_text(hjust = 0)
      )
    
    if (ylab == "") {
      p <- p + theme(axis.title.y = element_blank())
    }
    
    # List to store plots for current PC
    pc_plots <- list()
    pc_plots[[1]] <- p
    pc_small_plots <- list()
    pc_small_plots[[1]] <- textGrob(Title_small,
      gp = gpar(fontsize = 13, margin = unit(c(0, 0, 0, 0), "cm")), # Setze alle Margins auf 0
      just = "center" # Zentriere den Text
    )

    for(v in importance_df$Feature) {
      pc_plots[[length(pc_plots) + 1]]  <- create_partial_plot_grob(rfgt$models[[pc]]$finalModel, rfgt$models[[pc]]$trainingData, rfgt$models[[pc]]$predictor, v)
      gp <-  create_small_partial_plot_grob(rfgt$models[[pc]]$finalModel, rfgt$models[[pc]]$trainingData, rfgt$models[[pc]]$predictor, v) 
      pc_small_plots[[length(pc_small_plots) + 1]] <- gp
      
      filename <- paste0(folder, "/Driver/", Object, "_", gsub("DIM\\.", "", toupper(pc)), "_",  gsub("\\.", "_", v))
      if (scale == TRUE)
        filename <- paste0(filename, "_Scaled")
      if (rank == TRUE)
        filename <- paste0(filename, "_Ranked")

      #browser()
      png(paste0(filename, ".png"), width = 1, height = 0.705, units = "in", res = 300)
      plot(gp +
             theme(
               plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),  # Setzt alle Margins auf 0 cm
               axis.title.x = element_blank(),  # Entfernt X-Achsentitel
               axis.title.y = element_blank(),  # Entfernt Y-Achsentitel
               axis.text.x = element_blank(),    # Entfernt X-Achsentexte
               axis.text.y = element_blank(),    # Entfernt Y-Achsentexte
               plot.title = element_blank()
             ))
      dev.off()
    }
    num_plots <- length(pc_plots)
    heights <- c(1/5, rep(4/5 / (num_plots - 1), num_plots - 1))
    all_plots[[length(all_plots) + 1]] <- arrangeGrob(grobs = pc_plots, ncol = 1, heights = heights)
    all_small_plots[[length(all_small_plots) + 1]] <- arrangeGrob(grobs = pc_small_plots, ncol = 1) #, heights = heights)
  }


  if (assessment == "purity") Typ <- "Purity"
  else Typ <- "Model"
  
  if (assessment == "purity") 
    cat("\t\tIncrease in node purity\n")
  else
    cat("\t\tMean Decrease in Accuracy\n")
  
  filename <- paste0(folder, "\\", Object, "_Drivers_Optimized_PCs_", Typ)
  if (scale == TRUE)
    filename <- paste0(filename, "_Scaled")
  if (rank == TRUE)
    filename <- paste0(filename, "_Ranked")
  filename <- paste0(filename, ".pdf")
  cat(paste0("\tAusgabe für ", Object, " in die Datei '", filename, "'\n"))
  
  pdf(filename, width = 16.5, height = 11.7)
  grid.arrange(grobs = all_plots, ncol = length(all_plots))
  dev.off()
  
  while (dev.cur() > 1) {
    dev.off()
  }
  
  filename <- paste0(folder, "\\", Object, "_Drivers_Optimized_PCs_", Typ)
  if (scale == TRUE)
    filename <- paste0(filename, "_Scaled")
  if (rank == TRUE)
    filename <- paste0(filename, "_Ranked")

  for(i in 1:length(all_small_plots)) {
    cat(paste0("\tAusgabe für ", Object, " in die Datei '", paste0(filename, "_",i,".png"), "'\n"))
    png(paste0(filename, "_",i,".png"), width = 2.1, height = 11.7, units = "in", res = 300)
    grid.arrange(grobs = all_small_plots[i], ncol = 1)
    dev.off()
  }
  
  return(rfgt)
}

plot_PCs_drivers <- function(Object, pca_result, data, ResultDirectory = "",
                                   scale = FALSE, rank = FALSE)
{
  rf_qual <- plot_PCs_drivers_(Object, pca_result, data, ResultDirectory, assessment = "purity", scale = scale, rank = scale)
  return(rf_qual)
}

doDriversForPrimaryComponents <- function(withInterpolation = FALSE)
{
  Variables <- DriverVariables[!DriverVariables %in% ExcludeDriverVariables]
  scale <- FALSE
  rank <- FALSE

  ResultDirectory <<- getResultDirectory(withInterpolation, ResultDirectory)
  rf_quality <- NULL
  
  rf_quality <- rbind(rf_quality, plot_PCs_drivers('Ly1_30', pca_ly1_30, ly1_30[, (colnames(ly1_30) %in% Variables)],
                                                    ResultDirectory = ResultDirectory, scale = scale, rank = rank))
  rf_quality <- rbind(rf_quality, plot_PCs_drivers('Ly2_30', pca_ly2_30, ly2_30[, (colnames(ly2_30) %in% Variables)],
                                                    ResultDirectory = ResultDirectory, scale = scale, rank = rank))
  rf_quality <- rbind(rf_quality, plot_PCs_drivers('Ly3_30', pca_ly3_30, ly3_30[, (colnames(ly3_30) %in% Variables)],
                                                    ResultDirectory = ResultDirectory, scale = scale, rank = rank))
  
  rf_quality <- rbind(rf_quality, plot_PCs_drivers('Ly1_60', pca_ly1_60, ly1_60[, (colnames(ly1_60) %in% Variables)],
                                                    ResultDirectory = ResultDirectory, scale = scale, rank = rank))
  rf_quality <- rbind(rf_quality, plot_PCs_drivers('Ly2_60', pca_ly2_60, ly2_60[, (colnames(ly2_60) %in% Variables)],
                                                    ResultDirectory = ResultDirectory, scale = scale, rank = rank))
  rf_quality <- rbind(rf_quality, plot_PCs_drivers('Ly3_60', pca_ly3_60, ly3_60[, (colnames(ly3_60) %in% Variables)],
                                                    ResultDirectory = ResultDirectory, scale = scale, rank = rank))

  GW_Variables <<- Variables[!Variables %in% c("Saturation", "Preasure head", "Groundwater below surface")]
  rf_quality <- rbind(rf_quality, plot_PCs_drivers('Ly1_90', pca_ly1_90, ly1_90[, (colnames(ly1_90) %in% GW_Variables)],
                                                   ResultDirectory = ResultDirectory, scale = scale, rank = rank))
  rf_quality <- rbind(rf_quality, plot_PCs_drivers('Ly2_90', pca_ly2_90, ly2_90[, (colnames(ly2_90) %in% GW_Variables)],
                                                   ResultDirectory = ResultDirectory, scale = scale, rank = rank))
  rf_quality <- rbind(rf_quality, plot_PCs_drivers('Ly3_90', pca_ly3_90, ly3_90[, (colnames(ly3_90) %in% GW_Variables)],
                                                   ResultDirectory = ResultDirectory, scale = scale, rank = rank))
  
  
  exclude <- c("Lower boundary", "Soil moisture", "Saturation", "Soil temperature", "Preasure head")
  GW_Variables <- DriverVariables[!(DriverVariables %in% exclude)]
  
  
  rf_quality$PC <- as.numeric(rf_quality$PC)
  rf_quality$`PC % Var explained` <- as.numeric(rf_quality$`PC % Var explained`)
  rf_quality$`RF Mean of squared residuals` <- as.numeric(rf_quality$`RF Mean of squared residuals`)
  rf_quality$`RF % Var explained` <- as.numeric(rf_quality$`RF % Var explained`)
  
  
  
  filename = paste0(ResultDirectory, "Lysimeters model quality.xlsx")
  save_excel(filename, rf_quality)
  cat(paste0("\tDie Modellergebnisse wurden in die Datei '", filename, "' geschrieben.\n"))  
}

doOptimizedDriversForPrimaryComponents <- function(withInterpolation = FALSE)
{
  Variables <- DriverVariables[!(DriverVariables %in% ExcludeDriverVariables)]
  scale <- FALSE
  rank <- FALSE
  
  
  ResultDirectory <<- getResultDirectory(withInterpolation, ResultDirectory)
  
  rf_quality <- NULL
  
  Variables <- c(Variables, "Mean soil temperature")
  Variables <- Variables[!(Variables %in% c("Pressure head", "Soil temperature"))]

  rf_quality <- rbind(rf_quality, plot_OptimizedPCs_drivers('Lysimeters', pca_ly, ly[, (colnames(ly) %in% Variables)],
                                                     ResultDirectory = ResultDirectory, scale = scale, rank = rank))

  rf_quality <- rbind(rf_quality, plot_OptimizedPCs_drivers('Ly1', pca_ly1, ly1[, (colnames(ly1) %in% Variables)],
                                                            ResultDirectory = ResultDirectory, scale = scale, rank = rank))

  rf_quality <- rbind(rf_quality, plot_OptimizedPCs_drivers('Ly2', pca_ly2, ly2[, (colnames(ly2) %in% Variables)],
                                                            ResultDirectory = ResultDirectory, scale = scale, rank = rank))

  rf_quality <- rbind(rf_quality, plot_OptimizedPCs_drivers('Ly3', pca_ly3, ly3[, (colnames(ly3) %in% Variables)],
                                                            ResultDirectory = ResultDirectory, scale = scale, rank = rank))

  rf_quality <- NULL
  
  Variables <- DriverVariables[!(DriverVariables %in% ExcludeDriverVariables)]
  
  rf_quality <- rbind(rf_quality, plot_OptimizedPCs_drivers('Ly1_30', pca_ly1_30, ly1_30[, (colnames(ly1_30) %in% Variables)],
                                                            ResultDirectory = ResultDirectory, scale = scale, rank = rank))
  rf_quality <- rbind(rf_quality, plot_OptimizedPCs_drivers('Ly2_30', pca_ly2_30, ly2_30[, (colnames(ly2_30) %in% Variables)],
                                                            ResultDirectory = ResultDirectory, scale = scale, rank = rank))
  rf_quality <- rbind(rf_quality, plot_OptimizedPCs_drivers('Ly3_30', pca_ly3_30, ly3_30[, (colnames(ly3_30) %in% Variables)],
                                                            ResultDirectory = ResultDirectory, scale = scale, rank = rank))
  
  
  rf_quality <- rbind(rf_quality, plot_OptimizedPCs_drivers('Ly1_60', pca_ly1_60, ly1_60[, (colnames(ly1_60) %in% Variables)],
                                                            ResultDirectory = ResultDirectory, scale = scale, rank = rank))
  rf_quality <- rbind(rf_quality, plot_OptimizedPCs_drivers('Ly2_60', pca_ly2_60, ly2_60[, (colnames(ly2_60) %in% Variables)],
                                                            ResultDirectory = ResultDirectory, scale = scale, rank = rank))
  rf_quality <- rbind(rf_quality, plot_OptimizedPCs_drivers('Ly3_60', pca_ly3_60, ly3_60[, (colnames(ly3_60) %in% Variables)],
                                                            ResultDirectory = ResultDirectory, scale = scale, rank = rank))
  
  GW_Variables <<- Variables[!Variables %in% c("Saturation", "Preasure head", "Groundwater below surface")]
  rf_quality <- rbind(rf_quality, plot_OptimizedPCs_drivers('Ly1_90', pca_ly1_90, ly1_90[, (colnames(ly1_90) %in% GW_Variables)],
                                                            ResultDirectory = ResultDirectory, scale = scale, rank = rank))
  rf_quality <- rbind(rf_quality, plot_OptimizedPCs_drivers('Ly2_90', pca_ly2_90, ly2_90[, (colnames(ly2_90) %in% GW_Variables)],
                                                            ResultDirectory = ResultDirectory, scale = scale, rank = rank))
  rf_quality <<- rbind(rf_quality, plot_OptimizedPCs_drivers('Ly3_90', pca_ly3_90, ly3_90[, (colnames(ly3_90) %in% GW_Variables)],
                                                             ResultDirectory = ResultDirectory, scale = scale, rank = rank))
  
  
  exclude <- c("Lower boundary", "Soil moisture", "Saturation", "Soil temperature", "Preasure head")
  GW_Variables <- DriverVariables[!(DriverVariables %in% exclude)]
  
  rf_quality <- rbind(rf_quality, plot_OptimizedPCs_drivers('GW', pca_gw, gw[, (colnames(gw) %in% GW_Variables)],
                                                            ResultDirectory = ResultDirectory, scale = scale, rank = rank))

  rf_quality <- rbind(rf_quality, plot_OptimizedPCs_drivers('A', pca_a, aut[, (colnames(aut) %in% Variables)],
                                                             ResultDirectory = ResultDirectory, scale = scale, rank = rank))
}




