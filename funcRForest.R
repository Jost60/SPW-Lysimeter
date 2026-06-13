#
#Article:		Impacts of groundwater level management on geochemical processes at an agricultural site with shallow groundwater
#Journal:		Environmental Earth Sciences
#Authors:		Jörg Steidl, Ottfried Dietrich, Christoph Merz
#Corresponding: jsteidl@zalf.de
#
### Random Forests #############################################################
analyse_RandomForest_ModelValidation <- function(data, predictor) {
  formula_str <- as.formula(paste("`", predictor, "`", " ~ .", sep = ""))

  set.seed(123)
  trainIndex <- createDataPartition(data[[predictor]], p = 0.7, list = FALSE)
  train_data <- data[trainIndex, ]
  test_data <- data[-trainIndex, ]
  
  rf_model <- randomForest(formula_str, data = train_data, importance=TRUE)
  predictions <- predict(rf_model, newdata = test_data)
 
  return(list(
    predictor=predictor,
    rf_data = data,
    train_data = train_data,
    test_data = test_data,
    rf_model = rf_model,
    predictions = predictions
  ))
}
analyse_RandomForest_Robust <- function(data, predictor) {
  formula_str <- as.formula(paste("`", predictor, "`", " ~ .", sep = ""))
  set.seed(123)
  rf_model <- randomForest::randomForest(as.formula(formula_str), data = data, importance=TRUE, ntree = 500)
  return(list(
    predictor=predictor,
    rf_data = data,
    rf_model = rf_model
  ))
}
Craete_ImpotanceVariables_plot <- function(ImportanceVariables, assessment = "purity",
                                           Title = paste0('Lysimeter '),
                                           xlab = "Increase in node purity", ylab = "Variable")
{

  ImportanceVariables <- as.data.frame(ImportanceVariables)
  rownames(ImportanceVariables) <- gsub("_", " ", rownames(ImportanceVariables))
  if (assessment == "purity") {
        p <- ggplot(ImportanceVariables, aes(x=IncNodePurity, 
                                         y=reorder(rownames(ImportanceVariables), 
                                     IncNodePurity))) 
  }
  else {
    names(ImportanceVariables) <- c("Importance")
    p <- ggplot(ImportanceVariables, aes(x=ImportanceVariables$Importance, #$'%IncMSE', 
                                         y=reorder(rownames(ImportanceVariables), 
                                                   ImportanceVariables$Importance))) 
  }
  p <- p +
    geom_bar(stat='identity') +
    ggtitle(Title) +
    scale_fill_discrete(name="Variable Group") +
    ylab(ylab) +
    xlab(xlab) +
    theme_classic() + 
    theme(plot.title = element_text(size = 10)) +
    theme(axis.text.y = element_text(size = 7)) +
    theme(
      plot.title = element_text(hjust = 0)
    )

  if (ylab == "") {
    p <- p + theme(axis.title.y = element_blank())
  }
  
  print(p)
  return(p)
}
Craete_Partial_Dependence_Plots  <- function(RandomForest, impvar)
{
  cat("\t\tPartialPlots\n")
  RandomForest_Model <- RandomForest$rf_model
  RandomForest_Data <- RandomForest$rf_data
  if ("predictor" %in% names(RandomForest())) {
    RandomForest_Predictor <- RandomForest$predictor
  } else {
    RandomForest_Predictor <- RandomForest$pred
  }

  for(v in impvar)
  {
    vv <- gsub("_", " ", v)
    cat(paste0("\t\t\tRelation: ", RandomForest_Predictor, " vs ", vv, "\n"))
    To.Eval <- paste0("partialPlot(RandomForest_Model, RandomForest_Data, '", v,
                      "', main = '', xlab='", vv,"')")  #, ylab=", element, ", xlab='", v,"')")
    p <- eval(parse(text = To.Eval))
    
    quantiles  <- quantile(RandomForest_Data[[v]], c(0.1, 0.9))
    usr <- par("usr")  # Aktuelle Plot-Grenzen abrufen
    clip(usr[1], usr[2], usr[3], usr[4])
    abline(v = quantiles, col = 'gray', lty = 2)
    do.call("clip", as.list(c(usr[1], usr[2], usr[3], usr[4])))
  }
}  
create_partial_plot <- function(rf_model, rf_data, predictor, variable)
{
  cat(paste0("\t\t\tRelation: ", predictor, " vs ", variable, "\n"))
  To.Eval <- paste0("partialPlot(rf_model, rf_data, '", gsub(" ", "_", variable),
                    "', main = '', xlab='", variable,"')")  #, ylab=", element, ", xlab='", v,"')")
  p <- eval(parse(text = To.Eval))

  variable_ <- gsub(" ", "_", variable)
  quantiles  <- quantile(rf_data[[variable_]], c(0.1, 0.9))
  usr <- par("usr")  # Aktuelle Plot-Grenzen abrufen
  clip(usr[1], usr[2], usr[3], usr[4])
  abline(v = quantiles, col = 'gray', lty = 2)
  do.call("clip", as.list(c(usr[1], usr[2], usr[3], usr[4])))
  return(p)
}

create_partial_plot_grob <- function(rf_model, rf_data, predictor, variable) {
  partial_data <- create_partial_plot(rf_model, rf_data, predictor, gsub("_", " ", variable))
  quantiles  <- quantile(rf_data[[gsub(" ", "_", variable)]], c(0.1, 0.9))
  if (!is.data.frame(partial_data)) {
    partial_data <- data.frame(x = partial_data$x, y = partial_data$y)
  }
  plot <- ggplot(partial_data, aes(x = partial_data[, 1], y = partial_data[, 2])) +
    geom_line(linewidth = ParPlotLineWidth, color = "black") +
    geom_vline(xintercept = quantiles, linetype = "dashed", linewidth = ParPlotLineWidth, color = "gray") +
    labs(x = gsub("_", " ", variable)) 
  if (ParPlotTitle) plot <- plot + ggtitle("------")
  plot <- plot + theme_minimal()
  if (!ParPlotXachseVisible)
    plot <- plot + theme(
      axis.text.x = element_blank(),   # Entfernt die Textmarken der x-Achse
      axis.ticks.x = element_blank(),  # Entfernt die Tick-Marken der x-Achse
    )
  if (!ParPlotYachseVisible)
    plot <- plot + theme(
      axis.title.y = element_blank(),  # Entfernt den Titel der x-Achse
      axis.text.y = element_blank(),   # Entfernt die Textmarken der x-Achse
      axis.ticks.y = element_blank(),  # Entfernt die Tick-Marken der x-Achse
    )
  else 
    plot <- plot + theme(
      axis.title.y = element_blank(),  # Entfernt den Titel der x-Achse
    )
  plot <- plot +
    theme(panel.border = element_rect(color = "gray", fill = NA, linewidth = ParPlotLineWidth),
          panel.grid = element_blank()) #+
  return(plot)
}

create_small_partial_plot_grob <- function(rf_model, rf_data, predictor, variable) {
  
  partial_plot <- create_partial_plot_grob(rf_model, rf_data, predictor, variable)
  gp <- partial_plot +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.25) # Fügt die horizontale Linie bei y = 0 hinzu
  if (variable != "Saturation") {
    gp <- gp + geom_vline(xintercept = 0, color = "black", linewidth = 0.25)  # Fügt die vertikale Linie bei x = 0 hinzu
  }
  gp <- gp +
    labs(title = gsub("\\.", " ", variable), x = "", y = "") +
    theme(
      legend.position = "none",
      axis.line.x = element_blank(),
      axis.line.y = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      plot.title = element_text(size = 11, hjust = 0.5))

  return(gp)
}

Charts_RandomForests_Results <- function(Object, directory, rf, Component,
                                         assessment = "Purity", 
                                         xlab = "Increase in node purity", 
                                         ylab = "Variable")
{
  txt <- strsplit(Object, "_")[[1]]
  if (length(txt) < 2) {
    txt[2] <- c("")
  }
  else {
    txt[1] <- gsub("Ly", "Lysimeter ", txt[1])
    txt[2] <- paste0(" ", txt[2], "cm")
  }


  r_squared_formatted <- paste0(" (r²=", format(round(rf$rf_model$rsq[length(rf$rf_model$rsq)], 2), nsmall = 2),", n=", nrow(rf$rf_data), ")")
  rf$rf_model$finalModel$rsq
  
  Title <- paste0(txt[1], txt[2], ": ", Component, r_squared_formatted)
  print(Title)

  if (assessment == "purity") {
    imp <- importance(rf$rf_model, type=2)
    imp <- as.data.frame(imp)
    impvar <- rownames(imp)[order(imp[, "IncNodePurity"], decreasing=TRUE), drop = FALSE]
  }
  else {
    imp <- importance(rf$rf_model, type=1)
    imp <- as.data.frame(imp)
    impvar <- rownames(imp)[order(imp[, "%IncMSE"], decreasing=TRUE), drop = FALSE]
  }
  pp <- list()
  i=1
  pp[[i]] <- Craete_ImpotanceVariables_plot(imp, assessment = assessment,
                                 Title = Title,
                                 xlab = xlab, ylab = ylab)
  for(v in impvar)
  {
    i = i + 1
    p <- create_partial_plot_grob(rf$rf_model, rf$rf_data, Component, v)
    pp[[i]] <- p 
  }
  return(pp)
}

Charts_RandomForests_Results_IncNodePurity <- function(object, directory, rf,
                       xlab = "Increase in node purity", ylab = "Variable")
{
  element <- strsplit(as.character(rf$predictor), "~", fixed=TRUE)
  if (length(element) > 1)
    element <- element[[2]]
  filename <- paste0(directory, "/", object, "_RandomForest_Purity_", element, ".pdf")
  cat(paste0("\tAusgabe für ", object, " - ", element, " in die Datei '", filename, "'\n"))
  
  cat("\t\tIncrease in node purity\n")
  imp <- importance(rf$rf_model, type=2)
  imp <- as.data.frame(imp)
  impvar <- rownames(imp)[order(imp$IncNodePurity, decreasing=TRUE), drop = FALSE]
  
  pdf(filename)
  Charts_RandomForests_Results(object, directory, rf, impvar, xlab = xlab, ylab = ylab)
  dev.off()
}
Charts_RandomForests_Results_IncMSE <- function(object, directory, rf, element,
                       xlab = "Percentage of increase in mean squared error", ylab = "Variable")
{
  element <- strsplit(as.character(element), "~", fixed=TRUE)
  if (length(element) > 1) 
    element <- element[[2]]  
  filename <- paste0(directory, "/", object, "_RandomForest_MSE_", element, ".pdf")
  cat(paste0("\tAusgabe für ", object, " - ", element, " in die Datei '", filename, "'\n"))

  cat("\t\tPercentage of increase in mean squared error\n")

  imp <- importance(rf$rf_model, type=1) 
  imp <- as.data.frame(imp)
  impvar <- rownames(imp)[order(imp$`%IncMSE`, decreasing=TRUE), drop = FALSE]
  pdf(filename)
  Charts_RandomForests_Results(object, directory, rf, impvar, xlab = xlab, ylab = ylab)
  dev.off()
}
RandomForest <- function(Object, Data, scale = FALSE)
{
  
  cat(paste("*** Random Forest: ", Object, "***********************************\n"))
  #Prüfen auf fehlende Werte:
  nas <- sum(is.na(Data))
  if (nas > 0)
  {
    cat("Die Daten haben ", nas , "Lücken! Es wir kein RF durchgeführt!\n")
    return ()
  }
  folder = paste0(ResultDirectory, Object)
  if (dir.exists(folder) == FALSE) dir.create(folder)
  
  if (scale == TRUE) {
    data_scaled <- as.data.frame(scale(Data))
  } else {
    data_scaled <- Data
  }
  rfs <- list()

  
  rn_data_scaled <- gsub(" ", "_", colnames(data_scaled))
  for(name in names(data_scaled))
  {
    if (grepl(" ", name))
    {
      name_ <- gsub(" ", "_", name)
    }
    else
      name_ <- name
    rf <- analyse_RandomForest_Robust(rn_data_scaled, name_)
    Charts_RandomForests_Results_IncNodePurity(Object, folder, rf, c(name))
    Charts_RandomForests_Results_IncMSE(Object, folder, rf, c(name))
    rfs <- c(rfs, rf)
  }
  return(rfs)
}


CompareLysimeterRF <- function(rfly1, rfly2, rfly3, name, colors = c("dodgerblue3", "dodgerblue", "dodgerblue4"), ytext_size = 16) {
  importance_Ly1 <- if (!is.null(rfly1)) {
    data.frame(Variable = rownames(importance(rfly1)),
               Importance_Ly1 = importance(rfly1, type = 1)[, 1])
  } else NULL

  importance_Ly2 <- if (!is.null(rfly2)) {
    data.frame(Variable = rownames(importance(rfly2)),
               Importance_Ly2 = importance(rfly2, type = 1)[, 1])
  } else NULL

  importance_Ly3 <- if (!is.null(rfly3)) {
    data.frame(Variable = rownames(importance(rfly3)),
               Importance_Ly3 = importance(rfly3, type = 1)[, 1])
  } else NULL

  if (!is.null(importance_Ly1)) importance_Ly1$Variable <- gsub("_", " ", importance_Ly1$Variable)
  if (!is.null(importance_Ly2)) importance_Ly2$Variable <- gsub("_", " ", importance_Ly2$Variable)
  if (!is.null(importance_Ly3)) importance_Ly3$Variable <- gsub("_", " ", importance_Ly3$Variable)
  
  if (is.null(importance_Ly1)) importance_Ly1 <- data.frame(Variable = rownames(importance(rfly2)), Importance_Ly1 = NA)
  if (is.null(importance_Ly2)) importance_Ly2 <- data.frame(Variable = rownames(importance(rfly1)), Importance_Ly2 = NA)
  if (is.null(importance_Ly3)) importance_Ly3 <- data.frame(Variable = rownames(importance(rfly1)), Importance_Ly3 = NA)
  
  importance_df <- merge(importance_Ly1, importance_Ly2, by = "Variable", all = TRUE)
  importance_df <- merge(importance_df, importance_Ly3, by = "Variable", all = TRUE)
  
  importance_long <- melt(importance_df, id.vars = "Variable", 
                          variable.name = "Model", value.name = "Importance")
  
  importance_long <- importance_long %>%
    mutate(Importance = ifelse(Importance < 0, NA, Importance))
  
  importance_long <- importance_long %>%
    group_by(Variable) %>%
    filter(!all(is.na(Importance))) %>%
    ungroup()
  
  importance_long$Model <- recode(importance_long$Model,
                                  Importance_Ly1 = "I",
                                  Importance_Ly2 = "II",
                                  Importance_Ly3 = "III")
  
  importance_long <- importance_long %>%
    arrange(desc(Importance)) %>%
    mutate(Variable = factor(Variable, levels = rev(unique(Variable[!is.na(Importance)]))))
  
  gg <- ggplot(importance_long, aes(x = Variable, y = Importance, fill = Model)) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.7, na.rm = TRUE) + 
    coord_flip() +  # Horizontale Ausrichtung der Balken
    labs(title = name, x = NULL, y = NULL) + 
    scale_fill_manual(values = colors, name = "Lysimeter") +
    theme_classic() +
    theme(
      legend.position = "top",
      legend.key.size = unit(0.5, "cm"),
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.title = element_text(size = 12),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 10)
    ) 
  
  print(gg)
  
  return(gg)
}

CompareLysimeterFeatures <- function(rfly1, rfly2, rfly3, name, colors = c("dodgerblue3", "dodgerblue", "dodgerblue4"), ytext_size = 16) {
  PC <- sub(".*PC([0-9]+).*", "\\1", name)
  
  importance_Ly1 <- if (!is.null(rfly1)) {
    rfly1$final_df %>% 
      select(Object, Feature, Importance) %>% 
      filter(grepl(paste0("Dim.", PC), rownames(rfly1$final_df)))   %>%
      mutate(Feature = gsub("\\.", " ", Feature))
  } else data.frame(Object = character(), Feature = character(), Importance = numeric())
  
  importance_Ly2 <- if (!is.null(rfly2)) {
    rfly2$final_df %>% 
      select(Object, Feature, Importance) %>% 
      filter(grepl(paste0("Dim.", PC), rownames(rfly2$final_df)))   %>%
      mutate(Feature = gsub("\\.", " ", Feature))
  } else data.frame(Object = character(), Feature = character(), Importance = numeric())
  
  importance_Ly3 <- if (!is.null(rfly3)) {
    rfly3$final_df %>% 
      select(Object, Feature, Importance) %>% 
      filter(grepl(paste0("Dim.", PC), rownames(rfly3$final_df)))   %>%
      mutate(Feature = gsub("\\.", " ", Feature))
  } else data.frame(Object = character(), Feature = character(), Importance = numeric())
  
  all_features <- union(union(importance_Ly1$Feature, importance_Ly2$Feature), importance_Ly3$Feature)
  
  importance_Ly1 <- importance_Ly1 %>% 
    right_join(data.frame(Feature = all_features), by = "Feature") %>%
    mutate(Importance = ifelse(is.na(Importance), 0, Importance))
  
  importance_Ly2 <- importance_Ly2 %>% 
    right_join(data.frame(Feature = all_features), by = "Feature") %>%
    mutate(Importance = ifelse(is.na(Importance), 0, Importance))
  
  importance_Ly3 <- importance_Ly3 %>% 
    right_join(data.frame(Feature = all_features), by = "Feature") %>%
    mutate(Importance = ifelse(is.na(Importance), 0, Importance))
  
  importance_data <- bind_rows(
    mutate(importance_Ly1, Model = "I"),
    mutate(importance_Ly2, Model = "II"),
    mutate(importance_Ly3, Model = "III")
  )
  
  
  importance_data_max <- importance_data %>%
    group_by(Feature) %>%
    summarise(MaxImportance = max(Importance)) %>%
    ungroup()
  sorted_features <- importance_data_max$Feature[order(importance_data_max$MaxImportance)]
  
  importance_data <- importance_data %>%
    mutate(Feature = factor(Feature, levels = sorted_features))
  
  gg <- ggplot(importance_data, aes(x = Importance, y = Feature, fill = Model)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +  # Breite der Balken festlegen
    labs(title = name, x = NULL, y = NULL) + 
    theme_minimal() +
    scale_fill_manual(values = colors, name = "Lysimeter") +  # Name für die Legende
    theme_classic() +
    theme(
      legend.position = "top",  
      legend.key.size = unit(0.5, "cm"),
      plot.title = element_text(hjust = 0.5, size = 20),
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 20),      
      axis.title = element_text(size = 20),
      axis.text.y = element_text(size = 18),
      axis.text.x = element_text(size = 18)
    ) 
  return(gg)
}

optimizedRandomForestsModelsRecursiveFeatureElimination <- function(Object, 
                                                                    pca_result, 
                                                                    driver_data, 
                                                                    ResultFolder = folder, 
                                                                    assessment = assessment,
                                                                    plot = TRUE, 
                                                                    scale = scale, 
                                                                    rank = rank, 
                                                                    ntree = 500)  {
  pca <- as.data.frame(pca_result$eig[,2]/100)
  pca[ "pc" ] <- rownames(pca)
  names(pca) <- c("pc.ve", "pc")
  pca$pc <- as.numeric(gsub("[^0-9]", "", pca$pc))
  
  # Extract significant PCs
  eigenvalues <- pca_result$eig
  significant_pcs <- which(eigenvalues[, 1] >= 1)
  pc_columns <- colnames(pca_result$ind$coord)[significant_pcs]
  
  models <- list()
  rf_quality <- NULL
  importances <- list()
  final_results <- list()


  for (pc in pc_columns) {
    target_pc <- pca_result$ind$coord[, pc]
    
    dataset <- data.frame(driver_data, target_pc)
    colnames(dataset)[ncol(dataset)] <- "target_pc"
    
    initial_rf_model <- randomForest(target_pc ~ ., data = dataset, importance = TRUE, ntree = ntree)
    
    importance_result <- importance(initial_rf_model, type = 1)    # 1 - IncMSE, 2 - für Mean Decrease in Gini
    important_features <- rownames(importance_result)[importance_result[, 1] > 0]
    
    important_features <- rownames(importance_result)[!is.na(importance_result[, 1]) & importance_result[, 1] > 0]
    
    if (length(important_features) == 0) {
      print(paste("Keine wichtigen Features für PC", pc))
      next
    }

    filtered_data <- dataset[, c(important_features, "target_pc")]
    set.seed(123)
    train_control <- trainControl(method = "cv", number = 10, returnResamp = "final")
    rf_model <- train(
      target_pc ~ ., data = filtered_data, method = "rf",
      trControl = train_control,
      tuneLength = 5,
      importance = TRUE
    )
    rf_model$predictor <- paste0("PC", gsub("DIM\\.", "", toupper(pc)))
    models[[pc]] <- rf_model
    
    oob_r_squared <- rf_model$finalModel$rsq[length(rf_model$finalModel$rsq)]
    oob_r_squared_formatted <- paste0(" (r²=", format(round(oob_r_squared, 2), nsmall = 2),", n=", nrow(filtered_data), ")")
    
    predictions <- predict(rf_model, newdata = filtered_data)
    manual_r_squared <- 1 - (sum((filtered_data$target_pc - predictions)^2) / 
                               sum((filtered_data$target_pc - mean(filtered_data$target_pc))^2))
    importance_df <- data.frame(
      Feature = rownames(importance_result),
      Importance = importance_result[, 1]
    )
    importances[[pc]] <- importance_df[importance_df$Importance > 0, ]
    
    i <- as.numeric(gsub("DIM\\.", "", toupper(pc)))

    if (!is.null(importances[[pc]]) && nrow(importances[[pc]]) > 0) {
      merged_df <- cbind(
        Object = Object,
        PC = i,
        `PC % Var explained` = round(pca[pca$pc == i, "pc.ve"] * 100, 2),
        `PCA n` = nrow(pca_result$ind$coord),  # Anzahl der Werte in der PCA
        `RF Mean of squared residuals` = round(rf_model$finalModel$mse[length(rf_model$finalModel$mse)], 6),
        `RF trained % Var explained` = round(rf_model$finalModel$rsq[length(rf_model$finalModel$rsq)] * 100, 2),
        `RF predict % Var explained` = round(manual_r_squared * 100, 2),
        `RF n` = nrow(filtered_data),  # Anzahl der Werte im Random Forest Modell
        `RF n train` = nrow(rf_model$resample),  # Anzahl der Instanzen beim Training (Cross-Validation)
        importances[[pc]]
      )
      final_results[[pc]] <- merged_df
    }
  }
  
  if (length(final_results) > 0) {
    final_df <- do.call(rbind, final_results)
  } else {
    final_df <- data.frame()  
  }
  
  return(list(
    models = models,
    importances = importances,
    final_df = final_df  
  ))
}

doRandomForests <- function()
{
  #### RandomForests     ###############################################################
  NumbersOfImportanceVariable <- 15

  RandomForest1_30 <- RandomForest('Ly1_30', ly1_30, scale = TRUE)
  RandomForest2_30 <- RandomForest('Ly2_30', ly2_30, scale = TRUE)
  RandomForest3_30 <- RandomForest('Ly3_30', ly3_30, scale = TRUE)
   
  RandomForest1_60 <- RandomForest('Ly1_60', ly1_60, scale = TRUE)
  RandomForest2_60 <- RandomForest('Ly2_60', ly2_60, scale = TRUE)
  RandomForest3_60 <- RandomForest('Ly3_60', ly3_60, scale = TRUE)
   
  RandomForest1_90 <- RandomForest('Ly1_90', ly1_90, scale = TRUE)
  RandomForest2_90 <- RandomForest('Ly2_90', ly2_90, scale = TRUE)
  RandomForest3_90 <- RandomForest('Ly3_90', ly3_90, scale = TRUE)
}
