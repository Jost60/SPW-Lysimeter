#
#Article:		Impacts of groundwater level management on geochemical processes at an agricultural site with shallow groundwater
#Journal:		Environmental Earth Sciences
#Authors:		Jörg Steidl, Ottfried Dietrich, Christoph Merz
#Corresponding: jsteidl@zalf.de
#
### Primary component analysis #################################################
eigen_greater_than_one <- function(pca)
{
  return(
    pca$eig[1:length(pca$eig[,1][pca$eig[,1]   > 1])]  
  )
}
chart_onePC_Loadings <- function(object, folder, pca, pc, with_Y_AxisText = FALSE, 
                                xlab = "Corr. coeff.",
                                ylab = "Variable",
                                axistextsize = NULL,
                                axistitlesize = NULL,
                                axistitleface = NULL,
                                explainedVarianz = FALSE)
{
   load <- pca$rotation
   if (is.null(load))
     load <- pca$var$coord
   sorted.loadings <- load[order(load[, pc]), pc]
   
   myTitle <- "Loadings Plot for PC1"
   myXlab  <- "Variable Loadings"
   df <- as.data.frame(sorted.loadings)
   df[ "var" ] <- rownames(df)
   
   save_excel(paste0(folder, "/", object, "_Loadings_", pc, ".xlsx"), df[order(-df$sorted.loadings), ])

   
   Title <- paste0("PC", pc)
   if (explainedVarianz) {
     mydf <- as.data.frame(round(pca$eig[,2], 2))
     mydf[ "PC" ] <- rownames(mydf)
     names(mydf) <- c("% Var explained", "PC")
     mydf$PC <- as.numeric(gsub("[^0-9]", "", mydf$PC))
     Title <- paste0("PC", pc, " explained variance = ", round(mydf[mydf$PC == pc, "% Var explained"], 2), "%, n = ", nrow(pca$ind$coord))
   }
 
 
  gp <-  ggplot(df, aes(x = sorted.loadings, y = var, fill = factor(sorted.loadings))) +
     geom_bar(stat = "identity") + # "steelblue") +
     scale_fill_grey() +
     geom_vline(xintercept = 0, color = "black", size = 0.5, linetype = "dashed") +
     labs(title = Title, x = xlab, y = ylab) +
     theme_minimal() +
     theme(legend.position = "none",
           axis.line.x = element_line(color = "black", linewidth = 0.5),
           axis.ticks.x = element_line(color = "black", linewidth = 0.5),
           panel.grid = element_blank()) +
     scale_x_continuous(breaks = c(-1, 0, 1), limits = c(-1, 1), expand = c(0, 0))
 
 if (!is.null(axistextsize))
   gp <- gp + theme(axis.text = element_text(size=axistextsize))
 if (!is.null(axistitlesize))
   gp <- gp + theme(axis.title = element_text(size=axistitlesize))
 if (!is.null(axistitleface))
 {
   gp <- gp + theme(axis.title = element_text(face=axistitleface)) #+
 }
 if (pc > 1) {
   if (with_Y_AxisText == FALSE) {
   gp <- gp + theme(axis.text.y = element_blank(),
                    axis.ticks.y = element_blank(),
                    axis.title.y = element_blank() )
   }
 }
 if (ylab == "") {
   gp <- gp + theme(axis.title.y = element_blank())
 }

 return(gp)
}
chart_PointLine <- function(data, x, y,  
                            xlab = "Principal Component", 
                            ylab = "Proportion of explained variance")
{
  if (dim(data)[2] < 2)
  {
    df <- as.data.frame(data)
    y <- df[[1]]
    df[ "pc" ] <- as.numeric(rownames(df))
    x <- df[[2]]
  }
  else
  {
    df <- as.data.frame(data)
  }
  return(
    ggplot(df, aes(x=x, y=y)) +
      geom_path(linemitre = 1) + #position = "jitter") +
      geom_point(shape = 21, stat='identity',color='black',fill='white', size = 3 ) +
      theme_classic() +
      theme_light() +
      theme(panel.grid = element_blank(), panel.border = element_rect(color = "black", linewidth = 0.5),
            plot.title = element_text(hjust = 0.5),
            axis.line = element_line(color = "black", linewidth = 0.5), 
            axis.ticks = element_line(color = "black", linewidth = 0.5),
      ) + 
      xlab(xlab) +
      ylab(ylab) +
      scale_x_continuous(breaks =  seq(0, length(df$pc), by = 2)) +
      scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits = c(0, 1)) #, expand = c(0, 0))
  )
}
PC_Communalties <- function(pca)
{
  Namen <- c()
  for(i in 1:length(eigen_greater_than_one(pca)))
    Namen <- c(Namen, c(paste0("Dim.",i)))
  communalities <- pca$var$cos2
  
  comm <- communalities[, Namen] 
  lm <- melt(comm)
  names(lm) <- c("communalities", "PC", "value")
  lm$PC <- gsub("[^0-9]", "", lm$PC)
  return(lm[order(lm$PC, -lm$value),])
}


chart_PC_Communalties <- function(pca, xlab = "Communality", ylab = "Variable")
{
  lm <- PC_Communalties(pca)
  gg <- ggplot(lm[order(lm$PC, decreasing=F),], aes(x = as.numeric(value), y = communalities, fill = PC)) +
      geom_bar(stat = "identity") +
      theme_bw() + scale_fill_grey() +
      ylab(ylab) +
      xlab(xlab)
  
  if (ylab == "") {
    gg <- gg + theme(axis.title.y = element_blank())
  }
  return(gg)
}
chart_PC_Loadings <- function(object, folder, pca, 
                              axistextsize = NULL, 
                              axistitlesize = NULL, 
                              axistitleface = NULL, 
                              xlab = "Corr. coeff.", ylab = "Variable",
                              ylab_size = 1) {
  gs <- list()
  num_components <- length(eigen_greater_than_one(pca))
  for (i in 1:num_components) {
    gs[[i]] <- chart_onePC_Loadings(object, folder, pca, i, with_Y_AxisText = FALSE, 
                                    axistextsize, axistitlesize, axistitleface, 
                                    xlab = xlab, 
                                    ylab = ylab)
  }
  return(
    grid.arrange(grobs = gs,
                 ncol = num_components, nrow = 1,
                 #widths = c(label_width_y, rep(1, num_components - 1)),
                 #widths = rep(unit(label_width, "lines"), num_components),
                 common.axis.y = TRUE, align = "v")
  )
}
chart_PC_complete <- function(object, data, ResultDirectory = "", scale = FALSE, rank = FALSE)
{
  cat(paste("*** Principal component analysis: ", object, "***********************************\n"))
  data <- data[, !(colnames(data) %in% c("AG-Nr", "Datum", "Messstelle", "Tiefe"))]
 
  folder = paste0(ResultDirectory, object)
  if (dir.exists(folder) == FALSE) 
  {
    cat(paste("\tZielverzeichnis:: ", folder, "wurde angelegt!\n"))
    dir.create(folder)
  }
  else {
    cat("Zielverzeichnis: ", folder, "\n")
  }
    
  filename <- paste0(folder, "/", object, "_Data.xlsx")
  save_excel(filename, data)
  filename <- paste0(paste0(folder, "/", object, "_Data"), ".pdf")
  pdf(paste0(paste0(folder, "/", object, "_Data"), ".pdf"))
  cat(paste("\tDatei: ", filename, "wird geschrieben!\n"))
  plot(data, pch=16, cex=0.25)
  dev.off()
  

  #Prüfen auf fehlende Werte:
  nas <- sum(is.na(data))
  if (nas > 0)
  {
    cat(paste("*** Die Daten haben", nas , "Lücken! Es wir keine PCA durchgeführt!\n"))
    return ()
  }
  if (scale == TRUE) {
    cat("\tDie Daten werden skaliert!\n")
    data_scaled <- as.data.frame(scale(data))
    filename <- paste0(folder, "/", object, "_Data_scaled.xlsx")
    save_excel(filename, data_scaled)

    filename <- paste0(paste0(folder, "/", object, "_Data_normalized"), ".pdf")
    cat(paste("\tDatei: ", filename, "wird geschrieben!\n"))
    pdf(filename)
    plot(data_scaled, pch=16, cex=0.25)
    dev.off()
    
    
  } else {
    data_scaled <- data
  }

  if (rank == TRUE) 
  {
    cat("\tAnstelle der Werte wird ihr Rang verwendet!\n")
    data_scaled_rank <- as.data.frame(apply(data_scaled, 2, rank))
    
    filename <- paste0(folder, "/", object, "_Data_scaled_ranked.xlsx")
    save_excel(filename, data_scaled_rank)
    
    filename <- paste0(paste0(folder, "/", object, "_Data_ranked"), ".pdf")
    cat(paste("\tDatei: ", filename, "wird geschrieben!\n"))
    pdf(filename)
    plot(data_scaled_rank, pch=16, cex=0.25)
    dev.off()
  } else {
    data_scaled_rank <- data_scaled
  }
  
  gs <- list()
  pca <- FactoMineR::PCA(data_scaled_rank, ncp = length(data_scaled_rank), graph = FALSE)

  df <- as.data.frame(round(pca$eig[,2], 2))
  df[ "PC" ] <- rownames(df)
  names(df) <- c("% Var explained", "PC")
  
  df$PC <- as.numeric(gsub("[^0-9]", "", df$PC))
  df <- df[df$PC[] <= length(eigen_greater_than_one(pca)),]
  save_excel(paste0(folder, "/", object, "_ExplainedVariances.xlsx"), df)
  
  filename <- paste0(folder, "/", object, "_Communalties.xlsx")
  save_excel(filename, PC_Communalties(pca))
  
  filename <- paste0(folder, "/", object, "_Scores.xlsx")
    component_scores <- pca$ind$coord
    score_col_names <- paste0("PC", 1:ncol(component_scores))  # z.B. PC1, PC2, ...
    my_data_with_scores <- cbind(data, component_scores)
    colnames(my_data_with_scores)[(ncol(data) + 1):ncol(my_data_with_scores)] <- score_col_names
  save_excel(filename, my_data_with_scores)

  
  ev<-paste0("Data: explained variance = ", round(sum(df$`% Var explained`), 2), "%, n = ", nrow(data_scaled_rank))
  if (scale == TRUE) 
    ev <- paste0(ev, ", scaled")
  if (rank == TRUE)
    ev <- paste0(ev, ", ranked")
  gs[[1]] <- chart_PointLine(df, df$PC, df$`% Var explained` / 100)
  gs[[2]] <- chart_PC_Communalties(pca, ylab = "")
  gs[[3]] <- chart_PC_Loadings(object, paste0(folder, "/"), pca, 13, 13, c("bold"), ylab = "")
  for(i in 1:2)
  {
   gs[[i]] <- gs[[i]] + theme(axis.text = element_text(size = 13),
                              axis.title = element_text(size = 13,  face =c("bold")),
                              legend.text = element_text(size= 13),
                              legend.title = element_text(size = 13,  face =c("bold")))
  }
  n <- length(eigen_greater_than_one(pca)) -1
  title <- 'Principal Components at Lysimeter '



  
    txt <- strsplit(object, "_")[[1]]
  txt[1] <- gsub("Ly", "", txt[1])
  if (length(txt) < 2) 
  {
    txt <- c(txt, "")
  }
  else 
  {
    txt[2] <- paste0(" ", txt[2], "cm")
  }
  title <- paste0(title, txt[1], txt[2], " - ", ev)

  gg <- grid.arrange(grobs=gs, 
                     top = grid::textGrob(title, 
                                          gp = grid::gpar(fontsize = 16, face =c("bold"))), 
                                          ncols = 2, nrows =2, 
                                          layout_matrix = rbind(c(1, 2), c(3)),
                     widths = c(2, 3),
                     heights = c(1, 1))

  filename <- paste0(folder, "/", object, "_PrincipalComponents")
  if (rank == TRUE) filename <- paste0(filename, "_ranked")
  
  filename <- paste0(filename, ".pdf")
  cat(paste("\tDatei: ", filename, "wird geschrieben!\n"))
  ggsave(filename, gg, width = 13.28, height = 9.96, dpi = 300)

  
  filename <- paste0(folder, "/", object, "_Communalities", ".jpg")
  cat(paste("\tDatei: ", filename, "wird geschrieben!\n"))
  ggsave(filename, chart_PC_Communalties(pca, ylab = "") + theme(axis.text = element_text(size = 40),
                                                                axis.title = element_text(size = 40,  face =c("bold")),
                                                                legend.text = element_text(size= 40),
                                                                legend.title = element_text(size = 40,  face =c("bold"))), width = 13.28, height = 9.96, dpi = 300)

  filename <- paste0(folder, "/", object, "_PrincipalComponents")
  if (rank == TRUE) filename <- paste0(filename, "_ranked")
  filename <- paste0(filename, ".jpg")
  cat(paste("\tDatei: ", filename, "wird geschrieben!\n"))
  ggsave(filename, gg, width = 13.28, height = 9.96, dpi = 300)

  correlations(object, folder, data, filename = paste0("/", object, "_correlation PC variables"))

  df <- as.data.frame(round(pca$eig[,2], 2))
  df[ "PC" ] <- rownames(df)
  names(df) <- c("% Var explained", "PC")
  
  df$PC <- as.numeric(gsub("[^0-9]", "", df$PC))
  df <- df[df$PC[] <= length(eigen_greater_than_one(pca)),]
  save_excel(paste0(folder, "/", object, "_ExplainedVariances.xlsx"), df)
  
  eigenvalues <- pca$eig
  significant_pcs <- which(eigenvalues[, 1] >= 1)
  gs <- list()
  for (i in 1:length(significant_pcs)) {
    # Bild speichern
    img_file <- paste0(folder, "/", object, "_pca_loadings_", i, ".png")
    gs[[i]] <- chart_onePC_Loadings(object, folder, pca, i, with_Y_AxisText = TRUE,
                                    axistextsize = NULL, 
                                    axistitlesize = NULL, 
                                    axistitleface = NULL, 
                                    xlab = "", ylab = "", explainedVarianz = TRUE)
    
    chart_PC_Loadings(object, folder, pca)
    
    gs[[i]] <- gs[[i]] + 
      labs(title = paste0("PC", i)) +
      theme(axis.text = element_text(size=18), 
            axis.title = element_text(size=18),
            plot.title = element_text(size = 20, hjust = 0.5, vjust = 1)#,  # Titel mittig und etwas näher zum Plot
            #plot.margin = unit(c(2, 1, 1, 1), "cm")  # Platz für den Titel schaffen
            )
    
    ggsave(filename = img_file, plot = gs[[i]], width = 6, height = 4)
  }
  
  ggsave(filename = paste0(folder, "/", object, "_pca_loadings.png"), plot = chart_PC_Loadings(object, folder, pca), width = 6, height = 4)
  
  return(pca)
}

dfPCAs <- function(Ly, Depth, pca) {
  eigenvalues <- pca$eig
  significant_pcs <- which(eigenvalues[, 1] > 1)
  variance_percentage <- eigenvalues[significant_pcs, 2]
  df <- data.frame(
    Ly = rep(Ly, each = length(significant_pcs)),     # Wiederhole "Ly" für jede PC
    Depth = rep(Depth, each = length(significant_pcs)), # Wiederhole "Depth" für jede PC
    PC = significant_pcs,                              # PC-Nummern
    percentage_of_variance = variance_percentage        # Prozentsatz der erklärten Varianz
  )
  return(df)
}

dfPCAs <- function(Ly, Depth, pca) {
  eigenvalues <- pca$eig
  significant_pcs <- which(eigenvalues[, 1] > 1)
  variance_percentage <- eigenvalues[significant_pcs, 2]
  
  loadings <- pca$var$coord[, significant_pcs, drop = FALSE]
  loading_df <- data.frame(
    Variable = rep(rownames(loadings), times = ncol(loadings)),
    PC = rep(colnames(loadings), each = nrow(loadings)),  # PC als "Dim.1", "Dim.2" etc.
    Loading = as.vector(loadings)
  )
  
  loading_df$PC <- as.numeric(sub("Dim.", "", loading_df$PC))
  loading_df <- loading_df %>%
    filter(Loading >= 0.5) %>%
    arrange(desc(Loading))  # Sortiere nach Loadings absteigend
  
  df <- data.frame(
    Ly = rep(Ly, each = length(significant_pcs)),
    Depth = rep(Depth, each = length(significant_pcs)),
    PC = significant_pcs,
    percentage_of_variance = variance_percentage
  )
  
  library(dplyr)
  final_df <- df %>%
    left_join(loading_df, by = "PC")  # Verbinde über die Spalte "PC"
  
  return(final_df)
}



doPrimaryComponentAnalysis <- function(withInterpolation = FALSE)
{
  ExcludeVariables <- c(DriverVariables, "Orthophosphate", "Orthophosphate Phosphorus", "Conductivity", "Dissolved organic carbon", "bilanz3", #"Nitrate",
                        "cum Climatic water balance", "cum Groundwater flow", "cum Precipitation",  "cum Evapotranspiration", "Groundwater flow")
  Rank = TRUE
  ResultDirectory <- getResultDirectory(withInterpolation, ResultDirectory)
  
  pca_a <<- chart_PC_complete('A', aut[, !(colnames(aut) %in% ExcludeVariables)], ResultDirectory = ResultDirectory, scale = TRUE, rank = Rank)
  pca_gw <<- chart_PC_complete('GW', gw[, !(colnames(gw) %in% ExcludeVariables)], ResultDirectory = ResultDirectory, scale = TRUE, rank = Rank)

  pca_ly <<- chart_PC_complete('Lysimeters', ly[, !(colnames(ly) %in% ExcludeVariables)], ResultDirectory = ResultDirectory, scale = TRUE, rank = Rank)
  
  pca_ly1 <<- chart_PC_complete('Ly1', ly1[, !(colnames(ly1) %in% ExcludeVariables)], ResultDirectory = ResultDirectory, scale = TRUE, rank = Rank)
  pca_ly2 <<- chart_PC_complete('Ly2', ly2[, !(colnames(ly2) %in% ExcludeVariables)], ResultDirectory = ResultDirectory, scale = TRUE, rank = Rank)
  pca_ly3 <<- chart_PC_complete('Ly3', ly3[, !(colnames(ly3) %in% ExcludeVariables)], ResultDirectory = ResultDirectory, scale = TRUE, rank = Rank)
  # 
  pca_ly1_30 <<- chart_PC_complete('Ly1_30', ly1_30[, !(colnames(ly1_30) %in% ExcludeVariables)], ResultDirectory = ResultDirectory, scale = TRUE, rank = Rank)
  pca_ly2_30 <<- chart_PC_complete('Ly2_30', ly2_30[, !(colnames(ly2_30) %in% ExcludeVariables)], ResultDirectory = ResultDirectory, scale = TRUE, rank = Rank)
  pca_ly3_30 <<- chart_PC_complete('Ly3_30', ly3_30[, !(colnames(ly3_30) %in% ExcludeVariables)], ResultDirectory = ResultDirectory, scale = TRUE, rank = Rank)
  
  pca_ly1_60 <<- chart_PC_complete('Ly1_60', ly1_60[, !(colnames(ly1_60) %in% ExcludeVariables)], ResultDirectory = ResultDirectory, scale = TRUE, rank = Rank)
  pca_ly2_60 <<- chart_PC_complete('Ly2_60', ly2_60[, !(colnames(ly2_60) %in% ExcludeVariables)], ResultDirectory = ResultDirectory, scale = TRUE, rank = Rank)
  pca_ly3_60 <<- chart_PC_complete('Ly3_60', ly3_60[, !(colnames(ly3_60) %in% ExcludeVariables)], ResultDirectory = ResultDirectory, scale = TRUE, rank = Rank)
  
  pca_ly1_90 <<- chart_PC_complete('Ly1_90', ly1_90[, !(colnames(ly1_90) %in% ExcludeVariables)], ResultDirectory = ResultDirectory, scale = TRUE, rank = Rank)
  pca_ly2_90 <<- chart_PC_complete('Ly2_90', ly2_90[, !(colnames(ly2_90) %in% ExcludeVariables)], ResultDirectory = ResultDirectory, scale = TRUE, rank = Rank)
  pca_ly3_90 <<- chart_PC_complete('Ly3_90', ly3_90[, !(colnames(ly3_90) %in% ExcludeVariables)], ResultDirectory = ResultDirectory, scale = TRUE, rank = Rank)

  explainedVariances <- NULL
  explainedVariances <- rbind(explainedVariances, dfPCAs('Ly1', 30, pca_ly1_30))
  explainedVariances <- rbind(explainedVariances, dfPCAs('Ly1', 60, pca_ly1_60))
  explainedVariances <- rbind(explainedVariances, dfPCAs('Ly1', 90, pca_ly1_90))
  explainedVariances <- rbind(explainedVariances, dfPCAs('Ly2', 30, pca_ly2_30))
  explainedVariances <- rbind(explainedVariances, dfPCAs('Ly2', 60, pca_ly2_60))
  explainedVariances <- rbind(explainedVariances, dfPCAs('Ly2', 90, pca_ly2_90))
  explainedVariances <- rbind(explainedVariances, dfPCAs('Ly3', 30, pca_ly3_30))
  explainedVariances <- rbind(explainedVariances, dfPCAs('Ly3', 60, pca_ly3_60))
  explainedVariances <- rbind(explainedVariances, dfPCAs('Ly3', 90, pca_ly3_90))
  explainedVariances <- rbind(explainedVariances, dfPCAs('A', 'A', pca_a))
  explainedVariances <- rbind(explainedVariances, dfPCAs('GW', 'GW', pca_gw))
  
  filename <- paste0(ResultDirectory, "/", "PrimaryComponentes.xlsx")
  save_excel(filename, explainedVariances)
}
