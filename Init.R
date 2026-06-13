#
#Article:		Impacts of groundwater level management on geochemical processes at an agricultural site with shallow groundwater
#Journal:		Environmental Earth Sciences
#Authors:		Jörg Steidl, Ottfried Dietrich, Christoph Merz
#Corresponding: jsteidl@zalf.de
#
# Packages #####################################################################
#
install_and_load_package <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
    message(paste("Das Paket", package_name, "wird installiert."))
  } else {
    #message(paste("Das Paket", package_name, "ist bereits installiert."))
  }
  library(package_name, character.only = TRUE)
  message(paste("Das Paket", package_name, "ist geladen."))
}

#Data
install_and_load_package("lubridate")

#PCA
install_and_load_package("reshape2")
install_and_load_package("FactoMineR")
install_and_load_package("factoextra")
install_and_load_package("psych")

#Random Forest
#install.packages("caTools")
install_and_load_package("reshape2")
install_and_load_package("caTools") 
install_and_load_package("randomForest")
#library(randomForestExplainer)
install_and_load_package("datasets")
install_and_load_package("caret")
install_and_load_package("e1071")

#further
install_and_load_package("corrplot")
install_and_load_package("correlation")
install_and_load_package("RPostgreSQL")
install_and_load_package("odbc")
install_and_load_package("readxl")
install_and_load_package("openxlsx")
install_and_load_package("caret")
install_and_load_package("zoo")
install_and_load_package("dplyr")
install_and_load_package("tidyr")
install_and_load_package("gridExtra")
install_and_load_package("grid")
install_and_load_package("ggtext")
install_and_load_package("RColorBrewer")
install_and_load_package("rlang")

#Word
install_and_load_package("officer")
install_and_load_package("flextable")

rm(list = ls(envir = globalenv()), envir = globalenv())

# Code sources #################################################################
source("funcData.R")
source("funcPCA.R")
source("funcRForest.R")
source("funcPCDrivers.R")

getResultDirectory <- function(withInterpolation = TRUE, Directory = "") {
  if (withInterpolation == TRUE)
    if (!grepl(InterpolationNameSign, Directory)) {
      Directory <- paste0(gsub("\\\\$", "", Directory), InterpolationNameSign, "\\")
      cat("Zielverzeichnis", Directory, "\n")
      return(Directory)
    }
  return(Directory)
}
plotCommunalities <- function(withInterpolation) {
  folder <- getResultDirectory(withInterpolation, ResultDirectory)  
  
  # Daten zusammenstellen
  pcc <- data.frame("Ly" = "I", "Depth" = 30, PC_Communalties(pca_ly1_30))
  pcc <- rbind(pcc, data.frame("Ly" = "I", "Depth" = 60, PC_Communalties(pca_ly1_60)))
  pcc <- rbind(pcc, data.frame("Ly" = "I", "Depth" = 90, PC_Communalties(pca_ly1_90)))
  pcc <- rbind(pcc, data.frame("Ly" = "II", "Depth" = 30, PC_Communalties(pca_ly2_30)))
  pcc <- rbind(pcc, data.frame("Ly" = "II", "Depth" = 60, PC_Communalties(pca_ly2_60)))
  pcc <- rbind(pcc, data.frame("Ly" = "II", "Depth" = 90, PC_Communalties(pca_ly2_90)))
  pcc <- rbind(pcc, data.frame("Ly" = "III", "Depth" = 30, PC_Communalties(pca_ly3_30)))
  pcc <- rbind(pcc, data.frame("Ly" = "III", "Depth" = 60, PC_Communalties(pca_ly3_60)))
  pcc <- rbind(pcc, data.frame("Ly" = "III", "Depth" = 90, PC_Communalties(pca_ly3_90)))
  
  # Umwandeln in Faktoren
  pcc$Depth <- as.factor(pcc$Depth)
  pcc$Ly <- as.factor(pcc$Ly)

  # Labels für Facettierung
  facet_labels <- labeller(
    Ly = function(x) paste("Lysimeter ", x),
    Depth = function(x) paste(x, " cm")
  )
  
  # Farben für die PC
  pc_colors <- c("I" = "bisque4", "II" = "bisque3", "III" = "bisque1", "IV" = "beige")
  
  # Sortiere pcc nach Ly, Depth und communalities
  pcc <- pcc %>% 
    arrange(Ly, Depth, communalities)  # Sortiere nach Ly, Depth und communalities
  # ggplot erstellen
  communalities_order <- c("Ammonium", "Bicarbonate", "Calcium", "Carbon dioxide", "Chloride", "Iron", 
                           "Manganese", "Magnesium", "Nitrate", 
                           "Oxygen", "pH", "Potassium", "Redox potential", "Sodium", 
                           "Sulfate")
  
  pcc$communalities <- factor(pcc$communalities, levels = communalities_order)
  
  # Überprüfen der Levels
  print("Levels von communalities nach Sortierung:")
  print(levels(pcc$communalities))
  # Umwandeln von value in numerisch, falls notwendig
  pcc$value <- as.numeric(as.character(pcc$value))
  
  # ggplot erstellen
  gg <- ggplot(pcc, aes(x = as.numeric(value), y = communalities, fill = PC)) +
    geom_bar(stat = "identity") +
    scale_y_discrete(limits = levels(pcc$communalities)) +  # Sicherstellen, dass die Y-Achse die Levels korrekt anzeigt
    scale_x_continuous(breaks = c(0, 1), limits = c(0, 1), expand = c(0, 0)) +
    theme_classic() +
    ylab("") +
    xlab("Communalities") +
    facet_grid(Depth ~ Ly, labeller = facet_labels, space = "free") +
    theme(
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      panel.spacing = unit(1, "lines"),   # Erhöhung des Abstands zwischen den Facetten
      strip.text.x = element_text(size = 12),
      strip.text.y = element_text(size = 12),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12, face = "bold")
    )
  # Plot anzeigen
  print(gg)

  # Ergebnisse speichern
  filename <- paste0(folder, "/", "Lysimeters Communalities", ".jpg")
  cat(paste("\tDatei: ", filename, "wird geschrieben!\n"))
  ggsave(filename, gg, width = 13.28, height = 9.96, dpi = 300)
  
  filename <- paste0(folder, "/", "Lysimeters Communalities", ".xlsx")
  save_excel(filename, pcc)
}



Loadings <- function(pca, ly, depth) {
  num_components <- length(eigen_greater_than_one(pca))
  loadings <- data.frame()
  
  # Entferne eventuelle Endungen von Zeilennamen
  rownames(pca$var$coord) <- gsub("\\.[0-9]+$", "", rownames(pca$var$coord))

  for (i in 1:num_components) {
    # Prüfe, ob pca$var$coord die erwartete Dimension hat
    if (nrow(pca$var$coord) > 0 && ncol(pca$var$coord) >= i) {
      load <- pca$var$coord
      sorted.loadings <- load[order(load[, i]), i]
      
      # Erstelle einen DataFrame für die i-te Hauptkomponente
      component_loadings <- data.frame(
        "Ly" = ly,
        "Depth" = depth,
        "PC" = i,
        "Variable" = rownames(load)[order(load[, i])],
        "Loadings" = sorted.loadings
      )
      
      # Füge die Daten zu 'loadings' hinzu
      loadings <- rbind(loadings, component_loadings)
    } else {
      warning(paste("Component", i, "does not exist in PCA results."))
    }
  }
  return(loadings)
}
plotLoadings_GW_A <- function(withInterpolation) {
  Load <- Loadings(pca_gw, "GW", 0)
  pc_labeller <- labeller(PC = function(values) paste0("PC", values))
  gp <-  ggplot(Load, aes(x = Loadings, y = Variable, fill = factor(Loadings))) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.9, color = "black", size = 0.15) +
    scale_fill_grey() +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.25) +  # Linie bei x = 0
    labs(title = paste("Lysimeter", unique(Load$Ly), "Depth =", unique(Load$Depth), "cm"), x = "Correlation Coefficient", y = "") +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.line.x = element_line(color = "black", linewidth = 0.25),
      axis.ticks.x = element_line(color = "black", linewidth = 0.25),
      panel.grid = element_blank() ,
      axis.text.y = element_text(size = 8),  # Reduziere die Schriftgröße der y-Achse
      axis.text.x = element_text(size = 8),  # Reduziere die Schriftgröße der x-Achse
      axis.title = element_text(size = 8),  # Titel der Achsen kleiner
      plot.title = element_text(size = 8),  # Titelgröße anpassen
      panel.spacing = unit(1, "lines")  # Erhöht den Abstand zwischen den Facetten
    ) +
    scale_x_continuous(breaks = c(-1, 0, 1), limits = c(-1, 1), expand = c(0, 0)) +
    facet_grid(~ PC, labeller = pc_labeller)
  
  gg <- grid.arrange(grobs = list(gp), ncol = 1, heights = rep(1, length(list(gp))) * 2)
  
  folder <- getResultDirectory(withInterpolation, ResultDirectory)
  filename <- paste0(folder, "PCA_Loadings_GW", ".jpg")
  cat(paste("\tDatei: ", filename, "wird geschrieben!\n"))
  ggsave(filename, gg, dpi = 600) #, width = 13.28, height = 9.96, dpi = 300)
  
  Load <- Loadings(pca_a, "A", 0)
  pc_labeller <- labeller(PC = function(values) paste0("PC", values))
  gp <-  ggplot(Load, aes(x = Loadings, y = Variable, fill = factor(Loadings))) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.9, color = "black", size = 0.15) +
    scale_fill_grey() +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.25) +  # Linie bei x = 0
    labs(title = paste("Lysimeter", unique(Load$Ly), "Depth =", unique(Load$Depth), "cm"), x = "Correlation Coefficient", y = "") +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.line.x = element_line(color = "black", linewidth = 0.25),
      axis.ticks.x = element_line(color = "black", linewidth = 0.25),
      panel.grid = element_blank() ,
      axis.text.y = element_text(size = 8),  # Reduziere die Schriftgröße der y-Achse
      axis.text.x = element_text(size = 8),  # Reduziere die Schriftgröße der x-Achse
      axis.title = element_text(size = 8),  # Titel der Achsen kleiner
      plot.title = element_text(size = 8),  # Titelgröße anpassen
      panel.spacing = unit(1, "lines")  # Erhöht den Abstand zwischen den Facetten
    ) +
    scale_x_continuous(breaks = c(-1, 0, 1), limits = c(-1, 1), expand = c(0, 0)) +
    facet_grid(~ PC, labeller = pc_labeller)
  
  gg <- grid.arrange(grobs = list(gp), ncol = 1, heights = rep(1, length(list(gp))) * 2)
  
  folder <- getResultDirectory(withInterpolation, ResultDirectory)
  filename <- paste0(folder, "PCA_Loadings_A", ".jpg")
  cat(paste("\tDatei: ", filename, "wird geschrieben!\n"))
  ggsave(filename, gg, dpi = 600) #, width = 13.28, height = 9.96, dpi = 300)
  
}
plotLoadingsLysimetersDepth <- function(pca, ly, depth, withInterpolation) {
  Load <- Loadings(pca, ly,  depth)
  pc_labeller <- labeller(PC = function(values) paste0("PC", values))

  gp <-  ggplot(Load, aes(x = Loadings, y = Variable, fill = factor(Loadings))) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.9, color = "black", size = 0.15) +
    scale_fill_grey() +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.25) +  # Linie bei x = 0
    labs(title = paste("Lysimeter", unique(Load$Ly), "Depth =", unique(Load$Depth), "cm"), x = "Correlation Coefficient", y = "") +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.line.x = element_line(color = "black", linewidth = 0.25),
      axis.ticks.x = element_line(color = "black", linewidth = 0.25),
      panel.grid = element_blank() ,
      axis.text.y = element_text(size = 8),  # Reduziere die Schriftgröße der y-Achse
      axis.text.x = element_text(size = 8),  # Reduziere die Schriftgröße der x-Achse
      axis.title = element_text(size = 8),  # Titel der Achsen kleiner
      plot.title = element_text(size = 8),  # Titelgröße anpassen
      panel.spacing = unit(1, "lines")  # Erhöht den Abstand zwischen den Facetten
    ) +
    scale_x_continuous(breaks = c(-1, 0, 1), limits = c(-1, 1), expand = c(0, 0)) +
    facet_grid(~ PC, labeller = pc_labeller)
  
   print(gp)
  

  folder <- getResultDirectory(withInterpolation, ResultDirectory)
  if (ly == "I")
    filename <- "Ly1"
  if (ly == "II")
    filename <- "Ly2"
  if (ly == "III")
    filename <- "Ly3"
  if (depth > 0)
    filename <- paste0(filename, "_", depth)
  
  filename <- paste0(folder, "", filename, "\\", filename, "_PCA_Loadings", ".jpg")
  cat(paste("\tDatei: ", filename, "wird geschrieben!\n"))
  ggsave(filename, gp, dpi = 600) #, width = 13.28, height = 9.96, dpi = 300)
  return(gp)
}
plotAllLoadingsLysimeter <- function(ly, gs, withInterpolation) {

  for(i in 1:3)
  {
      gs[[i]] <- gs[[i]] + 
        labs(title = NULL, x = NULL, y = NULL) +
        theme(plot.title = element_blank(),   # Titel auf NULL setzen
              axis.title.y = element_blank(),
              plot.margin = unit(c(0, 0.5, 0, 0.5), "cm"), 
              axis.text.y = element_text(size = 7),
              #axis.text.y = element_text(size = 6),  # Reduziere die Schriftgröße der y-Achse
              axis.text.x = element_text(size = 8),  # Reduziere die Schriftgröße der x-Achse
              axis.title = element_text(size = 8),  # Titel der Achsen kleiner
              strip.text.x = element_text(size = 8),  # Hier wird die Textgröße der x-Facettenbeschriftung angepasst
              strip.text.y = element_text(size = 8), # Falls y-Facettenbeschriftung ebenfalls angepasst werden soll
              
        )  # Schriftgröße der Y-Achse weiter reduzieren
      #+
      #  scale_x_continuous(expand = c(0, 0))  
      if (i==3) {
        gs[[i]] <- gs[[i]] + 
          labs(title = NULL, x = "Correlation Coefficient", y = NULL)
      }
  }
  gg <- grid.arrange(grobs = gs, ncol = 1, heights = rep(1, length(gs)) * 2)

  folder <- getResultDirectory(withInterpolation, ResultDirectory)
  if (ly > 0) {
    filename <- paste0(folder, "PCA_Loadings", "_Ly", ly, ".jpg")
  } else {
    filename <- paste0(folder, "PCA_Loadings", "_Ly", ".jpg")
  }
  
  cat(paste("\tDatei: ", filename, "wird geschrieben!\n"))
  ggsave(filename, gg, dpi = 600) #, width = 13.28, height = 9.96, dpi = 300)
}
plotLoadingsLysimeters <- function(withInterpolation) {
  
  p1 <- plotLoadingsLysimetersDepth(pca_ly1_30, "I", 30, withInterpolation)
  p2 <- plotLoadingsLysimetersDepth(pca_ly1_60, "I", 60, withInterpolation)
  p3 <- plotLoadingsLysimetersDepth(pca_ly1_90, "I", 90, withInterpolation)
  plotAllLoadingsLysimeter(1, list(p1, p2, p3), withInterpolation)

  
  p1 <- plotLoadingsLysimetersDepth(pca_ly2_30, "II", 30,  withInterpolation)
  p2 <- plotLoadingsLysimetersDepth(pca_ly2_60, "II", 60,  withInterpolation)
  p3 <- plotLoadingsLysimetersDepth(pca_ly2_90, "II", 90,  withInterpolation)
  plotAllLoadingsLysimeter(2, list(p1, p2, p3), withInterpolation)

  p1 <- plotLoadingsLysimetersDepth(pca_ly3_30, "III", 30,  withInterpolation)
  p2 <- plotLoadingsLysimetersDepth(pca_ly3_60, "III", 60,  withInterpolation)
  p3 <- plotLoadingsLysimetersDepth(pca_ly3_90, "III", 90,  withInterpolation)
  plotAllLoadingsLysimeter(3, list(p1, p2, p3), withInterpolation)

  p1 <- plotLoadingsLysimetersDepth(pca_ly1, "I", 0, withInterpolation)
  p2 <- plotLoadingsLysimetersDepth(pca_ly2, "II", 0, withInterpolation)
  p3 <- plotLoadingsLysimetersDepth(pca_ly3, "III", 0, withInterpolation)
  plotAllLoadingsLysimeter(0, list(p1, p2, p3), withInterpolation)
}

aplot <- function(data, Parameter, Einheit) {

  g <- ggplot(subset(data, Stoff == Parameter), aes(x = Messstelle, y = Konzentration, fill = Depth)) +
    geom_violin(aes(fill = Depth), scale = "width", trim = TRUE, position = position_dodge(width = 1)) +
    geom_boxplot(width = 0.2, outlier.shape = NA, position = position_dodge(width = 1)) +
    labs(title = Parameter, x = NULL, y = Einheit)  +
    theme_classic() +
    theme(legend.position = "none") +
    theme(
      axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5),
      axis.title.x = element_text(size = 10, angle = 0, hjust = 0.5),
      axis.title.y = element_text(size = 10),
      strip.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
      legend.text = element_text(size = 12),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black")
    ) 
  return(g)
}
plotAnalyzedComponentesVariables <- function(withInterpolation, Data) {
  folder <- getResultDirectory(withInterpolation, ResultDirectory)  
  data <- Data
  data <- lysimeters
  cols <- c(DriverVariables, "Orthophosphate", "Conductivity", "Dissolved organic carbon", "Air temperatur")
  
  if (!("pH" %in% ComponentesVariables)) {
    cols <- c(cols, "pH")
  }
  
  data <- data[, !(colnames(data) %in% c(cols, "AG-Nr"))]
  data <- data %>%
    mutate(Messstelle = ifelse(Messstelle == 'Ly1', 'I', Messstelle)) %>%
    mutate(Messstelle = ifelse(Messstelle == 'Ly2', 'II', Messstelle)) %>%
    mutate(Messstelle = ifelse(Messstelle == 'Ly3', 'III', Messstelle))
  
  data_long <- melt(data, id.vars = c("Datum", "Messstelle", "Tiefe"),
                    variable.name = "Stoff", value.name = "Konzentration")
  names(data_long)[names(data_long) == "Tiefe"] <- "Depth"
  
  data_long$Messstelle <- factor(data_long$Messstelle, levels = c("I", "II", "III"))
  
  # Definieren Sie die Farben und stellen Sie sicher, dass sie mit den Levels in "Depth" übereinstimmen
  depth_colors <- c("30" = "darkolivegreen3", "60" = "khaki3", "90" = "lightblue3")
  
  # Überprüfen Sie, ob die Levels in der 'Depth'-Spalte den Farben entsprechen
  if (!all(unique(data_long$Depth) %in% names(depth_colors))) {
    stop("Es gibt keine Übereinstimmung zwischen den Levels in der Depth-Spalte und den definierten Farben.")
  }
  
  # Erstellen Sie den ersten Plot und überprüfen Sie die Legende
  p1 <- aplot(data_long, "Calcium", "") +
    theme(legend.position = "left") +
    scale_fill_manual(values = depth_colors) +
    scale_color_manual(values = depth_colors)
  
  g1 <- ggplotGrob(p1)
  l_name <- NULL
  
  # Prüfen, ob eine Legende vorhanden ist
  if ("guide-box" %in% g1$layout$name) {
    l_name <- "guide-box"
  } else if ("guide-box-left" %in% g1$layout$name) {
    l_name <- "guide-box-left"
  } else if ("guide-box-right" %in% g1$layout$name) {
    l_name <- "guide-box-right"
  } else if ("guide-box-bottom" %in% g1$layout$name) {
    l_name <- "guide-box-bottom"
  } else if ("guide-box-top" %in% g1$layout$name) {
    l_name <- "guide-box-top"
  } else {
    print("Es wurde keine Legende gefunden!")
    stop()
  }
  
  # Extrahiere die gefundene Legende
  legend <- g1$grobs[[which(g1$layout$name == l_name)]]

  plot_list <- list()
  for (var in sort(ComponentesVariables)) {
    unit <- expression("gm"^{-3})
    if (var == "Carbon dioxide")
      unit <- expression("mmolL"^{-3})
    if (var == "Bicarbonate")
      unit <- expression("mmolL"^{-3})
    if  (var == "pH")
      unit <- expression("pH")
    if  (var == "Redox potential")
      unit <- expression("mV")

    plot_list[[var]] <- aplot(data_long, var, unit) + 
      scale_fill_manual(values = depth_colors) + 
      scale_color_manual(values = depth_colors)
  }
  
  num_plots <- length(plot_list) + 1
  num_cols <- 4  
  num_rows <- ceiling((num_plots) / num_cols)  
  layout_matrix <- matrix(1:(num_plots), nrow = num_rows, ncol = num_cols, byrow = TRUE)
  arranged_plots <- arrangeGrob(
    grobs = c(plot_list, list(legend)),  # Leerer Plot für das letzte Feld
    layout_matrix = layout_matrix
  )
  final_plot <- arrangeGrob(arranged_plots, 
                            textGrob("Lysimeter", gp = gpar(fontsize = 14)), 
                            ncol = 1, heights = c(10, 0.5))
  gg <- grid.arrange(final_plot)
  
  filename <- paste0(folder, "Lysimeters analyzed laboratory data.pdf")
  cat(paste("\tDatei:", filename, "wird geschrieben!\n"))
  ggsave(filename, gg, width = 297, height = 210, units = "mm", dpi = 300)
  
  filename <- paste0(folder, "Lysimeters analyzed laboratory data.jpg")
  cat(paste("\tDatei:", filename, "wird geschrieben!\n"))
  ggsave(filename, gg, width = 297, height = 210, units = "mm", dpi = 300)
}
plotAnalyzedDriverVariables___ <- function(withInterpolation, Data) {
  folder <- getResultDirectory(withInterpolation, ResultDirectory)  
  data <- Data
  data$Soil <- ifelse(data$Tiefe <= 60, "Topsoil", "Subsoil")
  cols <- c(DriverVariables, "Datum", "Messstelle", "Soil")
  data <- data[, (colnames(data) %in% c(cols))]
  #data <- data[, !names(data) %in% (ExcludeDriverDataNames)]
  
  data <- data %>%
    mutate(Messstelle = ifelse(Messstelle == 'Ly1', 'I', Messstelle))
  data <- data %>%
    mutate(Messstelle = ifelse(Messstelle == 'Ly2', 'II', Messstelle))
  data <- data %>%
    mutate(Messstelle = ifelse(Messstelle == 'Ly3', 'III', Messstelle))
  
  data_long <- melt(data, id.vars = c("Datum", "Messstelle", "Soil"),
                    variable.name = "Stoff", value.name = "Konzentration")
  
  
  data_long$Messstelle <- factor(data_long$Messstelle, levels = c("I", "II", "III"))
  data_long$Soil <- factor(data_long$Soil, levels = c("Topsoil", "Subsoil"))
  # Plot erstellen
  
  
  
  gg <- ggplot(data_long, aes(x = Messstelle, y = Konzentration, fill = Soil)) +
    geom_violin(aes(fill = Soil), scale = "width", trim = TRUE, position = position_dodge(width = 1)) +
    geom_boxplot(width = 0.2, outlier.shape = NA, position = position_dodge(width = 1)) +
    facet_wrap(~ Stoff, scales = "free_y") +
    # scale_fill_brewer(palette = "Pastel1", name = "Soil") +
    scale_fill_manual(values = c("Topsoil" = "white",  "Subsoil" = "gray90"), name = "Soil") +
    labs(
      title = "Lysimeters: analysed driver data",
      subtitle = "Measured values of various parameters by lysimeter and soil depth",
      x = "Lysimeter",  # Hier ändern wir die Achsenbeschriftung
      y = "Values"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5),  
      axis.title.x = element_text(size = 12, angle = 0, hjust = 0.5), 
      axis.title.y = element_text(size = 12), 
      strip.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
      legend.text = element_text(size = 12),
      panel.grid.major = element_blank(),  # Haupt-Gitterlinien entfernen
      panel.grid.minor = element_blank(),   # Neben-Gitterlinien entfernen
      axis.line = element_line(color = "black") ,  # Achsenlinien hinzufügen
      axis.ticks = element_line(color = "black")#,  # Achsenticks hinzufügen
      #legend.position = c(0.9, 0.1),  # Legendenposition in den Raum des 12. Charts verschieben
      #legend.justification = c("right", "bottom")  # Legende rechts unten
    ) +
    guides(fill = guide_legend(title = NULL))
  
  if (ncol(data) %% 2 == 0) {
    gg <- gg +
      theme(
        legend.position = c(0.9, 0.1),  # Legendenposition in den Raum des 12. Charts verschieben
        legend.justification = c("right", "bottom")  # Legende rechts unten
      )
  }
  filename <- paste0(folder, "Lysimeters analysed driver data.pdf")
  cat(paste("\tDatei: ", filename, "wird geschrieben!\n"))
  ggsave(filename, gg, width = 297, height = 210, units = "mm", dpi = 300)
  
  filename <- paste0(folder, "Lysimeters analysed driver data.jpg")
  cat(paste("\tDatei: ", filename, "wird geschrieben!\n"))
  ggsave(filename, gg, width = 297, height = 210, units = "mm", dpi = 300)
  
}
plotAnalyzedDriverVariables <- function(withInterpolation, Data) {
  folder <- getResultDirectory(withInterpolation, ResultDirectory)  
  data <- Data
  data <- lysimeters

  cols <- c(DriverVariables, "Datum", "Messstelle", "Tiefe")
  data <- data[, (colnames(data) %in% c(cols))]
  
  data <- data %>%
    mutate(Messstelle = ifelse(Messstelle == 'Ly1', 'I', Messstelle))
  data <- data %>%
    mutate(Messstelle = ifelse(Messstelle == 'Ly2', 'II', Messstelle))
  data <- data %>%
    mutate(Messstelle = ifelse(Messstelle == 'Ly3', 'III', Messstelle))
  
  data_long <- melt(data, id.vars = c("Datum", "Messstelle", "Tiefe"),
                    variable.name = "Stoff", value.name = "Konzentration")
  names(data_long)[names(data_long) == "Tiefe"] <- "Depth"
  
  
  data_long$Messstelle <- factor(data_long$Messstelle, levels = c("I", "II", "III"))

  # Plot erstellen
  
  depth_colors <- c("30" = "darkolivegreen3", "60" = "khaki3", "90" = "lightblue3")
  p1 <- aplot(data_long, "Evapotranspiration", "mm")  +
    theme(legend.position = "left")  + scale_fill_manual(values = depth_colors) + scale_color_manual(values = depth_colors)
  g1 <- ggplotGrob(p1)
  legend <- g1$grobs[[which(g1$layout$name == "guide-box")]] 
  
  
  plot_list <- list(
    aplot(data_long, "Evapotranspiration", expression("mm d"^-1))  + scale_fill_manual(values = depth_colors) + scale_color_manual(values = depth_colors),
    aplot(data_long, "Climatic water balance", expression("mm d"^-1))  + scale_fill_manual(values = depth_colors) + scale_color_manual(values = depth_colors),
    aplot(data_long, "Groundwater below surface", "cm")  + scale_fill_manual(values = depth_colors) + scale_color_manual(values = depth_colors),
    aplot(data_long, "Groundwater flow", expression("mm d"^-1))  + scale_fill_manual(values = depth_colors) + scale_color_manual(values = depth_colors),
    aplot(data_long, "Leaf area index", "Index")  + scale_fill_manual(values = depth_colors) + scale_color_manual(values = depth_colors),
    #aplot(data_long, "pF", "pF")  + scale_fill_manual(values = depth_colors) + scale_color_manual(values = depth_colors),
    aplot(data_long, "Pressure head", "kPa")  + scale_fill_manual(values = depth_colors) + scale_color_manual(values = depth_colors),
    aplot(data_long, "Precipitation", expression("mm d"^-1))  + scale_fill_manual(values = depth_colors) + scale_color_manual(values = depth_colors),
#    aplot(data_long, "Saturation", "(-)")  + scale_fill_manual(values = depth_colors) + scale_color_manual(values = depth_colors),
    aplot(data_long, "Soil temperature", "°C")  + scale_fill_manual(values = depth_colors) + scale_color_manual(values = depth_colors)
  )
  
  # Layoutmatrix anpassen, um die Legende in Position 15 einzufügen
  layout_matrix <- rbind(
    c(1, 2, 3),
    c(4, 5, 6),
    c(7, 8, 9)  # Legende kommt in Feld 9
  )
  
  # Plots mit der Legende arrangieren
  arranged_plots <- arrangeGrob(
    grobs = c(plot_list, list(legend)),  # Keine leeren Plots mehr
    layout_matrix = layout_matrix
  )
  final_plot <- arrangeGrob(
    arranged_plots,
    textGrob("Lysimeter", gp = gpar(fontsize = 14)), 
    ncol = 1, 
    heights = c(10, 0.5)
  )
  # Vollständiges Layout anzeigen
  gg <- grid.arrange(final_plot)
  

  filename <- paste0(folder, "Lysimeters analysed driver data.pdf")
  cat(paste("\tDatei: ", filename, "wird geschrieben!\n"))
  ggsave(filename, gg, width = 297, height = 210, units = "mm", dpi = 300)
  
  filename <- paste0(folder, "Lysimeters analysed driver data.jpg")
  cat(paste("\tDatei: ", filename, "wird geschrieben!\n"))
  ggsave(filename, gg, width = 297, height = 210, units = "mm", dpi = 300)
  
}

MakeTheModels <- function(InterpolationNameSign = "_interpolation",
                          withInterpolation = TRUE,
                          maxgap_Labor = 35,
                          maxgap_Driver = 2) {
  InterpolationNameSign <- InterpolationNameSign
  withInterpolation <- withInterpolation
  maxgap_Labor <- maxgap_Labor
  maxgap_Driver <- maxgap_Driver
  
  #Löschen aller Objecte im Arbeistbereich und Laden aller Daten der Lysimeter
  Load_LysimeterData(withInterpolation, maxgap_Labor, maxgap_Driver)
  
  plotAnalyzedComponentesVariables(withInterpolation, lysimeters)

  #Ausführen aller Hauptkomponentenanalysen
  doPrimaryComponentAnalysis(withInterpolation)
  
  plotCommunalities(withInterpolation)
  plotLoadingsLysimeters(withInterpolation)
  plotLoadings_GW_A(withInterpolation)

  doOptimizedDriversForPrimaryComponents(withInterpolation)
}

doConcentratePCs <- function(object, pca, ResultDirectory = "") {
  
  folder = paste0(ResultDirectory, object)
  
  eigenvalues <- pca$eig
  significant_pcs <- which(eigenvalues[, 1] >= 1)
  gs <- list()
  for (i in 1:length(significant_pcs)) {
    img_file <- paste0(folder, "/", object, "_PCA_Loadings_", i, ".png")
    gs[[i]] <- chart_onePC_Loadings(object, folder, pca, i, with_Y_AxisText = TRUE,
                                    axistextsize = NULL, 
                                    axistitlesize = NULL, 
                                    axistitleface = NULL, 
                                    xlab = "Corr. coeff.", ylab = "Variable")
    ggsave(filename = img_file, plot = gs[[i]], width = 6, height = 4)
  }
  return(gs)
}

doConcentratePC_Results <- function(PCAS, ResultDirectory = "") {
  
  PCAS <- data.frame(
    pca = I(list(pca_ly1_30, pca_ly2_30, pca_ly3_30)),  # I() wird verwendet, um eine Liste in eine Spalte zu packen
    object = c("ly1_30", "ly2_30", "ly3_30")
  )
  
  
  folder = paste0(ResultDirectory, object)
  
  grobs <- list()
  pics <- data.frame()
  for (i in seq_along(PCAS)) {
    grobs <- doConcentratePCs(PCAS[i]$object, PCAS[i]$pca, ResultDirectory = "")
    pics$col <- I(grobs)
    colnames(pics)[colnames(pics) == "col"] <- PCAS[i]$object
  }
  
  ft <- flextable(data.frame(matrix(ncol = length(significant_pcs), nrow = 1)))
  
  for (i in seq_along(significant_pcs)) {
    ft <- compose(ft, i = 1, j = i, value = as_paragraph(external_img(src = charts_list[[i]][[1]], width = 6, height = 4)))
  }
  
  doc <- body_add_flextable(doc, value = ft)
  
  print(doc, target = paste0(folder, "/", object, "_PrincipalComponents.docx"))
}


# Control parameters ###########################################################

#Umgebungsvariablen müssen gesetzt werden!
db_user <- Sys.getenv("DB_USER")
if (db_user == "") {
  stop(
    "Die Umgebungsvariable 'DB_USER' ist nicht gesetzt.\n",
    "Bitte die Umgebungsvariable definieren."
  )
}
pythonPath <- Sys.getenv("PYTHON_PATH")
if (WorkDirectory == "") {
  stop(
    "Die Umgebungsvariable 'PYTHON_PATH' ist nicht gesetzt.\n",
    "Bitte den Pfad als Umgebungsvariable definieren."
  )
}
WorkDirectory <- Sys.getenv("WORKDIRECTORY")
if (WorkDirectory == "") {
  stop(
    "Die Umgebungsvariable 'WORKDIRECTORY' ist nicht gesetzt.\n",
    "Bitte den Pfad als Umgebungsvariable definieren."
  )
}
dbLaboratoryData <- Sys.getenv("DB_LABORATORY_DATA")
if (dbLaboratoryData == "") {
  stop(
    "Die Umgebungsvariable 'DB_LABORATORY_DATA' ist nicht gesetzt.\n",
    "Bitte den Pfad zur Access-Datenbank (*.accdb) als Umgebungsvariable definieren."
  )
}
dbO2_Redox <- <- Sys.getenv("DB_REDOX")
if (dbO2_Redox == "") {
  stop(
    "Die Umgebungsvariable 'DB_REDOX' ist nicht gesetzt.\n",
    "Bitte den Pfad zur Access-Datenbank (*.accdb) als Umgebungsvariable definieren."
  )
}


FirstDay <- as.POSIXct("2020-03-01", format = "%Y-%m-%d")
LastDay <- as.POSIXct("2024-12-01", format = "%Y-%m-%d")

maxgap_Labor <- 35
maxgap_Driver <- 4

ParPlotLineWidth <- 0.5
ParPlotTitle <- FALSE
ParPlotXachseVisible <- TRUE
ParPlotYachseVisible <- TRUE


BaseDataNames <- c("Datum", "Messstelle", "Tiefe", "AG-Nr")
MeltDataNames <- c("Datum", "AG-Nr", "Messstelle", "Tiefe")

ComponentesVariables <- c("Calcium",         "Sodium",          "pH",
                          "Iron",            "Manganese",       "Ammonium", 
                          "Nitrate",         "Potassium",       "Chloride",       
                          "Sulfate",         "Magnesium",       "Bicarbonate",  
                          "Redox potential", "Oxygen",          "Carbon dioxide"
)

DriverVariables <- c("Precipitation",              "Evapotranspiration", 
                     "Air temperature",            "Leaf area index",
                     "Climatic water balance",     "Groundwater flow",
                     "Groundwater below surface",  "Soil moisture",                     
                     "Soil temperature",           "pF",
                     "Pressure head",              "psi"
)
ExcludeDriverVariables <- c(  
  "Air temperature", 
  "Orthophosphate",
  "Orthophosphate Phosphorus",
  "o_PO4", "o_PO4_P",
  "pF", 
  "psi",
  "Soil moisture",  
  "cum Precipitation",          "cum Groundwater flow", 
  "cum Evapotranspiration",     "cum Climatic water balance",
  "Pre-Evapotranspiration 40",  "Pre-Precipitation 40",
  "Pre-Groundwater flow 40",    "Pre-Climatic water balance 40",
  "Pre-Evapotranspiration 30",  "Pre-Precipitation 30",
  "Pre-Groundwater flow 30",    "Pre-Climatic water balance 30"
)




withInterpolation <- FALSE
InterpolationNameSign <- "_interpolation"
ResultDirectory  <- getResultDirectory(withInterpolation, paste0(WorkDirectory, "results\\"))


#Tabellen der Mess- und Analysedaten
dtLysimeterMeasurementData <- c("lysikorr", "ts-Vgl_Lysi-d")

dtLy2_Sentec <- c("lysikorr", "lys_4282sensor_d")

dtLAI <- "data komplettiert/LAI.csv"

dtLaboratoryData <- "tab_DatenLabor"
dtTheta_T_ly1 <- "tab_EnviroScanLy1_neu_d"
dtTheta_T_ly2 <- "tab_EnviroScanLy2_neu_d"
dtTheta_T_ly3 <- "tab_EnviroScanLy3_neu_d"

dtO2_Redox <-  "Tageswerte"

