#
#Article:		Impacts of groundwater level management on geochemical processes at an agricultural site with shallow groundwater
#Journal:		Environmental Earth Sciences
#Authors:		Jörg Steidl, Ottfried Dietrich, Christoph Merz
#Corresponding: jsteidl@zalf.de
#
### DataBase ###################################################################
ReadTable <- function(dbconnection, Tablename, Msg = FALSE)
{
  if (is.null(dbconnection)) {
    print("ViewExists: dbconnection ist leer!.")
  }
  
  if (Msg)
    print(paste(paste(Tablename, collapse = "."), "is read."))
  dt <- dbReadTable(dbconnection, Tablename)
  dbDisconnect(dbconnection)
  return (dt)
}
ReadMeasuringDataTable <- function(dbconnection, Tablename, Msg = FALSE)
{
  return (ReadDataTable(dbconnection, Tablename, Msg = TRUE))
}


ReadDataTable <- function(Tablename, dbconnection, Msg = FALSE)
{
  if (is.null(dbconnection)) {
    print("ViewExists: dbconnection funktioniert nicht!.")
  }
  
  con <- dbconnection
  if (is.null(con)) {
    print("ViewExists: dbconnection funktioniert nicht!.")
  }
  
  if (Msg)
    print(paste(paste(Tablename, collapse = "."), "is read."))
  dt <- dbReadTable(conn=con, Tablename)
  dbDisconnect(con)
  return (dt)
}
ViewExists <- function(Schema, ViewName, dbconnection)
{
  if (is.null(dbconnection)) {
    print("ViewExists: dbconnection funktioniert nicht!.")
  }
  
  con <- dbconnection
  if (is.null(con)) {
    print("ViewExists: dbconnection funktioniert nicht!.")
  }
  
  view <- NULL
  tryCatch({
    query <- glue::glue("SELECT viewname FROM pg_views WHERE schemaname = '{Schema}' AND viewname = '{ViewName}'")
    view <- dbGetQuery(con, query)$viewname
  }, error = function(e) {
    print(paste("Fehler:", e$message))
  })
  dbDisconnect(con)
  
  table_exists = TRUE
  if (is.null(view) || length(view) == 0)
    table_exists = FALSE
  if (!table_exists)
    print(paste("The table", c(Schema, ViewName), " does not exists, but is a prerequisite for the following presentation!"))
  return(table_exists)
}
SelectDataTable <- function(selectStr, dbconnection,  Msg = FALSE)
{  
  if (is.null(dbconnection)) {
    print("ViewExists: dbconnection funktioniert nicht!.")
  }
  con <- dbconnection
  if (is.null(con)) {
    print("SelectDataTable: dbconnection funktioniert nicht!.")
  }
  
  if (Msg)
    print(paste(selectStr, "is read."))
  print(selectStr)
  dt <- dbGetQuery(dbconnection, selectStr)
  
  dbDisconnect(dbconnection)
  return (dt)
}

SelectDataTable <- function(selectStr, dbconnection,  Msg = FALSE)
{  
  if (Msg)
    print(paste(selectStr, "is read."))
  print(selectStr)
  dt <- dbGetQuery(dbconnection, selectStr)
  
  dbDisconnect(dbconnection)
  return (dt)
}

save_plot <- function(GraficalObject, filename, paperformat = "A4-landscape") {
  if (paperformat == "A3-landscape") {
    width <- 16.5
    height <- 11.7
  } else if (paperformat == "A4-landscape") {
    width <- 11.7
    height <- 8.3
  } else if (paperformat == "A5-landscape") {
    width <- 8.3
    height <- 5.8
  } else if (paperformat == "A3-portrait") {
    height <- 16.5
    width <- 11.7
  } else if (format == "A4-portrait") {
    height <- 11.7
    width <- 8.3
  } else if (paperformat == "A5-portrait") {
    height <- 8.3
    width <- 5.8
  } else if (!is.null(papierformat)) {
    pint("Invalid paper format specified.")
  }
  
  
  if (tolower(tools::file_ext(filename)) == "pdf") {
    fileformat <- "pdf"
  } else if (tolower(tools::file_ext(filename)) %in% c("jpg", "jpeg")) {
    fileformat <- "jpg"
  } else {
    print(paste("The file format", fileformat, "is not supported."))
  }
  
  if (fileformat == "pdf") {
    if (is.null(paperformat)) {
      pdf(filename)
    } else pdf(filename, width = width, height = height)
  } else if (fileformat == "jpg") {
    if (is.null(paperformat)) {
      jpeg(filename)
    } else jpeg(filename, width = width, height = height)
  }
  
  try({
    plot(GraficalObject)
    print(paste("The file ", filename, " is written!"))
  }, silent = TRUE)
  dev.off()
}

save_data <- function(object, folder, data)
{
  folder = paste0(folder, object, "/")
  if (dir.exists(folder) == FALSE) 
  {
    dir.create(folder)
    cat(paste("\tZielverzeichnis:: ", folder, "wurde angelegt!\n"))
  }
  else {
    cat("Zielverzeichnis: ", folder, "\n")
  }
  
  if (dir.exists(folder) == FALSE) {
    cat("Das Zielverzeichnis ", folder, " existiert nicht!\n")
    cat("Es werden keine Daten gesichert!")
    return()
  }
  
  excel_file <-paste0(folder, object, ".xlsx")
  save_excel(excel_file, data)
  
  if (is.null(object))
    filename <- paste0("Lysimeters correlation analysed data")
  else
    filename <- paste0(object, " correlation analysed data")
  
  
  correlations(object, folder, data[, !(colnames(data) %in% c("AG-Nr", "Datum", "Messstelle", "Tiefe"))], filename)
  
  PlotData(object, folder, data)
}


save_excel <- function(filename, data)
{
  excel_file <- filename
  write.xlsx(data, file = excel_file, rowNames = FALSE)
  
}

read_data <- function(filename)
{
  excel_file <- filename
  cat("Die Daten werden aus dieser Datei gelesen: ", excel_file, "\n")
  df <- as.data.frame(read.xlsx(excel_file))
  colnames(df) <- gsub(".", " ", colnames(df), fixed = TRUE)
  return((df))
}
#### Date testing ##############################################################
#### Plot data ###############################################################
PlotData <- function(Object, folder, Data)
{
  path <- folder  
  folder = path
  
  if (dir.exists(folder) == FALSE) {
    cat("Das Zielverzeichnis ", folder, " existiert nicht!\n")
    cat("Es werden keine Daten geplottet!")
    return()
  }
  
  cols <- !(colnames(Data) %in% DriverVariables)
  
  data <- Data[, cols]
  data <- melt(data, id.vars = MeltDataNames,
               variable.name = "Stoff", value.name = "Konzentration")
  gg <- ggplot(data, aes(x = interaction(Messstelle, Tiefe), y = Konzentration, fill = Messstelle)) +
    geom_violin(scale = "width", trim = TRUE) +
    geom_boxplot(width=0.1, col="black", fill="white", outlier.shape = NA) +
    facet_wrap(~ Stoff, scales = "free_y") +
    scale_fill_brewer(palette = "Pastel1", name = "Lysimeter") +  # oder verwenden Sie eine andere Palette
    labs(title = "Lysimeters: analysed laboratory data",
         subtitle = "Concentration of various substances by depth and lysimeter",
         x = "Depth in m",
         y = "Value") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  if (is.null(Object))
    filename <- paste0(path, "Lysimeters analyzed laboratory data.pdf")
  else
    filename <- paste0(path, Object, " analysed laboratory data.pdf")
  cat(paste("\tDatei: ", filename, "wird geschrieben!\n"))
  ggsave(filename, gg, width = 297, height = 210, units = "mm", dpi = 300)
  
  data$Datum <- as.POSIXct(data$Datum)
  
  filtered_data <- data %>%
    group_by(Stoff) %>%
    filter(all(Konzentration != -1))
  stoffe <- unique(filtered_data$Stoff)
  
  farben <- c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#00FFFF", "#FF00FF", "#C0C0C0", "#808080", "#800000", "#008000", "#000080", "#800080",  "#808070", "#800040", "#008020", "#008090")
  
  names(farben) <- stoffe
  gg <-  ggplot(data, aes(x = Datum, y = Konzentration, color = Stoff)) +
    geom_point() +
    facet_wrap(~ `AG-Nr`, scales = "free_y", ncol = 1) +  
    scale_color_manual(values  = farben, name = "Substance") +  
    scale_x_datetime(date_labels = "%b %Y", date_breaks = "1 month") +
    labs(title = "Lysimeters: analysed laboratory data",
         subtitle = "Concentration of various substances by time",
         x = "Date",
         y = "Value",
         color = "Stoff") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  if (is.null(Object))
    filename <- paste0(path, "Check lysimeters analysed laboratory data.pdf")
  else
    filename <- paste0(path, "Check ", Object, " analysed laboratory data.pdf")
  cat(paste("\tDatei: ", filename, "wird geschrieben!\n"))
  ggsave(filename, gg, width = 297, height = 210, units = "mm", dpi = 300)
  
  cols <- (colnames(Data) %in% c(MeltDataNames, DriverVariables))
  data <- Data[, cols]
  
  data <- melt(data, id.vars = c(MeltDataNames),
               variable.name = "Stoff", value.name = "Konzentration")
  gg <- ggplot(data, aes(x = interaction(Messstelle, Tiefe), y = Konzentration, fill = Messstelle)) +
    geom_violin(scale = "width", trim = TRUE) +
    geom_boxplot(width=0.1, col="black", fill="white", outlier.shape = NA) +
    facet_wrap(~ Stoff, scales = "free_y") +
    scale_fill_brewer(name = "Paramaeter") +  # oder verwenden Sie eine andere Palette
    labs(title = "Lysimeters: analysed driver data",
         subtitle = "Component drivers by depth and lysimeter",
         x = "Depth in m",
         y = "Value") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  if (is.null(Object))
    filename <- paste0(path, "Lysimeters analysed driver data.pdf")
  else
    filename <- paste0(path, Object, " analysed driver data.pdf")
  cat(paste("\tDatei: ", filename, "wird geschrieben!\n"))
  ggsave(filename, gg, width = 297, height = 210, units = "mm", dpi = 300)
  
  data$Datum <- as.POSIXct(data$Datum)
  gg <-  ggplot(data, aes(x = Datum, y = Konzentration, color = Stoff)) +
    geom_point() +
    facet_wrap(~ `AG-Nr`, scales = "free_y", ncol = 1) +  
    scale_color_brewer(palette = "PuBu", name = "Parameter") +  
    scale_x_datetime(date_labels = "%b %Y", date_breaks = "1 month") +
    labs(title = "Lysimeters: analysed driver data",
         subtitle = "Component drivers by time",
         x = "Date",
         y = "Value",
         color = "Stoff") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  if (is.null(Object))
    filename <- paste0(path, "Check lysimeters analysed driver data.pdf")
  else
    filename <- paste0(path, "Check ", Object, " analysed driver data.pdf")
  cat(paste("\tDatei: ", filename, "wird geschrieben!\n"))
  ggsave(filename, gg, width = 297, height = 210, units = "mm", dpi = 300)
}

PlotLaborData <- function(Object, folder, Data)
{
  if (is.null(Object))
    path <- folder
  else
    path <- paste0(folder, Object, "/")
  
  folder = path
  if (dir.exists(folder) == FALSE) {
    cat("Das Zielverzeichnis ", folder, " existiert nicht!\n")
    cat("Es werden keine Daten geplottet!")
    return()
  }
  
  data <- Data[!grepl("Ly4", Data$`AG-Nr`), ]
  data <- data[!grepl("GW", data$`AG-Nr`), ]
  data <- data[!grepl("Precip", data$`AG-Nr`), ]
  data <- data[!grepl("\\A|A$|A[^0-9]", data$`AG-Nr`), ]
  
  data <- melt(data, id.vars = c("Datum", "AG-Nr", "Messstelle", "Tiefe"),
               variable.name = "Stoff", value.name = "Konzentration")
  data <- data %>%
    filter(!is.na(Konzentration))
  
  gg <- ggplot(data, aes(x=factor(interaction(Messstelle, Tiefe), levels = sort(unique(interaction(Messstelle, Tiefe)))), y = Konzentration, fill = Messstelle)) +
    geom_violin(scale = "width", trim = TRUE) +
    geom_boxplot(width=0.1, col="black", fill="white", outlier.shape = NA) +
    facet_wrap(~ Stoff, scales = "free_y") +
    scale_fill_brewer(palette = "Pastel1", name = "Lysimeter") +  # oder verwenden Sie eine andere Palette
    labs(title = "Lysimeters: complete laboratory data",
         subtitle = "Concentration of various substances by depth and lysimeter",
         x = "Depth in m",
         y = "Value") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  filename <- paste0(path, "Lysimeters original laboratory data.pdf")
  cat(paste("\tDatei: ", filename, "wird geschrieben!\n"))
  ggsave(filename, gg, width = 297, height = 210, units = "mm", dpi = 300)
  
  filtered_data <- data %>%
    group_by(Stoff) %>%
    filter(all(Konzentration != -1))
  
  stoffe <- unique(filtered_data$Stoff)
  farben <- c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#00FFFF", "#FF00FF", "#C0C0C0", "#808080", "#800000", "#008000", "#000080", "#800080",  "#808070", "#800040", "#008020", "#008025")
  names(farben) <- stoffe
  gg <-  ggplot(data, aes(x = Datum, y = Konzentration, color = Stoff)) +
    geom_point() +
    facet_wrap(~ `AG-Nr`, scales = "free_y", ncol = 1) +  # Nach AG-Nr facetieren
    scale_color_manual(values  = farben, name = "Substance") +  # Farbpalette einstellen
    scale_x_datetime(date_labels = "%b %Y", date_breaks = "1 month") +
    labs(title = "Lysimeters: complete laboratory data",
         subtitle = "Concentration of various substances by time",
         x = "Date",
         y = "Value",
         color = "Stoff") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  filename <- paste0(path, "Check lysimeters original laboratory data.pdf")
  cat(paste("\tDatei: ", filename, "wird geschrieben!\n"))
  ggsave(filename, gg, width = 297, height = 210, units = "mm", dpi = 300)
}

NV_Test <- function(data)
{
  ex <- c("Datum", "AG-Nr", "Messstelle", "Tiefe" )
  data <- data[, !(colnames(data) %in% ex)]
  
  column_names <- names(data)
  
  
  for (col in column_names) {
    shapiro_test_result <- shapiro.test(data[[col]])
    test_statistic <- shapiro_test_result$statistic
    p_value <- shapiro_test_result$p.value
    alpha <- 0.05
    
    if (p_value < alpha) {
      cat("Für die Spalte '", col, "': Die Nullhypothese wird abgelehnt. Die Daten sind nicht normalverteilt.\n", sep = "")
    } else {
      cat("Für die Spalte '", col, "': Die Nullhypothese kann nicht abgelehnt werden. Die Daten könnten normalverteilt sein.\n", sep = "")
    }
    
    cat("Teststatistikwert:", test_statistic, "\n")
    cat("p-Wert:", p_value, "\n\n")
  }
}
correlations <- function(Object, folder, data, 
                         filename = "Correlation")
{
  filename <- paste0(folder, filename, ".pdf")
  cat("\tDatei ", filename, " wird gespeichert!\n")
  pdf(filename)
  corrplot(cor(data.matrix(data)), method="circle", type="lower", tl.cex = 0.75)
  dev.off()
}
PlotModells <- function(Object, folder, data, X, Y, scales, filename)
{
  if (is.null(Object))
    path <- folder
  else
    path <- paste0(folder, Object, "/")
  
  x_col <- names(data)[4]
  y_col <- names(data)[5]
  
  coefficients <- data %>%
    group_by(Messstelle, Tiefe, AG_Nr) %>%
    do({
      model <- lm(reformulate(x_col, y_col), data = .)
      data.frame(
        Messstelle = unique(.$Messstelle),
        Tiefe = unique(.$Tiefe),
        AG_Nr = unique(.$AG_Nr),
        Intercept = coef(model)[1],
        Slope = coef(model)[2],
        R2 = summary(model)$r.squared,
        N = nrow(.)
      )
    }) %>%
    ungroup() %>%
    distinct(Messstelle, Tiefe, AG_Nr, .keep_all = TRUE)
  
  gg <- ggplot(data, aes_string(x = x_col, y = y_col, color = "as.factor(Tiefe)")) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(~ AG_Nr, scales = scales) +
    scale_color_brewer(palette = "Set1", name = "Tiefe") +
    labs(
      title = "Regression Curve",
      subtitle = "per lysimeter und depth",
      x = X,
      y = Y
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5),  
      axis.title.x = element_text(size = 12, angle = 0, hjust = 0.5), 
      axis.title.y = element_text(size = 12), 
      strip.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
      legend.text = element_text(size = 12)
    ) +
    geom_text(
      data = coefficients,
      aes(x = Inf, y = -Inf,
          label = paste(#"Intercept:", round(Intercept, 2),
            #"\nSlope:", round(Slope, 2),
            "\nR²:", round(R2, 2), " n=", N)),
      hjust = 1.1, vjust = -0.1, size = 3, color = "black"
    )
  filename <- paste0(path, filename, ".pdf")
  cat(paste("\tDatei: ", filename, "wird geschrieben!\n"))
  ggsave(filename, gg, width = 297, height = 210, units = "mm", dpi = 300)
  
  return(coefficients)
}
#### Read in and configure data ################################################
soil_temperatures_moistures <- function(df, excludedColNames = c(), 
                                        withInterpolation = FALSE, 
                                        maxgap_Labor = 5,
                                        maxgap_Driver = 30) 
{
  if (!"Soil temperature" %in% colnames(df)) {
    df$`Soil temperature` <- NA
  }
  
  if (!"Soil moisture" %in% colnames(df)) {
    df$`Soil moisture` <- NA
  }
  
  if (!"Pressure head" %in% colnames(df)) {
    df$`Pressure head` <- NA
  } #  kPa
  
  for (i in 1:nrow(df)) {
    if (!is.na(df$Messstelle[i]) && !is.na(df$Mst[i]) && !is.na(df$Ca[i])) {
      if (df$Messstelle[i] == df$Mst[i]) {
        if (df$Tiefe[i] == 30) {
          df$`Soil temperature`[i] <- df$`T-30`[i]
          if ("T30" %in% colnames(df)) {
            df$`Soil moisture`[i] <- df$`T30`[i]
          }
          if ("Psi-30" %in% colnames(df)) {
            df$`Pressure head`[i] <- df$`Psi-30`[i]
          }
        }
        if (df$Tiefe[i] == 60) {
          df$`Soil temperature`[i] <- df$`T-60`[i]
          if ("T60" %in% colnames(df)) {
            df$`Soil moisture`[i] <- df$`T60`[i]
          }
          if ("Psi-60" %in% colnames(df)) {
            df$`Pressure head`[i] <- df$`Psi-60`[i]
          }
        }
        if (df$Tiefe[i] == 90) {
          df$`Soil temperature`[i] <- df$`T-90`[i]
          if ("T90" %in% colnames(df)) {
            df$`Soil moisture`[i] <- df$`T90`[i]
          }        
          if ("Psi-90" %in% colnames(df)) {
            df$`Pressure head`[i] <- df$`Psi-90`[i]
          }
        }
      }
    }
  }
  
  interpolate_na_column <- function(data, column, max_gap = 4) {
    data$Datum <- as.Date(data$Datum)
    
    interpolate_na <- function(x) {
      if (all(is.na(x)) || all(is.nan(x))) {
        return(rep(NA, length(x)))
      } else {
        return(na.approx(x, maxgap = max_gap, na.rm = FALSE))
      }
    }
    
    data <- data %>%
      arrange(Messstelle, Tiefe, Datum) %>%
      group_by(Messstelle, Tiefe) %>%
      mutate(!!column := interpolate_na(.data[[column]])) %>%
      ungroup()
    
    return(data)
  }
  
  if (withInterpolation == TRUE) {
    df <- df %>%
      group_modify(~ interpolate_na_column(.x, "Soil temperature", maxgap_Driver)) %>%
      ungroup()
    df <- df %>% 
      group_modify(~ interpolate_na_column(.x, "Soil moisture", maxgap_Driver)) %>%
      ungroup()  
    df <- df %>% 
      group_modify(~ interpolate_na_column(.x, "HCO3", maxgap_Driver)) %>%
      ungroup()
    
    #Laborwerte
    df <- df %>% 
      group_modify(~ interpolate_na_column(.x, "NH4_N", maxgap_Labor)) %>%
      ungroup()
    df <- df %>% 
      group_modify(~ interpolate_na_column(.x, "NO3_N", maxgap_Labor)) %>%
      ungroup()
    df <- df %>% 
      group_modify(~ interpolate_na_column(.x, "pH", maxgap_Labor)) %>%
      ungroup()
  }
  
  if (length(excludedColNames) == 0) {
    return(df)
  }
  
  excludedColNames <- c(excludedColNames, "Mst", "Precipitation", 
                        "gwf1", "gwf2", "gwf3",
                        "p1", "p2", "p3",
                        "Psi-30",  "Psi-60",  "Psi-90",
                        "Theta-30",  "Theta-60",  "Theta-90",
                        "T-30",  "T-60",  "T-90",
                        "T10", "T20", "T30", "T40", "T50", "T60", "T70", "T80", "T90", "T120",
                        "lysi3_masse", "bilanz3")
  df <- df[, !(colnames(df) %in% excludedColNames)]
  return(df)
}
oxi_redox_column <- function(df, Object, tiefe) {
  spaltenname_oxi <- paste0(Object, "_", tiefe, "oxi")
  
  if (!(spaltenname_oxi %in% colnames(df))) {
    stop(paste("Die Spalte", spaltenname_oxi, "existiert nicht im DataFrame."))
  }
  
  spaltenname_redox <- paste0(Object, "_", tiefe, "redox")
  
  if (!(spaltenname_redox %in% colnames(df))) {
    stop(paste("Die Spalte", spaltenname_redox, "existiert nicht im DataFrame."))
  }  
  
  if (!("Oxi" %in% colnames(df))) {
    df$Oxi <- NA
  }
  
  if (!("Redox" %in% colnames(df))) {
    df$Redox <- NA
  }
  
  condition <- !is.na(df$Messstelle) & !is.na(df$Tiefe) & df$Messstelle == Object & df$Tiefe == tiefe
  
  df$Oxi[condition] <- df[[spaltenname_oxi]][condition]
  df$Redox[condition] <- df[[spaltenname_redox]][condition]
  
  return(df)
}
lysimeter_part <- function(df, Object = NA, tiefe = NA)
{
  if (is.na(Object)) {
    if (!is.na(tiefe)) {
      d <- subset(df, Tiefe == tiefe)
    } else {
      d <- df
    }
  } else {
    if (!is.na(tiefe)) {
      d <- subset(df, Tiefe == tiefe & Messstelle == Object)
    } else {
      d <- subset(df, Messstelle == Object)
    }
  }
  return(d)
}
renaming <- function(water, lai, data)
{
  names(data)[names(data) == "Theta_Ly1"] <- "Theta"
  names(data)[names(data) == "ThetaLy2"] <- "Theta"
  names(data)[names(data) == "Theta_Ly3"] <- "Theta"
  names(data)[names(data) == "Theta"] <- "Soil moisture"
  
  names(data)[names(data) == "Tsoil_Ly1"] <- "Tsoil"
  names(data)[names(data) == "TsoilLy2_30"] <- "Tsoil"
  names(data)[names(data) == "Tsoil_Ly3"] <- "Tsoil"
  names(data)[names(data) == "Tsoil"] <- "Soil temperature"
  
  if ('GW' %in% data$Messstelle) {
    lai$Lai = lai$Lai1
  }
  if ('Ly1' %in% data$Messstelle) {
    lai$Lai = lai$Lai1
  }
  if ('Ly2' %in% data$Messstelle) {
    lai$Lai = lai$Lai2
  }
  if ('Ly3' %in% data$Messstelle) {
    lai$Lai = lai$Lai3
  }
  
  if ('Soil temperature' %in% colnames(data))
    data <- data %>%
    select(-"Soil temperature", everything()) %>%
    select(everything(), "Soil temperature")  
  if ('Groundwater below surface' %in% colnames(data))
    data <- data %>%
    select(-`Groundwater below surface`, everything()) %>%
    select(everything(), `Groundwater below surface`)
  
  data <- boundaries(water, lai, data)
  
  names(data)[names(data) == "Lai"] <- "Leaf area index"
  names(data)[names(data) == "SO4"] <- "Sulfate"
  
  names(data)[names(data) == "Ly1_redox"] <- "Redox"
  names(data)[names(data) == "Ly2_redox"] <- "Redox"
  names(data)[names(data) == "Ly3_redox"] <- "Redox"
  names(data)[names(data) == "Redox"] <- "Redox"
  
  names(data)[names(data) == "Redox"] <- "Redox potential"
  
  names(data)[names(data) == "Ly1_ox"] <- "Oxi"
  names(data)[names(data) == "Ly2_oxi"] <- "Oxi"
  names(data)[names(data) == "Ly3_oxi"] <- "Oxi"
  names(data)[names(data) == "Oxi"] <- "Oxygen"
  
  names(data)[names(data) == "NO3"] <- "Nitrate"
  names(data)[names(data) == "NO3_N"] <- "Nitrate"
  
  names(data)[names(data) == "DOC"] <- "Dissolved organic carbon"
  names(data)[names(data) == "o_PO4"] <- "Orthophosphate"
  names(data)[names(data) == "o_PO4_P"] <- "Orthophosphate Phosphorus"
  names(data)[names(data) == "NH4_N"] <- "Ammonium"
  names(data)[names(data) == "LF"] <- "Conductivity" 
  
  names(data)[names(data) == "Na"] <- "Sodium"
  names(data)[names(data) == "Mn"] <- "Manganese"
  names(data)[names(data) == "Mg"] <- "Magnesium"
  names(data)[names(data) == "K"] <- "Potassium"
  names(data)[names(data) == "HCO3"] <- "Bicarbonate"
  names(data)[names(data) == "Fe"] <- "Iron"
  names(data)[names(data) == "Cl"] <- "Chloride"
  names(data)[names(data) == "Ca"] <- "Calcium"
  
  
  Vars <- colnames(data)  #"Datum", "Messstelle"
  Namen <- c("AG.Nr", "Lai1", "Lai2", "Lai3") #, "Tiefe", "gwf3")
  data <- data[, !(Vars %in% c("AG.Nr", "Lai1", "Lai2", "Lai3"))]
  return(data)
}
boundaries <- function(water, lai, ly)
{ 
  if (nrow(ly) == 0) {
    stop("Data enthält keine Zeilen!\nDas Script wurde gestoppt!!!")
  }
  Vars <- colnames(water)
  Namen <- c("p")
  
  if (ly$Messstelle[1] =='Ly1')
  {
    water <- water[, (colnames(water) %in% c("zeit", "bilanz1", "eta1", "p1", "t", "gwf1", "kliwa1",
                                             "cum_kliwa1", "cum_p1", "cum_eta1", "cum_bilanz1"))]
    names(water)[names(water) == "bilanz1"] <- "bilanz"
    names(water)[names(water) == "eta1"] <- "eta"
    names(water)[names(water) == "p1"] <- "p"
    names(water)[names(water) == "gwf1"] <- "Groundwater below surface"
    names(water)[names(water) == "kliwa1"] <- "kliwa"
    names(water)[names(water) == "cum_p1"] <- "cum_p"
    names(water)[names(water) == "cum_bilanz1"] <- "cum_bilanz"
    names(water)[names(water) == "cum_eta1"] <- "cum_eta"
    names(water)[names(water) == "cum_kliwa1"] <- "cum_kliwa"
  }
  if (ly$Messstelle[1] =='Ly2')
  {
    water <- water[, (colnames(water) %in% c("zeit", "bilanz2", "eta2", "p2", "t", "gwf2", "kliwa2",
                                             "cum_kliwa2", "cum_p2", "cum_eta2", "cum_bilanz2"))]
    names(water)[names(water) == "bilanz2"] <- "bilanz"
    names(water)[names(water) == "eta2"] <- "eta"
    names(water)[names(water) == "p2"] <- "p"
    names(water)[names(water) == "gwf2"] <- "Groundwater below surface"
    names(water)[names(water) == "kliwa2"] <- "kliwa"
    names(water)[names(water) == "cum_p2"] <- "cum_p"
    names(water)[names(water) == "cum_bilanz2"] <- "cum_bilanz"
    names(water)[names(water) == "cum_eta2"] <- "cum_eta"
    names(water)[names(water) == "cum_kliwa2"] <- "cum_kliwa"
  }
  if (ly$Messstelle[1] =='Ly3')
  {
    water <- water[, (colnames(water) %in% c("zeit", "bilanz3", "eta3", "p3", "t", "gwf3",  "kliwa3",
                                             "cum_kliwa3", "cum_p3", "cum_eta3", "cum_bilanz3"))]
    names(water)[names(water) == "bilanz3"] <- "bilanz"
    names(water)[names(water) == "eta3"] <- "eta"
    names(water)[names(water) == "p3"] <- "p"
    names(water)[names(water) == "gwf3"] <- "Groundwater below surface"
    names(water)[names(water) == "kliwa3"] <- "kliwa"
    names(water)[names(water) == "cum_p3"] <- "cum_p"
    names(water)[names(water) == "cum_bilanz3"] <- "cum_bilanz"
    names(water)[names(water) == "cum_eta3"] <- "cum_eta"
    names(water)[names(water) == "cum_kliwa3"] <- "cum_kliwa"
  }
  
  if (ly$Messstelle[1] =='GW')
  {
    water <- water[, (colnames(water) %in% c("zeit", "bilanz3", "etgras", "p", "t", "gwfr",  "kliwa3",
                                             "cum_kliwa3", "cum_p3", "cum_eta3", "cum_bilanz3"))]
    names(water)[names(water) == "bilanz3"] <- "bilanz"
    names(water)[names(water) == "etgras"] <- "eta"
    names(water)[names(water) == "p"] <- "p"
    names(water)[names(water) == "gwfr"] <- "Groundwater below surface"
    water$bilanz <- water$p - water$eta
    names(water)[names(water) == "kliwa3"] <- "kliwa"
    names(water)[names(water) == "cum_p3"] <- "cum_p"
    names(water)[names(water) == "cum_bilanz3"] <- "cum_bilanz"
    names(water)[names(water) == "cum_eta3"] <- "cum_eta"
    names(water)[names(water) == "cum_kliwa3"] <- "cum_kliwa"
  }
  
  if (ly$Tiefe[1] =='A')
  {
    water <- water[, (colnames(water) %in% c("zeit", "bilanz3", "etgras", "p", "t", "gwfr",  "kliwa3",
                                             "cum_kliwa3", "cum_p3", "cum_eta3", "cum_bilanz3"))]
    names(water)[names(water) == "bilanz3"] <- "bilanz"
    names(water)[names(water) == "etgras"] <- "eta"
    names(water)[names(water) == "p"] <- "p"
    names(water)[names(water) == "gwfr"] <- "Groundwater below surface"
    
    #water$bilanz <- water$p - water$eta
    names(water)[names(water) == "kliwa3"] <- "kliwa"
    names(water)[names(water) == "cum_p3"] <- "cum_p"
    names(water)[names(water) == "cum_bilanz3"] <- "cum_bilanz"
    names(water)[names(water) == "cum_eta3"] <- "cum_eta"
    names(water)[names(water) == "cum_kliwa3"] <- "cum_kliwa"
  }
  
  ly$Datum <- as.Date(ly$Datum, format = "%d- %b %y")
  
  colnames(water)[colnames(water) == "eta"] <- "Evapotranspiration"
  colnames(water)[colnames(water) == "p"] <- "Precipitation"
  colnames(water)[colnames(water) == "t"] <- "Air temperature"
  
  colnames(water)[names(water) == "kliwa"] <- "Climatic water balance"
  colnames(water)[names(water) == "bilanz"] <- "Groundwater flow"
  colnames(water)[names(water) == "cum_p"] <- "cum Precipitation"
  colnames(water)[names(water) == "cum_bilanz"] <- "cum Groundwater flow"
  colnames(water)[names(water) == "cum_eta"] <- "cum Evapotranspiration"
  colnames(water)[names(water) == "cum_kliwa"] <- "cum Climatic water balance"
  
  ly <- merge(ly, water,  by.x = "Datum", by.y = "zeit")
  ly <- Reduce(function(x, y) merge(x, y, by = "Datum", all = FALSE), list(ly, lai))
  ly <- ly[order(ly$Datum), ]
  return(ly)
}

calculate_cumulative_hyyear <- function(data, columne) {
  col_sym <- sym(columne)
  date_sym <- sym("zeit")
  
  cum_col_name <- paste0("cum_", columne)
  cum_col_sym <- sym(cum_col_name)
  
  data_cumulative <- data %>%
    mutate(year = ifelse(month(!!date_sym) >= 11, year(!!date_sym) + 1, year(!!date_sym))) %>%
    arrange(year, !!date_sym) %>%
    group_by(year) %>%
    mutate(!!cum_col_sym := cumsum(!!col_sym))
  
  return(data_cumulative)
}
calculate_cumulative_probes <- function(lywas, columne, spw_labor, tage = 40) {
  col_sym <- sym(columne)
  date_sym <- sym("zeit")
  
  if (grepl("^eta", columne)) {
    cum_col_name <- paste("Pre-Evapotranspiration", tage)
  } else if (grepl("^p", columne)) {
    cum_col_name <- paste("Pre-Precipitation", tage)
  } else if (grepl("^bilanz", columne)) {
    cum_col_name <- paste("Pre-Groundwater flow", tage)
  } else if (grepl("^kliwa", columne)) {
    cum_col_name <- paste("Pre-Climatic water balance", tage)
  } else {
    stop("Ungültiger Spaltenname. Muss mit 'eta' oder 'p' beginnen.")
  }
  
  data_cumulative <- spw_labor %>%
    group_by(Messstelle, Tiefe) %>%
    mutate(!!cum_col_name := sapply(1:n(), function(i) {
      current_date <- Datum[i]
      previous_date <- ifelse(i == 1, current_date - 1, Datum[i - 1])
      
      days_diff <- as.numeric(difftime(current_date, previous_date, units = "days"))
      if (days_diff > tage) {
        previous_date <- current_date - tage
      }
      
      sum(lywas[[col_sym]][lywas[[date_sym]] > previous_date & lywas[[date_sym]] <= current_date 
      ], na.rm = TRUE)
    }))
  return(data_cumulative)
}

Load_LysimeterData <- function(withInterpolation = TRUE, maxgap_Labor = 30, maxgap_Driver = 5)
{  
  folder <- getResultDirectory(withInterpolation, ResultDirectory)  
  if (dir.exists(folder) == FALSE) {
    dir.create(folder)
    cat(paste("\tZielverzeichnis:: ", folder, "wurde angelegt!\n"))
  } else {
    cat("Zielverzeichnis: ", folder, "\n")
  }
  if (dir.exists(folder) == FALSE) {
    dir.create(folder)
    cat(paste("\tZielverzeichnis:: ", folder, "wurde angelegt!\n"))
  } else {
    cat("Zielverzeichnis: ", folder, "\n")
  }
  
  
  #### DataBase# ###############################################################
  cat("Die Anmeldung wird benötigt, um die Tabelle 'lysikorr.ts-Vgl_Lysi-d' von hymdb zu laden!\n")
  
  db_user <- rstudioapi::askForPassword("Database username")
  password <- rstudioapi::askForPassword(paste("Database password for database user: ", db_user))
  if (is.null(password))  {
    cat("Ohne Passwort können keine Daten eingelesen werden!\n")
    return()
  }
  tryCatch({
    options(show.error.messages = FALSE)
    dbcon <- dbConnect(
      dbDriver("PostgreSQL"),
      dbname = "hymdb",
      host = "10.10.40.8",
      port = 5432,
      user = db_user,
      password = password
    )
    options(show.error.messages = TRUE)
    
    if (inherits(dbcon, "PostgreSQLConnection")) {
      cat("Verbindung zur Datenbank erfolgreich hergestellt!\n")
      cat("Die Daten werden eingelesen!\n")
    } else {
      cat("\033[31m", "Verbindung zur Datenbank fehlgeschlagen.\n", "\033[0m\n", sep = "")
      return()
    }
  }, error = function(e) {
    cat("\033[31m", "Fehler beim Verbinden zur Datenbank:", e$message, "\n", "\033[0m\n", sep = "")
    return()
  })
  
  
  #### Wasser ##################################################################
  cat("Messdaten der Lysimeter werden geladen!\n")
  
  lywas <-  dbReadTable(dbcon, dtLysimeterMeasurementData)
  
  lywas$kliwa1 <- lywas$p1 - lywas$eta1 
  lywas$kliwa2 <- lywas$p2 - lywas$eta2 
  lywas$kliwa3 <- lywas$p3 - lywas$eta3 
  lywas <- calculate_cumulative_hyyear(lywas, "kliwa1")
  lywas <- calculate_cumulative_hyyear(lywas, "kliwa2")
  lywas <- calculate_cumulative_hyyear(lywas, "kliwa3")
  
  lywas <- calculate_cumulative_hyyear(lywas, "bilanz1")
  lywas <- calculate_cumulative_hyyear(lywas, "bilanz2")
  lywas <- calculate_cumulative_hyyear(lywas, "bilanz3")
  
  lywas <- calculate_cumulative_hyyear(lywas, "p1")
  lywas <- calculate_cumulative_hyyear(lywas, "p2")
  lywas <- calculate_cumulative_hyyear(lywas, "p3")
  
  lywas <- calculate_cumulative_hyyear(lywas, "eta1")
  lywas <- calculate_cumulative_hyyear(lywas, "eta2")
  lywas <- calculate_cumulative_hyyear(lywas, "eta3")
  
  #### LAI #####################################################################
  cat("Daten zum LAI werden geladen!\n")
  lai <- read.csv(dtLAI, header = TRUE, sep = ';', fileEncoding = "ISO-8859-1")
  lai$Datum <- as.POSIXct(lai$Datum, format = "%d.%m.%Y")
  lai$Datum <- as.Date(lai$Datum)
  all_dates <- data.frame(Datum = seq(min(lai$Datum), max(lai$Datum), by = "day"))
  lai <- all_dates %>%
    left_join(lai, by = "Datum") %>%
    arrange(Datum)
  lai <- lai %>%
    mutate(across(Lysi1:Ref_außen, ~ na.approx(., x = Datum, na.rm = FALSE)))
  colnames(lai)[colnames(lai) == "Lysi1"] <- "Lai1"
  colnames(lai)[colnames(lai) == "Lysi2"] <- "Lai2"
  colnames(lai)[colnames(lai) == "Lysi3"] <- "Lai3"
  lai <- lai[, (colnames(lai) %in% list("Datum", "Lai1", "Lai2", "Lai3"))]
  lai <- data.frame(lai)
  
  #### Labor, Theta             ######################################################
  cat("Labordaten werden geladen!\n")
  
  conn <-  dbConnect(odbc::odbc(), driver = "{Microsoft Access Driver (*.mdb, *.accdb)}", dbq=dbLaboratoryData)
  query <- paste("SELECT * FROM", dtLaboratoryData)
  spw_labor <- dbGetQuery(conn, query)
  spw_labor <- spw_labor[, !(colnames(spw_labor) %in% c("o_PO4_P", "NO2_N", "O2a", "O2b", "Temp"))] #, "Redox"
  spw_labor$Datum <- as.POSIXct(spw_labor$Datum, format = "%Y-%m-%d")
  
  
  spw_labor$HCO3[spw_labor$Tiefe == "GW" | spw_labor$Tiefe == "A"] <- spw_labor$KS4_3[spw_labor$Tiefe == "GW" | spw_labor$Tiefe == "A"]
  spw_labor$HCO3 <- as.numeric(gsub(",", ".", spw_labor$HCO3))
  spw_labor$HCO3 <- as.numeric(spw_labor$HCO3)
  spw_labor <- spw_labor[, !(colnames(spw_labor) %in% c("KS4_3"))]
  spw_labor <- subset(spw_labor, Datum >= FirstDay & Datum <= LastDay)
  
  PlotLaborData(NULL, folder, spw_labor)
  
  cat("Daten zum Wassergehalt und zur Bodentemperatur werden geladen!\n")
  query <- paste("SELECT * FROM", dtTheta_T_ly1)
  evLy1 <- dbGetQuery(conn, query)
  excludedColNames <- colnames(evLy1)
  evLy1 <- evLy1 %>%
    mutate(Mst = 'Ly1')
  colnames(evLy1)[colnames(evLy1) == "Datum1"] <- "Datum"
  evLy1$Datum <- as.POSIXct(evLy1$Datum, format = "%Y-%m-%d", tz = "UTC")
  
  evLy11 <- dbReadTable(dbcon, c("lysikorr", "lys_4281sensor-d"))
  evLy11$Mst <- "Ly1"
  evLy11  <- evLy11 %>% rename(Datum = zeit)
  evLy11$Datum <- as.POSIXct(evLy11$Datum, format = "%Y-%m-%d", tz = "UTC")
  names(evLy11) <- gsub("\\.", "-", names(evLy11))
  
  query <- paste("SELECT * FROM", dtTheta_T_ly2)
  evLy2 <- dbGetQuery(conn, query)
  evLy2 <- evLy2 %>%
    mutate(Mst = 'Ly2')
  colnames(evLy2)[colnames(evLy2) == "Datum1"] <- "Datum"
  evLy2$Datum <- as.POSIXct(evLy2$Datum, format = "%Y-%m-%d", tz = "UTC")
  
  evLy21 <- dbReadTable(dbcon, c("lysikorr", "lys_4282sensor-d"))
  evLy21$Mst <- "Ly2"
  evLy21  <- evLy21 %>% rename(Datum = zeit)
  evLy21$Datum <- as.POSIXct(evLy21$Datum, format = "%Y-%m-%d", tz = "UTC")
  names(evLy21) <- gsub("\\.", "-", names(evLy21))
  
  
  query <- paste("SELECT * FROM", dtTheta_T_ly3)
  evLy3 <- dbGetQuery(conn, query)
  evLy3 <- evLy3 %>%
    mutate(Mst = 'Ly3')
  colnames(evLy3)[colnames(evLy3) == "Datum1"] <- "Datum"
  evLy3$Datum <- as.POSIXct(evLy3$Datum, format = "%Y-%m-%d", tz = "UTC")
  
  evLy31 <- dbReadTable(dbcon, c("lysikorr", "lys_4293sensor-d"))
  evLy31$Mst <- "Ly3"
  evLy31  <- evLy31 %>% rename(Datum = zeit)
  evLy31$Datum <- as.POSIXct(evLy31$Datum, format = "%Y-%m-%d", tz = "UTC")
  names(evLy31) <- gsub("\\.", "-", names(evLy31))
  
  dbDisconnect(conn)
  
  excludedColNames <- setdiff(excludedColNames, "Datum")
  evLy1 <- evLy1[, !names(evLy1) %in% c("batt", "m", "q")]
  evLy11 <- evLy11[, !names(evLy11) %in% c("batt", "m", "q")]
  evLy2 <- evLy2[, !names(evLy2) %in% c("batt", "m", "q")]
  evLy21 <- evLy21[, !names(evLy21) %in% c("batt", "m", "q")]
  evLy3 <- evLy3[, !names(evLy3) %in% c("batt", "m", "q")]
  evLy31 <- evLy31[, !names(evLy31) %in% c("batt", "m", "q")]
  
  lyw <- lywas
  colnames(lyw)[colnames(lyw) == "zeit"] <- "Datum"
  lyw <- lyw[, (colnames(lyw) %in% c("Datum", "p1", "p2", "p3"))]
  lyw$Datum <- as.POSIXct(lyw$Datum, format = "%Y-%m-%d", tz = "UTC")
  spw_labor$Datum <- as.POSIXct(spw_labor$Datum, format = "%Y-%m-%d", tz = "UTC")
  #CO2
  spw_labor$`Carbon dioxide` <- ifelse(is.na(spw_labor$`pH`), NA,
                                       ifelse(is.na(spw_labor$`HCO3`), NA, ((10^(-spw_labor$`pH`) * (spw_labor$`HCO3` / 1000)) / 10^(-6.352) / 10^(-1.468))))
  
  
  excel_file <-paste0(folder,"/Lysimeters original laboratory data.xlsx")
  write.xlsx(spw_labor, file = excel_file, rowNames = FALSE)
  
  
  spw_d <- merge(spw_labor, lyw, by = "Datum", all = TRUE) %>%
    filter(Datum >= as.Date(FirstDay))
  
  spw_d <- spw_d %>%
    filter(!(is.na(Messstelle)))
  
  spw_d$Datum <- as.Date(spw_d$Datum)
  evLy1$Datum <- as.Date(evLy1$Datum)
  evLy11$Datum <- as.Date(evLy11$Datum)
  evLy2$Datum <- as.Date(evLy2$Datum)
  evLy21$Datum <- as.Date(evLy21$Datum)
  evLy3$Datum <- as.Date(evLy3$Datum)
  evLy31$Datum <- as.Date(evLy31$Datum)
  
  spw_d <- merge(spw_d, evLy1, by = "Datum", all = TRUE) %>%
    filter(Datum >= as.Date(FirstDay))
  colnames(spw_d)[colnames(spw_d) == "p1"] <- "Precipitation"
  spw_d <- spw_d %>%
    filter(!(is.na(Messstelle)))
  spw_d <- soil_temperatures_moistures(spw_d, excludedColNames, 
                                       withInterpolation, 
                                       maxgap_Labor, maxgap_Driver)
  
  spw_d <- merge(spw_d, evLy11, by = "Datum", all.x = TRUE) %>%
    filter(Datum >= as.Date(FirstDay))
  colnames(spw_d)[colnames(spw_d) == "p1"] <- "Precipitation"
  spw_d <- spw_d %>%
    filter(!(is.na(Messstelle)))
  spw_d <- soil_temperatures_moistures(spw_d, excludedColNames, 
                                       withInterpolation, 
                                       maxgap_Labor, maxgap_Driver)
  
  spw_d <- merge(spw_d, evLy2, by = "Datum", all.x = TRUE) %>%
    filter(Datum >= as.Date(FirstDay))
  colnames(spw_d)[colnames(spw_d) == "p2"] <- "Precipitation"
  spw_d <- spw_d %>%
    filter(!(is.na(Messstelle)))
  spw_d <- soil_temperatures_moistures(spw_d, excludedColNames, 
                                       withInterpolation, 
                                       maxgap_Labor, maxgap_Driver)
  
  spw_d <- merge(spw_d, evLy21, by = "Datum", all.x = TRUE) %>%
    filter(Datum >= as.Date(FirstDay))
  colnames(spw_d)[colnames(spw_d) == "p2"] <- "Precipitation"
  spw_d <- spw_d %>%
    filter(!(is.na(Messstelle)))
  spw_d <- soil_temperatures_moistures(spw_d, excludedColNames, 
                                       withInterpolation, 
                                       maxgap_Labor, maxgap_Driver)
  
  
  spw_d <- merge(spw_d, evLy3, by = "Datum", all = TRUE) %>%
    filter(Datum >= as.Date(FirstDay))
  colnames(spw_d)[colnames(spw_d) == "p3"] <- "Precipitation"
  spw_d <- spw_d %>%
    filter(!(is.na(Messstelle)))
  spw_d <- soil_temperatures_moistures(spw_d, excludedColNames, 
                                       withInterpolation, 
                                       maxgap_Labor, maxgap_Driver)
  
  spw_d <- merge(spw_d, evLy31, by = "Datum", all.x = TRUE) %>%
    filter(Datum >= as.Date(FirstDay))
  colnames(spw_d)[colnames(spw_d) == "p3"] <- "Precipitation"
  spw_d <- spw_d %>%
    filter(!(is.na(Messstelle)))
  spw_d <- soil_temperatures_moistures(spw_d, excludedColNames, 
                                       withInterpolation, 
                                       maxgap_Labor, maxgap_Driver)
  
  spw_d <- spw_d %>%
    filter(!(Messstelle == "Ly4"))
  spw_d <- spw_d %>%
    filter(!is.na(Ca))
  
  excel_file <-paste0(folder,"/Lysimeters moisture data.xlsx")
  write.xlsx(spw_d, file = excel_file, rowNames = FALSE)
  
  spw_d <- spw_d %>%
    group_by(Messstelle, Tiefe) %>%
    mutate(
      Wassersaettigung = `Soil moisture` / max(`Soil moisture`, na.rm = TRUE)
    )
  
  spw_d <- spw_d %>% rename(`Saturation` = `Wassersaettigung`)
  
  
  #### Psi, pF                  ######################################################
  spw_d$pF <- ifelse(is.na(spw_d$`Pressure head`), NA,
                     ifelse(spw_d$`Pressure head` > 0, log10(spw_d$`Pressure head` * 10), 0))  #10.1972
  
  spw_d$`psi` <- spw_d$`Pressure head`
  spw_d$`Pressure head` <- -spw_d$`Pressure head`
  
  
  spw_labor <- spw_d[, !(colnames(spw_d) %in% c("gwf2", "gwf3"))]
  colnames(spw_labor)[colnames(spw_labor) == "Messstelle.x"] <- "Messstelle"
  
  #### Datataker: Oxi & Redox   ######################################################
  cat("Daten zu Sauerstoff und Redox werden geladen!\n")
  
  table_exists_query <- "SELECT EXISTS (
  SELECT 1 
  FROM information_schema.tables 
  WHERE table_schema = 'lysikorr' 
  AND table_name = 'spw_datataker_d');"
  
  result <- dbGetQuery(dbcon, table_exists_query)
  
  if (!result[1, 1]) {
    cat("Die Tabelle 'lysikorr.spw_datataker_d' existiert.\n")
    
    conn <- dbConnect(odbc::odbc(), driver = "{Microsoft Access Driver (*.mdb, *.accdb)}", dbq=dbO2_Redox)
    query <- paste("SELECT * FROM", dtO2_Redox)
    spw_datataker <- dbGetQuery(conn, query)
    
    spw_datataker$Datum <- as.POSIXct(spw_datataker$Timestamp_d, format = "%Y-%m-%d")
    dbWriteTable(dbcon, c("lysikorr", "spw_datataker"), spw_datataker1, overwrite = TRUE, row.names = FALSE)
    
    dbDisconnect(conn)
  }
  
  
  spw_datataker <- dbReadTable(dbcon, c("lysikorr", "spw_datataker_d"))
  
  spw_datataker$Datum <- as.Date(spw_datataker$Datum)
  spw_data <- merge(spw_labor, spw_datataker, by = "Datum", all = TRUE)
  
  # Nachweisgrenzen
  spw_data$NO3_N[spw_data$NO3_N < 0.01] <- 0.005
  spw_data$NH4_N[spw_data$NH4_N < 0.04] <- 0.02
  spw_data$o_PO4[spw_data$o_PO4 < 0.03] <- 0.015
  # Nachweisgrenzen
  
  spw_data$DOC[is.na(spw_data$DOC)] <- -1           
  spw_data$LF[is.na(spw_data$LF)] <- -1             
  spw_data$pH[is.na(spw_data$pH)] <- -1             
  spw_data$o_PO4[is.na(spw_data$o_PO4)] <- -1       
  
  
  spw_data <- oxi_redox_column(spw_data, "Ly1", 30)
  spw_data <- oxi_redox_column(spw_data, "Ly1", 60)
  spw_data <- oxi_redox_column(spw_data, "Ly1", 90)
  
  spw_data <- oxi_redox_column(spw_data, "Ly2", 30)
  spw_data <- oxi_redox_column(spw_data, "Ly2", 60)
  spw_data <- oxi_redox_column(spw_data, "Ly2", 90)
  
  spw_data <- oxi_redox_column(spw_data, "Ly3", 30)
  spw_data <- oxi_redox_column(spw_data, "Ly3", 60)
  spw_data <- oxi_redox_column(spw_data, "Ly3", 90)
  
  spw_data <- spw_data %>%
    filter(!is.na(Messstelle))
  spw_data$Datum <- as.Date(spw_data$Datum)
  
  
  spw_data <- spw_data %>%
    filter(!((spw_data$`AG-Nr` == "Ly2_30") & Datum >= as.Date("2022-02-01")))
  spw_data$Redox = spw_data$Redox + 210
  
  
  exclude <- c("Timestamp_d", "Ly1_30oxi", "Ly1_60oxi", "Psi-30",  "Psi-60",  "Psi-90", 
               "Ly1_90oxi", "Ly2_30oxi", "Ly2_60oxi", "Ly2_90oxi", "Ly3_30oxi", 
               "Ly3_60oxi", "Ly3_90oxi", "Ly4_30oxi", "Ly4_60oxi", "Ly4_90oxi", 
               "Ly1_30redox", "Ly1_60redox", "Ly1_90redox", "Ly2_30redox", "Ly2_60redox", 
               "Ly2_90redox", "Ly3_30redox", "Ly3_60redox", "Ly3_90redox", "Ly4_30redox", 
               "Ly4_60redox", "Ly4_90redox", "n")
  spw_data <- spw_data[, !(colnames(spw_data) %in% exclude)]
  
  #CO2
  spw_data$`Carbon dioxide` <- ifelse(is.na(spw_data$`pH`), NA,
                                      ifelse(is.na(spw_data$`HCO3`), NA, ((10^(-spw_data$`pH`) * (spw_data$`HCO3` / 1000)) / 10^(-6.352) / 10^(-1.468))))
  
  
  ######### Hier werden zusätzliche GW-Daten übernommen  
  #Umgebungsvariable muss gesetzt werden!
  dbGWData <- Sys.getenv("DB_GW_DATA")
  if (dbLaboratoryData == "") {
    stop(
      "Die Umgebungsvariable 'DB_GW_DATA' ist nicht gesetzt.\n",
      "Bitte den Pfad zur Excel-Datenbank (*.xlsx) als Umgebungsvariable definieren."
    )
  }

  datei <- dbGWData
  gw_data <- read_excel(datei, sheet = "Tabelle2")
  gw_data <- gw_data %>%
    rename(`Tiefe` = `AG-Nr`)
  gw_data$Redox = gw_data$Redox + 210
  
  spw_data$Oxi_old <- spw_data$Oxi
  spw_data$`Soil temperature old` <- spw_data$`Soil temperature`
  spw_data$`Redox old` = spw_data$`Redox`
  
  spw_data <- spw_data %>%
    left_join(gw_data %>% select(Datum, `Tiefe`, `mg/l-O2`), by = c("Datum", "Tiefe"), suffix = c("", "_new")) %>%
    mutate(Oxi = coalesce(`mg/l-O2`, Oxi)) %>%
    select(-`mg/l-O2`)  # Entfernt die temporäre Spalte mg/l-O2 
  
  
  spw_data <- spw_data %>%
    left_join(gw_data %>% select(Datum, `Tiefe`, `°c`), by = c("Datum", "Tiefe"), suffix = c("", "_new")) %>%
    mutate(`Soil temperature` = coalesce(`°c`, `Soil temperature`)) %>%
    select(-`°c`)
  
  gw_data <- gw_data %>%
    rename(red = `Redox`)
  spw_data <- spw_data %>%
    left_join(gw_data %>% select(Datum, `Tiefe`, `red`), by = c("Datum", "Tiefe"), suffix = c("", "_new")) %>%
    mutate(Redox = coalesce(`red`, Redox)) %>%
    select(-`red`)  # Entfernt die temporäre Spalte °C 
  ######### Hier werden zusätzliche GW-Daten übernommen  
  
  spw_data$Oxi_old <- NULL
  spw_data$`Soil temperature old` <- NULL
  spw_data$`Redox old` <- NULL
  
  
  
  
  spw_data_na_rows <- spw_data[!is.na(spw_data$Datum), ] # & 
  spw_data_na_rows <- spw_data_na_rows[!(spw_data_na_rows$Tiefe %in% c('ip')), ] # & %in% c('A', 'GW', 'ip')), ] # & 
  spw_data_na_rows <- spw_data_na_rows[!(spw_data_na_rows$Messstelle %in% c('Ly4')), ]
  
  excel_file <-paste0(folder,"/Lysimeters missing data.xlsx")
  # DataFrame in Excel schreiben
  write.xlsx(spw_data_na_rows, file = excel_file, rowNames = FALSE)
  
  ly_data <<- na.omit(spw_data[!(spw_data$Tiefe %in% c('A', 'GW', 'ip', 'pF', 'psi', 'Oxi')), ])
  
  y1 <- renaming(lywas, lai, subset(ly_data, ly_data$Messstelle == 'Ly1'))
  y2 <- renaming(lywas, lai, subset(ly_data, ly_data$Messstelle == 'Ly2'))
  y3 <- renaming(lywas, lai, subset(ly_data, ly_data$Messstelle == 'Ly3'))
  
  ly <<- rbind(y1, y2, y3)
  
  #### Redundanzen reduzieren ##################################################
  #Reduzierung des Datenbestandes auf Variablen der Primärkomponenten und Variablen der Treiber
  ly <- ly[, (colnames(ly) %in% c(BaseDataNames, ComponentesVariables, DriverVariables))]
  ly <- ly[, !(colnames(ly) %in% ExcludeDriverVariables)]
  ly <- subset(ly, !apply(ly == -1, 1, any))
  
  ly <- subset(ly, Datum >= FirstDay & Datum <= LastDay)
  
  gw_data <- spw_data[(spw_data$Tiefe %in% c('GW')), ]
  gw_data <- na.omit(gw_data[, !(colnames(gw_data) %in% c('LF', #'pH', 
                                                          'o_PO4', 
                                                          'DOC', #'Oxi', 
                                                          'Temp', #'Soil temperature', "Carbon dioxide",
                                                          'Soil moisture', 'Saturation', 'pF', 'psi', 'Pressure head'))])
  
  gw <- renaming(lywas, lai, gw_data)
  gw <- gw[, !(colnames(gw) %in% ExcludeDriverVariables)]
  gw <- gw[, !(colnames(gw) %in% c("Soil moisture"))] #"Oxygen", "Soil temperature"))]
  gw <<- na.omit(gw)
  
  a_data <- spw_data[(spw_data$Tiefe %in% c('A')), ]
  a_data <- na.omit(a_data[, !(colnames(a_data) %in% c('LF', #'pH', 
                                                       'o_PO4', 
                                                       'DOC', 'Oxi', 
                                                       'Temp', 'Soil temperature', "Carbon dioxide",
                                                       'Soil moisture', 'Saturation', 'pF', 'psi', 'Pressure head'))])
  aut <- renaming(lywas, lai, a_data)
  aut <- aut[, !(colnames(aut) %in% ExcludeDriverVariables)]
  aut <- aut[, !(colnames(aut) %in% c("Soil moisture",
                                      "Oxygen", "Soil temperature"))]
  aut <<- na.omit(aut)
  
  fehlende_spalten <- setdiff(names(ly), names(gw))
  missing_cols_gw <- data.frame(matrix(NA_real_, ncol = length(fehlende_spalten), nrow = nrow(gw)))
  names(missing_cols_gw) <- fehlende_spalten
  gw1 <- gw %>%
    bind_cols(missing_cols_gw) %>%
    mutate(across(everything(), ~ replace_na(., -1)))
  
  fehlende_spalten <- setdiff(names(ly), names(aut))
  missing_cols_a <- data.frame(matrix(NA_real_, ncol = length(fehlende_spalten), nrow = nrow(aut)))
  names(missing_cols_a) <- fehlende_spalten
  aut1 <- aut %>%
    bind_cols(missing_cols_a) %>%
    mutate(across(everything(), ~ replace_na(., -1)))
  
  all <- rbind(ly, gw1, aut1)
  excel_file <-paste0(folder,"/Lysimetersdata.xlsx")
  write.xlsx(subset(all, Datum >= FirstDay & Datum <= LastDay), file = excel_file, rowNames = FALSE)
  
  all <- na.omit(all)
  excel_file <-paste0(folder,"/Lysimeters analysed data.xlsx")
  # DataFrame in Excel schreiben
  write.xlsx(all, file = excel_file, rowNames = FALSE)
  
  
  ly <- na.omit(ly)
  lysimeters <<- ly
  
  mean_temperatures <- lysimeters %>%
    group_by(Datum, Messstelle) %>%
    summarise(`Mean soil temperature` = mean(`Soil temperature`, na.rm = TRUE), .groups = "drop")
  
  lysimeters <- lysimeters %>%
    left_join(mean_temperatures, by = c("Datum", "Messstelle"))
  
  ly1 <<- lysimeter_part(lysimeters, Object <- 'Ly1')
  ly2 <<- lysimeter_part(lysimeters, Object <- 'Ly2')
  ly3 <<- lysimeter_part(lysimeters, Object <- 'Ly3')
  
  ly1_30 <<- lysimeter_part(ly, 'Ly1', 30)
  ly2_30 <<- lysimeter_part(ly, 'Ly2', 30)
  ly3_30 <<- lysimeter_part(ly, 'Ly3', 30)
  
  ly1_60 <<- lysimeter_part(ly, 'Ly1', 60)
  ly2_60 <<- lysimeter_part(ly, 'Ly2', 60)
  ly3_60 <<- lysimeter_part(ly, 'Ly3', 60)
  
  ly1_90 <<- lysimeter_part(ly, 'Ly1', 90)
  ly2_90 <<- lysimeter_part(ly, 'Ly2', 90)
  ly3_90 <<- lysimeter_part(ly, 'Ly3', 90)
  
  NV_Test(ly)  
  ly <- lysimeter_part(lysimeters[, !(colnames(lysimeters) %in% c("AG-Nr", "Datum", "Messstelle"))])
  correlations(NULL, folder, ly, "Lysimeters correlation analysed data")
  ly <- lysimeter_part(ly[, !(colnames(ly) %in% DriverVariables)])
  ly <- lysimeter_part(ly[, !(colnames(ly) %in% c("Tiefe"))])
  correlations(NULL, folder, ly, "Lysimeters PCA correlation analysed data")
  
  ly <<- lysimeter_part(lysimeters[, !(colnames(lysimeters) %in% c("AG-Nr", "Datum", "Messstelle"))])
  
  PlotData(NULL, folder, lysimeters)
  
  
  
  save_data('GW', folder, gw)
  
  save_data('Ly1_30', folder, ly1_30)
  save_data('Ly2_30', folder, ly2_30)
  save_data('Ly3_30', folder, ly3_30)
  save_data('Ly1_60', folder, ly1_60)
  save_data('Ly2_60', folder, ly2_60)
  save_data('Ly3_60', folder, ly3_60)
  save_data('Ly1_90', folder, ly1_90)
  save_data('Ly2_90', folder, ly2_90)
  save_data('Ly3_90', folder, ly3_90)
}






