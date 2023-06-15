####Functions####

find_header_row <- function(directory, header_column) {
  i <- 0
  header_row <- NULL
  while (length(header_row) < 1) {
    line <- readLines(directory, i)
    header_row <- grep(header_column, line)
    i <- i + 1
  }
  return(header_row)
}

expCurrentSpec <- function(currentSpec) {
  return(switch(
    currentSpec,
    "Inward" = "InwardCurr",
    "Outward" = "OutwardCurr",
    "Both" = "Both"
  ))
}

prepare_raw_data <-
  function(data_list,
           currentSpec,
           timeCol,
           currentCol) {
    
    if(currentSpec == "Both") currentSpec <- c("InwardCurr" , "OutwardCurr")
    
    dataColNames <-
      data.frame(colName = c("Index", "Time", currentSpec),
                 colPos = as.numeric(c(1, timeCol, currentCol)))
    dataColNames <- dataColNames[order(dataColNames$colPos), ]
    
    data_list <- map(data_list, \(x, dataColNames) {
      x <- x[, dataColNames$colPos]
      names(x) <- dataColNames$colName
      return(x)
    }, dataColNames)
    
    data_list <- map(data_list, \(x) x[complete.cases(x),])
    return(data_list)
  }

dataListwoSettings <- function(dataList) {
  if (exists("settings", dataList))
    return(dataList[-which(names(dataList) == "settings")])
  return(dataList)
}
dataListwoRawData <- function(dataList) {
  if (exists("rawData", dataList))
    return(dataList[-which(names(dataList) == "rawData")])
  return(dataList)
}
dataListwoMedianData <- function(dataList) {
  if (exists("medianData", dataList))
    return(dataList[-which(names(dataList) == "medianData")])
  return(dataList)
}
dataListwoPeakTimeData <- function(dataList) {
  if (exists("peakTimeData", dataList))
    return(dataList[-which(names(dataList) == "peakTimeData")])
  return(dataList)
}

dataListwTypes <- function(dataList){
  dataList %<>% 
    dataListwoSettings()%>%
    dataListwoRawData() %>%
    dataListwoMedianData() %>%
    dataListwoPeakTimeData
  return(dataList)
}

getSettings <- function(dataList, setting)
  return(dataList$settings[[setting]])

createEditorPlot <- function(data_list, spec) {
  # Check the value of spec and select the appropriate column names
  if (spec == "InwardCurr") {
    x_col <- "Time"
    y_col <- "InwardCurr"
  } else if (spec == "OutwardCurr") {
    x_col <- "Time"
    y_col <- "OutwardCurr"
  } else if (spec == "Both") {
    x_col <- "Time"
    y1_col <- "InwardCurr"
    y2_col <- "OutwardCurr"
  }
  
  # Create the plot
  if (spec %in% c("InwardCurr", "OutwardCurr")) {
    final_plot <- ggplot(data_list, aes(x =  .data[[x_col]], y = .data[[y_col]])) +
      geom_line() +
      labs(x = "Time", y = spec)
  } else if (spec == "Both") {
    plot1 <-
      ggplot(data_list, aes(x =  .data[[x_col]], y = .data[[y1_col]])) +
      geom_line() +
      labs(x = "Time", y = "InwardCurr")
    
    plot2 <-
      ggplot(data_list, aes(x =  .data[[x_col]], y = .data[[y2_col]])) +
      geom_line() +
      labs(x = "Time", y = "OutwardCurr")
    
    # Arrange the plots vertically
    final_plot <- plot2 + plot1 + plot_layout(ncol = 1)
  }
  return(ggplotly(final_plot))
}

choseSelectedList <- function(data_list, selListNames) {
  selList <- data_list[names(data_list) %in% selListNames]
  return(selList)
}

combineListtoLong <- function(data_list) {
  return(bind_rows(data_list, .id = "Measurement"))
}

extractMeasIndex <- function(measNames) {
  return(word(measNames,-1,  sep = "_"))
}

createKineticPlot <-
  function(plotDataFrame,
           pTitle = "",
           pTheme = theme_prism(),
           pThemeOver = theme_few(),
           style =  "Overlayed",
           currSpec = NULL)
  {
    
    if(nrow(plotDataFrame) == 0) return(ggplot())
    
    
    kinPlot <-
      ggplot(plotDataFrame, aes(x = Time, y = .data[[currSpec]], color = Measurement)) +
      geom_line() + pTheme + ggtitle(pTitle)
    
    if (style == "Single"){
      kinPlot <-
        kinPlot + facet_wrap(
          ~ Measurement,
          ncol = 10,
          labeller = labeller(Measurement = extractMeasIndex)
        ) + pThemeOver
    }
    if (style == "Median") {
      if(currSpec == "InwardCurr") {
        kinPlot <-
          ggplot(plotDataFrame)   +
          geom_ribbon(aes(x = Time, ymin = Median + SD, ymax = Median),alpha = 0.3) +
          geom_line(aes(x = Time, y = Median)) + pTheme + ggtitle(pTitle)
      }
      if(currSpec == "OutwardCurr") {
        kinPlot <-
          ggplot(plotDataFrame)   +
          geom_ribbon(aes(x = Time, ymin = Median - SD, ymax = Median),alpha = 0.3) +
          geom_line(aes(x = Time, y = Median)) + pTheme + ggtitle(pTitle)
      }
    }
    
    kinPlot <- kinPlot + theme(legend.position = "right")
    return(kinPlot)
  }


createOverlayPlot <- function(plotDataFrame,
                              pTitle = "",
                              pTheme = theme_prism(),
                              pThemeOver = theme_few(),
                              style =  "Overlayed",
                              currSpec = NULL,
                              lineSize = 1) {
  
  if(nrow(plotDataFrame) == 0) return(ggplot())
  
  if(currSpec == "InwardCurr") {
    overlayPlot <-
      ggplot(plotDataFrame)   +
      geom_ribbon(aes(x = Time, ymin = Median + SD, ymax = Median, fill = Measurement),alpha = 0.3) +
      geom_line(aes(x = Time, y = Median, color = Measurement), linewidth = lineSize ) + pTheme + ggtitle(pTitle) +
      ylab("Percentage of Max. Curr.") + xlab("Time (s)")
  }
  if(currSpec == "OutwardCurr") {
    overlayPlot <-
      ggplot(plotDataFrame)   +
      geom_ribbon(aes(x = Time, ymin = Median - SD, ymax = Median, fill = Measurement),alpha = 0.3) +
      geom_line(aes(x = Time, y = Median, color = Measurement), linewidth = lineSize) + pTheme + ggtitle(pTitle) + 
      ylab("Percentage of Max. Curr.") + xlab("Time (s)")
  }
  
  if (style == "Single"){
    overlayPlot <-
      overlayPlot + facet_wrap(
        ~ Measurement,
        ncol = 10) + pThemeOver
  }
  
  
  overlayPlot <- overlayPlot + theme(legend.position = "right")
  
  return(overlayPlot)
}

pad_dataframes <- function(data_list) {
  min_length <- min(map_dbl(data_list, \(x) min(x$Index)))
  data_list <- map(data_list, \(x) pad_dataframe_before(x, min_length))
  max_length <- max(map_dbl(data_list, \(x) length(x$Index)))
  data_list <- map(data_list, \(x) pad_dataframe_after(x, max_length))
  return(data_list)
}

pad_dataframe_before <- function(df, min_length) {
  pad_rows <- df$Index[1] - min_length
  if(pad_rows == 0) return(df)
  pad_df <- data.frame(matrix(NA, ncol = ncol(df), nrow = pad_rows))
  colnames(pad_df) <- colnames(df)
  padded_df <- rbind(pad_df, df)
  return(padded_df)
}

pad_dataframe_after <- function(df, max_length) {
  num_rows <- nrow(df)
  if (num_rows < max_length) {
    pad_rows <- max_length - num_rows
    pad_df <- data.frame(matrix(NA, ncol = ncol(df), nrow = pad_rows))
    colnames(pad_df) <- colnames(df)
    padded_df <- rbind(df, pad_df)
  } else {
    padded_df <- df
  }
  return(padded_df)
}

calcKineticMedian <- function(data_list, currSpec) {
  data_list %<>% 
    combineListtoWide() %>%
    removeUneededColumns()
  
  median <- createRowMedian(data_list[,-1])
  SD <- createRowSD(data_list[,-1])
  medianDF <- data.frame(Time = data_list$Time, Median = median, SD = SD)
  return(medianDF)
}

combineListtoWide <- function(data_list) {
  
  pad_data <- pad_dataframes(data_list)
  
  combined_data_list <- bind_cols(pad_data, .name_repair = "universal_quiet")
  
  names_list <- rep(names(data_list), each = 3)
  names_df  <- colnames(data_list[[1]])
  namesComb <- paste0(names_list,".",names_df)
  colnames(combined_data_list) <- namesComb
  combined_data_list
  
  return(combined_data_list)
}

removeUneededColumns <- function(data_frame) {
  Time <- createRowMedian(data_frame[,grep("Time", colnames(data_frame))])
  data_frame <- data_frame[,-grep("Time", colnames(data_frame))]
  data_frame <- data_frame[,-grep("Index", colnames(data_frame))]
  data_frame <- cbind(Time, data_frame)
  return(data_frame)
}

createRowMedian <- function(data_frame) {
  return(rowMedians(as.matrix(data_frame), na.rm = TRUE))
}

createRowSD <- function(data_frame) {
  return(rowMads(as.matrix(data_frame), na.rm = TRUE))
}

normalize_data <- function(data_list, columnSpec) {
  
  # Normalize the specified column(s) within the desired range
  if (is.character(columnSpec)) {
    # For single column specification
    values <- data_list[[columnSpec]]
    if (columnSpec == "InwardCurr") {values <- values * -1}
    max_value <- max(values)
    max_norm_Index <- which.max(values)
    min_valueAkt <- min(values[1:max_norm_Index])
    min_valueInakt <- min(values[max_norm_Index:length(values)])
    min_norm_Akt_Index <- which.min(values[1:max_norm_Index])
    min_norm_Inakt_Index <- max_norm_Index + which.min(values[max_norm_Index:length(values)])
    
    
    normalizeIndices <- list(max_norm_Index = max_norm_Index, min_norm_Akt_Index = min_norm_Akt_Index, min_norm_Inakt_Index = min_norm_Inakt_Index)
    if ((min_valueAkt || min_valueInakt) == max_value) {
      stop("All values in the column are the same!")
    }
    if (columnSpec == "InwardCurr") {
      newVal <-  -100 * (values[1:max_norm_Index] - min_valueAkt) / (max_value - min_valueAkt)
      newVal <- c(newVal[-1], -100 * (values[max_norm_Index:length(values)] - min_valueInakt) / (max_value - min_valueInakt))
      data_list[[columnSpec]] <- newVal
      
    } else if (columnSpec == "OutwardCurr") {
      newVal <- 100 * (values[1:max_norm_Index] - min_valueAkt) / (max_value - min_valueAkt)
      newVal <- c(newVal[-1], 100 * (values[max_norm_Index:length(values)] - min_valueInakt) / (max_value - min_valueInakt))
      data_list[[columnSpec]] <- newVal }
  } else {
    # For "Both" column specification
    
    inward_values <- data_list[[columnSpec[1]]] * - 1
    outward_values <- data_list[[columnSpec[2]]]
    
    inward_max_value <- max(inward_values)
    inward_max_norm_Index <- which.max(inward_values)
    inward_min_valueAkt <- min(inward_values[1:inward_max_norm_Index])
    inward_min_valueInakt <- min(inward_values[inward_max_norm_Index:length(inward_values)])
    inward_min_norm_Akt_Index <- which.min(inward_values[1:inward_max_norm_Index])
    inward_min_norm_Inakt_Index <- which.min(inward_values[inward_max_norm_Index:length(inward_values)])
    
    inward_newVal <- -100 * (inward_values[1:inward_max_norm_Index] - inward_min_valueAkt) / (inward_max_value - inward_min_valueAkt)
    inward_newVal <- c(inward_newVal[-1], -100 * (inward_values[inward_max_norm_Index:length(inward_values)] - inward_min_valueInakt) / (inward_max_value - inward_min_valueInakt))
    
    outward_max_value <- max(outward_values)
    outward_max_norm_Index <- which.max(outward_values)
    outward_min_valueAkt <- min(outward_values[1:outward_max_norm_Index])
    outward_min_valueInakt <- min(outward_values[outward_max_norm_Index:length(outward_values)])
    outward_min_norm_Akt_Index <- which.min(outward_values[1:outward_max_norm_Index])
    outward_min_norm_Inakt_Index <- which.min(outward_values[outward_max_norm_Index:length(outward_values)])
    
    outward_newVal <- -100 * (outward_values[1:outward_max_norm_Index] - outward_min_valueAkt) / (outward_max_value - outward_min_valueAkt)
    outward_newVal <- c(outward_newVal[-1], 100 * (outward_values[outward_max_norm_Index:length(outward_values)] - outward_min_valueInakt) / (outward_max_value - outward_min_valueInakt))
    
    data_list[["InwardCurr"]] <- inward_newVal
    data_list[["OutwardCurr"]] <- outward_newVal
    
    normalizeIndices <- list(
      inward_max_norm_Index = inward_max_norm_Index,
      inward_min_norm_Akt_Index = inward_min_norm_Akt_Index,
      inward_min_norm_Inakt_Index = inward_min_norm_Inakt_Index,
      outward_max_norm_Index = outward_max_norm_Index,
      outward_min_norm_Akt_Index = outward_min_norm_Akt_Index,
      outward_min_norm_Inakt_Index = outward_min_norm_Inakt_Index
    )
    
  }
  return(list(data_list = data_list, normalizeIndices = normalizeIndices))
}

normalize_data_with_Info <- function(data_list, columnSpec, normInfo) {
  
  if (is.character(columnSpec)) {
    # For single column specification
    values <- data_list[[columnSpec]]
    if (columnSpec == "InwardCurr") {values <- values * -1}
    max_value <- values[normInfo$max_norm_Index]
    min_valueAkt <- values[normInfo$min_norm_Akt_Index]
    min_valueInakt <- values[normInfo$min_norm_Inakt_Index]
    
    
    if ((min_valueAkt || min_valueInakt) == max_value) {
      stop("All values in the column are the same!")
    }
    if (columnSpec == "InwardCurr") {
      newVal <-  -100 * (values[1:normInfo$max_norm_Index] - min_valueAkt) / (max_value - min_valueAkt)
      newVal <- c(newVal[-1], -100 * (values[normInfo$max_norm_Index:length(values)] - min_valueInakt) / (max_value - min_valueInakt))
      data_list[[columnSpec]] <- newVal
      
    } else if (columnSpec == "OutwardCurr") {
      newVal <- -100 * (values[1:normInfo$max_norm_Index] - min_valueAkt) / (max_value - min_valueAkt)
      newVal <- c(newVal[-1], 100 * (values[normInfo$max_norm_Index:length(values)] - min_valueInakt) / (max_value - min_valueInakt))
      data_list[[columnSpec]] <- newVal }
  } else {
    # For "Both" column specification
    
    inward_values <- data_list[[columnSpec[1]]] * - 1
    outward_values <- data_list[[columnSpec[2]]]
    
    inward_max_value <- inward_values[normInfo$inward_max_norm_Index]
    inward_min_valueAkt <-inward_values[normInfo$inward_min_norm_Akt_Index]
    inward_min_valueInakt <-inward_values[normInfo$inward_min_norm_Inakt_Index]
    
    inward_newVal <- -100 * (inward_values[1:normInfo$inward_max_norm_Index] - inward_min_valueAkt) / (inward_max_value - inward_min_valueAkt)
    inward_newVal <- c(inward_newVal[-1], -100 * (inward_values[normInfo$inward_max_norm_Index:length(inward_values)] - inward_min_valueInakt) / (inward_max_value - inward_min_valueInakt))
    
    outward_max_value <- outward_values[normInfo$outward_max_norm_Index]
    outward_min_valueAkt <- outward_values[normInfo$outward_min_norm_Akt_Index]
    outward_min_valueInakt <- outward_values[normInfo$outward_min_norm_Inakt_Index]
    
    outward_newVal <- -100 * (outward_values[1:normInfo$outward_max_norm_Index] - outward_min_valueAkt) / (outward_max_value - outward_min_valueAkt)
    outward_newVal <- c(outward_newVal[-1], 100 * (outward_values[normInfo$outward_max_norm_Index:length(outward_values)] - outward_min_valueInakt) / (outward_max_value - outward_min_valueInakt))
    
    data_list[["InwardCurr"]] <- inward_newVal
    data_list[["OutwardCurr"]] <- outward_newVal
  }
  return(data_list = data_list)
}

normalize_data_wo_Inakt <- function(data_list, columnSpec) {
  
  # Normalize the specified column(s) within the desired range
  if (is.character(columnSpec)) {
    # For single column specification
    values <- data_list[[columnSpec]]
    if (columnSpec == "InwardCurr") {values <- values * -1}
    max_value <- max(values)
    max_norm_Index <- which.max(values)
    min_value <- min(values)
    min_norm_Index <- which.min(values)
    
    if (min_value == max_value) {
      stop("All values in the column are the same!")
    }
    
    if (columnSpec == "InwardCurr" || columnSpec == "OutwardCurr") {
      newVal <- 100 * (values - min_value) / (max_value - min_value)
      if (columnSpec == "InwardCurr") {
        newVal <- newVal * -1
      }
      data_list[[columnSpec]] <- newVal
      normalizeIndices <- list(max_norm_Index = max_norm_Index, min_norm_Index = min_norm_Index)
      
    }
  } else {
    # For "Both" column specification
    inward_values <- data_list[[columnSpec[1]]] * -1
    outward_values <- data_list[[columnSpec[2]]]
    
    inward_max_value <- max(inward_values)
    inward_max_norm_Index <- which.max(inward_values)
    inward_min_value <- min(inward_values)
    inward_min_norm_Index <- which.min(inward_values)
    
    inward_newVal <- -100 * (inward_values - inward_min_value) / (inward_max_value - inward_min_value)
    
    outward_max_value <- max(outward_values)
    outward_max_norm_Index <- which.max(outward_values)
    outward_min_value <- min(outward_values)
    outward_min_norm_Index <- which.min(outward_values)
    
    outward_newVal <- 100 * (outward_values - outward_min_value) / (outward_max_value - outward_min_value)
    
    data_list[[columnSpec[1]]] <- inward_newVal
    data_list[[columnSpec[2]]] <- outward_newVal
    
    normalizeIndices <- list(
      inward_max_norm_Index = inward_max_norm_Index,
      inward_min_norm_Index = inward_min_norm_Index,
      outward_max_norm_Index = outward_max_norm_Index,
      outward_min_norm_Index = outward_min_norm_Index
    )
    
  }
  return(list(data_list = data_list, normalizeIndices = normalizeIndices))
}

normalize_data_with_Info_wo_Inakt <- function(data_list, columnSpec, normInfo) {
  
  if (is.character(columnSpec)) {
    # For single column specification
    values <- data_list[[columnSpec]]
    if (columnSpec == "InwardCurr") {values <- values * -1}
    max_value <- values[normInfo$max_norm_Index]
    min_value <- values[normInfo$min_norm_Index]
    
    if (min_value == max_value) {
      stop("All values in the column are the same!")
    }
    
    if (columnSpec == "InwardCurr" || columnSpec == "OutwardCurr") {
      newVal <- 100 * (values - min_value) / (max_value - min_value)
      if (columnSpec == "InwardCurr") {
        newVal <- newVal * -1
      }
      data_list[[columnSpec]] <- newVal
    }
  } else {
    # For "Both" column specification
    inward_values <- data_list[[columnSpec[1]]] * -1
    outward_values <- data_list[[columnSpec[2]]]
    
    inward_max_value <- inward_values[normInfo$inward_max_norm_Index]
    inward_min_value <- inward_values[normInfo$inward_min_norm_Index]
    
    inward_newVal <- -100 * (inward_values - inward_min_value) / (inward_max_value - inward_min_value)
    
    outward_max_value <- outward_values[normInfo$outward_max_norm_Index]
    outward_min_value <- outward_values[normInfo$outward_min_norm_Index]
    
    outward_newVal <- 100 * (outward_values - outward_min_value) / (outward_max_value - outward_min_value)
    
    data_list[[columnSpec[1]]] <- inward_newVal
    data_list[[columnSpec[2]]] <- outward_newVal
  }
  
  return(data_list)
}

smooth_data <- function(data, smoothingSpec, columnSpec, ...) {
  
  smoothed_data <- data
  for (col in columnSpec) {
    switch(
      smoothingSpec,
      SmoothingSpline = {
        smoothed_data[[col]] <- smooth.spline(data[["Time"]], data[[col]], ...)$y
      },
      GaussianKernel = {
        smoothed_data[[col]] <- ksmooth(data[["Time"]], data[[col]], "normal", ...)$y
      },
      Gaussian = {
        smoothed_data[[col]] <- smth.gaussian(data[[col]], ...)
      },
      LOESS = {
        smoothed_data[[col]] <- loess(data[[col]] ~ data$Time,  ...)$fitted
      },
      LOWESS = {
        smoothed_data[[col]] <- lowess(data[["Time"]], data[[col]], ...)$y
      },
      Fourier = {
        smoothed_data[[col]] <- fourier_filter(data[[col]], ...)
      },
      stop("Invalid smoothing specification!")
    )
  }
  
  return(smoothed_data)
}

fourier_filter <- function(data, cutoff_frequency = 100) {
  # Apply Fourier transform
  fft_data <- fft(data)
  
  # Identify the indices of the high-frequency components to be removed
  num_points <- length(fft_data)
  high_freq_indices <- seq(from = cutoff_frequency + 1, to = num_points - cutoff_frequency)
  
  # Remove the high-frequency components
  fft_data[high_freq_indices] <- 0
  
  # Apply inverse Fourier transform to obtain filtered data
  filtered_data <- Re(fft(fft_data, inverse = TRUE))
  
  return(filtered_data)
}

increase_resolution <- function(data, increase = 10) {
  
  
  x <- data$Index  # Assuming the x-values are stored in a column named 'Index'
  
  new_x <- seq(min(x), max(x), length.out = length(x) * increase)# Generate new x-values with increased resolution
  new_data <- data.frame(Index = new_x) 
  for (col in names(data)) {
    if (col != "Index") {
      y <- data[[col]]  # Select the column to interpolate
      
      interp <- approx(x, y, xout = new_x, method = "constant")  # Interpolate y-values at new x-values
      
      new_data[col] <- interp$y  # Store the interpolated column in the new dataframe
    }
  }
  
  new_x <- seq(min(x), min(x) - 1+length(x) * increase)# Generate new x-values with increased resolution
  new_x <- new_x - min(new_x) + min(x)
  
  new_data$Index <- new_x  # Add the new x-values column to the new dataframe
  
  return(new_data)
}

decrease_resolution <- function(data, decrease = 10) {
  x <- data$Index  # Assuming the x-values are stored in a column named 'Index'
  
  new_x <- x[seq(1, length(x), by = decrease)]  # Select every nth x-value
  
  new_data <- data.frame(Index = new_x)
  
  for (col in names(data)) {
    if (col != "Index") {
      y <- data[[col]]  # Select the column to downsample
      
      downsampled <- y[seq(1, length(y), by = decrease)]  # Select every nth y-value
      
      new_data[col] <- downsampled  # Store the downsampled column in the new dataframe
    }
  }
  
  return(new_data)
}

getStackTimePoints <- function(data_frame, stackPoint, columnSpec) {
  time_points <- data.frame(Index = numeric(), Time = numeric(), Column = character())
  
  for (col in columnSpec) {
    if (col == "InwardCurr") stackPoint <- stackPoint * - 1
    if (col == "OutwardCurr") stackPoint <- abs(stackPoint)
    
    time_series <- data_frame[[col]]
    # Find indices where the time series crosses the stack point value
    crossing_indices <-data_frame$Index[which(diff(sign(time_series - stackPoint)) != 0)]
    
    # Extract the corresponding time points
    crossing_indices <- crossing_indices[1]
    crossing_time_points <- data_frame$Time[which(data_frame$Index == crossing_indices)]
    time_points <- rbind(time_points, tibble(crossing_indices, crossing_time_points, col))
  }
  return(time_points)
}

moveTimeToStackTime <- function(data_frame, stackPoint_frame, stackTime){
  data_frame$Time <- data_frame$Time - stackPoint_frame$crossing_time_points + stackTime
  return(data_frame)
}

moveIndexToStackIndex <- function(data_frame,  stackInfo){
  data_frame$Index <- data_frame$Index - stackInfo$crossing_indices
  return(data_frame)
}

get_peak_times <- function(data_frame, peakPointsBef, peakPoint, peakPointsAft, currSpec, timeUntilStack = 0, durationPhotoswitch = 0, stackTime = 0) {
  if(currSpec == "InwardCurr") {
    data_frame[[currSpec]] <- data_frame[[currSpec]] * - 1}
  
  peakTimesBef <- c()
  for (i in seq_along(peakPointsBef)) {
    idx <- which(data_frame[[currSpec]] >= peakPointsBef[i])[1]
    peakTimesBef[i] <- data_frame$Time[idx]
  }
  
  idx <- which(data_frame[[currSpec]] >= peakPoint)[1]
  peakTime <- data_frame$Time[idx]
  
  peakTimesAft <- c()
  for (i in seq_along(peakPointsAft)) {
    idx <- which(data_frame[[currSpec]] <= peakPointsAft[i] & data_frame$Time > peakTime)[1]
    peakTimesAft[i] <- data_frame$Time[idx] - durationPhotoswitch
  }
  peakPointsBef <- paste0("$bP_{", peakPointsBef,"\\%}$")
  peakPoint <- paste0("$P_{", peakPoint,"\\%}$")
  peakPointsAft <- paste0("$pP_{", peakPointsAft,"\\%}$")
  
  peakTimes <- data.frame(Marker = c(peakPointsBef, peakPoint, peakPointsAft), Time = c(peakTimesBef, peakTime, peakTimesAft))
  peakTimes$Marker <- factor(peakTimes$Marker, levels = unique(peakTimes$Marker))
  peakTimes$Time <- peakTimes$Time - stackTime - timeUntilStack
  return(peakTimes)
}

splitStringValue <- function(value) {
  split_values <- strsplit(value, ";")[[1]]
  
  # Split each element by ","
  split_values <- strsplit(split_values, ",")
  
  # Convert the values to numbers
  numeric_values <- map(split_values,  as.numeric)
  names(numeric_values) <- c("peakPointsBef", "PeakPoint","peakPointsAft")
  return(numeric_values)
}


combinePeakTimeList <- function(data_list) {
  
  list_peak <-map(data_list, \(x) choseSelectedList(x,"peakTimeData")$peakTimeData)
  
  peak_list <- map(list_peak, \(x) {
    data_frame <- cbind(Marker = x[[1]]$Marker, map_df(x, \(y) y$Time)) 
    return(pivot_longer(data_frame,-Marker))})
  
  data_frame_peaks <- bind_rows(peak_list, .id = "Series")
  return(data_frame_peaks)
}

createPeakStatisticPlot <- function(peak_data_frame,           
                                    pTitle = "Plot",
                                    pTheme = theme_prism(),
                                    pThemeOver = theme_few(),
                                    style =  "Overlayed",
                                    lineSize = 1) {
  if(nrow(peak_data_frame) == 0) return(ggplot())
  
  statPlot <-  ggplot(peak_data_frame, aes(x = Marker, y = value, color = Series)) +
    geom_boxplot( position = position_dodge2(), linewidth = lineSize ) +
    geom_jitter(alpha = 0.4,position = position_jitterdodge()) +
    stat_boxplot(position = position_dodge(0.75),
                 geom = "errorbar",width = 0.3, linewidth = lineSize) +
    scale_x_discrete(labels = TeX)+
    labs(x = "", y = "Time (s)") +
    pTheme + 
    stat_compare_means(
      method = "wilcox.test",
      aes(label = ..p.signif..),
      show.legend = FALSE,
      symnum.args = list(
        cutpoints = c(0,  0.001, 0.01, 0.05, 1),
        symbols = c("***", "**", "*", "")
      )
    )
  
  if (style == "Single"){
    statPlot <-
      statPlot + facet_wrap(
        ~ Series,
        ncol = 10,
      ) + pThemeOver
  }
  
  return(statPlot)
}

colorPlot <- function(origPlot, colors){
  newPlot <- origPlot + 
    scale_color_manual(values = colors, aesthetics = c("colour", "fill"))
  return(newPlot)
}

changeAxisLims <- function(origPlot, xALims, xBreaks, yALims, yBreaks){
  # newPlot <- origPlot + coord_cartesian(xALims, yALims, expand = F)
  
  if (!is.null(xBreaks))  xBreaks <- as.numeric(str_split_1(xBreaks,","))
  yBreaks <- as.numeric(str_split_1(yBreaks,","))
  
  
  if (any(is.na(yBreaks))) {
    yBreaks <- pretty_breaks(n = 4)
  }
  if (any(is.na(xBreaks))) {
    xBreaks <- pretty_breaks(n = 4)
  }
  
  if (is.null(xALims)) {
    newPlot <- origPlot + 
      scale_y_continuous(breaks = yBreaks, limits = yALims, expand = c(0,0))
    return(newPlot)
  }
  
  newPlot <- origPlot + 
    scale_y_continuous(breaks = yBreaks, limits = yALims, expand = c(0,0)) +
    scale_x_continuous(breaks = xBreaks, limits = xALims, expand = c(0,0))
  return(newPlot)
}

changeFontSize <- function(origPlot, fontSize) {
  newPlot <- origPlot + theme(text = element_text(size = fontSize))
  return(newPlot)
}

customizePlot <- function(origPlot, colors, xALims, xBreaks, yALims, yBreaks, fontSize) {
  finalPlot <- origPlot %>%
    colorPlot(colors) %>%
    changeAxisLims(xALims, xBreaks, yALims, yBreaks) %>%
    changeFontSize(fontSize)
  return(finalPlot)
}

saveDataAsR <- function(data_list, color_Choices, color_Selected, selected_Colors, series_List, fileName) {
  save(data_list, color_Choices, color_Selected, selected_Colors, series_List, file = fileName)
}

saveDataAsXlsx <- function(data_list, fileName) {
  wb = createWorkbook()
  walk(names(data_list), \(x) addWorksheet(wb, paste0("Kin_",x)))
  walk2(data_list, names(data_list), \(x,y) writeDataTable(wb,paste0("Kin_",y), cbind(x$medianData,combineListtoWide(x$normalizedStackedData))))
  
  walk(names(data_list), \(x) addWorksheet(wb, paste0("Ttp_",x)))
  walk2(data_list, names(data_list), \(x,y) writeDataTable(wb,paste0("Ttp_",y), {
    combined_data_list <- bind_cols(x$peakTimeData, .name_repair = "universal_quiet")
    names_list <- rep(names(x$peakTimeData), each = 2)
    names_df  <- colnames(x$peakTimeData[[1]])
    namesComb <- paste0(names_list,".",names_df)
    colnames(combined_data_list) <- namesComb
    combined_data_list
  }
  ))
  saveWorkbook(wb, fileName, overwrite = TRUE)
}