shinyServer(function(input, output, session) {
  ####Reactives####
  
  ##### Observe Tabs######
  observeEvent(input$tabs, {
    ##### Import Export Data######
    if (input$tabs == "DataImportExport") {
      output$Workspace <- renderText({
        getwd()
      })
      output$StatusDataImport <- renderText({
        if (isDataImported == FALSE) {
          paste0("No data has been imported or loaded yet!")
        } else {
          paste0("Data has been successfully loaded!")
        }
      })
    }
      
    if (input$tabs == "ViewCalculation") {
      output$outPutCalc <- renderText({
      if (isCalculated == FALSE) {
        paste0("No calculation have been done!")
      } else {
        paste0("Calculations are finished!")
      }
      })
  }
  })
  ##### Change Workspace######
  observeEvent(input$ChangeWorkspace, {
    wddir <- choose.dir()
    if (!is.na(wddir))
      setwd(wddir)
    
    output$Workspace <- renderText({
      getwd()
    })
  })
  ####DataEditor####
  
  data_to_edit <- reactiveVal(data.frame(10, 10))
  data_edit <- dataEditServer("edit-1",
                              data = data_to_edit,
                              height = 750)
  
  observeEvent(input$pickerSeries, {
    updatePickerInput(
      session = session,
      inputId = "pickerType",
      choices = names(dataListwoSettings(dataList[[input$pickerSeries]])),
      selected = "preparedData"
    )
    updatePickerInput(
      session = session,
      inputId = "pickerMeasurement",
      choices = names(dataList[[input$pickerSeries]][[input$pickerType]])
    )
    
  }, ignoreInit = TRUE)
  
  observeEvent(input$pickerType, {
    updatePickerInput(
      session = session,
      inputId = "pickerMeasurement",
      choices = names(dataList[[input$pickerSeries]][[input$pickerType]])
    )
  },  ignoreInit = TRUE)
  
  observeEvent(input$pickerMeasurement, {
    editData <-
      dataList[[input$pickerSeries]][[input$pickerType]][[input$pickerMeasurement]]
    data_to_edit(editData)
    if (input$pickerType != "rawData") {
      editPlot <-
        createEditorPlot(editData, getSettings(dataList[[input$pickerSeries]], "currentSpec"))
      output$dataEditorPlot <- renderPlotly({
        editPlot
      })
    }
  },  ignoreInit = TRUE)
  
  observeEvent(input$saveEditorButton, {
    dataList[[input$pickerSeries]][[input$pickerType]][[input$pickerMeasurement]] <<-
      data_edit()
    if (input$pickerType != "rawData") {
      editData <-
        dataList[[input$pickerSeries]][[input$pickerType]][[input$pickerMeasurement]]
      editPlot <-
        createEditorPlot(editData, getSettings(dataList[[input$pickerSeries]], "currentSpec"))
      output$dataEditorPlot <- renderPlotly({
        editPlot
      })
    }
    
  })
  
  ####
  ####Import Modal####
  observeEvent(input$Import_Data, {
    isDataImported <- FALSE
    isolate({
      output$StatusDataImport <- renderText({
        if (isDataImported == TRUE) {
          paste0("Data import succesfull!")
        } else {
          paste0("Data import not succesfull!")
        }
      })
    })
  })
  
  # Add an observeEvent to handle the OK button press
  observeEvent(input$okBtn, {
    isDataImported <- FALSE
    if (input$newMeasName == "")
      return()
    paths <- choose.files()
    if (length(paths) == 0)
      return()
    newSeries <- input$newMeasSwitch
    seriesName <- input$newMeasName
    currentSpec <- expCurrentSpec(input$InputImportCurrentSpec)
    timeCol <- input$InputImportTimeCol
    currentCol <- input$InputImportCurrentCols
    
    tryCatch({
      newDataList <- importData(paths)
      
      preparedDataList <-
        prepare_raw_data(newDataList, currentSpec, timeCol, currentCol)
      
      sendSweetAlert(
        session = session,
        title = "Success !",
        text = "Data was imported",
        type = "success"
      )
      isDataImported <- TRUE
    },
    error = function(e) {
      sendSweetAlert(
        session = session,
        title = "Error...",
        HTML(paste0("Data was not imported: <br/> "), e),
        type = "error"
      )
      isDataImported <- FALSE
    })
    if (newSeries == TRUE) {
      dataList[[seriesName]][["rawData"]] <<- newDataList
      dataList[[seriesName]][["preparedData"]] <<- preparedDataList
      dataList[[seriesName]]$settings$currentSpec <<- currentSpec
    }
    if (newSeries == FALSE) {
      dataList[[seriesName]][["rawData"]] <<-
        c(dataList[[seriesName]][["rawData"]], newDataList)
      dataList[[seriesName]][["preparedData"]] <<-
        c(dataList[[seriesName]][["preparedData"]], preparedDataList)
      dataList[[seriesName]]$settings$currentSpec <<- currentSpec
    }
    
    updatePickerInput(
      session = session,
      inputId = "pickerSeries",
      choices = names(dataList),
      selected =  names(dataList)[1]
    )
    
    updateSelectInput(
      session = session,
      inputId = "seriesPickerKineticsView",
      choices = names(dataList),
      selected =  names(dataList)[1]
    )
    
    updatePickerInput(
      session = session,
      inputId = "seriesPickerCalculations",
      choices = names(dataList),
      selected =  names(dataList)[1]
    )
    
    updateCurSpecKineticsView(seriesName)
    
    isolate({
      output$StatusDataImport <- renderText({
        if (isDataImported == TRUE) {
          paste0("Data import succesfull!")
        } else {
          paste0("Data import not succesfull!")
        }
      })
    })
  })
  
  ####
  ####Import Data####
  importData <- function(paths) {
    delim <- input$InputImportSeperator
    header_var <- input$InputIndexName
    coltypes <- list(.default = col_double())
    
    skips <- map(paths, find_header_row, header_var)
    
    if (delim != "fixed width file") {
      data_list <- map2(
        paths,
        skips,
        \(path, skip)
        vroom(
          file = path,
          skip = skip ,
          delim = delim,
          col_names = F,
          col_types  = coltypes
        )
      )
    }
    else {
      data_list <- map2(
        paths,
        skips,
        \(path, skip)
        vroom_fwf(
          file = path,
          col_positions = fwf_empty(path, skip, n = 1000),
          skip = skip,
          col_types  = coltypes
        )
      )
    }
    names(data_list) <- file_path_sans_ext(basename(paths))
    return(data_list)
  }
  ####
  
  ####Calculations
  observeEvent(input$calculateSeries, {
    isCalculated <<- FALSE
    output$outPutCalc <- renderText({
    seriesName <- input$seriesPickerCalculations
    dataSmooth <- calculateSmoothing(seriesName)
    isCalculated <<- TRUE
    dataList[[seriesName]][["smoothedData"]] <<- dataSmooth[["data_list"]]
    dataList[[seriesName]]$settings$smoothingAlgo <<- dataSmooth$smoothingAlgo
    dataList[[seriesName]]$settings$smoothingParam <<- dataSmooth$smoothingParam

    dataNorm <- calculateNorming(seriesName, "smoothedData")
    dataList[[seriesName]][["normalizedData"]] <<- dataNorm
    
      if (isCalculated == FALSE) {
        paste0("No calculation have been done!")
      } else {
        paste0("Calculations are finished!")
      }
    })
    
    updatePickerTypeKineticsView(session)
  })
  
  
  calculateSmoothing <- function(series) {
    data_list <- dataList[[series]]$preparedData
    columnSpec <- getSettings(dataList[[series]], "currentSpec")
    smoothingAlgo <- input$inputAlgorithmSmoothing
    switch(
      smoothingAlgo,
      SmoothingSpline = {
        parList  <- list(spar = input$smoothingSplineSpar)
      },
      GaussianKernel = {
        parList  <- list(bandwidth = input$GaussianKernelBandwith)
        },
      Gaussian = {
        parList  <- list(window = input$gaussianWindow, alpha = input$GaussianAlpha)
      },
      LOESS = {
        parList  <- list(span = input$loessSpan, degree = input$loessDegree)
      },
      LOWESS = {
        parList  <- list(f = input$lowessF, iter = input$lowessIter)
      },
      Fourier = {
        parList  <- list(cutoff_frequency = input$FouriercutoffFrequency)
      },
      stop("Invalid Algorithm!")
    )
    data_list <- map(data_list, \(x) do.call(smooth_data, c(list(data = x, smoothingSpec = smoothingAlgo,columnSpec = columnSpec), parList )))
    return(c(list(data_list = data_list), smoothingAlgo = smoothingAlgo,  smoothingParam  = list(parList)))
  }
  
  calculateNorming <- function(series, data_temp) {
    data_list <- dataList[[series]][[data_temp]]
    columnSpec <- getSettings(dataList[[series]], "currentSpec")
    
    data_list <- map(data_list, \(x) normalize_data(x, columnSpec))
    return(data_list)
  }
  ####View Kinetics####
  ###Observers###
  observeEvent(input$seriesPickerKineticsView, {
    
    updatePickerTypeKineticsView(session)
    
    updateCurSpecKineticsView(input$seriesPickerKineticsView)
    
    updateCheckboxGroupButtons(
      session = session,
      inputId = "measurementPickerKineticsView",
      choices = names(dataList[[input$seriesPickerKineticsView]][[input$typePickerKineticsView]]),
      selected = names(dataList[[input$seriesPickerKineticsView]][[input$typePickerKineticsView]]),
      checkIcon = list(
        yes = tags$i(class = "fa fa-check-square",
                     style = "color: steelblue"),
        no = tags$i(class = "fa fa-square-o",
                    style = "color: steelblue")
      )
    )
  }, ignoreInit = TRUE)
  
  observeEvent(input$typePickerKineticsView, {
    updateCheckboxGroupButtons(
      session = session,
      inputId = "measurementPickerKineticsView",
      choices = names(dataList[[input$seriesPickerKineticsView]][[input$typePickerKineticsView]]),
      selected = names(dataList[[input$seriesPickerKineticsView]][[input$typePickerKineticsView]]),
      checkIcon = list(
        yes = tags$i(class = "fa fa-check-square",
                     style = "color: steelblue"),
        no = tags$i(class = "fa fa-square-o",
                    style = "color: steelblue")
      )
    )
    
   
  },  ignoreInit = TRUE)
  
  observeEvent(input$measurementPickerKineticsView, {
  },  ignoreInit = TRUE)
  
  output$kineticsViewPlot <- renderPlot({
    plot(createKinPlot())
  })
  ###
  ###Kinetic Plot###
  createKinPlot <- function() {
    data_list <- choseSelectedList(dataList[[input$seriesPickerKineticsView]][[input$typePickerKineticsView]],
                                   input$measurementPickerKineticsView)
    combinedList <-
      combineListtoLong(data_list)
    kineticPlot <-
      createKineticPlot(combinedList, style = input$curKineticsStyle,
                        pTitle = paste0(input$seriesPickerKineticsView,
                                        ": ", input$typePickerKineticsView)
      )
    
    if(all(dim(kineticPlot$data) == c(0,0))) kineticPlot = ggplot()
    return(kineticPlot)
  }
  ###
  updateCurSpecKineticsView <- function(series) {
    curSpec <-
      getSettings(dataList[[series]], "currentSpec")
    
    if (curSpec %in% c("InwardCurr", "OutwardCurr")) {
      updateRadioGroupButtons(
        session = session,
        inputId = "curSpecKineticsView",
        selected = curSpec,
        disabled = TRUE,
        checkIcon = list(
          yes = tags$i(class = "fa fa-circle",
                       style = "color: steelblue"),
          no = tags$i(class = "fa fa-circle-o",
                      style = "color: steelblue")
        )
      )
    }
    
    if (curSpec == "Both") {
      updateRadioGroupButtons(
        session = session,
        inputId = "curSpecKineticsView",
        selected = curSpec,
        disabled = FALSE,
        checkIcon = list(
          yes = tags$i(class = "fa fa-circle",
                       style = "color: steelblue"),
          no = tags$i(class = "fa fa-circle-o",
                      style = "color: steelblue")
        )
      )
    }
  }
  
  updatePickerTypeKineticsView <- function(session) {
  updateSelectInput(
    session = session,
    inputId = "typePickerKineticsView",
    choices = names(dataListwoRawData(dataListwoSettings(dataList[[input$seriesPickerKineticsView]]))),
    selected =  tail(names(dataListwoRawData(dataListwoSettings(dataList[[input$seriesPickerKineticsView]]))), n = 1)
  )
  }
  
  ####
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
})

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
    "Both" = c("InwardCurr", "OutwardCurr")
  ))
}

prepare_raw_data <-
  function(data_list,
           currentSpec,
           timeCol,
           currentCol) {
    dataColNames <-
      data.frame(colName = c("Index", "Time", currentSpec),
                 colPos = as.numeric(c(1, timeCol, currentCol)))
    dataColNames <- dataColNames[order(dataColNames$colPos), ]
    
    data_list <- map(data_list, \(x, dataColNames) {
      x <- x[, dataColNames$colPos]
      names(x) <- dataColNames$colName
      return(x)
    }, dataColNames)
    
    return(data_list)
  }

dataListwoSettings <- function(dataList)
  return(dataList[-which(names(dataList) == "settings")])

dataListwoRawData <- function(dataList)
  return(dataList[-which(names(dataList) == "rawData")])

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
      ggplot(data_list, aes(x =  .data[[x_col]], y = .data[[y_col1]])) +
      geom_line() +
      labs(x = "Time", y = "InwardCurr")
    
    plot2 <-
      ggplot(data_list, aes(x =  .data[[x_col]], y = .data[[y_col2]])) +
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
           pTitle = "Plot",
           pTheme = theme_prism(),
           pThemeOver = theme_few(),
           style =  "Overlayed")
  {
    kinPlot <-
      ggplot(plotDataFrame, aes(x = Time, y = InwardCurr, color = Measurement)) +
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
      
    }
      
    
    kinPlot <- kinPlot + theme(legend.position = "bottom")
    return(kinPlot)
  }

pad_dataframe_after<- function(df, max_length) {
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



combineListtoWide <- function(data_list) {
  max_length <- max(map_vec(data_list, nrow))
  
  pad_data <- map(data_list, pad_dataframe_after, max_length)
  
  combined_data_list <- bind_cols(pad_data, .name_repair = "universal_quiet")
  
  names_list <- rep(names(data_list), each = 3)
  names_df  <- colnames(data_list[[1]])
  namesComb <- paste0(names_list,".",names_df)
  colnames(combined_data_list) <- namesComb
  
  return(combined_data_list)
}

normalize_data <- function(data_list, columnSpec) {

  # Normalize the specified column(s) within the desired range
    if (is.character(columnSpec)) {
      # For single column specification
      values <- data_list[[columnSpec]]
      max_value <- max(values)
      min_value <- min(values)
      if (min_value == max_value) {
        stop("All values in the column are the same!")
      }
      if (columnSpec == "InwardCurr") {
        data_list[[columnSpec]] <- 100 * (values - max_value) / (max_value - min_value)
      } else if (columnSpec == "OutwardCurr") {
        data_list[[columnSpec]] <- 100 * (values - min_value) / (max_value - min_value)
      }
    } else {
      # For "Both" column specification
      inward_values <- data_list[[columnSpec[1]]]
      outward_values <- data_list[[columnSpec[2]]]
      max_inward <- max(inward_values)
      min_inward <- min(inward_values)
      max_outward <- max(outward_values)
      min_outward <- min(outward_values)
      if (min_inward == max_inward || min_outward == max_outward) {
        stop("All values in at least one of the columns are the same!")
      }
      data_list[[columnSpec[1]]] <- 100 * (inward_values - max_inward) / (max_inward - min_inward)
      data_list[[columnSpec[2]]] <- 100 * (outward_values - min_outward) / (max_outward - min_outward)
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
  
  new_x <- seq(min(x), max(x), length.out = length(x) * increase)  # Generate new x-values with increased resolution
  new_data <- data.frame(Index = new_x)
  for (col in names(data)) {
    if (col != "Index") {
      y <- data[[col]]  # Select the column to interpolate
      
      interp <- approx(x, y, xout = new_x)  # Interpolate y-values at new x-values
      
      new_data[col] <- interp$y  # Store the interpolated column in the new dataframe
    }
  }
  
  new_data$Index <- new_x  # Add the new x-values column to the new dataframe
  
  return(new_data)
}

getStackTimePoints <- function(data_frame, stackPoint, columnSpec) {
  time_points <- data.frame(Index = numeric(), Time = numeric(), Column = character())
  
  for (col in columnSpec) {
    if (col == "InwardCurr") stackPoint <- stackPoint * - 1
    if (col == "OutwardCurr") stackPoint <- abs(stackPoint)
    
      time_series <- data_frame[[col]]
      # Find indices where the time series crosses the stack point value
      crossing_indices <- which(diff(sign(time_series - stackPoint)) != 0)
      
      # Extract the corresponding time points
      crossing_indices <- crossing_indices[1]
      crossing_time_points <- data_frame$Time[crossing_indices]
      time_points <- rbind(time_points, tibble(crossing_indices, crossing_time_points, col))
  }
  return(time_points)
}
    