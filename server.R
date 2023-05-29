shinyServer(function(input, output, session) {
  ####Reactives####
  
  #### Observe Tabs######
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
  #### Change Workspace######
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
  
  ####Calculations####
  observeEvent(input$calculateSeries, {
    isCalculated <<- FALSE
    output$outPutCalc <- renderText({
      
      seriesName <- input$seriesPickerCalculations
      dataSmooth <- calculateSmoothing(seriesName)
      isCalculated <<- TRUE
      #Create the smoothed model of the measurements and extract Indizes
      dataList[[seriesName]][["smoothedData"]] <<- dataSmooth[["data_list"]]
      dataList[[seriesName]]$settings$smoothingAlgo <<- dataSmooth$smoothingAlgo
      dataList[[seriesName]]$settings$smoothingParam <<- dataSmooth$smoothingParam
      
      dataNorm <- calculateNorming(seriesName, "smoothedData")
      dataList[[seriesName]]$settings$normInfo <<- map(dataNorm, \(x) x[[2]])
      dataList[[seriesName]][["normalizedSmoothedData"]] <<- map(dataNorm, \(x) x[[1]])
      
      dataStacking <- calculateStacking(seriesName, "normalizedSmoothedData")
      dataList[[seriesName]]$settings$stackParam <<- dataStacking[[3]]
      dataList[[seriesName]]$settings$stackInfo <<- dataStacking[[2]]
      dataList[[seriesName]][["normalizedSmoothedStackedData"]] <<- dataStacking[[1]]
      #Norm and stack the "real" data
      dataNorm <- calculateNorming(seriesName, "preparedData", getSettings(dataList[[seriesName]], "normInfo"))
      dataList[[seriesName]][["normalizedData"]] <<- dataNorm
      
      dataStacking <- calculateStacking(seriesName, "normalizedData", getSettings(dataList[[seriesName]], "stackInfo"), getSettings(dataList[[seriesName]], "stackParam"))
      dataList[[seriesName]][["normalizedStackedData"]] <<- dataStacking
      
      updatePickerTypeKineticsView(session)
      
      if (isCalculated == FALSE) {
        paste0("No calculation have been done!")
      } else {
        paste0("Calculations are finished!")
      }
    })
    
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
  
  calculateNorming <- function(series, data_temp, normInfo = NULL) {
    data_list <- dataList[[series]][[data_temp]]
    columnSpec <- getSettings(dataList[[series]], "currentSpec")
    
    if(is.null(normInfo)) data_list <- map(data_list, \(x) normalize_data(x, columnSpec))
    if(!is.null(normInfo)) data_list <- map2(data_list, normInfo, \(x,y) normalize_data_with_Info(x, columnSpec, y))
    return(data_list)
  }
  
  calculateStacking <- function(series, data_temp, stackInfo = NULL, stackParam = NULL) {
    
    data_list <- dataList[[series]][[data_temp]]
    upsamplingResu <- input$resolutionUpsampling
    columnSpec <- getSettings(dataList[[series]], "currentSpec")
    stackTime <- input$inputStackTime
    stackPoint <- input$inputStackPoint
    
    if(!(is.null(stackParam) && is.null(stackInfo))) upsamplingResu <- stackParam$upsamplingResu
    
    data_list <- map(data_list, \(x) increase_resolution(x,upsamplingResu))
    
    if(is.null(stackParam) && is.null(stackInfo)) stackPoint_list <- map(data_list, \(x) getStackTimePoints(x, stackPoint, columnSpec))
    if(!(is.null(stackParam) && is.null(stackInfo))) stackPoint_list <- stackInfo
    
    data_list <- map2(data_list, stackPoint_list, \(x, y) moveTimeToStackTime(x, y, stackTime))
    
    data_list <- map2(data_list, stackPoint_list, \(x, y) moveIndexToStackIndex(x, y))
    
    if(!(is.null(stackParam) && is.null(stackInfo))) return(data_list)
    return(c(list(data_list = data_list), stackPoint_list = list(stackPoint_list), list(stackParam = list(stackTime = stackTime, stackPoint = stackPoint, upsamplingResu = upsamplingResu))))
    
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
  
 render_ggplot("kineticsViewPlot",
               expr = createKinPlot()

  )
  
  output$kineticsViewPlotly <- renderPlotly({
    return(ggplotly(createKinPlot()))
  }
  )
  
  # output$downloadKineticPlot <- downloadHandler(
  #   filename <- function() {
  #     paste('myKinetic', 'png', sep = ".")
  #   },
  #   content <- function(file) {
  #     png(file,
  #         width = input$shiny_width,
  #         height = input$shiny_height)
  # 
  #     plot <- createKinPlot()
  #     print(plot)
  #     dev.off()
  #   },
  #   contentType = "image/png"
  # )
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
      kinPlot <-
        ggplot(plotDataFrame, aes(x = Time, y = InwardCurr, color = Measurement)) +
        stat_summary(fun.y = "median", geom = "line") + pTheme + ggtitle(pTitle)
      
    }
    kinPlot <- kinPlot + theme(legend.position = "right")
    return(kinPlot)
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

combineListtoWide <- function(data_list) {

  pad_data <- pad_dataframes(data_list)
  
  combined_data_list <- bind_cols(pad_data, .name_repair = "universal_quiet")
  
  names_list <- rep(names(data_list), each = 3)
  names_df  <- colnames(data_list[[1]])
  namesComb <- paste0(names_list,".",names_df)
  colnames(combined_data_list) <- namesComb
  
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
      newVal <- -100 * (values[1:max_norm_Index] - min_valueAkt) / (max_value - min_valueAkt)
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
      
      interp <- approx(x, y, xout = new_x)  # Interpolate y-values at new x-values
      
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