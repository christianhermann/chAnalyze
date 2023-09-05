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
      
      output$StatusDataExport <- renderText({
        if (isDataSaved == FALSE) {
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
    volumes <<- c(Workspace = getwd(), Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
    
    output$Workspace <- renderText({
      getwd()
    })
  },ignoreInit = TRUE)
  
  ####DataEditor####
  
  data_to_edit <- reactiveVal(data.frame(10, 10))
  data_edit <- dataEditServer("edit-1",
                              data = data_to_edit,
                              height = 750)
  
  observeEvent(input$pickerSeries, {
    updatePickerInput(
      session = session,
      inputId = "pickerType",
      choices = names(dataListwTypes(dataList[[input$pickerSeries]])),
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
  
  volumes <- c(Workspace = getwd(), Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  shinyFileChoose(input, "okBtn", roots = volumes, session = session, restrictions = system.file(package = "base"), filetypes=c('', 'asc'))
  
  observeEvent(input$okBtn, {
    isDataImported <<- FALSE
    if (input$newMeasName == "")
      return()
    paths <- parseFilePaths(volumes, input$okBtn)
    paths <- unlist(paths$datapath)
    if (length(paths) == 0)
      return()
    toggleModal(session = session,modalId = "importDataModal", toggle = "close")
    
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
      isDataImported <<- TRUE
      
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
      seriesList$all <<- c(seriesList$all, seriesName)
      seriesList$Median <<- c(seriesList$all, seriesName)
      seriesList$Statistic <<- c(seriesList$all, seriesName)
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
      updateMeasurementPickerMedianView(session)
      updateMeasurementPickerStatisticView(session)
      
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
    calculateFunction(input$seriesPickerCalculations)
  },ignoreInit = TRUE)
  
  observeEvent(input$calculateAllSeries, {
    #  map(seriesList$all, calculateFunction)
      calculateFunction(seriesList$all)
  }, ignoreInit = TRUE)

  isolate({
    calculateFunction <- function(series_List) {
      isCalculated <<- FALSE
      output$outPutCalc <- renderText({
        for (series in series_List) {
        print(series)
        seriesName <- series
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
        
        dataMedian <- calcKineticMedian(dataStacking, getSettings(dataList[[series]], "currentSpec"))
        dataList[[seriesName]]$settings$selectedMeas <<- names(dataList[[seriesName]]$rawData)
        dataList[[seriesName]][["medianData"]] <<- dataMedian

        dataPeakTime <- calculatePeakTimes(dataList[[seriesName]][["normalizedSmoothedStackedData"]], seriesName)
        dataList[[seriesName]][["peakTimeData"]] <<- dataPeakTime
        
        updatePickerTypeKineticsView(session)
        }
        if (isCalculated == FALSE) {
          paste0("No calculation have been done!")
        } else {
          paste0("Calculations are finished!")
        }
      })
    }
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
    dataNormMin <- input$normMin
    dataNormMax <- input$normMax
    
    withoutSteepPeak <- as.numeric(input$WithSteepPeak)
    
    if(input$WithInact == 1) {
      if(is.null(normInfo)) data_list <- map(data_list, \(x) normalize_data(x, columnSpec, dataNormMin, dataNormMax))
      if(!is.null(normInfo)) data_list <- map2(data_list, normInfo, \(x,y) normalize_data_with_Info(x, columnSpec, y, withoutSteepPeak, dataNormMin, dataNormMax))
    }
    if(input$WithInact == 0) {
      if(is.null(normInfo)) data_list <- map(data_list, \(x) normalize_data_wo_Inakt(x, columnSpec, dataNormMin, dataNormMax))
      if(!is.null(normInfo)) data_list <- map2(data_list, normInfo, \(x,y) normalize_data_with_Info_wo_Inakt(x, columnSpec, y, withoutSteepPeak, dataNormMin, dataNormMax))
    }
    
    return(data_list)
  }
  
  calculateStacking <- function(series, data_temp, stackInfo = NULL, stackParam = NULL) {
    
    data_list <- dataList[[series]][[data_temp]]
    upsamplingResu <- input$resolutionUpsampling
    downsamplingResu <- input$resolutionDownsampling
    settingsSampling <- input$settingsSampling
    columnSpec <- getSettings(dataList[[series]], "currentSpec")
    stackTime <- input$inputStackTime
    stackPoint <- input$inputStackPoint
    
    if(!(is.null(stackParam) && is.null(stackInfo))) upsamplingResu <- stackParam$upsamplingResu
    
    if(settingsSampling == "Upsampling" || settingsSampling ==  "Both") data_list <- map(data_list, \(x) increase_resolution(x,upsamplingResu))
    
    if(is.null(stackParam) && is.null(stackInfo)) stackPoint_list <- map(data_list, \(x) getStackTimePoints(x, stackPoint, columnSpec))
    if(!(is.null(stackParam) && is.null(stackInfo))) stackPoint_list <- stackInfo
    
    data_list <- map2(data_list, stackPoint_list, \(x, y) moveTimeToStackTime(x, y, stackTime))
    
    data_list <- map2(data_list, stackPoint_list, \(x, y) moveIndexToStackIndex(x, y))
    
    if(!(is.null(stackParam) && is.null(stackInfo))) {
      if(settingsSampling == "Downsampling" || settingsSampling ==  "Both")  map(data_list, \(x) decrease_resolution(x,upsamplingResu))
      return(data_list)
    }
    return(c(list(data_list = data_list), stackPoint_list = list(stackPoint_list), list(stackParam = list(stackTime = stackTime, stackPoint = stackPoint, upsamplingResu = upsamplingResu, downsamplingResu = downsamplingResu, settingsSampling = settingsSampling))))
    
  }
  
  calculatePeakTimes <- function(data_list, series, data_temp) {
   # data_list <- data_list[[data_temp]]
    stackTime <-getSettings(dataList[[series]], "stackParam")$stackTime
    currSpec <- getSettings(dataList[[series]], "currentSpec")
    timeUntilStack <- input$inputTimeUntilStack
    durationPhotoswitch <- input$durationPhotoswitch
    peakPoints <- splitStringValue(input$statValueMarker)
    peakList <- map(data_list, \(x) get_peak_times(x,peakPoints$peakPointsBef, peakPoints$PeakPoint, peakPoints$peakPointsAft, currSpec, timeUntilStack, durationPhotoswitch, stackTime))
    return(peakList)
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
      selected = dataList[[input$seriesPickerKineticsView]]$settings$selectedMeas,
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
      selected = dataList[[input$seriesPickerKineticsView]]$settings$selectedMeas,
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
                expr = createKinPlot())
  
  output$kineticsViewPlotly <- renderPlotly({
    return(ggplotly(createKinPlot()))
  })
  
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
    dataList[[input$seriesPickerKineticsView]]$settings$selectedMeas <<- input$measurementPickerKineticsView
    currSpec <- input$curSpecKineticsView
    style <- input$curKineticsStyle
    if(style != "Median")     combinedList <- combineListtoLong(data_list)
    if(style == "Median") {   
      combinedList  <- calcKineticMedian(data_list, currSpec)
      dataList[[input$seriesPickerKineticsView]][["medianData"]] <<- combinedList}
    dataList[[input$seriesPickerKineticsView]][["peakTimeData"]]  <<- calculatePeakTimes(data_list, input$seriesPickerKineticsView, "normalizedSmoothedStackedData")
    
    kineticPlot <-
      createKineticPlot(combinedList, style =style,
                        pTitle = paste0(input$seriesPickerKineticsView,
                                        ": ", input$typePickerKineticsView),
                        currSpec = currSpec)
    
    if(all(dim(kineticPlot$data) == c(0,0))) kineticPlot = ggplot()
    return(kineticPlot)
  }
  
  output$measInfo <- renderUI(getInfo())
  getInfo <- function() {
    selPicker <- input$seriesPickerKineticsView
    selList <- dataList[[selPicker]]
    settings_list <- selList$settings
    settings_list <- settings_list[1:3]
    text <- HTML(paste0("<h4>Smoothing Algorithm:<//h4> <br>", settings_list$smoothingAlgo, "<h4>Parameters:<//h4> <br>", settings_list$smoothingParam))
    return(text)
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
      updateRadioGroupButtons(
        session = session,
        inputId = "curSpecMedianView",
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
      updateRadioGroupButtons(
        session = session,
        inputId = "curSpecMedianView",
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
      choices = names(dataListwTypes(dataList[[input$seriesPickerKineticsView]])),
      selected =  tail(names(dataListwTypes(dataList[[input$seriesPickerKineticsView]])), n = 1)
    )
    
  }
  
  ####
  ####Median OVerlay Plot####
  
  render_ggplot("kineticsOverlayPlot",
                expr = createMedianOverlayPlot())
  
  output$kineticsOverlayPlotly <- renderPlotly({
    return(ggplotly(createMedianOverlayPlot()))
  })
  
  
  createMedianOverlayPlot <-  function(){
    currSpec <- input$curSpecKineticsView
    data_list <- choseSelectedList(dataList,
                                   input$measurementPickerMedianView)
    dataNormMin <- input$normMin
    dataNormMax <- input$normMax
    
    seriesList$Median <<- input$measurementPickerMedianView 
    stackTime <- map(data_list,\(x) getSettings(x, "stackParam")$stackTime)
    colorList <- colorSelected$Median
    if(input$overlayPlot_switchColors == TRUE) colorList <- str_split_1(input$overlayPlot_colorsText,",")
    style <- input$curMedianStyle
    median_list <- map(data_list, \(x) x$medianData)
    curSpecs <- map(data_list,\(x) getSettings(x, "currentSpec"))
    if(input$overlayPlotNorming == 1) median_list <- pmap(list(median_list, curSpecs, stackTime), \(x,y,z) normalize_median_data(x,y, dataNormMin, dataNormMax,z))
    combined_dataframe <- combineListtoLong(median_list)
    xALims <- input$rangeXlimsOverlayPlot
    xBreaks <- input$XaxisBreaks
    yALims <- input$rangeYlimsOverlayPlot
    yBreaks <- input$YaxisBreaks
    fontSize <- input$fontSizeOverlayPlot
    lineSize <- input$lineSizeOverlayPlot
    
    newPlot <- createOverlayPlot(combined_dataframe, currSpec = currSpec, style = style, lineSize = lineSize)
    finalPlot <- customizePlot(newPlot, colorList, xALims, xBreaks, yALims, yBreaks, fontSize)
    
    if(input$overlayPlotLegend == 0) finalPlot <- finalPlot + theme(legend.position = "none")
    
    return(finalPlot)
  }
  
  updateMeasurementPickerMedianView <- function(session, selected = names(dataList)) {
    
    updateCheckboxGroupButtons(
      session = session,
      inputId = "measurementPickerMedianView",
      choices = names(dataList),
      selected = selected,
      checkIcon = list(
        yes = tags$i(class = "fa fa-check-square",
                     style = "color: steelblue"),
        no = tags$i(class = "fa fa-square-o",
                    style = "color: steelblue")
      )
    )
  }
  
  ###Colors###
  observeEvent(input$overlayPlot_colors, {
    colorSelected$Median <<- input$overlayPlot_colors
    colorChoices$Median <<- input$overlayPlot_colors
    
  }, ignoreInit = TRUE)
  
  observe({
    col <- input$overlayPlot_colorPicker
    colorChoices$Median <<- c(colorChoices$Median, col)
    updateSelectizeInput(
      inputId = "overlayPlot_colors",
      choices = colorChoices$Median,
      selected = colorSelected$Median
    )
    isolate(updateTextInput(session, inputId = "overlayPlot_colorsText",
                            label = NULL,
                            value  = c(input$overlayPlot_colorsText,col)))
  })
  
  ####
  ####StatisticsPlot####
  render_ggplot("kineticsStatisticPlot",
                expr = createStatisticOverlayPlot())
  
  output$kineticsStatisticPlotly <- renderPlotly({
    return(ggplotly(createStatisticOverlayPlot()))
  })
  
  
  createStatisticOverlayPlot <-  function(){
    currSpec <- input$curSpecStatisticView
    data_list <- choseSelectedList(dataList,
                                   input$measurementPickerStatisticView)
    
    seriesList$Statistic <<- input$measurementPickerStatisticView 
    
    colorList <- colorSelected$Statistic
    if(input$StatisticPlot_switchColors == TRUE) colorList <- input$StatisticPlot_colorsText
    style <- input$curStatisticStyle
    xALims <- input$rangeXlimsStatisticPlot
    xBreaks <- input$XaxisBreaksStatisticPlot
    yALims <- input$rangeYlimsStatisticPlot
    yBreaks <- input$YaxisBreaksStatisticPlot
    fontSize <- input$fontSizeStatisticPlot
    lineSize <- input$lineSizeStatisticPlot
    combined_dataframe <- combinePeakTimeList(data_list)
    
    newPlot <- createPeakStatisticPlot(combined_dataframe, lineSize = lineSize)
    
    finalPlot <- customizePlot(newPlot, colorList, xALims, xBreaks, yALims, yBreaks, fontSize)
    
    if(input$StatisticPlotLegend == 0) finalPlot <- finalPlot + theme(legend.position = "none")
    return(finalPlot)
  }
  
  updateMeasurementPickerStatisticView <- function(session, selected = names(dataList)) {
    
    updateCheckboxGroupButtons(
      session = session,
      inputId = "measurementPickerStatisticView",
      choices = names(dataList),
      selected = selected,
      checkIcon = list(
        yes = tags$i(class = "fa fa-check-square",
                     style = "color: steelblue"),
        no = tags$i(class = "fa fa-square-o",
                    style = "color: steelblue")
      )
    )
  }
  
  ###Colors###
  observeEvent(input$StatisticPlot_colors, {
    colorSelected$Statistic <<- input$StatisticPlot_colors
    colorChoices$Statistic <<- input$StatisticPlot_colors
    
  }, ignoreInit = TRUE)
  
  observe({
    col <- input$Statistic_colorPicker
    colorChoices$Statistic <<- c(colorChoices$Statistic, col)
    updateSelectizeInput(
      inputId = "StatisticPlot_colors",
      choices = colorChoices$Statistic,
      selected = colorSelected$Statistic
    )
    isolate(updateTextInput(session, inputId = "Statisticplot_colorsText",
                            label = NULL,
                            value  = c(input$StatisticPlot_colorsText,col)))
  })
  
  ####Saving/Loading
  
  
  shinyFileSave(input, "save", roots = volumes, session = session, restrictions = system.file(package = "base"))
  shinyFileSave(input, "Save_rData", roots = volumes, session = session, restrictions = system.file(package = "base"))
  shinyFileChoose(input, "Load_rData", roots = volumes, session = session, restrictions = system.file(package = "base"), filetypes=c('', 'rData'))
  
  observeEvent(input$save,{
    fileinfo <- parseSavePath(volumes, input$save)
    if (nrow(fileinfo) > 0) {
      try({
      saveDataAsXlsx(dataList, as.character(fileinfo$datapath))
      isDataSaved <<- TRUE
      sendSweetAlert(
        session = session,
        title = "Success !",
        text = paste0("Saved: ",as.character(fileinfo$datapath)),
        type = "success"
      )
      })
    }
  },ignoreInit = TRUE)
  
  observeEvent(input$Save_rData,{
    fileinfo <- parseSavePath(volumes, input$Save_rData)
    if (nrow(fileinfo) > 0) {
      try({
      saveDataAsR(dataList, colorChoices, colorSelected, selectedColors, seriesList, as.character(fileinfo$datapath))
      isDataSaved <<- TRUE
      sendSweetAlert(
        session = session,
        title = "Success !",
        text = paste0("Saved: ",as.character(fileinfo$datapath)),
        type = "success"
      )
      })
    }
  },ignoreInit = TRUE)
  
  ###Load R
  observeEvent(input$Load_rData,{
    fileinfo <- parseFilePaths(volumes, input$Load_rData)
    if (nrow(fileinfo) > 0) {
      tryCatch({
        load(fileinfo$datapath)
        isDataImported <<- FALSE
        dataList <<- c(dataList, data_list)
        colorChoices <<- color_Choices
        colorSelected <<- color_Selected
        selectedColors <<- selected_Colors
        seriesList <<- series_List
        
        updateSelectizeInput(
          session = session,
          inputId = "newMeasName",
          choices = seriesList
        )
        
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
        
        walk(seriesList$all, updateCurSpecKineticsView)
        updateMeasurementPickerMedianView(session, seriesList$Median)
        updateMeasurementPickerStatisticView(session, seriesList$Statistic)
        
        updateSelectizeInput(
          inputId = "overlayPlot_colors",
          choices = colorChoices$Median,
          selected = colorSelected$Median
        )
        
        updateSelectizeInput(
          inputId = "StatisticPlot_colors",
          choices = colorChoices$Statistic,
          selected = colorSelected$Statistic
        )
        
        
        isDataImported <<- TRUE
        sendSweetAlert(
          session = session,
          title = "Success !",
          text = "Data was imported",
          type = "success"
        )
      }
      , error = function(e) {
        isolate({
          output$StatusDataImport <- renderText({e})
        })
      }
      )
      isolate({
        output$StatusDataImport <- renderText({
          if (isDataImported == TRUE) {
            paste0("Data import succesfull!")
          } else {
            paste0("Data import not succesfull!")
          }
        })
      })
    }
  },ignoreInit = TRUE)

####

if (!interactive()) {
  session$onSessionEnded(function() {
    stopApp()
    q("no")
  })
}

})