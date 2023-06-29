source("globalStuff.r")

#### Factories and GUI Elements####
solidHeaderBoxFactory <-
  function(color, ...) {
    return(function(...) {
      box(
        ...,
        status = color,
        collapsible = TRUE,
        solidHeader = TRUE
      )
    })
  }
solidHeaderBoxes <-
  lapply(
    c(
      Blue = "primary",
      Green = "success",
      LightBlue = "info",
      Orange = "warning",
      red = "danger"
    ),
    solidHeaderBoxFactory
  )


colorBox <- function(name)
{
  return(
    solidHeaderBoxes$LightBlue(
      title = "Colors",
      width = 12,
      fluidPage(fluidRow(
        column(
          width = 6,
          selectizeInput(
            inputId = paste0(name,"Plot_colors"),
            "Plot Colors",
            choices = colorChoices[[name]],
            selected = colorSelected[[name]],
            multiple = T,
            options = list(create = TRUE)
          )
        ),
        column(
          width = 6,
          colorPicker(
            inputId = paste0(name,"Plot_colorPicker"),
            label = "Color List:",
            choices = list(
              "Standard" = c(
                "#000000",
                "#56B4E9",
                "#009E73",
                "#F0E442",
                "#0072B2",
                "#D55E00",
                "#CC79A7",
                "#999999",
                "#E69F00"
              ),
              "Blues" = brewer_pal(palette = "Blues")(9),
              "Reds" = brewer_pal(palette = "Reds")(9),
              "Greens" = brewer_pal(palette = "Greens")(9),
              "Greys" = brewer_pal(palette = "Greys")(9),
              "Purples" = brewer_pal(palette = "Purples")(9),
              "Reds" = brewer_pal(palette = "Reds")(9),
              "Yellow Orange" = brewer_pal(palette = "YlOrBr")(9)
            )
          )
        )
      ),
      fluidRow(
        column(
          10,
          textInput(
            inputId = paste0(name,"Plot_colorsText"),
            NULL,
            value = c("#000000,#0072B2,#009E73,#D55E00,#56B4E9,#F0E442,#CC79A7,#999999,#E69F00")
          )
        ),
        column(
          2,
          prettySwitch(inputId =  paste0(name,"Plot_switchColors"), "use Text colors", status = "primary"),
        )
      )),
      collapsed = TRUE
    )
  )
  
}


##### Sidebar######
sidebar <- dashboardSidebar(
  width = 200,
  sidebarMenu(
    id = "tabs",
    menuItem("Whats New",
             icon = icon("envelope"),
             tabName = "News"),
    menuItem(
      "Data",
      icon = icon("database"),
      menuSubItem("Import/Export", tabName = "DataImportExport"),
      menuSubItem("View and Edit Data", tabName = "DataView")
    ),
    menuItem(
      "Kinetics",
      icon = icon("chart-line"),
      menuSubItem("Calculation", tabName = "ViewCalculation"),
      menuSubItem("View Kinetics", tabName = "KineticsView"),
      menuSubItem("Overlays", tabName = "KineticsOverlays"),
      menuSubItem("Stats", tabName = "KineticsStatistic")
    ),
    menuItem(
      "Settings",
      icon = icon("wrench"),
      menuSubItem("Data Import", tabName = "SettingsDataImport"),
      menuSubItem("Data Manipulation", tabName = "SettingsDataManipulation"),
      menuSubItem("Other", tabName = "SettingsOther")
    ),
    menuItem("About",
             icon = icon("info"),
             tabName = "About")
  )
)

##### Body#####
body <- dashboardBody(
  #   tags$script("$(document).on('shiny:connected', function(event) {
  # var myWidth = $(window).width();
  # Shiny.onInputChange('shiny_width',myWidth)
  #
  # });"),
  #
  #   tags$script("$(document).on('shiny:connected', function(event) {
  # var myHeight = $(window).height();
  # Shiny.onInputChange('shiny_height',myHeight)
  #
  # });"),
  
  useSweetAlert(),
  tabItems(
    tabItem(
      "News",
      h3("Chanalyze - Ion Channel Kinetic Analyzer"),
      h4("New backend, new GUI, new everything!"),
      h4("Faster, more stable, new functionality, better error messages!"),
      h4("You will love it!")
    ),
    tabItem(
      "DataImportExport",
      fluidRow(
        solidHeaderBoxes$Blue(
          title = "Import Data",
          width = 3,
          actionButton("Import_Data",
                       "Import Measurements",
                       icon = icon("floppy-disk")),
            shinyFilesButton('Load_rData', 'Load rData file', 'Please select a file', FALSE, icon = icon("r-project", fill = "steelblue")),
          hr(),
          withLoader(
            textOutput("StatusDataImport"),
            proxy.height = "120px",
            type = "image",
            loader = "LoadingChannel.gif"
          )
        ),
        solidHeaderBoxes$Blue(
          title = "Export Data",
          width = 3,
          shinySaveButton("save", "Save as .xlsx", "Save file as...", filetype=list(xlsx="xlsx"), viewtype = "icon",icon = icon("file-excel")),
          shinySaveButton("Save_rData", "Save as .rData", "Save file as...", filetype=list(rData=".rData"), viewtype = "icon",icon = icon("r-project")),
          hr(),
          withLoader(
            textOutput("StatusDataExport"),
            proxy.height = "120px",
            type = "image",
            loader = "LoadingChannel.gif"
          )
        ),
        solidHeaderBoxes$Blue(
          title = "Workspace",
          width = 3,
          actionButton("ChangeWorkspace",
                       "Change Workspace!",
                       icon = icon("folder-open")),
          hr(),
          textOutput("Workspace")
        )
      ),
      bsModal(
        "importDataModal",
        "Import Data",
        "Import_Data",
        size = "small",
        # Add a switch and a text input to the modal dialog
        switchInput(
          inputId = "newMeasSwitch",
          onLabel = "New Series",
          offLabel = "Add to Existing",
          value = TRUE
        ),
        
        selectizeInput(
          inputId = "newMeasName",
          label = "Series Name",
          multiple = FALSE,
          choices = c(),
          options = list(create = T)
        ),
        
        # Add OK/Cancel buttons to the modal dialog
        footer = tagList(
          modalButton("Cancel"),
          #actionButton("okBtn", "OK", class = "btn-primary"),
          shinyFilesButton('okBtn', 'Ok', 'Please select files',multiple = TRUE),
          tags$head(tags$style(
            "#importDataModal .modal-footer{ display:none}"
          ))
        )
      )
      
    ),
    tabItem(
      "DataView",
      solidHeaderBoxes$Blue(
        title = "Select Measurement",
        width = 12,
        fluidRow(
          column(
            width = 3,
            pickerInput(
              inputId = "pickerSeries",
              label = "Series",
              choices = ""
            )
          ),
          column(
            width = 3,
            pickerInput(
              inputId = "pickerType",
              label = "Type",
              choices = ""
            )
          ),
          column(
            width = 3,
            pickerInput(
              inputId = "pickerMeasurement",
              label = "Measurement",
              choices = ""
            )
          ),
          column(
            width = 1,
            div(
              align = "center",
              style = "padding-top: 25px;width:100%",
              actionButton(inputId = "saveEditorButton",
                           label = "Save")
            )
          )
        )
      ),
      fluidRow(column(width = 4,
                      dataEditUI("edit-1")),
               column(width = 8,
                      plotlyOutput("dataEditorPlot")))
      
    ),
    tabItem("ViewCalculation",
            fluidRow(
              column(width = 6,
                     solidHeaderBoxes$Blue(
                       width = 12,
                       fluidRow(
                         column(
                           width = 8,
                           pickerInput(
                             inputId = "seriesPickerCalculations",
                             label = "Series",
                             choices = ""
                           )
                         ),
                         column(
                           width = 2,
                           div(
                             actionButton(inputId = "calculateSeries",
                                          label = "Calculate"),
                             style = 'top: 25px;position:relative;'
                           )
                         ),
                         column(
                           width = 2,
                           div(
                             actionButton(inputId = "calculateAllSeries",
                                          label = "Calculate All"),
                             style = 'top: 25px;position:relative;'
                           )
                         )
                       )
                     )),
              column(
                width = 4,
                solidHeaderBoxes$Blue(
                  title = "Status",
                  width = 12,
                  withLoader(textOutput("outPutCalc"),
                             type = "image",
                             loader = "LoadingChannel.gif")
                )
              )
            )),
    tabItem("KineticsView",
            fluidPage(
              fluidRow(
                column(
                  width = 2,
              dropdownButton(
                selectInput(
                  inputId = "seriesPickerKineticsView",
                  label = "choose Series",
                  choices = ""
                ),
                selectInput(
                  inputId = "typePickerKineticsView",
                  label = "choose Type",
                  choices = ""
                ),
                radioGroupButtons(
                  inputId = "curSpecKineticsView",
                  label = "Label",
                  choiceNames = c("Inward",
                                  "Outward"),
                  choiceValues = c("InwardCurr", "OutwardCurr"),
                  selected = "OutwardCurr",
                  disabled = TRUE,
                  individual = TRUE,
                  checkIcon = list(
                    yes = tags$i(class = "fa fa-circle",
                                 style = "color: steelblue"),
                    no = tags$i(class = "fa fa-circle-o",
                                style = "color: steelblue")
                  )
                ),
                radioGroupButtons(
                  inputId = "curKineticsStyle",
                  label = "Style",
                  choiceNames = c("Overlayed",
                                  "Single",
                                  "Median"),
                  choiceValues = c("Overlayed", "Single", "Median"),
                  selected = "Overlayed",
                  individual = TRUE,
                  checkIcon = list(
                    yes = tags$i(class = "fa fa-circle",
                                 style = "color: steelblue"),
                    no = tags$i(class = "fa fa-circle-o",
                                style = "color: steelblue")
                  )
                ),
                
                checkboxGroupButtons(
                  inputId = "measurementPickerKineticsView",
                  label = "choose Measurements",
                  choices = "",
                  checkIcon = list(
                    yes = tags$i(class = "fa fa-check-square",
                                 style = "color: steelblue"),
                    no = tags$i(class = "fa fa-square-o",
                                style = "color: steelblue")
                  ),
                  direction = "vertical"
                ),
                
                icon = icon("list-check"),
                
                tooltip = tooltipOptions(title = "Click to change inputs !")
              )
              ),
              column(
                width = 3,
                dropdownButton(
                  uiOutput("measInfo"),
                  icon = icon("info"),
                  tooltip = tooltipOptions(title = "Click to series information !")
                  
              )
              )
              ),
              fluidRow(
                column(
                  width = 12,
                  
                  ggplot_output("kineticsViewPlot", height  = "800px"),
                  # downloadButton("downloadKineticPlot", "Download the plot"),
                  actionButton(inputId = "kineticToPlotly",
                               label = "Show Interactive Plot")
                ),
                bsModal(
                  "kineticsViewPlotlyModal",
                  "View Kinetic",
                  "kineticToPlotly",
                  size = "large",
                  # Add a switch and a text input to the modal dialog
                  plotlyOutput("kineticsViewPlotly", height = "800px")
                )
              )
            )),
    tabItem("KineticsOverlays",
            fluidPage(
              fluidRow(
                column(
                  width = 2,
                  dropdownButton(
                    radioGroupButtons(
                      inputId = "curSpecMedianView",
                      label = "Label",
                      choiceNames = c("Inward",
                                      "Outward"),
                      choiceValues = c("InwardCurr", "OutwardCurr"),
                      selected = "OutwardCurr",
                      disabled = TRUE,
                      individual = TRUE,
                      checkIcon = list(
                        yes = tags$i(class = "fa fa-circle",
                                     style = "color: steelblue"),
                        no = tags$i(class = "fa fa-circle-o",
                                    style = "color: steelblue")
                      )
                    ),
                    radioGroupButtons(
                      inputId = "curMedianStyle",
                      label = "Style",
                      choiceNames = c("Overlayed",
                                      "Single"),
                      choiceValues = c("Overlayed", "Single"),
                      selected = "Overlayed",
                      individual = TRUE,
                      checkIcon = list(
                        yes = tags$i(class = "fa fa-circle",
                                     style = "color: steelblue"),
                        no = tags$i(class = "fa fa-circle-o",
                                    style = "color: steelblue")
                      )
                    ),
                    checkboxGroupButtons(
                      inputId = "measurementPickerMedianView",
                      label = "choose Measurements",
                      choices = "",
                      checkIcon = list(
                        yes = tags$i(class = "fa fa-check-square",
                                     style = "color: steelblue"),
                        no = tags$i(class = "fa fa-square-o",
                                    style = "color: steelblue")
                      ),
                      direction = "vertical"
                    ),
                    icon = icon("list-check"),
                    tooltip = tooltipOptions(title = "Click to change inputs !")
                  )
                ),
                column(
                  width = 2,
                  dropdownButton(
                    numericRangeInput(
                      inputId = "rangeXlimsOverlayPlot",
                      label = "X-Axis",
                      value = c(0, 10),
                      min = -10000,
                      max = 10000
                    ),
                    textInput(
                      inputId =  "XaxisBreaks",
                      label = "Ticks X-Axis",
                      placeholder = "Enter comma seperated values"
                    )
                    ,
                    numericRangeInput(
                      inputId = "rangeYlimsOverlayPlot",
                      label = "Y-Axis",
                      value = c(-200, 200),
                      min = -10000,
                      max = 10000
                    ),
                    textInput(
                      inputId = "YaxisBreaks",
                      label =  "Ticks Y-Axis",
                      placeholder = "Enter comma seperated values"
                    )
                    ,
                    fluidRow(
                      column(
                        width = 4,
                        numericInput(
                          inputId = "lineSizeOverlayPlot",
                          label =  "Linesize",
                          value = 1
                        )
                      ),
                      column(
                        width = 4,
                        numericInput(
                          inputId = "fontSizeOverlayPlot",
                          label =  "Fontsize",
                          value = 12
                        )
                      )
                    ),
                    prettyToggle(
                      inputId = "overlayPlotNorming",
                      label_on = "Medians will be normed!",
                      label_off = "Median stay raw!",
                      value = TRUE,
                      outline = TRUE,
                      plain = TRUE,
                      icon_on = icon("thumbs-up"),
                      icon_off = icon("thumbs-down"),
                      bigger = TRUE
                    ),
                    prettyToggle(
                      inputId = "overlayPlotLegend",
                      label_on = "Legend in plot!",
                      label_off = "No legend in plot!",
                      value = TRUE,
                      outline = TRUE,
                      plain = TRUE,
                      icon_on = icon("thumbs-up"),
                      icon_off = icon("thumbs-down"),
                      bigger = TRUE
                    ),
                    
                    
                    icon = icon("sliders"),
                    tooltip = tooltipOptions(title = "Click to change axes and more !")
                  )
                ),
                column(width = 8,
                       colorBox("overlay"),)
              ),
              fluidRow(
                column(
                  width = 12,
                  
                  ggplot_output("kineticsOverlayPlot", height  = "800px"),
                  actionButton(inputId = "OverlayToPlotly",
                               label = "Show Interactive Plot")
                ),
                bsModal(
                  "kineticsOverlayPlotlyModal",
                  "View Overlay",
                  "OverlayToPlotly",
                  size = "large",
                  # Add a switch and a text input to the modal dialog
                  plotlyOutput("kineticsOverlayPlotly", height = "800px")
                )
              )
            )),
    tabItem(
      "KineticsStatistic",
            fluidPage(
              fluidRow(
                column(
                  width = 2,
                  dropdownButton(
                    radioGroupButtons(
                      inputId = "curSpecStatisticView",
                      label = "Label",
                      choiceNames = c("Inward",
                                      "Outward"),
                      choiceValues = c("InwardCurr", "OutwardCurr"),
                      selected = "OutwardCurr",
                      disabled = TRUE,
                      individual = TRUE,
                      checkIcon = list(
                        yes = tags$i(class = "fa fa-circle",
                                     style = "color: steelblue"),
                        no = tags$i(class = "fa fa-circle-o",
                                    style = "color: steelblue")
                      )
                    ),
                    radioGroupButtons(
                      inputId = "curStatisticStyle",
                      label = "Style",
                      choiceNames = c("Overlayed",
                                      "Single"),
                      choiceValues = c("Overlayed", "Single"),
                      selected = "Overlayed",
                      individual = TRUE,
                      checkIcon = list(
                        yes = tags$i(class = "fa fa-circle",
                                     style = "color: steelblue"),
                        no = tags$i(class = "fa fa-circle-o",
                                    style = "color: steelblue")
                      )
                    ),
                    textInput(
                      inputId = "statValueMarker",
                      label = "Time to:",
                      value = "25,50,75;100;75,50,25,0"
                    ),
                    checkboxGroupButtons(
                      inputId = "measurementPickerStatisticView",
                      label = "choose Measurements",
                      choices = "",
                      checkIcon = list(
                        yes = tags$i(class = "fa fa-check-square",
                                     style = "color: steelblue"),
                        no = tags$i(class = "fa fa-square-o",
                                    style = "color: steelblue")
                      ),
                      direction = "vertical"
                    ),
                    icon = icon("list-check"),
                    tooltip = tooltipOptions(title = "Click to change inputs !")
                  )
                ),
                column(
                  width = 2,
                  dropdownButton(
                    numericRangeInput(
                      inputId = "rangeYlimsStatisticPlot",
                      label = "Y-Axis",
                      value = c(-5, 110),
                      min = -10000,
                      max = 10000
                    ),
                    textInput(
                      inputId = "YaxisBreaksStatisticPlot",
                      label =  "Ticks Y-Axis",
                      placeholder = "Enter comma seperated values"
                    )
                    ,
                    fluidRow(
                      column(
                        width = 4,
                        numericInput(
                          inputId = "lineSizeStatisticPlot",
                          label =  "Linesize",
                          value = 1
                        )
                      ),
                      column(
                        width = 4,
                        numericInput(
                          inputId = "fontSizeStatisticPlot",
                          label =  "Fontsize",
                          value = 12
                        )
                      )
                    ),
                    prettyToggle(
                      inputId = "StatisticPlotLegend",
                      label_on = "Legend in plot!",
                      label_off = "No legend in plot!",
                      value = TRUE,
                      outline = TRUE,
                      plain = TRUE,
                      icon_on = icon("thumbs-up"),
                      icon_off = icon("thumbs-down"),
                      bigger = TRUE
                    ),
                    
                    
                    icon = icon("sliders"),
                    tooltip = tooltipOptions(title = "Click to change axes and more !")
                  )
                ),
                column(width = 8,
                       colorBox("Statistic"))
              ),
              fluidRow(
                column(
                  width = 12,
                  ggplot_output("kineticsStatisticPlot", height  = "800px"),
                  actionButton(inputId = "StatisticToPlotly",
                               label = "Show Interactive Plot")
                ),
                bsModal(
                  "kineticsStatisticPlotlyModal",
                  "View Overlay",
                  "StatisticToPlotly",
                  size = "large",
                  # Add a switch and a text input to the modal dialog
                  plotlyOutput("kineticsStatisticPlotly", height = "800px")
                )
              )
            )
      ),
    tabItem(
      "SettingsDataImport",
      fluidRow(
        solidHeaderBoxes$Blue(
          title = "File specifications",
          width = 2,
          pickerInput(
            inputId = "InputImportSeperator",
            label = "File Seperator",
            choices = c(",", ";", " ", "|" , "t", "fixed width file")
          ),
          textInput(
            inputId = "InputIndexName",
            label = "First Column Name",
            value = "Index"
          )
        ),
        solidHeaderBoxes$Blue(
          title = "Column specifications",
          width = 4,
          fluidRow(
            column(
              6,
              pickerInput(
                inputId = "InputImportCurrentSpec",
                label = "Current",
                choices = c("Inward", "Outward", "Both"),
                selected = "Inward"
              ),
              selectizeInput(
                inputId = "InputImportCurrentCols",
                label = "Columns",
                multiple = TRUE,
                choices = c(1, 2, 3, 4, 5),
                selected = 3,
                options = list(create = T)
              )
            ),
            column(
              6,
              selectizeInput(
                inputId = "InputImportTimeCol",
                label = "Column Time",
                multiple = FALSE,
                choices = c(1, 2, 3, 4, 5),
                selected = 2,
                options = list(create = T)
                
              )
              
            )
          )
          
        )
      )
    ),
    tabItem(
      "SettingsDataManipulation",
      fluidRow(
        column(
          width = 2,
          solidHeaderBoxes$Blue(
            title = "Smoothing Algorithm",
            width = 12,
            pickerInput(
              "inputAlgorithmSmoothing",
              label = "Smoothing Algorithm",
              choices = c(
                "SmoothingSpline",
                "Gaussian",
                "GaussianKernel",
                "LOESS",
                "LOWESS",
                "Fourier"
              ),
              selected = "SmoothingSpline"
            )
          )
        ),
        column(
          width = 10,
          solidHeaderBoxes$Blue(
            title = "Smoothing Options",
            width = 12,
            fluidRow(
              column(
                2,
                h4(style = "text-align: center;",
                   "Smoothing Spline"),
                numericInput(
                  inputId = "smoothingSplineSpar",
                  label = "spar",
                  value = 0.45,
                  min = 0,
                  max = 1
                )
              ),
              column(
                2,
                style = "border-right: 1px solid black;border-left: 1px solid black",
                h4(style = "text-align: center;",
                   "Gaussian"),
                numericInput(
                  inputId = "gaussianWindow",
                  label = "window",
                  value = 0.01,
                  min = 0.0001,
                  max = 1
                ),
                numericInput(
                  inputId = "GaussianAlpha",
                  label = "alpha",
                  value = 1,
                  min = 1,
                  max = 1000
                )
              ),
              column(
                2,
                h4(style = "text-align: center;",
                   "Gaussian Kernel"),
                numericInput(
                  inputId = "GaussianKernelBandwith",
                  label = "bandwith",
                  value = 1,
                  min = 1,
                  max = 1000
                )
              ),
              column(
                2,
                style = "border-right: 1px solid black;border-left: 1px solid black",
                h4(style = "text-align: center;",
                   "LOESS"),
                numericInput(
                  inputId = "loessSpan",
                  label = "span",
                  value = 0.05,
                  min = 0.001,
                  max = 1
                ),
                numericInput(
                  inputId = "loessDegree",
                  label = "degree",
                  value = 1,
                  min = 0,
                  max = 2
                )
              ),
              column(
                2,
                style = "border-right: 1px solid black;",
                h4(style = "text-align: center;",
                   "LOWESS"),
                numericInput(
                  inputId = "lowessF",
                  label = "f",
                  value = 0.01,
                  min = 0.001,
                  max = 1
                ),
                numericInput(
                  inputId = "lowessIter",
                  label = "iter",
                  value = 5,
                  min = 1,
                  max = 20
                )
              ),
              column(
                2,
                h4(style = "text-align: center;",
                   "Fourier"),
                numericInput(
                  inputId = "FouriercutoffFrequency",
                  label = "Cutoff Frequency",
                  value = 95,
                  min = 1,
                  max = 10000
                )
              )
            )
          )
        )
      ),
      fluidRow(
        column(
          width = 3,
          solidHeaderBoxes$Blue(
            title = "Up/Downsampling",
            width = 12,
            numericInput(
              inputId = "resolutionUpsampling",
              label = "Factor Upsampling",
              value = 10,
              min = 1,
              max = 5000
            ),
            numericInput(
              inputId = "resolutionDownsampling",
              label = "Factor Downsampling",
              value = 10,
              min = 1,
              max = 5000
            ),
            radioGroupButtons(
              inputId = "settingsSampling",
              label = "Type:",
              choices = c("Upsampling",
                          "Downsampling",
                          "Both"),
              status = "primary",
              checkIcon = list(
                yes = icon("ok",
                           lib = "glyphicon"),
                no = icon("remove",
                          lib = "glyphicon")
              )
            )
          )
        ),
        column(
          width = 3,
          solidHeaderBoxes$Blue(
            title = "Kinetic Stacking",
            width = 12,
            tags$div(
              title = "Set the value of the kinetic at which they get stacked.",
              numericInput(
                "inputStackPoint",
                label = "Stackpoint",
                value = 10,
                min = 0,
                max = 100
              )
            ),
            tags$div(
              title = "Set the value of the time at the Stackpoint.",
              numericInput(
                "inputStackTime",
                label = "Stacktime",
                value = 1,
                min = 0
              )
            ),
            tags$div(
              title = "Account for the time delay, between the start of an activation and the stack point.",
              numericInput(
                "inputTimeUntilStack",
                label = "Time Until Stacking",
                value = 0,
                min = 0,
                max = 100,
                step = 0.001
              )
            ),
            tags$div(
              title = "Get more precise de/inactivations by setting the startpoint of the calculation # seconds after the activation beginning.",
              numericInput(
                "durationPhotoswitch",
                label = "Duration Photoswitch",
                value = 0,
                min = 0,
                max = 100,
                step = 0.001
              )
            )
          )
        )
      )
    ),
    tabItem(
      "SettingsOther",
      fluidRow(
        column(
          width = 10,
          solidHeaderBoxes$Blue(
            title = "Other",
            fluidRow(
            column(
              width = 6,
              radioGroupButtons(
              inputId = "WithInact",
              label = "Kinetic has:",
              choiceNames  = c("Activation & Inactivation", 
                          "Activation"),
              choiceValues = c(1,0), 
              selected = 1,
              direction = "vertical",
              status = "primary",
              checkIcon = list(
                yes = icon("ok", 
                           lib = "glyphicon"),
                no = icon("remove",
                          lib = "glyphicon"))
            )
            ),
            column(
              width = 6,
            radioGroupButtons(
              inputId = "WithSteepPeak",
              label = "Kinetic is:",
              choiceNames  = c("Normal", 
                               "with a very sharp and steep peak"),
              choiceValues = c(1,0), 
              selected = 1,
              direction = "vertical",
              status = "primary",
              checkIcon = list(
                yes = icon("ok", 
                           lib = "glyphicon"),
                no = icon("remove",
                          lib = "glyphicon"))
            )
            )
            )
            )
        )
      )
    )
            
            
  )
)





##### Header#####
header <- dashboardHeader(title = span(
  "Chanalyze",
  tags$img(src="Chanalyze.png", width = '20%')),
  titleWidth  = 200)
#####
#####


ui <- function(request) {
  dashboardPage(title = "Chanalyze - Ion Channel Kinetic Analyzer",  header, sidebar, body, skin = "red")
}
