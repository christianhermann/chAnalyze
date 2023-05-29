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


##### Sidebar######
sidebar <- dashboardSidebar(
  width = 200,
  sidebarMenu(
    id = "tabs",
    menuItem(
      "Whats New",
      icon = icon("envelope", lib = "glyphicon"),
      tabName = "News"
    ),
    menuItem(
      "Data",
      icon = icon("floppy-disk", lib = "glyphicon"),
      menuSubItem("Import/Export", tabName = "DataImportExport"),
      menuSubItem("View and Edit Data", tabName = "DataView")
    ),
    menuItem(
      "Kinetics",
      icon = icon("stats", lib = "glyphicon"),
      menuSubItem("Calculation", tabName = "ViewCalculation"),
      menuSubItem("View Kinetics", tabName = "KineticsView"),
      menuSubItem("Overlays", tabName = "KineticsOverlays")
    ),
    menuItem(
      "Settings",
      icon = icon("wrench",  lib = "glyphicon"),
      menuSubItem("Data Import", tabName = "SettingsDataImport"),
      menuSubItem("Data Manipulation", tabName = "SettingsDataManipulation"),
      menuSubItem("Other", tabName = "SettingsOther")
    ),
    menuItem(
      "About",
      icon = icon("info-sign", lib = "glyphicon"),
      tabName = "About"
    )
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
      h3("Ion Channel Kinetic Analyzer"),
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
          actionButton(
            "Import_Data",
            "Import Measurements",
            icon = icon("floppy-open", lib = "glyphicon")
          ),
          actionButton("Load_Rdata", " Load Rdata", icon = icon("open", lib =
                                                                  "glyphicon")),
          hr(),
          withLoader(
            textOutput("StatusDataImport"),
            proxy.height = 20,
            type = "image",
            loader = "LoadingChannel.gif"
          )
        ),
        solidHeaderBoxes$Blue(
          title = "Export Data",
          width = 2,
          actionButton("Save_xlsx", "Save .xlsx", icon = icon("book", lib =
                                                                "glyphicon")),
          actionButton("Save_rData", "Save Rdata", icon = icon("save", lib =
                                                                 "glyphicon"))
        ),
        solidHeaderBoxes$Blue(
          title = "Workspace",
          width = 3,
          actionButton(
            "ChangeWorkspace",
            "Change Workspace!",
            icon = icon("folder-open", lib = "glyphicon")
          ),
          hr(),
          textOutput("Workspace")
        )
      ),
      bsModal("importDataModal", "Import Data", "Import_Data", size = "small",
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
                actionButton("okBtn", "OK", class = "btn-primary"),
                tags$head(tags$style("#importDataModal .modal-footer{ display:none}"))
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
              choices = "")),
          column(
            width = 3,
            pickerInput(
              inputId = "pickerType",
              label = "Type",
              choices = "")),
          column(
            width = 3,
            pickerInput(
              inputId = "pickerMeasurement",
              label = "Measurement",
              choices = "")),
          column(
            width = 1,
            div(align = "center",style="padding-top: 25px;width:100%",
                actionButton(
                  inputId = "saveEditorButton",
                  label = "Save"
                )))
        )),
      fluidRow(
        column(
          width = 4,
          dataEditUI("edit-1")
        ),
        column(
          width = 8,
          plotlyOutput("dataEditorPlot")
        )
      )
      
    ),
    tabItem("ViewCalculation",
            fluidRow(
              column(
                width = 6,
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
                        actionButton(
                        inputId = "calculateSeries",
                        label = "Calculate"
                      ), 
                      style = 'top: 25px;position:relative;')
                    ),
                    column(
                      width = 2,
                      div(
                      actionButton(
                        inputId = "calculateAllSeries",
                        label = "Calculate All"
                      ), 
                      style = 'top: 25px;position:relative;')
                    )
                  )
                )
              ),
              column(
                width = 2,
                solidHeaderBoxes$Blue(
                  title = "Status",
                  width = 12,
                  withLoader(
                    textOutput("outPutCalc"),
                    type = "image",
                    loader = "LoadingChannel.gif"
                  )
                )
              )
            )
    ),
    tabItem("KineticsView",
            fluidPage(
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
                                style = "color: steelblue"))
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
                                style = "color: steelblue"))
                ),
                
                checkboxGroupButtons(
                  inputId = "measurementPickerKineticsView",
                  label = "choose Measurements",
                  choices = "",
                  checkIcon = list(
                    yes = tags$i(class = "fa fa-check-square", 
                                 style = "color: steelblue"),
                    no = tags$i(class = "fa fa-square-o", 
                                style = "color: steelblue")),
                  direction = "vertical"
                ),
                
                icon = icon("gear"),
                
                tooltip = tooltipOptions(title = "Click to see inputs !")
              ),
              fluidRow(
                column(
                  width = 12,

               ggplot_output("kineticsViewPlot", height  = "800px"),
               # downloadButton("downloadKineticPlot", "Download the plot"),
               actionButton(inputId = "kineticToPlotly",
                            label = "Show Interactive Plot")
                ),
               bsModal("kineticsViewPlotlyModal", "View Kinetic", "kineticToPlotly", size = "large",
                       # Add a switch and a text input to the modal dialog
              plotlyOutput("kineticsViewPlotly", height = "800px")
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
            choices = c(",", ";", " ", "|" ,"t", "fixed width file")
          ),
          textInput(
            inputId ="InputIndexName",
            label = "First Column Name",
            value = "Index"
          )
        ),
        solidHeaderBoxes$Blue(
          title = "Column specifications",
          width = 4,
          fluidRow(
            column(6, 
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
                     choices = c(1,2,3,4,5),
                     selected = 3,
                     options = list(create = T)
                   )),
            column(6,
                   selectizeInput(
                     inputId = "InputImportTimeCol",
                     label = "Column Time", 
                     multiple = FALSE,
                     choices = c(1,2,3,4,5),
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
            pickerInput("inputAlgorithmSmoothing", label = "Smoothing Algorithm",
                        choices = c("SmoothingSpline",
                                    "Gaussian",
                                    "GaussianKernel",
                                    "LOESS",
                                    "LOWESS",
                                    "Fourier"),
                        selected = "SmoothingSpline")
          ),
          solidHeaderBoxes$Blue(
            title = "Upsampling",
            width = 12,
            numericInput(
              inputId = "resolutionUpsampling",
              label = "Factor",
              value = 10,
              min = 1,
              max = 5000
            ),
            radioGroupButtons(
              inputId = "Id071",
              label = "Upsampling:",
              choices = c("Only Model", 
                          "All"),
              status = "primary",
              checkIcon = list(
                yes = icon("ok", 
                           lib = "glyphicon"),
                no = icon("remove",
                          lib = "glyphicon"))
            )
          )
        ),
        column(
          width = 10,
          solidHeaderBoxes$Blue(
            title = "Smoothing Options",
            width = 12,
            fluidRow(
              column(2,
                     h4(style="text-align: center;",
                        "Smoothing Spline"),
                     numericInput(
                       inputId = "smoothingSplineSpar",
                       label = "spar",
                       value = 0.5,
                       min = 0,
                       max = 1
                     )
              ),
              column(2,
                     style = "border-right: 1px solid black;border-left: 1px solid black",
                     h4(style="text-align: center;",
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
              column(2,
                     h4(style="text-align: center;",
                        "Gaussian Kernel"),
                     numericInput(
                       inputId = "GaussianKernelBandwith",
                       label = "bandwith",
                       value = 1,
                       min = 1,
                       max = 1000
                     )
              ),
              column(2,
                     style = "border-right: 1px solid black;border-left: 1px solid black",
                     h4(style="text-align: center;",
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
              column(2,
                     style = "border-right: 1px solid black;",
                     h4(style="text-align: center;",
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
              column(2,
                     h4(style="text-align: center;",
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
        )),
      fluidRow(
        column(
          width = 4,
          solidHeaderBoxes$Blue(
            title = "Kinetic Stacking",
            width = 12,
            tags$div(title="Set the value of the kinetic at which they get stacked.",
                     numericInput("inputStackPoint", label = "Stackpoint", value = 10, min = 0, max = 100)),
            tags$div(title="Set the value of the time at the Stackpoint.",
                     numericInput("inputStackTime", label = "Stacktime", value = 1, min = 0)),
            tags$div(title="Account for the time delay, between the start of an activation and the stack point.",
                     numericInput("inputTimeUntilStack", label = "Time Until Stacking", value = 0, min = 0, max = 100, step = 0.001))
          )
        ),
      )
    )
  )
  
)





##### Header#####
header <- dashboardHeader(title = span("IcKa"),
                          titleWidth  = 200)
#####


ui <- function(request) {
  dashboardPage(title = "Ion Channel Kinetic Analyzer",  header, sidebar, body, skin = "red")
}
