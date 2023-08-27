# Require Package (Special Rinno version with support for R 4 and higher https://github.com/bschneidr/RInno)
require(RInno)

# Build an installer
create_app(
  app_name = "Chanalyze",
  publisher = "Christian Hermann",
  license_file = "About\\License.txt",
  compression = "bzip",
  info_after = "About\\Contact.txt",
  pkgs = c(
    "plotly",
    "shinyBS",
    "shinyalert",
    "shinybusy",
    "shinycustomloader",
    "shinyWidgets",
    "shinydashboard",
    "shinydashboardPlus",
    "shinyjs",
    "shiny",
    "rstudioapi",
    "tryCatchLog",
    "esquisse",
    "scales",
    "gtools",
    "shinyFiles",
    "purrr",
    "vroom",
    "tools",
    "DataEditR",
    "ggplot2",
    "patchwork",
    "plotly",
    "dplyr",
    "ggprism",
    "ggthemes",
    "smoother",
    "stringr",
    "matrixStats",
    "fontawesome",
    "tidyr",
    "latex2exp",
    "ggpubr",
    "magrittr",
    "rvg",
    "officer",
    "svglite"
  ),
  user_browser = "electron",
  app_desc       = "Processing, evaluation and analysis of patch clamp data",
  app_icon       = "www\\Chanalyze.ico",
  prog_menu_icon = T,
  desktop_icon   = T)

#Change iss file after:
#Under Icon, change commondesktop to userdesktop.



compile_iss()
