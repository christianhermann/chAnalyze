# Require Package (Special Rinno version with support for R 4 and higher https://github.com/bschneidr/RInno)
require(RInno)

# Build an installer
create_app(
  app_name = "Chanalyze",
  publisher = "Christian Hermann",
  license_file = "About/License.txt",
  app_icon  = "www/Chanalyze.ico",
  compression = "bzip",
  info_after = "About/Contact.txt",
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
    "officer"
  ),
  user_browser = "electron"
)

compile_iss()
