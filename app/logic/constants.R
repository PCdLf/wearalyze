
box::use(
  bslib[bs_theme, font_google],
  htmlwidgets[JS],
  yaml[read_yaml]
)

#' Global theme for the app
#' Find other Bootswatch themes here: https://bootswatch.com
wearalyze_theme <- bs_theme(
  #version = 5,
  bootswatch = "cerulean",
  base_font = font_google("Ubuntu"),
  heading_font = font_google("Ubuntu"),
  code_font = font_google("Fira Code")
)

#' Configuration options for the app
app_config <- read_yaml("config/config.yml")
help_config <- read_yaml("config/help.yml")
device_config <- read_yaml("config/devices.yml")

#' Formatting of date axis labels in echarts
yearMonthDate <- JS('function (value) {
        var d = new Date(value);
        var datestring = d.getFullYear() + "-" + ("0"+(d.getMonth()+1)).slice(-2) + "-" + ("0" + d.getDate()).slice(-2)
        return datestring
      }')
