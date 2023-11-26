
box::use(
  bslib[bs_theme, font_google],
  yaml[read_yaml]
)

wearalyze_theme <- bs_theme(
  #version = 5,
  bootswatch = "cerulean",
  base_font = font_google("Ubuntu"),
  heading_font = font_google("Ubuntu"),
  code_font = font_google("Fira Code")
)

app_config <- read_yaml("config/config.yml")
help_config <- read_yaml("config/help.yml")