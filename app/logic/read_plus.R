
box::use(
  reticulate[source_python]
)

read_plus <- function(path) {
  
  source_python("./app/logic/read_plus.py")
  
  temp_dir <- paste0(tempdir(), "/extracted")
  
  avro_files <- read_e4_plus(path, temp_dir)
  
  return(avro_files)
  
}
