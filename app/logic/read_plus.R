
box::use(
  reticulate[source_python]
)

read_plus <- function(path) {
  
  reticulate::source_python("./app/logic/read_plus.py")
  
  avro_files <- read_e4_plus(path)
  
  return(avro_files)
  
}
