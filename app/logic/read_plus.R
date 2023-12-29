library(reticulate)
use_python("C:\\ProgramData\\anaconda3\\python.exe", required = TRUE)

source_python("C:\\Users\\acans\\Desktop\\read_plus.py")

root="C:\\Users\\acans\\Desktop\\2023-07-11.zip"
avro_files <- read_e4_plus(root)
