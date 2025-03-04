# execute only once
source("C:/Users/cherr/Dropbox/VS Code/research/RA_GH/00_func.R")


# 1. OSSE generation
path <- "C:/Users/cherr/Dropbox/VS Code/research/RA_GH/data/shimanto_y120.bin"

osse_num <- 3000
osse_list <- osse(path, osse_num)


# 2. export generated amax to csv file
rand <- do.call(cbind, osse_list[grep("^outflow_", names(osse_list))])
osse_file_name <- file.path(paste0("shimanto_stat", "_osse.csv"))
write.csv(rand, file = osse_file_name, row.names = FALSE)

# export to binary file
# osse_bin <- file.path(opt$dir, paste0(opt$river, gsub("\\.bin$", "", file_name), "_osse.bin"))
# writeBin(as.vector(unlist(rand)), file = osse_bin)