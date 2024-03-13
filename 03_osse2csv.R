source("/work/a06/stakahashi/workspace/02_gamlss.R")

# argument settings
option_list <- list(
  make_option(c("--output-path"), type="character", help="File path to save the generated CSV file", metavar="OUTPUT_PATH")
)

opt_parser <- OptionParser(usage="Usage: %prog --output-path=OUTPUT_PATH", option_list=option_list)
opt <- parse_args(opt_parser)

if (is.null(opt$output_path)) {
  stop("Please specify the file path to save the CSV file using --output-path option.")
}


osse_num <- 100
osse_list <- list()

# generate OSSE from data designated at "data" in 02_gamlss.R
for (i in 1:osse_num) {
  generated <- osse_min(mu, sigma, amax)
  osse_list[[paste0("outflow_", i)]] <- generated$outflow
  osse_list[[paste0("q2_", i)]] <- generated$q2
  osse_list[[paste0("q3_", i)]] <- generated$q3
  osse_list[[paste0("min_", i)]] <- rep(generated$min, 120)
  osse_list[[paste0("osseq2_", i)]] <- generated$osse2
  osse_list[[paste0("osseq3_", i)]] <- generated$osse3
}

# export generated amax to csv file
# rand <- do.call(cbind, osse_list[grep("^outflow_", names(osse_list))])
# write.csv(rand, file = opt$output_path, row.names = FALSE)

# export to binary file
# writeBin(as.numeric(unlist(rand)), opt$output_path)
