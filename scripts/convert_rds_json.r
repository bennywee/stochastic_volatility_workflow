library(jsonlite)

rds_to_json <- function(file_name, data_location){
    rds_file <- paste(data_location, "/", file_name, ".RDS", sep="")
    rds <- readRDS(rds_file)
    jsonlite::write_json(rds, paste(data_location, "/", file_name, ".json", sep=""), pretty=TRUE, auto_unbox=TRUE)
}

get_data_name <- function(data_name){
    return(unlist(strsplit(data_name, split='.', fixed=TRUE))[1])
}

data_path <- "data/simulated/sbc/sbc_data_gen_sv_ncp_ksc_priors"
data_list <- list.files(path = data_path, pattern = "*.RDS")
file_names <- unlist(lapply(data_list, get_data_name))
file_names <- file_names[file_names != "metadata"]

lapply(file_names, rds_to_json, data_location = data_path)
