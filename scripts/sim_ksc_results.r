library(jsonlite)
library(dplyr)
library(tidyr)
library(arrow)

config <- jsonlite::read_json("configs/ksc.json")

data_location <- paste("simulation_output/", config$simulation_name, "/tmp", sep ="") 
file_metadata <- as.data.frame(list.files(data_location))
names(file_metadata) <- c("files")

file_metadata <- separate_wider_delim(file_metadata, 
                     cols = files, 
                     delim = "_", 
                     names = c("dataset", "mcmc_seed"), 
                     cols_remove = FALSE) %>% 
    separate_wider_delim(., cols = mcmc_seed, delim = ".", names = c("mcmc_seed", "parquet")) %>% 
    select(-parquet)

file_metadata[file_metadata["dataset"] == 1, ][["files"]]

interation_list <- list()
iter_files <- file_metadata[file_metadata["dataset"] == 1, ][["files"]]
iter_files_path <- paste(data_location, "/", iter_files, sep = "")
interation_list <- lapply(iter_files_path, arrow::read_parquet)

arrow::read_parquet(file = data_location)
