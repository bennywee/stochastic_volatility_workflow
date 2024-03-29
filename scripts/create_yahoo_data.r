# Load data functions
source(here::here("R", "data.R"))
# source(here::here("configs", "config.r"))
config <- FALSE

# (Download) and load data
if (!(config)) {
    stock_index <- "GSPC"
    start_date <- "2023-01-01"
    end_date <- "2023-09-10"
}

raw_data_loc <- fin_data_location(stock_index, "raw", start_date, end_date)
preprocessed_data_loc <- fin_data_location(stock_index, "preprocessed", start_date, end_date)

if (file.exists(raw_data_loc)) {
    print("Data already downloaded.")
} else {
    generate_dataset(stock_index, "raw", start_date, end_date)
}

df <- read.csv(raw_data_loc)

# Log transformations and save in the preprocessed dicretory
df["log_price"] <- log(df["GSPC.Close"])
df["log_return"] <- log_return(df, "GSPC.Close")

if (file.exists(preprocessed_data_loc)) {
    print("Data already exists.")
} else {
    save_fin_data(df, "preprocessed", stock_index, start_date, end_date)
}
