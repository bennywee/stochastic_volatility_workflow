library(quantmod)

download_fin_data <- function(financial_series, start_date, end_date){
    data_env <- new.env()
    series  <- paste("^", "GSPC", sep = "")
    getSymbols(series, env = data_env, src = "yahoo",
            from = as.Date(start_date), to = as.Date(end_date))
    data <- get("GSPC", envir = data_env)
    return(data)
}

df <- download_fin_data(financial_series = "GSPC",
                        start_date = "1960-01-04",
                        end_date = "2023-01-01")

save_data <- function(name, start_date, end_date){

}