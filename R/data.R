library(quantmod)

download_fin_data <- function(financial_series, start_date, end_date){
    data_env <- new.env()
    series  <- paste("^", "GSPC", sep = "")
    getSymbols(series, env = data_env, src = "yahoo",
            from = as.Date(start_date), to = as.Date(end_date))
    data <- get("GSPC", envir = data_env)
    df <- as.data.frame(data)
    df$Date  <- rownames(df)
    return(df)
}

fin_data_location <- function(financial_series, type, start_date, end_date){
    path  <-  file.path("data", type, financial_series)
    start  <-  format(strptime(as.character(start_date), "%Y-%m-%d"), "%Y%m%d")
    end  <-  format(strptime(as.character(end_date), "%Y-%m-%d"), "%Y%m%d")
    date_string  <-  paste(start, "_", end, ".csv", sep = "")
    file_name <-  file.path(path, date_string)
    return(file_name)
}

save_fin_data <- function(dataframe, type, financial_series, start_date, end_date){
    path  <-  file.path("data", type, financial_series)
    
    if (!dir.exists(path)) {
        dir.create(path, recursive = TRUE)
    }

    file_name <- fin_data_location(financial_series, type, start_date, end_date)

    if (file.exists(file_name)) {
        stop("Data file already exists")
    } else {
        write.csv(dataframe, file_name, row.names = FALSE)
        print(paste("File successfully downloaded: ", file_name, sep = ""))
    }
}

generate_dataset <- function(financial_series, data_type, start_date, end_date){

    data <- download_fin_data(financial_series = financial_series,
                        start_date = start_date,
                        end_date = end_date)

    save_fin_data(dataframe = data,
                  financial_series = financial_series,
                  type = data_type,
                  start_date = start_date,
                  end_date = end_date)

}

log_return <- function(data, column){
    log_price = log(data[column])
    lag_log_price = rbind(NA, head(log_price, -1))
    log_returns <- log_price - lag_log_price
    return(log_returns)
}

gen.AR1 <- function(phi,mu,sig,T){
  
  # simulate AR(1) state vector 'h' having length 'T' 
  # using equation (1) and subsequent equation for initial state
  
  h <- rep(0,T)
  h[1] <- rnorm(1,mean=mu,sd=sig/sqrt(1-phi^2))
  
  for(i in 2:T){
    h[i] <- rnorm(1,mean=(mu+phi*(h[(i-1)]-mu)),sd=sig)
  }
  return(h)
}

simulate_ksc <- function(T=1000,
                         phi.true = 0.97779,
                         sig.true = 0.15850,
                         beta.true = 0.64733){
    
    mu.true  <-  log(beta.true^2)
    htrue <- gen.AR1(phi.true,mu.true,sig.true,T)
    yobs <- exp(htrue/2)*rnorm(T,0,1)

    df  <- data.frame(cbind(yobs, htrue))
    
    return(df)

}