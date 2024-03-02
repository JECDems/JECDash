library('dplyr')
library('tidyr')
library('jsonlite')
library('fredr')

fredr::fredr_set_key(Sys.getenv("FRED_API_KEY"))

start_year <- 2019

end_year <- 2024


variables <- c("CPIAUCSL","CPILFESL","PCEPI","PCEPILFE","PPIFIS")

num_series <- length(variables)

# Paparameters of the request
params <- list(
    series_id = variables,
    observation_start = rep(as.Date(paste0(start_year,"-01","-01")),
                            num_series),
    observation_end = rep(as.Date(paste0(end_year,"-12","-31")), num_series))
  
  # Actual request of data
pricesY <- purrr::pmap_dfr(
    .l = params,
    .f = ~ fredr::fredr(series_id = ..1,
                        observation_start = ..2, observation_end = ..3)) %>%
    dplyr::select(-c(realtime_start, realtime_end)) |>
    tidyr::pivot_wider(names_from = series_id, values_from = value) 
  
  inflation_names <- c('date', 'CPI','Core CPI','PCE','Core PCE','Producer Price Index')
  names(pricesY) <- inflation_names
  
  inflation <- pricesY %>%
    mutate(across(-date, ~(. - lag(., 12)) / lag(., 12) * 100)) |> 
    slice(-(1:12))
  
  json_list <- lapply(colnames(inflation)[-1], function(inflation_name) {
    list(
      dates = inflation$date,
      values = inflation[[inflation_name]]
    )
  })
  
  # Combine the list into a named list with state names as keys
  names(json_list) <- colnames(inflation)[-1]
  
  # Convert the list to JSON
  json_data <- toJSON(json_list, auto_unbox = TRUE, pretty = TRUE)
  
  write(json_data, file="InflationActions.JSON")
  