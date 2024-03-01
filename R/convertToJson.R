library('dplyr')
library('tidyr')
library('jsonlite')
library('fredr')

fredr::fredr_set_key(Sys.getenv("FRED_API_KEY"))

states <- data.frame(state = c(state.abb,"DC"))

start_year <- 2020

end_year <- 2024

variables <- NULL
for (state in states$state) {
  tmp <- paste0(state, "UR")
  variables <- c(variables, tmp)
}

variables <- c(variables, "UNRATE")

num_series <- length(variables)

# Paparameters of the request
params <- list(
    series_id = variables,
    observation_start = rep(as.Date(paste0(start_year,"-01","-01")),
                            num_series),
    observation_end = rep(as.Date(paste0(end_year,"-12","-31")), num_series))
  
  # Actual request of data
states_unemployment <- purrr::pmap_dfr(
    .l = params,
    .f = ~ fredr::fredr(series_id = ..1,
                        observation_start = ..2, observation_end = ..3)) %>%
    dplyr::select(-c(realtime_start, realtime_end)) |>
    tidyr::pivot_wider(names_from = series_id, values_from = value) |>
    dplyr::rename(USUR = UNRATE) |> 
    dplyr::as_tibble()
  
  states_names <- c("date",state.name,"District of Columbia","National")
  names(states_unemployment) <- states_names
  
  json_list <- lapply(colnames(states_unemployment)[-1], function(state_name) {
    list(
      dates = states_unemployment$date,
      values = states_unemployment[[state_name]]
    )
  })
  
  # Combine the list into a named list with state names as keys
  names(json_list) <- colnames(states_unemployment)[-1]
  
  # Convert the list to JSON
  json_data <- toJSON(json_list, auto_unbox = TRUE, pretty = TRUE)
  
  write(json_data, file="unemploymentActions.JSON")
  