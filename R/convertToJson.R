library(dplyr)
library(tidyr)
library(jsonlite)
library(JECTools)


convert_to_json_format <- function(csv_file_path) {
  states_unemployment <- get_unemployment(geography = 'State', 
                                          start_year = 2021,
                                          fred_key = FRED_API_KEY,
                                          BLS_key = BLS_API_KEY) |>
    transform_to_tibble()
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
}
