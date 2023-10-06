#' Title
#'
#' @param list_input list dataset list
#' @param project project id
#'
#' @return filtered dataset list by project
#'
#' @examples
check_project <- function(list_input,
                          project) {
  if (list_input$location$project == project) {
    return(list_input)
  } else {
    return(NULL)
  }
}

#' Title
#'
#' @param list_input
#'
#' @return dataset list
#'
#' @examples
add_ds_class <- function(list_input){

  class(list_input) <- c("lexis_ds", class(list_input))

  return(list_input)
}

#' Title
#'
#' @param lexis_ds
#' @param file_path
#'
#' @return
#'
#' @examples
create_download_request_body <- function(lexis_ds,
                                         file_path = "/"
                                         ){

  body <- list(access = lexis_ds$location$access[[1]],
       zone = lexis_ds$location$zone[[1]],
       project = lexis_ds$location$project[[1]],
       dataset_id = lexis_ds$location$internalID[[1]],
       path = file_path)

  return(body)
}

#' Title
#'
#' @param lexis_ds
#' @param search_value
#' @param ...
#'
#' @return
#'
#' @examples
ds_check_for_value <- function(lexis_ds,
                               search_value,
                               ...){
  extracted_values <- purrr::pluck(
    lexis_ds,
    ...
  ) |>
    unlist()

  if (length(extracted_values) == 1) {
    return(stringr::str_detect(extracted_values, search_value))
  } else if (length(extracted_values) > 1) {
    warning("Subsetting dataset returned multiple values. Please check the subsetting arguments.\nReturning FALSE.")
    return(FALSE)
  } else {
    return(FALSE)
  }
}
