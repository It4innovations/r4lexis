#' Title
#'
#' @param lexis_oauth_token
#' @param verbosity
#'
#' @return
#' @export
#'
#' @examples
get_workflows_list <- function(lexis_oauth_token,
                               verbosity = 0) {
  resp <- lexis_request_post(lexis_oauth_token,
                             request_api_path = "/airflow/api/v1/dags",
                             resp_body_type = "json")

  return(resp)
}

#' Title
#'
#' @param lexis_oauth_token
#' @param workflow_id
#' @param verbosity
#'
#' @return
#' @export
#'
#' @examples
get_workflow_info <- function(lexis_oauth_token,
                              workflow_id,
                              verbosity = 0) {
  resp <- lexis_request_post(
    lexis_oauth_token,
    request_api_path = paste0("/airflow/api/v1/dags/", workflow_id),
    resp_body_type = "json"
  )

  return(resp)
}


#' Title
#'
#' @param lexis_oauth_token
#' @param workflow_id
#' @param verbosity
#'
#' @return
#' @export
#'
#' @examples
get_workflow_details <- function(lexis_oauth_token,
                                 workflow_id,
                                 verbosity = 0) {
  resp <- lexis_request_post(
    lexis_oauth_token,
    request_api_path = paste0("/airflow/api/v1/dags/", workflow_id, "/details"),
    resp_body_type = "json"
  )

  return(resp)
}

#' Title
#'
#' @param lexis_oauth_token
#' @param workflow_id
#' @param verbosity
#'
#' @return
#' @export
#'
#' @examples
get_workflow_default_parameters <- function(lexis_oauth_token,
                                            workflow_id,
                                            verbosity = 0) {
  wf_details <- lexis_request_post(
    lexis_oauth_token,
    request_api_path = paste0("/airflow/api/v1/dags/", workflow_id, "/details"),
    resp_body_type = "json"
  )

  wf_default_parameters <- purrr::map(wf_details$params,
                                      ~ purrr::pluck(.x, "value"))

  return(wf_default_parameters)

}

#' Title
#'
#' @param lexis_oauth_token
#' @param workflow_id
#' @param verbosity
#' @param workflow_parameters
#' @param workflow_run_id
#'
#' @return
#' @export
#'
#' @examples
execute_workflow <- function(lexis_oauth_token,
                             workflow_id,
                             workflow_parameters,
                             workflow_run_id,
                             verbosity = 0) {
  workflow_input <- list()
  workflow_input$conf <- workflow_parameters
  workflow_input$conf$access_token <- lexis_oauth_token$access_token

  workflow_input$dag_run_id <- workflow_run_id

  resp <- lexis_request_post(
    lexis_oauth_token,
    request_api_path = paste0("airflow/api/v1/dags/", workflow_id, "/dagRuns"),
    req_body_json = workflow_input,
    resp_body_type = "json",
    verbosity = verbosity
  )

  return(resp)
}


#' Title
#'
#' @param lexis_oauth_token
#' @param workflow_id
#' @param verbosity
#'
#' @return
#' @export
#'
#' @importFrom purrr map_chr pluck
#' @importFrom tibble tibble
#' @importFrom lubridate as_datetime
#'
#' @examples
get_workflow_states <- function(lexis_oauth_token,
                                workflow_id,
                                verbosity = 0) {
  resp <- lexis_request_post(
    lexis_oauth_token,
    request_api_path = paste0("airflow/api/v1/dags/", workflow_id, "/dagRuns"),
    resp_body_type = "json",
    verbosity = verbosity
  )

  workflow_states <- tibble::tibble(
    `Workflow Run ID` = purrr::map_chr(resp$dag_runs,
                                       purrr::pluck,
                                       "dag_run_id"),
    `Execution Date` = purrr::map_chr(resp$dag_run,
                                      purrr::pluck,
                                      "execution_date") |>
      lubridate::as_datetime(),
    State = purrr::map_chr(resp$dag_run,
                           purrr::pluck,
                           "state")
  )

  return(workflow_states)
}
