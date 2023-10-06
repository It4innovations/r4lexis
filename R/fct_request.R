#' Title
#'
#' @param lexis_oauth_token
#' @param request_api_path
#' @param req_method
#' @param req_query
#' @param req_body_json
#' @param resp_body_type
#' @param verbosity
#'
#' @importFrom httr2 request req_auth_bearer_token req_url_path req_body_json req_url_query req_perform resp_body_json
#'
#' @return
#' @export
#'
#' @examples
lexis_request_post <- function(lexis_oauth_token,
                               request_api_path,
                               req_method = NULL,
                               req_query = NULL,
                               req_body_json = NULL,
                               resp_body_type = "",
                               base_path = "https://api.lexis.tech",
                               dry_run = FALSE,
                               verbosity = 0) {
  req <- httr2::request(base_path) |>
    httr2::req_auth_bearer_token(lexis_oauth_token$access_token) |>
    httr2::req_url_path(request_api_path)

  # Request part definition ----
  if (!is.null(req_method)) {
    req <- req |>
      httr2::req_method(req_method)
  }

  if (!is.null(req_query)) {
    req <- req |>
      httr2::req_url_query(req_query)
  }

  if (!is.null(req_body_json)) {
    req <- req |>
      httr2::req_body_json(req_body_json)
  }

  # Perform request ----
  if (dry_run) {
    resp <- req |>
      httr2::req_dry_run()
    return(resp)
  }

  resp <- req |>
    httr2::req_perform(verbosity = verbosity)

  # Response part definition ----

  if (resp_body_type == "") {

  } else if (resp_body_type == "json") {
    resp <- resp |>
      httr2::resp_body_json()
  } else {
    stop("Unknown body type for request!")
  }

  return(resp)
}
