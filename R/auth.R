
#' Title
#'
#' @param host_name
#' @param host_ip
#' @param port
#'
#' @importFrom httr2 oauth_client oauth_flow_auth_code obfuscated
#' @importFrom httpuv randomPort
#'
#' @return
#' @export
#'
#' @examples
get_lexis_oauth_token <- function(host_name = "localhost",
                                  host_ip = "127.0.0.1",
                                  port = httpuv::randomPort()) {
  lexis_client <- httr2::oauth_client(
    id = "LEXIS_R4LEXIS",
    secret = httr2::obfuscated(
      "sis6S3epq7IKIG6g9kbg-jVfNlJVBk1f5X66fWyQU-SPiZiqSNOKKR2tbhmzs69u"
    ),
    token_url = "https://aai.lexis.tech/auth/realms/LEXIS_AAI/protocol/openid-connect/token"
  )

  lexis_oauth_token <- lexis_client |>
    httr2::oauth_flow_auth_code(auth_url = "https://aai.lexis.tech/auth/realms/LEXIS_AAI/protocol/openid-connect/auth",
                                scope = c("openid"),
                                host_name = host_name,
                                host_ip = host_ip,
                                port = port)

  return(lexis_oauth_token)
}


# refresh ----
#   Method: POST
# URL: https://keycloak.example.com/auth/realms/myrealm/protocol/openid-connect/tokenBody type: x-www-form-urlencoded
# Form fields:
#   client_id : <my-client-name>
#   grant_type : refresh_token
# refresh_token: <my-refresh-token>


#' Title
#'
#' @param lexis_oauth_token
#' @param base_path
#' @param dry_run
#' @param verbosity
#'
#' @return
#' @export
#'
#' @examples
get_lexis_user_info <- function(lexis_oauth_token,
                                base_path = "https://aai.lexis.tech",
                                dry_run = FALSE,
                                verbosity = 0){
  resp <- lexis_request_post(
    lexis_oauth_token,
    base_path = base_path,
    resp_body_type = "json",
    request_api_path = "auth/realms/LEXIS_AAI/protocol/openid-connect/userinfo"
  )

  return(resp)
}