#' Title
#'
#' @param lexis_oauth_token
#' @param project
#' @param verbosity
#'
#' @importFrom purrr map discard
#' @importFrom fs file_move
#'
#' @return
#' @export
#'
#' @examples
get_dataset_list <- function(lexis_oauth_token,
                             project = NULL,
                             verbosity = 0) {
  resp_body <-
    lexis_request_post(
      lexis_oauth_token,
      request_api_path = "api/v0.2/dataset/search/metadata",
      req_body_json = "{}",
      resp_body_type = "json",
      verbosity = verbosity
    )

  # This check should not be needed when LEXIS v2 comes
  if (!is.null(project)) {
    resp_body <- resp_body |>
      purrr::map(check_project,
                 project) |>
      purrr::discard(is.null)
  }

  # Set lexis_ds class to all datasets
  resp_body <- resp_body |>
    purrr::map(add_ds_class)

  # Set lexis_ds_list class to the output
  class(resp_body) <- c("lexis_ds_list", class(resp_body))

  return(resp_body)
}

#' Title
#'
#' @param lexis_oauth_token
#' @param lexis_ds
#' @param destination_path
#' @param source_file_path
#' @param max_retries
#' @param verbosity
#' @param extract
#'
#' @importFrom curl new_handle handle_setheaders curl_download
#' @importFrom utils untar
#'
#' @return
#' @export
#'
#' @examples
download_file_from_dataset <- function(lexis_oauth_token,
                                       lexis_ds,
                                       destination_path,
                                       source_file_path = "/",
                                       # "/" is for the whole dataset
                                       max_retries = 100,
                                       verbosity = 0,
                                       extract = FALSE) {
  if (extract) {
    curl_destination <- tempfile()
  } else {
    curl_destination <- destination_path
  }

  resp_body <-
    lexis_request_post(
      lexis_oauth_token,
      request_api_path = "api/v0.2/transfer/download",
      req_body_json = create_download_request_body(lexis_ds,
                                                   source_file_path),
      resp_body_type = "json",
      verbosity = verbosity
    )

  auth_curl <- curl::new_handle() |>
    curl::handle_setheaders(Authorization = paste0('Bearer ', lexis_oauth_token$access_token))

  resp <- "error"
  count <- 0
  class(resp) <- "try-error"
  Sys.sleep(5)
  while (class(resp) != "character" | count == max_retries) {
    resp <-
      try(curl::curl_download(
        paste0(
          "https://api.lexis.tech/api/v0.2/transfer/download/",
          resp_body
        ),
        destfile = curl_destination,
        handle = auth_curl,
        quiet = FALSE
      ))
    Sys.sleep(5)
    count <- count + 1
  }

  if (extract) {
    temp_dir <- tempdir()
    utils::untar(curl_destination,
          exdir = temp_dir)

    extracted_path <-
      file.path(temp_dir,
                paste0("download_", resp_body),
                lexis_ds$location$internalID)

    move_file_list <- list.files(extracted_path,
                                 include.dirs = TRUE,
                                 recursive = TRUE)
    if (length(move_file_list) == 0) {
      warning("No files founded in the extracted directory!")
    }

    if (!dir.exists(destination_path)) {
      dir.create(destination_path)
    }

    fs::file_move(file.path(extracted_path, move_file_list),
                  destination_path)
    return(invisible(0))
  }

  return(resp_body)
}


#' Title
#'
#' @param lexis_oauth_token
#' @param access
#' @param project
#' @param internal_id
#' @param verbosity
#'
#' @return
#' @export
#'
#' @examples
delete_dataset <- function(lexis_oauth_token,
                           access,
                           project,
                           internal_id,
                           verbosity = 0) {
  req_body <- list(access = access,
                   project = project,
                   internalID = internal_id)

  resp <-
    lexis_request_post(
      lexis_oauth_token,
      request_api_path = "api/v0.2/dataset",
      req_method = "DELETE",
      req_body_json = req_body,
      verbosity = verbosity
    )

  return(resp)
}

#' Title
#'
#' @param file_path
#' @param file_stream
#' @param lexis_token
#' @param metadata
#' @param url
#' @param chunk_size
#' @param retries
#' @param retry_delay
#' @param show_progress
#'
#' @importFrom stringr str_replace
#' @importFrom rtus TusClient
#'
#' @return
#' @export
#'
#' @examples
upload_dataset <- function(file_path = NULL,
                           file_stream = NULL,
                           lexis_oauth_token,
                           metadata,
                           url = "https://api.lexis.tech/api/v0.2/transfer/upload",
                           chunk_size = 2048,
                           retries = 10,
                           retry_delay = 15,
                           show_progress = TRUE) {
  tus <-
    rtus::TusClient$new(url = url,
                       headers = list(Authorization = paste(
                         'Bearer', lexis_oauth_token$access_token
                       )))

  uploader <- tus$uploader(
    file_path = file_path,
    file_stream = file_stream,
    chunk_size = chunk_size,
    retries = retries,
    retry_delay = retry_delay,
    metadata = metadata
  )

  uploader$url <- uploader$create_url()

  # This should be removed with LEXIS v2
  if (grepl("portal.msad.it4i.lexis.tech:8443",
            uploader$url)) {
    uploader$url <- uploader$url |>
      stringr::str_replace("portal.msad.it4i.lexis.tech:8443",
                           "api.lexis.tech")
  }

  file_size <- uploader$get_file_size()
  last_offset <- uploader$get_offset()
  num_chunks <- ceiling((file_size - last_offset) / chunk_size)
  progress <- last_offset / file_size * 100
  step <- chunk_size / file_size * 100
  cat("\n")
  for (i in seq_len(num_chunks)) {
    uploader$upload_chunk()
    if (show_progress) {
      cat(round(progress, 2), "% \n", sep = "")
      progress <- progress + step
    }
  }
  cat("100% \n")
  return(invisible(0))
}


#' Title
#'
#' @param lexis_oauth_token
#' @param internalID
#' @param project
#' @param path
#' @param access
#' @param recursive
#' @param zone
#'
#' @return
#' @export
#'
#' @examples
get_dataset_file_list <- function(lexis_oauth_token,
                                  internalID,
                                  project,
                                  path = "",
                                  access = "project",
                                  recursive = TRUE,
                                  zone = "IT4ILexisZone") {
  file_list <- lexis_request_post(
    lexis_oauth_token,
    "api/v0.2/dataset/listing",
    req_body_json = list(
      internalID = internalID,
      access = access,
      project = project,
      path = path,
      recursive = recursive,
      zone = zone
    ),
    resp_body_type = "json"
  )

  return(file_list)
}


#

#' Title
#'
#' @param lexis_ds_list list of dataset made by get_dataset_list() function
#' @param search_value character, what should be matched value
#' @param ... values, which should be passed to purrr::pluck() function used to extract values from nested lists
#'
#' @return
#' @export
#'
#' @examples
extract_dataset <- function(lexis_ds_list,
                            search_value,
                            ...){

  subset_ind <- purrr::map_lgl(lexis_ds_list,
                               ds_check_for_value,
                               search_value,
                               ...)


  output_ds <- lexis_ds_list[subset_ind]

  # If one dataset is returned unlist it to return the lexis_ds object
  if (length(output_ds) == 1) {
    output_ds <- unlist(output_ds,
                        recursive = FALSE)
    class(output_ds) <- c(class(lexis_ds_list[[1]]))
  } else {
    class(output_ds) <- c(class(lexis_ds_list))
  }

  return(output_ds)
}


