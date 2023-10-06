print.lexis_ds_list <- function(x, ...) {
  ds_titles <-
#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
    purrr::map(x, purrr::possibly(function(x) {
      paste0(
        x$metadata$title[[1]],
        "\n      UUID:    ",
        x$location$internalID[[1]],
        "\n      Project: ",
        x$location$project[[1]]
      )
    },
    NULL)) |>
    purrr::discard(is.null) |>
    unlist()

  ds_titles <-
    paste0("  [", seq_along(ds_titles), "] ", ds_titles, "\n")

  cat("\nNumber of datasets: ",
      length(x),
      "\n",
      "Datasets:\n",
      ds_titles,
      sep = "")
}

#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
print.lexis_ds <- function(x, ...) {
  title <- ifelse(length(x$metadata$title) == 1,
                  paste(x$metadata$title[[1]], collapse = ", "),
                  "")
  project <- ifelse(length(x$location$project) == 1,
                    paste(x$location$project[[1]], collapse = ", "),
                    "")
  creator <- ifelse(length(x$metadata$creator) == 1,
                    paste(x$metadata$creator[[1]], collapse = ", "),
                    "")
  owner <- ifelse(length(x$metadata$owner) == 1,
                  paste(x$metadata$owner[[1]], collapse = ", "),
                  "")
  contributor <- ifelse(
    length(x$metadata$contributor) == 1,
    paste(x$metadata$contributor[[1]], collapse = ", "),
    ""
  )
  creation_date <- ifelse(
    length(x$metadata$CreationDate) == 1,
    paste(x$metadata$CreationDate[[1]], collapse = ", "),
    ""
  )
  access <- ifelse(length(x$location$access) == 1,
                   paste(x$location$access[[1]], collapse = ", "),
                   "")
  compression <- ifelse(length(x$flags$compression) == 1,
                        paste(x$flags$compression[[1]], collapse = ", "),
                        "")
  encryption <- ifelse(length(x$flags$encryption) == 1,
                       paste(x$flags$encryption[[1]], collapse = ", "),
                       "")
  UUID <- ifelse(
    length(x$location$internalID) == 1,
    paste(x$location$internalID[[1]], collapse = ", "),
    ""
  )
  zone <- ifelse(length(x$location$zone) == 1,
                 paste(x$location$zone[[1]], collapse = ", "),
                 "")
  description <- ifelse(
    length(x$metadata$description) == 1,
    paste(x$metadata$description[[1]], collapse = ", "),
    ""
  )

  cat(
    "\n",
    "Title:         ", title, "\n",
    "Project:       ", project, "\n",
    "Creator:       ", creator, "\n",
    "Owner:         ", owner, "\n",
    "Contributor:   ", contributor, "\n",
    "Creation Date: ", creation_date, "\n",
    "Access Policy: ", access, "\n",
    "Compression:   ", compression, "\n",
    "Encryption:    ", encryption, "\n",
    "UUID:          ", UUID, "\n",
    "Irods Zone:    ", zone, "\n\n",
    "Description:\n", description, "\n",
    sep = ""
  )
}
