
#' @export
dsthemer_palette <- function(org, theme = "light", palette = NULL){
  l <- load_dsthemer_json(org)
  palette <- palette %||% "categorical"
  palette <- paste0("color_palette_", palette)
  l[[theme]]$palettes[[palette]]
}

#' @export
org_dsthemer_list <- function(){
  file_path_sans_ext(list.files(system.file("themes", package = "dsthemer")))
}

dsthemer_logo <- function(org, theme) {
  files_type <- c("svg", "png")
  img_path <- purrr::map_chr(files_type, ~ {
    system.file(file.path("logos", org, paste0(theme, ".", .x)), package = "dsthemer")
  }, .id = NULL) |>
    purrr::discard(~ !nzchar(.)) |>
    head(1)

  if (length(img_path) == 0) {
    warning("Logo file not found for ", org, " with theme ", theme, ".")
    return(NULL)
  } else {
    return(img_path)
  }
}


load_dsthemer_json <- function(org) {
  org <- org %||% "datasketch"
  themes_path <- system.file("themes", package = "dsthemer")
  all_orgs <- file_path_sans_ext(list.files(themes_path))
  if(!org %in% all_orgs)
    stop("Org does not exists, must be one of: ", paste(all_orgs, collapse = ", "))

  jsonlite::read_json(file.path(themes_path, paste0(org, ".json")))
}




