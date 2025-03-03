#' @export
dsthemer <- function(org, theme = "light") {
  remove_theme <- setdiff( c("light", "dark"), theme)
  l <- load_dsthemer_json(org)
  remove_dark <- grep("palettes", names(l$dark))
  l$dark <- l$dark[-remove_dark]
  remove_light <- grep("palettes", names(l$light))
  l$light <- l$light[-remove_light]
  l <- l[-grep(remove_theme, names(l))]
  l_choose <- l[[theme]]
  l[[theme]] <- NULL
  l <- c(l, l_choose)
  l
}


#' @export
dsthemer_palette <- function(org, theme = "light", palette = NULL){
  l <- load_dsthemer_json(org)
  palette <- palette %||% "categorical"
  palette <- paste0("color_palette_", palette)
  l[[theme]]$palettes[[palette]]
}


#' @export
dsthemer_background <- function(org, theme = "light") {
  l <- load_dsthemer_json(org)
  l[[theme]]$background_color
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
  org <- org %||% "general"
  if (!org %in% org_dsthemer_list()) org <- "general"
  themes_path <- system.file("themes", package = "dsthemer")
  all_orgs <- file_path_sans_ext(list.files(themes_path))
  if(!org %in% all_orgs)
    stop("Org does not exists, must be one of: ", paste(all_orgs, collapse = ", "))

  jsonlite::read_json(file.path(themes_path, paste0(org, ".json")))
}




