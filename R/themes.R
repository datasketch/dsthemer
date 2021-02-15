#' Get Themer
#' @export
dsthemer_get <- function(org, theme = NULL, palette = NULL){
  if(!org %in% org_dsthemer_list())
    stop("org doesn't have a defined theme")
  l <- load_dsthemer_yaml(org)
  first_dsthemer_name <- names(l$themes)[1]
  theme <- theme %||% first_dsthemer_name
  available_themes <- names(l$themes)
  if(!theme %in% available_themes)
    stop("Theme does not exist for this org. Try one of: ",
         paste(available_themes, collapse = ", "))
  thm <- modifyList(l$themes[[first_dsthemer_name]], l$themes[[theme]], keep.null = TRUE)
  first_palette_name <- names(l$palettes[[1]])[1]
  palette <- palette %||% first_palette_name
  thm$palette_colors <- l$palettes[[theme]][[palette]]$colors
  if(!is.null(theme))
    thm$logo <- dsthemer_logo(org, theme)
  thm
}

#' @export
dsthemer_list <- function(org = NULL){
  if(is.null(org)){
    themes <- list.files(system.file("themes", package = "dsthemer"))
    return(file_path_sans_ext(themes))
  }
  l <- load_dsthemer_yaml(org)
  names(l$themes)
}

#' @export
dsthemer_palettes <- function(org, theme, type = NULL){
  l <- load_dsthemer_yaml(org)
  x <- names(l$palettes[[theme]])
  names(x) <- unlist(lapply(l$palettes[[theme]], function(x) x$name))
  if(is.null(type)){
    return(x)
  } else{
    thm <- l$palettes[[theme]]
    thm <- Filter(function(x){ x$type == type}, thm)
    nms <- unlist(lapply(thm, function(x) x$name))
    x <- names(thm)
    names(x) <- nms
    return(x)
  }
}

#' @export
dsthemer_palette <- function(org, theme, palette){
  l <- load_dsthemer_yaml(org)
  l$palettes[[theme]][[palette]]$colors
}

#' @export
org_dsthemer_list <- function(){
  file_path_sans_ext(list.files(system.file("themes", package = "dsthemer")))
}


dsthemer_logo <- function(org, theme){
  system.file(file.path("logos",org, paste0(theme, ".png")), package = "dsthemer")
}

load_dsthemer_yaml <- function(org){
  themes_path <- system.file("themes", package = "dsthemer")
  all_orgs <- file_path_sans_ext(list.files(themes_path))
  if(!org %in% all_orgs)
    stop("Org does not exists, must be one of: ", paste(all_orgs, collapse = ", "))
  # load org yaml
  yaml::yaml.load_file(file.path(themes_path, paste0(org, ".yaml")))
}






