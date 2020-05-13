
get_theme <- function(org, theme = NULL, palette = NULL){
  l <- load_theme_yaml(org)
  first_theme_name <- names(l$themes)[1]
  theme <- theme %||% first_theme
  available_themes <- names(l$themes)
  if(!theme %in% available_themes)
    stop("Theme does not exist for this org. Try one of: ",
         paste(available_themes, collapse = ", "))
  thm <- modifyList(l$themes[[first_theme_name]], l$themes[[theme]], keep.null = TRUE)
  first_palette_name <- names(l$palettes[[1]])[1]
  palette <- palette %||% first_palette_name
  thm$palette_colors <- l$palettes[[theme]][[palette]]$colors
  thm
}

get_org_themes <- function(org){
  l <- load_theme_yaml(org)
  names(l$themes)
}

get_org_palettes <- function(org, theme){
  l <- load_theme_yaml(org)
  x <- names(l$palettes[[theme]])
  names(x) <- unlist(lapply(l$palettes[[theme]], function(x) x$name))
  x
}

get_org_palette <- function(org, theme, palette){
  l <- load_theme_yaml(org)
  l$palettes[[theme]][[palette]]$colors
}


load_theme_yaml <- function(org){
  themes_path <- system.file("themes", package = "dsthemes")
  all_orgs <- file_path_sans_ext(list.files(themes_path))
  if(!org %in% all_orgs)
    stop("Org does not exists, must be one of: ", paste(all_orgs, collapse = ", "))
  # load org yaml
  yaml::yaml.load_file(file.path(themes_path, paste0(org, ".yaml")))
}

