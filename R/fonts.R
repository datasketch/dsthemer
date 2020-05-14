
#' @export
available_fonts <- function(){
  fonts_path <- system.file("fonts", package = "dsthemer")
  fonts <- file_path_sans_ext(list.files(fonts_path))
  fonts
}
