file_path_sans_ext <- function (x, compression = FALSE)
{
  if (compression)
    x <- sub("[.](gz|bz2|xz)$", "", x)
  sub("([^.]+)\\.[[:alnum:]]+$", "\\1", x)
}

#' @export
dsthemer_sys_file <- function(...){
  system.file(..., package = "dsthemer")
}
