
#' @export
dsthemer_presets <- function(th){
  th_params <- yaml::read_yaml(system.file("app/theme_parmesan_init_params.yaml",
                                           package = "dsthemer"))
  lapply(names(th), function(x){
    l <- th[x]
    names(l) <- th_params[[x]]
    l
  }) %>% setNames(names(th))
}
