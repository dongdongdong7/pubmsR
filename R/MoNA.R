#' @title Load MoNA MS/MS Tibble
#' @description
#' Load MassBank of North America.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' monaMsTb <- load_monaMsTb()
load_monaMsTb <- function(){
  monaMsTb_path <- system.file("extdata", "monaMsTb.rds", package = "pubmsR")
  message("Load monaMsTb...")
  if(file.exists(monaMsTb_path)) monaMsTb <- tibble::tibble(readRDS(monaMsTb_path))
  else stop("Can not find monaMsTb, please redownload!")
  return(monaMsTb)
}
