.download_hmdbMsTb <- function(){
  url <- "https://github.com/dongdongdong7/pubmsR/raw/refs/heads/main/inst/extdata/hmdbMsTb.rds"
  destpath <- system.file("extdata", "hmdbMsTb.rds", package = "pubmsR")
  download.file(url = url,
                destfile = destpath)
}

.download_monaMsTb <- function(){
  url <- "https://github.com/dongdongdong7/pubmsR/raw/refs/heads/main/inst/extdata/monaMsTb.rds"
  destpath <- system.file("extdata", "monaMsTb.rds", package = "pubmsR")
  download.file(url = url,
                destfile = destpath)
}

.download_lipidblastMsTb <- function(){
  url <- "https://github.com/dongdongdong7/pubmsR/raw/refs/heads/main/inst/extdata/lipidblastMsTb.rds"
  destpath <- system.file("extdata", "lipidblastMsTb.rds", package = "pubmsR")
  download.file(url = url,
                destfile = destpath)
}

#' @title Download MsTb from pubmsR
#' @description
#' MsTb is stored with Git LFS, so user should use this function to download.
#'
#' @return NULL
#' @export
#'
#' @examples
#' download_MsTb()
download_MsTb <- function(){
  message("Downloading hmdbMsTb...")
  tryCatch(.download_hmdbMsTb(),
           error = function(e) {
             print(e)
           })

  message("Downloading monaMsTb...")
  tryCatch(.download_monaMsTb(),
           error = function(e) {
             print(e)
           })

  message("Downloading lipidblastMsTb")
  tryCatch(.download_lipidblastMsTb(),
           error = function(e) {
             print(e)
           })

  print("Download over!")
}
