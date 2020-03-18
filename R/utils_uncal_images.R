#' Find uncalibarated images
#'
#' @param proj_name Name of project
#' @param proj_dir Path to the project
#' @param img_dir Path to the folder holding the raw images
#'
#' @return A vector of image names
#'
#' @importFrom stringr str_extract_all
#' @importFrom glue glue
#' @importFrom utils read.table
#' @importFrom purrr map_chr
#'
#' @examples
#' \dontrun{
#' test <- find_uncal_photos(proj_name = "lake_ellsmere_2019",
#'                           proj_dir  = '../New_Zealand/Processing/Lake_Ellsmere',
#'                           img_dir   = '../New_Zealand/Raw_Photos/Lake_Ellsmere')
#'}
#' @export


find_uncal_photos <- function(proj_name, proj_dir, img_dir = proj_dir, extra_0 = TRUE) {

  # Read in log file
  project_log <- paste(proj_dir,
                       '/',
                       proj_name,
                       '/',
                       proj_name,
                       '.log',
                       sep = "")

  log_df      <- utils::read.table(file = project_log,
                                   sep = '\n',
                                   header = FALSE,
                                   stringsAsFactors = FALSE,
                                   quote = "\"")

  # get index of uncalibrated photos and subset out those lines
  uncal_ind   <- vapply(log_df$V1,
                        function(x) grepl("image is not calibrated", x),
                        logical(1L))


  uncal_lines <- log_df[uncal_ind, ]

  # get image names

  i <- ifelse(extra_0, 4, 3)

  img_nms     <- stringr::str_extract_all(uncal_lines,
                                          paste('DJI_[0-9]{',
                                                i,
                                                '}|([ ]?\\(([^()]+)\\))?\\.JPG',
                                                sep = "")
                                                )

  img_nms     <- purrr::map_chr(img_nms, ~paste(.x[1], .x[2], sep = ""))

  if(all(!grepl('JPG', img_nms))) {
    img_nms <- paste0(img_nms, '.JPG', sep = "")
  }

  img_paths   <- glue::glue('{img_dir}/{img_nms}')

  return(img_paths)

}

#' Move uncalibrated images to a separate folder or delete them
#'
#' @param img_paths the paths to the images to move or delete
#' @param delete TRUE or FALSE
#' @param move_to if delete is false, where to move the images
#'
#' @return New image paths, or TRUE if deleted
#'
#' @importFrom fs file_move file_move
#' @export

handle_uncal_photos <- function(img_paths, move_to, delete = FALSE) {

  if(delete) {

    fs::file_delete(img_paths)

    out <- TRUE

  } else {

    out <- fs::file_move(img_paths, move_to)

  }

  invisible(out)

}

