## verify_output_dirs ##########################################################
#' Checks if directories for outputs exists, and if not creates them
#'
#' @param base_dirs A list of directories in which subdirectories should be created, if they don't exist
#' @param notebook_name Name of subdir
#' @return created_any Whether or not any output dir was created
#' @export
verify_output_dirs <- function(base_dirs, notebook_name) {

  for (base_dir in base_dirs) {
    if (!dir.exists(file.path(base_dir,notebook_name))) {
      dir.create(file.path(base_dir,notebook_name), recursive = TRUE)
    }
  }
}
