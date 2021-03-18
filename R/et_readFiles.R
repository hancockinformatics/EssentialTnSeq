#' et_readFiles
#'
#' @param tool String; one of "Gumbel" or "Tradis".
#' @param conditions List of conditions (character). These should correspond to
#'   file names, and should be specific and non-overlapping.
#' @param num_reps The number of replicates, corresponding to the number of files
#' for each condition.
#' @param data_folder Directory containing files for all conditions and
#'   replicates.
#'
#' @return Nested and named list of data frames for all conditions and
#'   replicates.
#'
#' @export
#'
#' @import dplyr
#' @import purrr
#' @import readr
#' @import stringr
#'
#' @description Reads in multiples files, corresponding to different conditions
#'   and replicates from TnSeq analysis with Gumbel of Tradis. Creates a nested,
#'   named list of data frames for further analysis. This function is set to
#'   look for files recursively, meaning it will descend into sub-directories
#'   from the given starting point.
#'
#' @references None.
#'
#' @seealso \url{https://github.com/hancockinformatics/EssentialTnSeq}
#'
#' @examples
#' \dontrun{
#'   et_readFiles(
#'     tool = "Gumbel",
#'     conditions = c("treatment", "control"),
#'     num_reps = 3,
#'     data_folder = "GumbelResults"
#'   )
#' }
#'
et_readFiles <- function(tool, conditions, num_reps, data_folder) {


  # Make tool name lower case so we know what to expect.
  tool <- tolower(tool)

  # Stop and print error if tool specified incorrectly
  if (tool %in% c("gumbel", "tradis") == FALSE) {
    stop('Please enter either "Gumbel" or "Tradis" for tool.')
  }


  # Generate list of files to be used
  if (tool == "tradis") {
    my_files <- conditions %>% map(
      ~list.files(
        path       = data_folder,
        pattern    = paste0(., ".*csv.all.csv"),
        full.names = TRUE,
        recursive  = TRUE
      )
    )

  } else if (tool == "gumbel") {
    my_files <- conditions %>% map(
      ~list.files(
        path       = data_folder,
        pattern    = paste0(., ".*locus_tags.tsv"),
        full.names = TRUE,
        recursive  = TRUE
      )
    )
  }

  # Check for each condition that we have grabbed the right number of files. If
  # we don't, stop and provide an error message to the user.
  for (i in 1:length(conditions)) {
    if (length(my_files[[i]]) != num_reps) {
      stop(paste0(
        "The condition '", conditions[i], "' matches the wrong number of ",
        "files (not the same as number of replicates specified). Please ",
        "ensure condition names are specific and non-overlapping."
      ))
    }
  }

  rep_names <- seq(num_reps) %>%
    as.character() %>%
    paste0("r", .)

  # Now set the names based on conditions and replicates, after we've checked we
  # have the right number of files.
  my_files <- my_files %>%
    map(~set_names(., rep_names)) %>% # Set replicate names within each condition
    set_names(., conditions)  # Set condition names for the overall list

  # Print info for conditions and files for the user
  for (i in 1:length(conditions)) {
    message(paste0(
      str_to_title(tool), " files for condition ", conditions[i], ":"
    ))

    message(paste0(
      "\t", as.character(my_files[[unlist(conditions[i])]]), "\n"
    ))
  }


  # Read files and select columns based on specified tool
  if (tool == "gumbel") {

    raw_dfs <- map(my_files, function(x)
      map(x, function(y)
        read_tsv(y, progress = FALSE, col_types = cols())
      )
    )
    select_dfs <- map(raw_dfs, function(x)
      map(x, function(y)
        select(y, locus_tag, Call)
      )
    )

  } else if (tool == "tradis") {
    raw_dfs <- map(my_files, function(x)
      map(x, function(y)
        read_csv(y, progress = FALSE, col_types = cols())
      )
    )
    select_dfs <- map(raw_dfs, function(x)
      map(x, function(y)
        select(y, locus_tag, read_count)
      )
    )

  }

  return(select_dfs)
}
