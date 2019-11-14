#' et_readFiles
#'
#' @param tool String; one of "Gumbel" or "Tradis".
#' @param conditions List of conditions. These should correspond to file names,
#'   and should be spcific and non-overlapping.
#' @param reps Number of replicates, also corresponding to the number of files
#'   for each condition
#' @param data_folder Directory containing files for all conditionas and
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
#'
#' @description Reads in multiples files, corresponding to different conditions
#'   and replicates from TnSeq analysis with Gumbel of Tradis. Creates a nested,
#'   named list of data frames for further analysis.
#'
#' @references None.
#'
#' @seealso \url{https://github.com/travis-m-blimkie/EssentialTnSeq}
#'
et_readFiles <- function(tool, conditions, reps, data_folder) {


  # Make tool name lower case so we know what to expect.
  tool <- str_to_lower(tool)

  # Stop and print error if tool specified incorrectly
  if (tool %in% c("gumbel", "tradis") == FALSE) {
    stop('Please enter either "Gumbel" or "Tradis" for tool.')
  }


  # Generate list of files to be used
  if (tool == "tradis") {
    my_files <- conditions %>%
      map(~list.files(data_folder,
                      pattern = paste0(., ".*csv.all.csv"),
                      full.names = TRUE,
                      ignore.case = TRUE,
                      recursive = TRUE))

  } else if (tool == "gumbel") {
    my_files <- conditions %>%
      map(~list.files(data_folder,
                      pattern = paste0(., ".*locus_tags.tsv"),
                      full.names = TRUE,
                      ignore.case = TRUE,
                      recursive = TRUE))
  }

  # Check for each condition that we have grabbed the right number of files. If
  # we don't, stop and provide an error message to the user.
  for (i in 1:length(conditions)) {
    if (length(my_files[[i]]) != length(reps)) {
      stop(paste0("The condition '", conditions[i], "' matches the wrong ",
                  "number of files (not the same as number of replicates ",
                  "specified). Please ensure condition names are specific and ",
                  "non-overlapping."))
    }
  }

  # Now set the names based on conditions and replicates, after we've checked we
  # have the right number of files.
  my_files <- my_files %>%
    map(~set_names(., reps)) %>% # Set replicate names within each condition
    set_names(., conditions) # Set condition names for the overall list

  # Print info for conditions and files for the user
  for (i in 1:length(conditions)) {
    writeLines(paste0(tool, " files for condition ", conditions[i], ":"))
    writeLines(paste0("\t", as.character(my_files[[unlist(conditions[i])]])))
    writeLines("")
  }


  # Read files and select columns based on specified tool
  if (tool == "gumbel") {

    # Read in raw Gumbel files
    raw_dfs <- map(my_files, function(x)
      map(x, function(y)
        read_tsv(y, progress = FALSE, col_types = cols())
      )
    )

    # Select Gumbel columns needed for analysis
    select_dfs <- map(raw_dfs, function(x)
      map(x, function(y)
        select(y, locus_tag, Call)
      )
    )

  } else if (tool == "tradis") {

    # Read Tradis data frames
    raw_dfs <- map(my_files, function(x)
      map(x, function(y)
        read_csv(y, progress = FALSE, col_types = cols())
      )
    )

    # Select Tradis columns needed for analysis
    select_dfs <- map(raw_dfs, function(x)
      map(x, function(y)
        select(y, locus_tag, read_count)
      )
    )

  }

  return(select_dfs)
}
