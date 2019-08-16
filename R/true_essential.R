#' true_essential
#'
#' @param dfs List of data frames for all conditions/replicates
#' @param tool Tool that was used to genrate essentiality data; currently
#'   supports either Tradis or Transit/Gumbel
#' @param total_num Total number of conditions/replicates
#'
#' @return
#' @export
#'
#' @examples
#'
#' Based on a set of data frames containing TnSeq results (from Tradis or
#' Transit/Gumbel), returns genes which are condidered essential in all
#' conditions/replicates.
#'
true_essential <- function(dfs, tool, total_num) {

  # Required functions
  requireNamespace(tidyverse)

  # Code for Tradis
  if (str_to_lower(tool) == "tradis") {
    true_ess <- plyr::join_all(dfs, by = "locus_tag", type = "full") %>%
      mutate(true_count = rowSums(. == 0)) %>%
      filter(true_count == total_num) %>%
      pull(locus_tag)

    # Code for Transit/Gumbel
  } else if (str_to_lower(tool) == "gumbel" | str_to_lower(tool) == "transit") {
    true_ess <- plyr::join_all(dfs, by = "locus_tag", type = "full") %>%
      mutate(true_count = rowSums(. == "E")) %>%
      filter(true_count == total_num) %>%
      pull(locus_tag)
  }

  return(locus_tag)

}
