# =============================================================================
# 1. dividing
# =============================================================================
#' Creates variable 'group' that randomly splits the dataframe into Group A
#' and Group B, balanced on a stratifying variable.
#' @param split  Variable name (string) on which to balance the split
#' @param marge  Allowed imbalance in group size (1 = perfectly equal)
#' @param id     Unique subject ID column name (string)
#' @param dataframe  Input dataframe
#' @examples dataGrouped <- dividing("zip", 1, "PSEUDOIDEXT", data)

dividing <- function(split, marge, id, dataframe){
  set.seed(4321)

  splitct <- 0
  index   <- 0

  while(!(splitct > ((nrow(dataframe)/2) - marge) &
          splitct < ((nrow(dataframe)/2) + marge))){
    in1     <- unique(dataframe[, split]) %>% table %>% names
    tmp1    <- sample(in1, length(in1)/2, replace = FALSE)
    index   <- which(dataframe[, split] %in% tmp1)
    splitct <- length(index)
    print(splitct)
  }

  a <- dataframe[index,  id]
  b <- dataframe[-index, id]

  dataframe <- dataframe %>%
    mutate(group = case_when(
      .[, id] %in% a ~ "A",
      .[, id] %in% b ~ "B")) %>%
    dplyr::select(-split)
}

