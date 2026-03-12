# =============================================================================
# 2 & 3. to_con / to_dig  (dummy function)
# =============================================================================
#' to_con: scale continuous variables (>= 20 unique values) and factorise
#'         categorical variables; does NOT expand multi-level factors to dummies.
#' to_dig: as to_con(), but additionally expands multi-level factors into
#'         dummy variables via createDummyFeatures().
#' @param dataframe  Input dataframe
#' @param exclude    Character vector of column names to leave untouched
#' @examples
#'   exceptions <- c("PSEUDOIDEXT","idFU","group","dmNew","event_selected","time")
#'   dataCon <- to_con(dataGrouped, exceptions)
#'   dataDig <- to_dig(dataGrouped, exceptions)

to_con <- function(dataframe, exclude){
  t1 <- dplyr::select(dataframe, -exclude) %>%
    lapply(function(x) length(unique(x)))

  numeric           <- names(t1[which(t1 >= 20)])
  factors           <- names(t1[which(t1 <= 3)])
  factorsMultiLevel <- names(t1[which(t1 > 3 & t1 < 20)])

  dataframe <- dataframe %>%
    mutate_at(numeric, funs(c(scale(.))))

  dataframe[factors]           <- lapply(dataframe[factors],           factor)
  dataframe[factorsMultiLevel] <- lapply(dataframe[factorsMultiLevel], factor)
  dataframe
}

to_dig <- function(dataframe, exclude){
  t1 <- dplyr::select(dataframe, -exclude) %>%
    lapply(function(x) length(unique(x)))

  numeric           <- names(t1[which(t1 >= 20)])
  factors           <- names(t1[which(t1 <= 3)])
  factorsMultiLevel <- names(t1[which(t1 > 3 & t1 < 20)])

  dataframe <- dataframe %>%
    mutate_at(numeric, funs(c(scale(.))))

  dataframe[factors]           <- lapply(dataframe[factors],           factor)
  dataframe[factorsMultiLevel] <- lapply(dataframe[factorsMultiLevel], factor)
  dataframe <- createDummyFeatures(dataframe, cols = factorsMultiLevel)
}

