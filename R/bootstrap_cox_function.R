#' The bootstrap_cox function
#'

bootstrap_cox_function <- function(variables, number, model_data, ndraws,
                                   l = "lambda.1se") {
  set.seed(4321)

  vals <- dfVulcanoTwo %>%
    mutate(term = sub("0[A-Z]*$", "0", term)) %>%
    filter(term %in% variables) %>%
    filter(casesComplete > number) %>%
    dplyr::select(term)

  extract_names <- coxph(
    as.formula(paste("Surv(time, event_selected) ~",
                     paste(sub("0[A-Za-z]*$", "0", vals[, 1]), collapse = " + "))),
    dataCon) %>%
    tidy()

  coef_mtx <- extract_names[, 1]

  for (i in 1:ndraws) {
    bootstrap_ids   <- sample(seq(nrow(model_data)), nrow(model_data), replace = TRUE)
    bootstrap_data  <- model_data[bootstrap_ids, ]

    dfGroup <- bootstrap_data %>%
      dplyr::select("event_selected", "time", "age0", "sex0", vals$term) %>%
      na.omit()

    m1x     <- model.matrix(~ . -1 - time - event_selected, dfGroup)
    m1y     <- Surv(dfGroup$time, dfGroup$event_selected)
    m1bestL <- cv.glmnet(m1x, m1y, family = "cox", alpha = 1, nfolds = 10)
    m1e     <- glmnet(m1x, m1y, family = "cox", lambda = m1bestL[[l]], alpha = 1)

    coef_b       <- as.matrix(coef(m1e))
    sel_b        <- rownames(coef_b)[!is.na(coef_b[, 1]) & coef_b[, 1] != 0]
    if (length(sel_b) == 0) next

    bootstrap_model <- coxph(
      as.formula(paste("Surv(time, event_selected) ~",
                       paste(sub("0[A-Za-z]*$", "0", sel_b), collapse = " + "))),
      bootstrap_data)

    model_tidy <- tidy(bootstrap_model, exponentiate = T) %>%
      dplyr::select(term, p.value) %>%
      rename(i = p.value)

    coef_mtx <- coef_mtx %>% merge(., model_tidy, by = "term", all = T)
  }

  trans <- function(x){
    x %>%
      mutate_at(.vars = vars(contains("model")),
                .funs = funs(as.numeric(case_when(
                  .  < 0.05 ~ "1",
                  . >= 0.05 ~ "0",
                  TRUE      ~ NA_character_)))) %>%
      mutate(total = dplyr::select(., contains("model")) %>% rowSums(na.rm = TRUE)) %>%
      dplyr::select(term, total) %>%
      arrange(desc(total))
  }

  colnames(coef_mtx) <- c("term", paste("model", seq(1, ndraws, by = 1), sep = ""))
  trans(coef_mtx)
}
