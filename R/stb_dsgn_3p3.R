## -----------------------------------------------------------------------------
##
##  DESCRIPTION:
##      This file contains R functions for phase 1 study designs
##
##  DATE:
##      AUGUST, 2024
## -----------------------------------------------------------------------------

#' Default design parameter
#'
inter_3p3_dpara_ext <- function(rst) {
    rst$sample_size <- rst$n_dose * 6
    rst$size_dose   <- 6

    rst
}


#'  3+3 Design escalation
#'
#'
#' @param ava_dose available dose levels
#'
#' @export
#'
tp3_escalation <- function(data, cur_dose, ava_dose, ...) {

    cur_data  <- data %>% filter(dose == cur_dose)
    n_dlt     <- sum(cur_data$tox)
    n         <- nrow(cur_data)

    if (0 == n_dlt) {
        decision <- 1
    } else if (1 == n_dlt && 3 == n) {
        decision <- 0
    } else if (n_dlt <= 1) {
        decision <- 1
    } else {
        decision <- -1
    }


    next_dose <- min(cur_dose + decision, max(ava_dose))
    next_dose <- max(next_dose, min(ava_dose))
    tox_dose  <- data %>%
        group_by(dose) %>%
        summarize(ntox = sum(tox)) %>%
        filter(ntox >= 2)

    if (next_dose %in% tox_dose$dose) {
        ## stop
        next_dose <- cur_dose
    }

    next_data <- data %>% filter(dose == next_dose)
    if (6 == nrow(next_data))
        next_dose <- -1


    ## update available dose
    if (-1 == decision) {
        ava_dose <- ava_dose[which(ava_dose < cur_dose)]
    }

    list(next_dose = next_dose,
         ava_dose  = ava_dose)
}

#' 3+3 Dose Recommendation
#'
#'
tp3_recommend <- function(data, ...) {

    cur_data <- data %>%
        group_by(dose) %>%
        summarize(dlt_rate = mean(tox)) %>%
        filter(dlt_rate < 1 / 3)

    if (0 == nrow(cur_data)) {
        rst <- NA
    } else {
        rst <- max(cur_data$dose)
    }

    rst
}
