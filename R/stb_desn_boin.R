## -----------------------------------------------------------------------------
##
##  DESCRIPTION:
##      This file contains R functions for BOIN design
##
##  DATE:
##      AUGUST, 2023
## -----------------------------------------------------------------------------

#' Describe the design
#'
#' @export
#'
boin_describe <- function(x, ...) {
    cat("    target_tox:    target toxicity rate (default 0.3)\n")
    cat("    boin_upper:    upper bound factor for BOIN target toxicity \n")
    cat("                   interval (default 1.4) \n")
    cat("    boin_lower:    lower bound factor for BOIN target toxicity \n")
    cat("                   interval (default 0.6) \n")
}


#' Default design parameter
#'
inter_boin_dpara_ext <- function(lst) {
    bound <- BOIN::get.boundary(
                       target     = lst$target_tox,
                       ncohort    = lst$size_dose,
                       cohortsize = 1,
                       p.saf      = lst$boin_lower * lst$target_tox,
                       p.tox      = lst$boin_upper * lst$target_tox)

    lst$boin_bound <- bound
    lst
}

#' Default design parameter
#'
#'
internal_boin_dpara <- function(lst_para) {
    rst <- list(
        target_tox = 0.3,
        boin_upper = 1.4,
        boin_lower = 0.6
    )

    rst <- c(lst_para, rst)
    inter_boin_dpara_ext(rst)
}


#'  BOIN escalation
#'
#'
#'
#' @export
#'
boin_escalation <- function(lst_para, data, cur_dose, ava_dose, ...) {

    cur_data  <- data %>% filter(dose == cur_dose)
    n_tot     <- nrow(data)
    n_dose    <- nrow(cur_data)
    n_dlt     <- sum(cur_data$tox)

    ## max sample size reached
    if (n_tot >= lst_para$sample_size ||
        n_dose >= lst_para$size_dose) {
        rst <- list(next_dose = -1,
                    ava_dose  = ava_dose)
        return(rst)
    }

    ## boin esclation
    bound <- lst_para$boin_bound$boundary_tab[, n_dose]

    if (n_dlt <= bound[1]) {
        decision <- 1
        ele      <- FALSE
    } else if (n_dlt < bound[2]) {
        decision <- 0
        ele      <- FALSE
    } else if (n_dlt < bound[3]) {
        decision <- -1
        ele      <- FALSE
    } else {
        decision <- -1
        ele      <- TRUE
    }

    next_dose <- cur_dose + decision
    next_dose <- min(next_dose, max(ava_dose))
    next_dose <- max(next_dose, min(ava_dose))

    ## update available dose
    if (ele) {
        ava_dose <- ava_dose[which(ava_dose < cur_dose)]
    }

    ## return
    list(next_dose = next_dose,
         ava_dose  = ava_dose)
}

#' BOIN dose recommendation
#'
#'
boin_recommend <- function(lst_para, data, ...) {

    data <- data %>%
        group_by(dose) %>%
        summarize(n     = n(),
                  n_tox = sum(tox))

    rst <- BOIN::select.mtd(target = lst_para$target_tox,
                            npts   = data$n,
                            ntox   = data$n_tox)

    rst$MTD
}
