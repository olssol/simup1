## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
##
##   DESCRIPTION:
##       DEFINE SIMULATION TOOLBOX CLASSES OF PHASE 1 DESIGNS
##
##
##   DESIGNS:
##     10: STB_DESIGN_DOSE_FIX
##
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
##                     Generic Dose escaltion for Phase I
## -----------------------------------------------------------------------------

#'
#' @export
#'
setClass("STB_DESIGN_P1",
         contains = "STB_DESIGN")

setMethod("stb_describe",
          "STB_DESIGN_P1",
          function(x, ...) {
              desp1_describe(x, ...)
          })

setMethod("stb_set_default_para",
          "STB_DESIGN_P1",
          function(x) {
              internal_desp1_dpara()
          })

setMethod("stb_plot_design",
           "STB_DESIGN_P1",
          function(x, reference = 0.3, ...) {

    tox <- x@design_para$tox_rate
    dta <- data.frame(Dose = seq_len(length(tox)),
                      Tox  = tox)

    rst <- ggplot(data = dta, aes(x = Dose, y = Tox)) +
        geom_line() +
        geom_point() +
        theme_bw() +
        labs(y = "Toxicity Rate") +
        scale_y_continuous(limits = c(0, 1))

    if (!is.null(reference))
        rst <- rst + geom_hline(yintercept = reference, lty = 2)

    rst
})


setMethod("stb_para<-",
          "STB_DESIGN_P1",
          function(x, value) {
    x    <- callNextMethod(x, value)
    para <- inter_desp1_dpara_ext(x@design_para)
    x@design_para <- para
    x
})

setMethod("stb_generate_cohort",
          "STB_DESIGN_P1",
          function(x, dose, data, inx, ...) {

    cur_cohort <- desp1_generate_cohort_flex(x@design_para,
                                             dose,
                                             data, ...) %>%
        mutate(cohort = inx)

    rbind(data, cur_cohort)
})

setMethod("stb_analyze_data",
          "STB_DESIGN_P1",
          function(x, data, ...) {
    desp1_summarize(data, x@design_para$n_dose)
})

## -----------------------------------------------------------------------------
##                     The 3+3 Design
## -----------------------------------------------------------------------------

#' The 3+3 design
#'
#' @export
#'
setClass("STB_DESIGN_P1_3P3",
         contains = "STB_DESIGN_P1")

setMethod("stb_describe",
          "STB_DESIGN_P1_3P3",
          function(x, ...) {

    cat("Type: \n")
    cat("    The 3+3 design derived from a generic phase I study design \n\n")
    callNextMethod(x, ...)
})


## next dose = -1: stop the trial
setMethod("stb_escalation",
          "STB_DESIGN_P1_3P3",
          function(x, dose, data, ...) {

    next_dose <- tp3_escalation(x@design_para,
                                data,
                                cur_dose = dose)
    return(next_dose)
})

## next dose = -1: stop the trial
setMethod("stb_recommend",
          "STB_DESIGN_P1_3P3",
          function(x, data, ...) {

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
})

## -----------------------------------------------------------------------------
##                     Accelerated Titration
## -----------------------------------------------------------------------------

#' The Accelerated Titration Design
#'
#' @export
#'
setClass("STB_DESIGN_P1_ACCTIT",
         contains = "STB_DESIGN_P1_3P3")

setMethod("stb_describe",
          "STB_DESIGN_P1_ACCTIT",
          function(x, ...) {

    cat("Type: \n")
    cat("    The accelerated titration design derived from the 3+3 design \n\n")
    callNextMethod(x, ...)
})

setMethod("stb_set_default_para",
          "STB_DESIGN_P1_ACCTIT",
          function(x, ...) {
    rst <- callNextMethod(x, ...)
    rst$n_reuse_regular <- 3
    rst$size_cohort_acc <- 1
    rst$n_reuse_acc     <- 1
    rst
})

## next dose = -1: stop the trial
setMethod("stb_escalation",
          "STB_DESIGN_P1_ACCTIT",
          function(x, dose, data, ...) {

    next_dose <- acctit_escalation(x@design_para,
                                   data,
                                   cur_dose = dose)
    return(next_dose)
})

## -----------------------------------------------------------------------------
##                     Accelerated Titration
## -----------------------------------------------------------------------------


## -----------------------------------------------------------------------------
##                     10. Dose escaltion for FIX study
## -----------------------------------------------------------------------------
#'
#' @export
#'
setClass("STB_DESIGN_DOSE_FIX",
         contains = "STB_DESIGN")

setMethod("stb_describe",
          "STB_DESIGN_DOSE_FIX",
          function(x, ...) {
              callNextMethod()
              desfix_describe(x, ...)
          })

setMethod("stb_set_default_para",
          "STB_DESIGN_DOSE_FIX",
          function(x) {
              internal_desfix_dpara()
          })

setMethod("stb_plot_design",
          "STB_DESIGN_DOSE_FIX",
          function(x, ...) {
              desfix_plot_scenario(x@design_para, ...)
          })

setMethod("stb_generate_data",
          "STB_DESIGN_DOSE_FIX",
          function(x, ...) {
              desfix_gen_data(x@design_para, ...)
          })

setMethod("stb_analyze_data",
          "STB_DESIGN_DOSE_FIX",
          function(x, data_ana, ...) {
              rst <- desfix_single_trial(data_ana[[1]],
                                         lst_design = x@design_para,
                                         ...)

              list(rst)
          })

setMethod("stb_simu_gen_raw",
          "STB_DESIGN_DOSE_FIX",
          function(x, lst, ...) {
              n_reps  <- length(lst)
              rst     <- list()
              for (i in seq_len(length(lst))) {
                  rst[[i]]  <- lst[[i]][[1]] %>% mutate(rep = i)
              }

              list(n_reps = n_reps,
                   rst    = rbindlist(rst))
          })

setMethod("stb_simu_gen_summary",
          "STB_DESIGN_DOSE_FIX",
          function(x, lst, ...) {
              rst <- desfix_summary(
                  lst$rst, x@design_para, ...)
              list(rst)
          })
