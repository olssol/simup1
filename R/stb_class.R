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
#' @include stb_c_generic.R
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

setMethod("stb_para<-",
          "STB_DESIGN_P1",
          function(x, value) {
    x    <- callNextMethod(x, value)
    para <- inter_desp1_dpara_ext(x@design_para)
    x@design_para <- para
    x
})

setMethod("stb_plot_design",
           "STB_DESIGN_P1",
          function(x, ...) {

    tox <- x@design_para$tox_rate
    plot_tox(tox, ...)
})

setMethod("stb_generate_cohort",
          "STB_DESIGN_P1",
          function(x, dose, data, ...) {

    desp1_generate_cohort_flex(x@design_para,
                               dose,
                               data,
                               ...)
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

setMethod("stb_set_default_para",
          "STB_DESIGN_P1_3P3",
          function(x) {
    callNextMethod(x) %>%
        inter_3p3_dpara_ext()
})

setMethod("stb_para<-",
          "STB_DESIGN_P1_3P3",
          function(x, value) {
    x             <- callNextMethod(x, value)
    lst_para      <- inter_3p3_dpara_ext(x@design_para)
    x@design_para <- lst_para
    x
})

## next dose = -1: stop the trial
setMethod("stb_escalation",
          "STB_DESIGN_P1_3P3",
          function(x, dose, data, ava_dose, ...) {

    desp1_escalation(data     = data,
                     cur_dose = dose,
                     ava_dose = ava_dose,
                     lst_para = x@design_para,
                     ...)
})

## next dose = -1: stop the trial
setMethod("stb_recommend",
          "STB_DESIGN_P1_3P3",
          function(x, data, ...) {
    tp3_recommend(data, ...)
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

## -----------------------------------------------------------------------------
##                    BOIN Design
## -----------------------------------------------------------------------------

#' The Accelerated Titration Design
#'
#' @export
#'
setClass("STB_DESIGN_P1_BOIN",
         contains = "STB_DESIGN_P1")

setMethod("stb_describe",
          "STB_DESIGN_P1_BOIN",
          function(x, ...) {

    cat("Type: \n")
    cat("    The BOIN design \n\n")
    callNextMethod(x, ...)
    boin_describe()
})

setMethod("stb_set_default_para",
          "STB_DESIGN_P1_BOIN",
          function(x, ...) {
    callNextMethod(x, ...) %>%
        internal_boin_dpara() %>%
        inter_boin_dpara_ext()
})

setMethod("stb_para<-",
          "STB_DESIGN_P1_BOIN",
          function(x, value) {
    x    <- callNextMethod(x, value)
    para <- inter_boin_dpara_ext(x@design_para)
    x@design_para <- para
    x
})

setMethod("stb_plot_design",
           "STB_DESIGN_P1",
          function(x, ...) {

    tox       <- x@design_para$tox_rate
    reference <- x@design_para$target_tox

    plot_tox(tox, reference = reference, ...)
})


## next dose = -1: stop the trial
setMethod("stb_escalation",
          "STB_DESIGN_P1_BOIN",
          function(x, dose, data, ava_dose, ...) {

    desp1_escalation(data     = data,
                     cur_dose = dose,
                     ava_dose = ava_dose,
                     f_esc    = boin_escalation,
                     lst_para = x@design_para,
                     ...)
})

## next dose = -1: stop the trial
setMethod("stb_recommend",
          "STB_DESIGN_P1_BOIN",
          function(x, data, ava_dose, ...) {

    if (0 == length(ava_dose))
        return(NA)

    inx <- which(data$dose %in% ava_dose)
    boin_recommend(x@design_para,
                   data[inx, ],
                   ...)
})


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
