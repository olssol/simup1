## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
##
##                                UI
##
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
tip_txt <- function(label, message, placement = "bottom") {
    label |>
        span(
            tooltip(
                bs_icon("info-circle"),
                message,
                placement = placement))
}

ui_settings <- list(
    accordion(
        accordion_panel(
            "Doses",
            textInput("inTox",
                      tip_txt("Toxicity Rates",
                              "Enter toxicity rate for each dose level,
                               separated by ','"),
                      "0.01, 0.02, 0.05, 0.07, 0.40, 0.5"),

            checkboxInput("inChkRes",
                          "Specify response rates",
                          FALSE),

            conditionalPanel(
                condition = "input.inChkRes == true",
                textInput("inRes",
                          tip_txt("Response Rates",
                                  "Enter response rate for each dose level,
                               separated by ','"),
                          "0.2"),
                textInput("inRho",
                          "Odds Ratio",
                          "0.1")
            )
        ),

        accordion_panel(
            "Trial Design",

            selectInput(
                inputId = "inDesign",
                label = "Choose a design:",
                choices = c("3+3"  = "dose_acctit",
                            "BOIN" = "dose_boin"),
                selected = "dose_acctit"
            ),

            checkboxInput("inChkAcc",
                          "Allow acceleration",
                          FALSE),

            checkboxInput("inChkTit",
                          "Allow intra-patient dose escalation",
                          FALSE)
        ),

        conditionalPanel(
            condition = "input.inDesign == 'dose_boin'",
            accordion_panel(
                "BOIN Design",
                numericInput("inTarTox",
                             "Target toxicity rate",
                             0.3,
                             min = 0, max = 1, step = 0.01),
                numericInput("inBoinLow",
                             "Lower bound of toxicity rate",
                             0.6,
                             min = 0, max = 1, step = 0.01),
                numericInput("inBoinUp",
                             "Upper bound of toxicity rate",
                             1.4,
                             min = 1, max = 2, step = 0.01),
                numericInput("inSampleSize",
                             "Maximum sample size",
                             24,
                             min = 1, step = 1),
                numericInput("inSizeDose",
                             "Maximum sample size for each dose",
                             9,
                             min = 1, step = 1),
                checkboxInput("inBOINBound",
                             tip_txt("Apply boundary for MTD Selection",
                                     "Whether to impose the condition that the
                                     isotonic estimate of toxicity
                                     probability for the selected MTD must be
                                     less than de-escalation
                                     boundary"),
                              value = TRUE)
            )
        ),

        conditionalPanel(
            condition = "input.inChkAcc == true",
            accordion_panel(
                "Acceleration",
                numericInput("inAccSize",
                             tip_txt("Cohort Size",
                                     "Cohort size at the accelaration phase"),
                             1,
                             min = 0, step = 1),
            )
        ),

        conditionalPanel(
            condition = "input.inChkTit == true",
            accordion_panel(
                "Intra-Patient Dose Escalation",
                numericInput("inAccMaxCycle",
                             "Max Number of Doses for Recycled Patients",
                             3,
                             min = 1, step = 1),
                numericInput("inAccFactor",
                             tip_txt("Toxicity Inflation Factor for Recycled Patients",
                                     "Assuming recycled patients will have higher toxicity rate"),
                             1.1,
                             min = 1, step = 0.1),
                numericInput("inRegReuse",
                             tip_txt("Number of Recycled Patients Allowed in Each Cohort",
                                     "Number of patients allowed for intra-patient
                                  dose escalation in each cohort"),
                             3,
                             min = 0, step = 1),
                conditionalPanel(
                    condition = "input.inChkAcc == true",
                    numericInput("inAccReuse",
                                 tip_txt("Number of Recycled Patients Allowed in Each Cohort for Accleration",
                                         "Number of patients allowed for intra-patient
                                  dose esclation"),
                                 1,
                                 min = 0, step = 1)
                )
            )),

        accordion_panel(
            "Operation Setting",

            numericInput("inEnrollRate",
                         tip_txt("Enrollment Per Month",
                                 "Number of patients enrolled by month"),
                         0.5,
                         min = 0, step = 0.1),
            numericInput("inDltDays",
                         tip_txt("DLT Window Days",
                                 "Number of days for dose limiting toxicity"),
                         28,
                         min = 0, step = 1),

            numericInput("inRegSize",
                         tip_txt("Cohort Size",
                                 "Cohort size after acceleration"),
                         3,
                         min = 0, step = 1)
        ),

        accordion_panel(
            "Simulation",
            numericInput("inSimuRep",
                         "Number of Replications",
                         500,
                         min = 0, step = 100),
            numericInput("inSimuCore",
                         "Number of Cores",
                         10,
                         min = 0, step = 1),
            numericInput("inSimuSeed",
                         "Random Seed",
                         10000,
                         min = 0, step = 100)
        )
    )
)

panel_design <- list(
    accordion(
        accordion_panel(
            "Toxicity Rates",
            plotOutput("plt_design")
        ),
        accordion_panel(
            "Design Details",
            verbatimTextOutput("txt_design")
        ),
        accordion_panel(
            "Design Parameters",
            verbatimTextOutput("txt_design_para")
        )
    )
)

panel_trial <- list(
    layout_columns(
        card(
            card_header("Simulated Data"),
            DT::dataTableOutput("dt_trial"),
            min_height  = 500,
            full_screen = TRUE
        ),

        card(
            card_header("Results by Dose"),
            DT::dataTableOutput("dt_trial_rst1")
        ),

        card(
            card_header("Results by Study"),
            DT::dataTableOutput("dt_trial_rst2")
        ),
        col_widths  = c(12, 12, 12)
    )
)

panel_simu <- list(
    layout_columns(
        card(
            actionButton("inBtnSimu",
                         "Simulate",
                         class = "btn btn-success")
        ),
        card(
            card_header("Dose Recommended"),
            DT::dataTableOutput("dt_simu_rst1")
        ),

        card(
            card_header("Results by Dose"),
            DT::dataTableOutput("dt_simu_rst2")
        ),

        card(
            card_header("Results by Study"),
            DT::dataTableOutput("dt_simu_rst3")
        ),
        col_widths  = c(12, 12, 12, 12)
    )
)

panel_history <- list(
    card(
        actionButton("inBtnClear",
                     "Clear History",
                     class = "btn btn-success")
    ),
    card(
        DT::dataTableOutput("dt_simu_hist")
    )
)


tab_main <- function() {
    page_sidebar(
        title   = "Phase I Dose Escalation Simulation",
        theme   = bs_theme(
            bootswatch = "simplex",
            base_font = font_google("Inter")
        ),

        sidebar = sidebar(
            title = "Design Setting",
            width = 350,
            !!!ui_settings
        ),

        navset_pill(
            nav_panel(title = "Design", !!!panel_design),
            nav_panel(title = "Single Trial", !!!panel_trial),
            nav_panel(title = "Operating Characteristics",
                      !!!panel_simu),
            nav_panel(title = "History", !!!panel_history)
        )
    )
}


## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
##
##                                DATA
##
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
get_design_para <- reactive({

    if (input$inChkTit) {
        n_reuse_regular <- input$inRegReuse
        n_reuse_acc     <- input$inAccReuse
    } else {
        n_reuse_regular <- 0
        n_reuse_acc     <- 0
    }

    if (input$inChkAcc) {
        size_cohort_acc <- input$inAccSize
    } else {
        size_cohort_acc <- input$inRegSize
    }

    list(
        tox_rate = tkt_assign(input$inTox),
        res_rate = tkt_assign(input$inRes),
        rho      = tkt_assign(input$inRho),

        par_enroll = list(type       = "by_rate",
                          pt_per_mth = input$inEnrollRate),

        dlt_days            = input$inDltDays,
        size_cohort_regular = input$inRegSize,
        size_cohort_acc     = size_cohort_acc,
        n_reuse_regular     = n_reuse_regular,
        n_reuse_acc         = n_reuse_acc,
        acc_max_dose        = input$inAccMaxCycle,
        intra_fac           = input$inAccFactor,

        ## BOIN design
        target_tox          = input$inTarTox,
        boin_upper          = input$inBoinUp,
        boin_lower          = input$inBoinLow,
        sample_size         = input$inSampleSize,
        size_dose           = input$inSizeDose,
        boin_boundMTD       = input$inBOINBound
    )
})

get_design <- reactive({

    lst_para <- get_design_para()
    req(lst_para)

    design <- input$inDesign
    req(design)

    xx <- stb_create_design(design)
    stb_para(xx) <- lst_para
    xx
})

get_trial <- reactive({
    xx <- get_design()

    if (is.null(xx))
        return(NULL)

    stb_create_trial(xx,
                     seed = input$inSimuSeed)
})

plot_design <- reactive({
    xx <- get_design()

    if (is.null(xx))
        return(NULL)

    stb_plot_design(xx)
})

get_simu_rst <- reactive({
    xx <- get_design()

    if (is.null(xx))
        return(NULL)

    zz <- stb_create_simustudy(xx,
                               n_rep  = input$inSimuRep,
                               n_core = input$inSimuCore,
                               seed   = input$inSimuSeed)

    rst             <- stb_get_simu_summary(zz)
    userLog$history <- rbind(userLog$history,
                             rst$by_study)

    rst
}) %>%
    bindEvent(input$inBtnSimu)

observeEvent(input$inBtnClear, {
    userLog$history <- NULL
})
