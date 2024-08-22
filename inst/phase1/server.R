shinyServer(function(input, output, session) {

    source("design_ui.R", local = TRUE)
    userLog         <- reactiveValues()
    userLog$history <- NULL

    ##---------main-------------------------
    ##--------------------------------------
    output$mainpage <- renderUI({
        tab_main()
    })

    ##---------design-----------------------
    ##--------------------------------------
    output$dt_trial <- DT::renderDataTable({
                               trial <- get_trial()

                               if (is.null(trial))
                                   return(NULL)

                               rst <- stb_get_trial_data(trial)
                               rst %>%
                                   select(-one_of(c("mth_enroll",
                                                    "day_enroll",
                                                    "response",
                                                    "TR00",
                                                    "TR01",
                                                    "TR10",
                                                    "TR11")))
                           },
                           options = list(paging = FALSE, info = FALSE)
                           )

    output$plt_design <- renderPlot({
        plot_design()
    })

    output$txt_design <- renderPrint({
        xx <- get_design()
        req(xx)

        stb_describe(xx)
    })

    output$txt_design_para <- renderPrint({
        xx <- get_design()
        req(xx)
        stb_get_para(xx)
    })

    ##---------trial------------------------
    ##--------------------------------------
    output$dt_trial_rst1 <- DT::renderDataTable({
                                    trial <- get_trial()

                                    if (is.null(trial))
                                        return(NULL)

                                    stb_get_trial_result(trial)$by_dose
                           },
                           options = list(
                               paging = FALSE,
                               dom = 't',
                               info = FALSE)
                           )

    output$dt_trial_rst2 <- DT::renderDataTable({
                                    trial <- get_trial()

                                    if (is.null(trial))
                                        return(NULL)

                                    stb_get_trial_result(trial)$by_study
                           },
                           options = list(
                               paging = FALSE,
                               dom = 't',
                               info = FALSE)
                           )

    ##---------simu-------------------------
    ##--------------------------------------
    output$dt_simu_rst1 <- DT::renderDataTable({
                                    rst <- get_simu_rst()

                                    if (is.null(rst))
                                        return(NULL)

                                    rst$dose_recommend
                                },
                                options = list(
                                    paging = FALSE,
                                    dom = 't',
                                    info = FALSE)
                                )

    output$dt_simu_rst2 <- DT::renderDataTable({
                                    rst <- get_simu_rst()

                                    if (is.null(rst))
                                        return(NULL)

                                    rst$by_dose
                                },
                                options = list(
                                    paging = FALSE,
                                    dom = 't',
                                    info = FALSE)
                                )

    output$dt_simu_rst3 <- DT::renderDataTable({
                                    rst <- get_simu_rst()

                                    if (is.null(rst))
                                        return(NULL)

                                    rst$by_study
                                },
                                options = list(
                                    paging = FALSE,
                                    dom = 't',
                                    info = FALSE)
                                )
    ##---------history----------------------
    ##--------------------------------------
    output$dt_simu_hist <- DT::renderDataTable({
                                   userLog$history
                               },
                               options = list(
                                   paging = FALSE,
                                   dom = 't',
                                   info = FALSE)
                               )

})
