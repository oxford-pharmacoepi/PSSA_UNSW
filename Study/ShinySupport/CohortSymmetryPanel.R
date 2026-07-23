cohortSymmetryPanel <- function(resultTypes,
                                title = "CohortSymmetry",
                                includeProtocolBenchmark = FALSE) {
  plotXAxis <- if (identical(resultTypes, "temporal_symmetry")) {
    "variable_level"
  } else {
    "index_cohort_name"
  }

  content <- list(
    tidy = OmopViewer::omopViewerPanels$default$content$tidy,
    table = list(
      title = "Table",
      output_type = "gt",
      reactive = paste(
        "<filtered_data> |> cohortSymmetryTable(",
        "header = input$header,",
        "group = input$group_column,",
        "hide = input$hide)",
        sep = "\n"
      ),
      render = "<reactive_data>",
      filters = OmopViewer::omopViewerPanels$default$content$table$filters,
      download = OmopViewer::omopViewerPanels$default$content$table$download
    ),
    plot = list(
      title = "Package plot",
      output_type = "plotly",
      reactive = paste(
        "<filtered_data> |> cohortSymmetryPlot(",
        "x = input$x,",
        "facet = input$facet,",
        "colour = input$colour)",
        sep = "\n"
      ),
      render = paste(
        "plot <- <reactive_data>",
        "if (inherits(plot, \"ggplot\")) {",
        "plotly::ggplotly(plot)",
        "} else {",
        "plot",
        "}",
        sep = "\n"
      ),
      filters = list(
        x = list(
          button_type = "pickerInput",
          label = "x axis",
          choices = c(
            "cdm_name",
            "<group>",
            "<strata>",
            "<additional>",
            "<settings>",
            "variable_name",
            "variable_level"
          ),
          selected = plotXAxis,
          multiple = FALSE
        ),
        facet = list(
          button_type = "pickerInput",
          label = "Facet",
          choices = c(
            "cdm_name",
            "cohort_date_range",
            "combination_window",
            "days_prior_observation",
            "washout_window"
          ),
          selected = "cdm_name",
          multiple = TRUE
        ),
        colour = list(
          button_type = "pickerInput",
          label = "Colour",
          choices = c(
            "cdm_name",
            "<group>",
            "<strata>",
            "<additional>",
            "<settings>",
            "variable_name"
          ),
          selected = "variable_name",
          multiple = TRUE
        )
      ),
      download = list(
        label = "Download plot",
        render = paste(
          "plot <- <reactive_data>",
          "ggplot2::ggsave(file, plot = plot, width = 11, height = 8.5)",
          sep = "\n"
        ),
        filename = "cohort_symmetry_plot.png"
      )
    )
  )

  if (includeProtocolBenchmark) {
    content$benchmark_table <- list(
      title = "Protocol table",
      output_type = "gt",
      reactive = "<filtered_data> |> cohortSymmetryBenchmarkTable()",
      render = "<reactive_data>",
      filters = list(),
      download = OmopViewer::omopViewerPanels$default$content$table$download
    )
    content$benchmark_plot <- list(
      title = "Primary forest plot",
      output_type = "plotly",
      reactive = "<filtered_data> |> cohortSymmetryBenchmarkPlot()",
      render = "plotly::ggplotly(<reactive_data>)",
      filters = list(),
      download = list(
        label = "Download plot",
        render = "ggplot2::ggsave(file, plot = <reactive_data>, width = 11, height = 8.5)",
        filename = "primary_asr_forest_plot.png"
      )
    )
    content$sensitivity_plot <- list(
      title = "Sensitivity plot",
      output_type = "plotly",
      reactive = "<filtered_data> |> cohortSymmetrySensitivityPlot()",
      render = "plotly::ggplotly(<reactive_data>)",
      filters = list(),
      download = list(
        label = "Download plot",
        render = "ggplot2::ggsave(file, plot = <reactive_data>, width = 14, height = 10)",
        filename = "asr_sensitivity_plot.png"
      )
    )
  }

  structure(
    list(
      title = title,
      icon = "chart-line",
      data = list(result_type = resultTypes),
      automatic_filters = c(
        "group",
        "strata",
        "additional",
        "settings",
        "variable_name",
        "estimate_name"
      ),
      exclude_filters = c("cdm_name"),
      filters = list(
        cdm_name = list(
          button_type = "pickerInput",
          label = "CDM name",
          column = "cdm_name",
          column_type = "main",
          choices = "choices$",
          selected = "selected$",
          multiple = TRUE
        )
      ),
      content = content
    ),
    class = "omopviewer_panel"
  )
}

appendCohortSymmetryShinyFunctions <- function(shinyDirectory) {
  functionsFile <- file.path(shinyDirectory, "functions.R")
  helperFile <- file.path(studyPath, "ShinySupport", "CohortSymmetryFunctions.R")
  write(
    c("", "# CohortSymmetry protocol helpers", readLines(helperFile)),
    file = functionsFile,
    append = TRUE
  )
}
