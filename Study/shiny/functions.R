backgroundCard <- function(fileName) {
  # read file
  content <- readLines(fileName)
  
  # extract yaml metadata
  # Find the positions of the YAML delimiters (----- or ---)
  yamlStart <- grep("^---|^-----", content)[1]
  yamlEnd <- grep("^---|^-----", content)[2]
  
  if (any(is.na(c(yamlStart, yamlEnd)))) {
    metadata <- NULL
  } else {
    # identify YAML block
    id <- (yamlStart + 1):(yamlEnd - 1)
    # Parse the YAML content
    metadata <- yaml::yaml.load(paste(content[id], collapse = "\n"))
    # eliminate yaml part from content
    content <- content[-(yamlStart:yamlEnd)]
  }
  
  tmpFile <- tempfile(fileext = ".md")
  writeLines(text = content, con = tmpFile)
  
  # metadata referring to keys
  backgroundKeywords <- list(
    header = "bslib::card_header",
    footer = "bslib::card_footer"
  )
  keys <- names(backgroundKeywords) |>
    rlang::set_names() |>
    purrr::map(\(x) {
      if (x %in% names(metadata)) {
        paste0(backgroundKeywords[[x]], "(metadata[[x]])") |>
          rlang::parse_expr() |>
          rlang::eval_tidy()
      } else {
        NULL
      }
    }) |>
    purrr::compact()
  
  arguments <- c(
    # metadata referring to arguments of card
    metadata[names(metadata) %in% names(formals(bslib::card))],
    # content
    list(
      keys$header,
      bslib::card_body(shiny::HTML(markdown::markdownToHTML(
        file = tmpFile, fragment.only = TRUE
      ))),
      keys$footer
    ) |>
      purrr::compact()
  )
  
  unlink(tmpFile)
  
  do.call(bslib::card, arguments)
}
summaryCdmName <- function(data) {
  if (length(data) == 0) {
    return(list("<b>CDM names</b>" = ""))
  }
  x <- data |>
    purrr::map(\(x) {
      x |>
        dplyr::group_by(.data$cdm_name) |>
        dplyr::summarise(number_rows = dplyr::n(), .groups = "drop")
    }) |>
    dplyr::bind_rows() |>
    dplyr::group_by(.data$cdm_name) |>
    dplyr::summarise(
      number_rows = as.integer(sum(.data$number_rows)),
      .groups = "drop"
    ) |>
    dplyr::mutate(label = paste0(.data$cdm_name, " (", .data$number_rows, ")")) |>
    dplyr::pull("label") |>
    rlang::set_names() |>
    as.list()
  list("<b>CDM names</b>" = x)
}
summaryPackages <- function(data) {
  if (length(data) == 0) {
    return(list("<b>Packages versions</b>" = ""))
  }
  x <- data |>
    purrr::map(\(x) {
      x |>
        omopgenerics::addSettings(
          settingsColumn = c("package_name", "package_version")
        ) |>
        dplyr::group_by(.data$package_name, .data$package_version) |>
        dplyr::summarise(number_rows = dplyr::n(), .groups = "drop") |>
        dplyr::right_join(
          omopgenerics::settings(x) |>
            dplyr::select(c("package_name", "package_version")) |>
            dplyr::distinct(),
          by = c("package_name", "package_version")
        ) |>
        dplyr::mutate(number_rows = dplyr::coalesce(.data$number_rows, 0))
    }) |>
    dplyr::bind_rows() |>
    dplyr::group_by(.data$package_name, .data$package_version) |>
    dplyr::summarise(
      number_rows = as.integer(sum(.data$number_rows)),
      .groups = "drop"
    ) |>
    dplyr::group_by(.data$package_name) |>
    dplyr::group_split() |>
    as.list()
  lab <- "<b>"
  names(x) <- x |>
    purrr::map_chr(\(x) {
      if (nrow(x) > 1) {
        lab <<- "<b style='color:red'>"
        paste0("<b style='color:red'>", unique(x$package_name), " (Multiple versions!) </b>")
      } else {
        paste0(
          x$package_name, " (version = ", x$package_version,
          "; number records = ", x$number_rows,")"
        )
      }
    })
  x <- x |>
    purrr::map(\(x) {
      if (nrow(x) > 1) {
        paste0(
          "version = ", x$package_version, "; number records = ",
          x$number_rows
        ) |>
          rlang::set_names() |>
          as.list()
      } else {
        x$package_name
      }
    })
  list(x) |>
    rlang::set_names(nm = paste0(lab, "Packages versions</b>"))
}
summaryMinCellCount <- function(data) {
  if (length(data) == 0) {
    return(list("<b>Min Cell Count Suppression</b>" = ""))
  }
  x <- data |>
    purrr::map(\(x) {
      x |>
        omopgenerics::addSettings(settingsColumn = "min_cell_count") |>
        dplyr::group_by(.data$min_cell_count) |>
        dplyr::summarise(number_rows = dplyr::n(), .groups = "drop") |>
        dplyr::right_join(
          omopgenerics::settings(x) |>
            dplyr::select("min_cell_count") |>
            dplyr::distinct(),
          by = "min_cell_count"
        ) |>
        dplyr::mutate(number_rows = dplyr::coalesce(.data$number_rows, 0))
    }) |>
    dplyr::bind_rows() |>
    dplyr::group_by(.data$min_cell_count) |>
    dplyr::summarise(
      number_rows = as.integer(sum(.data$number_rows)),
      .groups = "drop"
    ) |>
    dplyr::mutate(min_cell_count = as.integer(.data$min_cell_count)) |>
    dplyr::arrange(.data$min_cell_count) |>
    dplyr::mutate(
      label = dplyr::if_else(
        .data$min_cell_count == 0L,
        "<b style='color:red'>Not censored</b>",
        paste0("Min cell count = ", .data$min_cell_count)
      ),
      label = paste0(.data$label, " (", .data$number_rows, ")")
    ) |>
    dplyr::pull("label") |>
    rlang::set_names() |>
    as.list()
  lab <- ifelse(any(grepl("Not censored", unlist(x))), "<b style='color:red'>", "<b>")
  list(x) |>
    rlang::set_names(nm = paste0(lab, "Min Cell Count Suppression</b>"))
}
summaryPanels <- function(data) {
  if (length(data) == 0) {
    return(list("<b>Panels</b>" = ""))
  }
  x <- data |>
    purrr::map(\(x) {
      if (nrow(x) == 0) {
        res <- omopgenerics::settings(x) |>
          dplyr::select(!c(
            "result_id", "package_name", "package_version", "group", "strata",
            "additional", "min_cell_count"
          )) |>
          dplyr::relocate("result_type") |>
          as.list() |>
          purrr::map(\(x) sort(unique(x)))
      } else {
        sets <- c("result_type", omopgenerics::settingsColumns(x))
        res <- x |>
          omopgenerics::addSettings(settingsColumn = sets) |>
          dplyr::relocate(dplyr::all_of(sets)) |>
          omopgenerics::splitAll() |>
          dplyr::select(!c(
            "variable_name", "variable_level", "estimate_name",
            "estimate_type", "estimate_value", "result_id"
          )) |>
          as.list() |>
          purrr::map(\(values) {
            values <- as.list(table(values))
            paste0(names(values), " (number rows = ", values, ")") |>
              rlang::set_names() |>
              as.list()
          })
      }
      res
    })
  list(x) |>
    rlang::set_names(nm = "<b>Panels</b>")
}
simpleTable <- function(result,
                        header = character(),
                        group = character(),
                        hide = character()) {
  # initial checks
  if (length(header) == 0) header <- character()
  if (length(group) == 0) group <- NULL
  if (length(hide) == 0) hide <- character()
  
  if (nrow(result) == 0) {
    return(gt::gt(dplyr::tibble()))
  }
  
  result <- result |>
    omopgenerics::addSettings() |>
    omopgenerics::splitAll() |>
    addProtocolPairMetadata() |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.logical),
        ~dplyr::case_when(
          is.na(.x) ~ NA_character_,
          .x ~ "Yes",
          TRUE ~ "No"
        )
      )
    ) |>
    dplyr::select(-"result_id")
  
  # format estimate column
  formatEstimates <- c(
    "N (%)" = "<count> (<percentage>%)",
    "N" = "<count>",
    "median [Q25 - Q75]" = "<median> [<q25> - <q75>]",
    "mean (SD)" = "<mean> (<sd>)",
    "[Q25 - Q75]" = "[<q25> - <q75>]",
    "range" = "[<min> <max>]",
    "[Q05 - Q95]" = "[<q05> - <q95>]"
  )
  result <- result |>
    visOmopResults::formatEstimateValue(
      decimals = c(integer = 0, numeric = 1, percentage = 0)
    ) |>
    visOmopResults::formatEstimateName(estimateName = formatEstimates) |>
    suppressMessages() |>
    visOmopResults::formatHeader(header = header) |>
    dplyr::select(!dplyr::any_of(c("estimate_type", hide)))
  if (length(group) > 1) {
    id <- paste0(group, collapse = "; ")
    result <- result |>
      tidyr::unite(col = !!id, dplyr::all_of(group), sep = "; ", remove = TRUE)
    group <- id
  }
  result <- result |>
    visOmopResults::formatTable(groupColumn = group)
  return(result)
}
tidyDT <- function(x,
                   columns,
                   pivotEstimates) {
  groupColumns <- omopgenerics::groupColumns(x)
  strataColumns <- omopgenerics::strataColumns(x)
  additionalColumns <- omopgenerics::additionalColumns(x)
  settingsColumns <- omopgenerics::settingsColumns(x)
  settingsColumns <- setdiff(
    settingsColumns,
    c("cdm_name", groupColumns, strataColumns, additionalColumns)
  )
  
  # split and add settings
  x <- x |>
    omopgenerics::splitAll() |>
    omopgenerics::addSettings() |>
    addProtocolPairMetadata()
  
  # remove density
  x <- x |>
    dplyr::filter(!.data$estimate_name %in% c("density_x", "density_y"))
  
  # estimate columns
  if (pivotEstimates) {
    estCols <- unique(x$estimate_name)
    x <- x |>
      omopgenerics::pivotEstimates()
  } else {
    estCols <- c("estimate_name", "estimate_type", "estimate_value")
  }
  
  # order columns
  cols <- list(
    "CDM name" = "cdm_name", "Group" = groupColumns, "Strata" = strataColumns,
    "Additional" = additionalColumns, "Settings" = settingsColumns,
    "Variable" = c("variable_name", "variable_level"),
    "Protocol" = c(
      "pair_id", "marker_type", "tier", "expected_association",
      "expected_direction", "include_in_benchmark", "protocol_note"
    )
  ) |>
    purrr::map(\(x) x[x %in% columns]) |>
    purrr::compact()
  cols[["Estimates"]] <- estCols
  x <- x |>
    dplyr::select(dplyr::all_of(unname(unlist(cols))))
  
  # prepare the header
  container <- shiny::tags$table(
    class = "display",
    shiny::tags$thead(
      purrr::imap(cols, \(x, nm) shiny::tags$th(colspan = length(x), nm)) |>
        shiny::tags$tr(),
      shiny::tags$tr(purrr::map(unlist(cols), shiny::tags$th))
    )
  )
  
  # create DT table
  DT::datatable(
    data = x,
    filter = "top",
    container = container,
    rownames = FALSE,
    options = list(searching = FALSE)
  )
}

addProtocolPairMetadata <- function(data) {
  if (!exists("protocolPairs", inherits = TRUE)) return(data)

  pairs <- get("protocolPairs", inherits = TRUE)
  if (all(c("index_cohort_name", "marker_cohort_name") %in% names(data))) {
    data <- dplyr::left_join(
      data, pairs,
      by = c("index_cohort_name", "marker_cohort_name")
    )
  } else if (all(c("index_name", "marker_name") %in% names(data))) {
    data <- dplyr::left_join(
      data, pairs,
      by = c(
        "index_name" = "index_cohort_name",
        "marker_name" = "marker_cohort_name"
      )
    )
  }

  if ("include_in_benchmark" %in% names(data) &&
      !"benchmark_status" %in% names(data)) {
    data <- data |>
      dplyr::mutate(
        benchmark_status = dplyr::case_when(
          is.na(.data$include_in_benchmark) ~ "Unknown",
          .data$include_in_benchmark ~ "Yes",
          TRUE ~ "No"
        )
      )
  }

  data
}

classifySequenceRatioEvidence <- function(data) {
  required <- c(
    "point_estimate", "lower_CI", "upper_CI", "expected_direction"
  )
  for (column in setdiff(required, names(data))) {
    data[[column]] <- NA_character_
  }

  data |>
    dplyr::mutate(
      point_estimate = suppressWarnings(as.numeric(.data$point_estimate)),
      lower_CI = suppressWarnings(as.numeric(.data$lower_CI)),
      upper_CI = suppressWarnings(as.numeric(.data$upper_CI)),
      invalid_interval = !is.na(.data$point_estimate) &
        !is.na(.data$lower_CI) & !is.na(.data$upper_CI) &
        (.data$lower_CI > .data$point_estimate |
          .data$point_estimate > .data$upper_CI),
      observed_direction = dplyr::case_when(
        is.na(.data$point_estimate) | is.na(.data$lower_CI) |
          is.na(.data$upper_CI) ~ "Unavailable",
        .data$invalid_interval ~ "Invalid interval",
        .data$lower_CI > 1 ~ "Above 1",
        .data$upper_CI < 1 ~ "Below 1",
        TRUE ~ "Includes 1"
      ),
      validated_status = dplyr::case_when(
        is.na(.data$expected_direction) |
          .data$expected_direction == "unknown" ~ "Not assessed",
        .data$observed_direction == "Unavailable" ~ "Unavailable",
        .data$observed_direction == "Invalid interval" ~ "Invalid interval",
        .data$expected_direction == "above_1" &
          .data$observed_direction == "Above 1" ~ "Validated",
        .data$expected_direction == "not_above_1" &
          .data$observed_direction == "Below 1" ~ "Validated",
        .data$observed_direction == "Includes 1" ~ "Inconclusive (CI includes 1)",
        TRUE ~ "Not validated"
      )
    ) |>
    dplyr::select(-"invalid_interval")
}

filterProtocolPairs <- function(result, indexColumn, markerColumn,
                                selectedIndex, selectedMarker) {
  if (!exists("protocolPairs", inherits = TRUE)) return(result)

  pairs <- get("protocolPairs", inherits = TRUE) |>
    dplyr::filter(
      .data$index_cohort_name %in% selectedIndex,
      .data$marker_cohort_name %in% selectedMarker
    )
  if (nrow(pairs) == 0) return(result[0, ])

  results <- purrr::pmap(
    pairs,
    function(index_cohort_name, marker_cohort_name, ...) {
      omopgenerics::filterGroup(
        result,
        !!rlang::sym(indexColumn) == !!index_cohort_name,
        !!rlang::sym(markerColumn) == !!marker_cohort_name
      )
    }
  )
  do.call(omopgenerics::bind, results)
}
prepareResult <- function(result, resultList) {
  purrr::map(resultList, \(x) filterResult(result, x))
}
filterResult <- function(result, filt) {
  nms <- names(filt)
  for (nm in nms) {
    q <- paste0(".data$", nm, " %in% filt[[\"", nm, "\"]]") |>
      rlang::parse_exprs() |>
      rlang::eval_tidy()
    result <- omopgenerics::filterSettings(result, !!!q)
  }
  return(result)
}
getValues <- function(result, resultList) {
  resultList |>
    purrr::imap(\(x, nm) {
      res <- filterResult(result, x)
      values <- res |>
        dplyr::select(!c("estimate_type", "estimate_value")) |>
        dplyr::distinct() |>
        omopgenerics::splitAll() |>
        dplyr::select(!"result_id") |>
        as.list() |>
        purrr::map(\(x) sort(unique(x)))
      valuesSettings <- omopgenerics::settings(res) |>
        dplyr::select(!dplyr::any_of(c(
          "result_id", "result_type", "package_name", "package_version",
          "group", "strata", "additional", "min_cell_count"
        ))) |>
        as.list() |>
        purrr::map(\(x) sort(unique(x[!is.na(x)]))) |>
        purrr::compact()
      values <- c(values, valuesSettings)
      names(values) <- paste0(nm, "_", names(values))
      values
    }) |>
    purrr::flatten()
}
getSelected <- function(choices) {
  purrr::imap(choices, \(vals, nm) {
    if (grepl("_denominator_sex$", nm)) {
      if ("Both" %in% vals) return("Both")
      return(vals[[1]])
    }
    
    if (grepl("_denominator_age_group$", nm)) {
      bounds <- regmatches(vals, regexec("^(\\d+) to (\\d+)$", vals))
      valid <- vapply(bounds, length, integer(1)) == 3
      if (any(valid)) {
        ranges <- vapply(bounds[valid], \(x) as.numeric(x[3]) - as.numeric(x[2]), numeric(1))
        return(vals[valid][[which.max(ranges)]])
      } else {
        return(vals[[1]])
      }
    }
    
    if (grepl("_outcome_cohort_name$", nm)) {
      return(vals[[1]])
    }
    
    vals
  })
}
renderInteractivePlot <- function(plt, interactive) {
  if (interactive) {
    plotly::renderPlotly(plt)
  } else {
    shiny::renderPlot(plt)
  }
}

# CohortSymmetry helpers
cohortSymmetryExport <- function(functionNames) {
  if (!requireNamespace("CohortSymmetry", quietly = TRUE)) {
    return(NULL)
  }
  
  for (functionName in functionNames) {
    fn <- tryCatch(
      getExportedValue("CohortSymmetry", functionName),
      error = function(e) NULL
    )
    if (!is.null(fn)) {
      return(fn)
    }
  }
  NULL
}

cohortSymmetryResultType <- function(result) {
  resultTypes <- tryCatch(
    unique(omopgenerics::settings(result)$result_type),
    error = function(e) character()
  )
  if (any(grepl("temporal", resultTypes, ignore.case = TRUE))) {
    "temporal"
  } else if (any(grepl("adjusted", resultTypes, ignore.case = TRUE))) {
    "adjusted"
  } else {
    "sequence"
  }
}

cohortSymmetryCall <- function(functionNames, result, args = list()) {
  fn <- cohortSymmetryExport(functionNames)
  if (is.null(fn)) {
    return(NULL)
  }
  
  calls <- list(
    c(list(result), args),
    c(list(result = result), args),
    c(list(x = result), args),
    list(result),
    list(result = result),
    list(x = result)
  )
  
  for (callArgs in calls) {
    value <- tryCatch(
      do.call(fn, callArgs),
      error = function(e) NULL
    )
    if (!is.null(value)) {
      return(value)
    }
  }
  NULL
}

cohortSymmetryTable <- function(result, header = character(), group = character(), hide = character()) {
  resultType <- cohortSymmetryResultType(result)

  tableData <- result |>
    omopgenerics::addSettings() |>
    omopgenerics::splitAll() |>
    addProtocolPairMetadata() |>
    omopgenerics::pivotEstimates()

  if (identical(resultType, "temporal")) {
    tableData <- tableData |>
      dplyr::mutate(
        pair = paste(.data$index_name, .data$marker_name, sep = " -> ")
      ) |>
      dplyr::select(
        dplyr::any_of(c(
          "cdm_name", "pair", "analysis_label", "analysis_id",
          "marker_type", "variable_level", "count", "expected_direction",
          "benchmark_status"
        ))
      ) |>
      dplyr::rename(
        analysis = "analysis_label",
        day = "variable_level",
        temporal_count = "count",
        marker = "marker_type",
        protocol_expectation = "expected_direction",
        benchmark_pair = "benchmark_status"
      ) |>
      dplyr::arrange(.data$pair, .data$analysis, suppressWarnings(as.numeric(.data$day)))
  } else {
    tableData <- tableData |>
      classifySequenceRatioEvidence() |>
      dplyr::mutate(
        pair = paste(.data$index_cohort_name, .data$marker_cohort_name, sep = " -> "),
        estimate = dplyr::case_when(
          !is.na(.data$point_estimate) & !is.na(.data$lower_CI) & !is.na(.data$upper_CI) ~
            paste0(.data$point_estimate, " [", .data$lower_CI, ", ", .data$upper_CI, "]"),
          !is.na(.data$point_estimate) ~ as.character(.data$point_estimate),
          TRUE ~ NA_character_
        )
      ) |>
      dplyr::select(
        dplyr::any_of(c(
          "cdm_name", "pair", "analysis_label", "analysis_id",
          "marker_type", "variable_name", "estimate", "expected_direction",
          "observed_direction", "validated_status", "benchmark_status"
        ))
      ) |>
      dplyr::rename(
        analysis = "analysis_label",
        marker = "marker_type",
        estimate_type = "variable_name",
        protocol_expectation = "expected_direction",
        observed_direction = "observed_direction",
        validation = "validated_status",
        benchmark_pair = "benchmark_status"
      ) |>
      dplyr::arrange(.data$pair, .data$analysis, .data$estimate_type)
  }

  if ("analysis" %in% names(tableData)) {
    tableData <- tableData |>
      dplyr::mutate(
        analysis = dplyr::coalesce(.data$analysis, .data$analysis_id)
      ) |>
      dplyr::select(-dplyr::any_of("analysis_id"))
  }

  gt::gt(tableData) |>
    gt::sub_missing(columns = dplyr::everything(), missing_text = "\u2014")
}

cohortSymmetryPlot <- function(result, x = "index_cohort_name", facet = "cdm_name", colour = "variable_name") {
  resultType <- cohortSymmetryResultType(result)
  functionNames <- switch(
    resultType,
    temporal = c("plotTemporalSymmetry"),
    adjusted = c("plotAdjustedSequenceRatios", "plotAdjustedSequenceRatio", "plotSequenceRatios"),
    c("plotSequenceRatios", "plotSequenceRatio")
  )
  
  plotArgs <- if (identical(resultType, "temporal")) list() else list(x = x, facet = facet, colour = colour)
  plot <- cohortSymmetryCall(functionNames = functionNames, result = result, args = plotArgs)
  
  if (!is.null(plot)) {
    return(plot)
  }
  
  tidyResult <- result |>
    omopgenerics::tidy()
  
  if ("point_estimate" %in% names(tidyResult)) {
    tidyResult <- tidyResult |>
      dplyr::mutate(
        plot_value = suppressWarnings(as.numeric(.data$point_estimate)),
        lower_CI = if ("lower_CI" %in% names(tidyResult)) suppressWarnings(as.numeric(.data$lower_CI)) else NA_real_,
        upper_CI = if ("upper_CI" %in% names(tidyResult)) suppressWarnings(as.numeric(.data$upper_CI)) else NA_real_
      ) |>
      dplyr::filter(!is.na(.data$plot_value))
  } else if ("estimate_value" %in% names(tidyResult)) {
    tidyResult <- tidyResult |>
      dplyr::mutate(plot_value = suppressWarnings(as.numeric(.data$estimate_value))) |>
      dplyr::filter(!is.na(.data$plot_value))
  } else if ("count" %in% names(tidyResult)) {
    tidyResult <- tidyResult |>
      dplyr::mutate(plot_value = suppressWarnings(as.numeric(.data$count))) |>
      dplyr::filter(!is.na(.data$plot_value))
  } else {
    tidyResult <- dplyr::tibble()
  }
  
  if (nrow(tidyResult) == 0) {
    return(ggplot2::ggplot() + ggplot2::theme_void())
  }
  
  if (identical(resultType, "temporal")) {
    if (!"count" %in% names(tidyResult)) {
      return(ggplot2::ggplot() + ggplot2::theme_void())
    }
    tidyResult <- tidyResult |>
      dplyr::mutate(
        time = suppressWarnings(as.integer(.data$variable_level)),
        count = suppressWarnings(as.integer(.data$count))
      ) |>
      dplyr::filter(!is.na(.data$time), !is.na(.data$count), .data$time != 0)
    
    if (nrow(tidyResult) == 0) {
      return(ggplot2::ggplot() + ggplot2::theme_void())
    }
    
    if ("index_name" %in% names(tidyResult) && "marker_name" %in% names(tidyResult)) {
      tidyResult <- tidyResult |>
        dplyr::mutate(cohort_pair = paste(.data$index_name, .data$marker_name, sep = " -> "))
    } else if ("group_level" %in% names(tidyResult)) {
      tidyResult <- tidyResult |>
        dplyr::mutate(cohort_pair = .data$group_level)
    } else {
      tidyResult <- tidyResult |>
        dplyr::mutate(cohort_pair = "Temporal symmetry")
    }
    
    return(
      ggplot2::ggplot(
        tidyResult,
        ggplot2::aes(x = .data$time, y = .data$count, fill = .data$time > 0)
      ) +
        ggplot2::geom_col() +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
        ggplot2::facet_wrap(ggplot2::vars(.data$cohort_pair), scales = "free_y") +
        ggplot2::labs(x = "Time", y = "Individuals (N)", fill = NULL) +
        ggplot2::theme(legend.position = "none")
    )
  }
  
  if ("variable_level" %in% names(tidyResult)) {
    ratioRows <- tidyResult |>
      dplyr::filter(grepl("sequence_ratio", .data$variable_level, ignore.case = TRUE))
    if (nrow(ratioRows) > 0) {
      tidyResult <- ratioRows
    }
  }
  
  firstAvailable <- function(columns, fallback) {
    columns <- columns[columns %in% names(tidyResult)]
    if (length(columns) > 0) columns[[1]] else fallback
  }
  
  x <- firstAvailable(x, firstAvailable(c("index_cohort_name", "variable_name"), names(tidyResult)[[1]]))
  colour <- firstAvailable(colour, firstAvailable(c("variable_name", "marker_cohort_name"), x))
  facet <- facet[facet %in% names(tidyResult)]
  
  if ("index_cohort_name" %in% names(tidyResult) && "marker_cohort_name" %in% names(tidyResult)) {
    tidyResult <- tidyResult |>
      dplyr::mutate(cohort_pair = paste(.data$index_cohort_name, .data$marker_cohort_name, sep = " -> "))
    if (identical(x, "index_cohort_name")) {
      x <- "cohort_pair"
    }
  }
  
  if (length(facet) > 0) {
    tidyResult$.facet <- apply(tidyResult[, facet, drop = FALSE], 1, paste, collapse = " | ")
  }
  
  plot <- ggplot2::ggplot(
    tidyResult,
    ggplot2::aes(
      x = .data[[x]],
      y = .data$plot_value,
      colour = .data[[colour]]
    )
  ) +
    ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.4)) +
    ggplot2::labs(x = NULL, y = NULL, colour = NULL)
  
  if (all(c("lower_CI", "upper_CI") %in% names(tidyResult)) && any(!is.na(tidyResult$lower_CI))) {
    plot <- plot +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = .data$lower_CI, ymax = .data$upper_CI),
        width = 0.15,
        position = ggplot2::position_dodge(width = 0.4)
      )
  }
  
  if (length(facet) > 0) {
    plot <- plot + ggplot2::facet_wrap(ggplot2::vars(.data$.facet))
  }
  
  plot + ggplot2::coord_flip()
}

filterProtocolPairIds <- function(result, pairs, indexColumn, markerColumn) {
  if (nrow(pairs) == 0) return(result[0, ])

  filtered <- purrr::map2(
    pairs$index_cohort_name,
    pairs$marker_cohort_name,
    function(indexName, markerName) {
      omopgenerics::filterGroup(
        result,
        !!rlang::sym(indexColumn) == !!indexName,
        !!rlang::sym(markerColumn) == !!markerName
      )
    }
  )
  do.call(omopgenerics::bind, filtered)
}

protocolDisplayName <- function(x) {
  tools::toTitleCase(gsub("_", " ", x, fixed = TRUE))
}

analysisShortLabel <- function(analysisId, changedParameter, windowDays,
                               blackoutDays, priorObservation) {
  dplyr::case_when(
    analysisId == "primary" ~ "Primary",
    changedParameter == "window" ~ paste0(windowDays, "d window"),
    changedParameter == "blackout" ~ paste0(blackoutDays, "d blackout"),
    changedParameter == "prior_observation" ~
      paste0(priorObservation, "d history"),
    TRUE ~ analysisId
  )
}

sequenceRatioEvidenceData <- function(result) {
  result |>
    omopgenerics::tidy() |>
    addProtocolPairMetadata() |>
    dplyr::filter(
      !is.na(.data$pair_id),
      .data$variable_level == "sequence_ratio"
    ) |>
    classifySequenceRatioEvidence() |>
    dplyr::mutate(
      is_primary = tolower(as.character(.data$is_primary)) == "true",
      window_days = suppressWarnings(as.integer(.data$window_days)),
      blackout_days = suppressWarnings(as.integer(.data$blackout_days)),
      protocol_prior_observation = suppressWarnings(
        as.integer(.data$protocol_prior_observation)
      ),
      pair = paste(
        protocolDisplayName(.data$index_cohort_name),
        protocolDisplayName(.data$marker_cohort_name),
        sep = " \u2192 "
      ),
      marker_label = dplyr::if_else(
        .data$marker_type == "proxy", "Drug proxy", "Condition"
      ),
      analysis_short = analysisShortLabel(
        .data$analysis_id, .data$changed_parameter, .data$window_days,
        .data$blackout_days, .data$protocol_prior_observation
      ),
      benchmark_pair = dplyr::if_else(
        .data$include_in_benchmark, "Yes", "No", missing = "Unknown"
      ),
      estimate = sprintf(
        "%.2f [%.2f, %.2f]", .data$point_estimate,
        .data$lower_CI, .data$upper_CI
      )
    ) |>
    dplyr::arrange(.data$pair, dplyr::desc(.data$is_primary), .data$analysis_id)
}

sequenceRatioEvidenceExport <- function(data) {
  data |>
    dplyr::transmute(
      cdm_name = .data$cdm_name,
      pair_id = .data$pair_id,
      pair = .data$pair,
      marker_type = .data$marker_type,
      tier = .data$tier,
      expected_association = .data$expected_association,
      protocol_expectation = .data$expected_direction,
      benchmark_pair = .data$benchmark_pair,
      analysis_id = .data$analysis_id,
      analysis = .data$analysis_label,
      estimate_type = .data$variable_name,
      point_estimate = .data$point_estimate,
      lower_CI = .data$lower_CI,
      upper_CI = .data$upper_CI,
      observed_direction = .data$observed_direction,
      validation = .data$validated_status
    )
}

sequenceRatioEvidenceTable <- function(data) {
  tableData <- data |>
    dplyr::transmute(
      Database = .data$cdm_name,
      `Index → marker` = .data$pair,
      Marker = .data$marker_label,
      Analysis = .data$analysis_short,
      Estimate = .data$estimate,
      `Protocol expectation` = dplyr::recode(
        .data$expected_direction,
        above_1 = "Above 1", not_above_1 = "Not above 1", unknown = "Unknown"
      ),
      Observed = .data$observed_direction,
      Validation = .data$validated_status,
      Benchmark = .data$benchmark_pair
    )

  DT::datatable(
    tableData,
    rownames = FALSE,
    filter = "top",
    class = "stripe hover compact",
    options = list(
      pageLength = 25,
      lengthMenu = c(10, 25, 50, 100),
      scrollX = TRUE,
      autoWidth = TRUE,
      deferRender = TRUE,
      order = list(list(1, "asc"), list(3, "asc")),
      columnDefs = list(list(className = "dt-nowrap", targets = c(0, 2, 3, 4, 6, 7, 8)))
    )
  ) |>
    DT::formatStyle(
      "Observed",
      backgroundColor = DT::styleEqual(
        c("Above 1", "Below 1", "Includes 1", "Unavailable", "Invalid interval"),
        c("#dff3ea", "#e8eef7", "#fff1cc", "#edf0f2", "#f9d9dc")
      )
    ) |>
    DT::formatStyle(
      "Validation",
      fontWeight = DT::styleEqual("Validated", "600"),
      color = DT::styleEqual(
        c("Validated", "Not validated", "Inconclusive (CI includes 1)"),
        c("#176b46", "#a23b3f", "#8a5a00")
      )
    )
}

safeRatio <- function(numerator, denominator) {
  dplyr::if_else(denominator > 0, numerator / denominator, NA_real_)
}

weightedMetric <- function(value, weight) {
  keep <- is.finite(value) & is.finite(weight) & weight > 0
  if (!any(keep)) return(NA_real_)
  stats::weighted.mean(value[keep], weight[keep])
}

sequencePerformanceMetrics <- function(data) {
  if (nrow(data) == 0) return(dplyr::tibble())

  data |>
    dplyr::filter(
      .data$benchmark_pair == "Yes",
      .data$expected_association %in% c("positive", "negative")
    ) |>
    dplyr::group_by(
      .data$cdm_name, .data$marker_label, .data$analysis_id,
      .data$analysis_short, .data$is_primary
    ) |>
    dplyr::summarise(
      n_positive = sum(.data$expected_association == "positive"),
      true_positive = sum(
        .data$expected_association == "positive" &
          .data$observed_direction == "Above 1", na.rm = TRUE
      ),
      positive_below_1 = sum(
        .data$expected_association == "positive" &
          .data$observed_direction == "Below 1", na.rm = TRUE
      ),
      n_negative = sum(.data$expected_association == "negative"),
      true_negative = sum(
        .data$expected_association == "negative" &
          .data$observed_direction == "Below 1", na.rm = TRUE
      ),
      false_positive = sum(
        .data$expected_association == "negative" &
          .data$observed_direction == "Above 1", na.rm = TRUE
      ),
      inconclusive = sum(.data$observed_direction == "Includes 1", na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      false_negative = .data$n_positive - .data$true_positive,
      sensitivity = safeRatio(.data$true_positive, .data$n_positive),
      specificity = safeRatio(.data$true_negative, .data$n_negative),
      false_positive_rate = safeRatio(.data$false_positive, .data$n_negative),
      false_negative_rate = safeRatio(.data$false_negative, .data$n_positive),
      ppv = safeRatio(
        .data$true_positive,
        .data$true_positive + .data$false_positive
      ),
      npv = safeRatio(
        .data$true_negative,
        .data$true_negative + .data$positive_below_1
      )
    ) |>
    dplyr::arrange(dplyr::desc(.data$is_primary), .data$analysis_id, .data$marker_label)
}

sequencePerformanceTable <- function(data) {
  if (nrow(data) == 0) {
    return(DT::datatable(dplyr::tibble(Message = "No benchmark controls for these filters")))
  }
  tableData <- data |>
    dplyr::transmute(
      Database = .data$cdm_name,
      Marker = .data$marker_label,
      Design = .data$analysis_short,
      `Positive controls` = .data$n_positive,
      Sensitivity = scales::percent(.data$sensitivity, accuracy = 0.1),
      `False-negative rate` = scales::percent(.data$false_negative_rate, accuracy = 0.1),
      `Negative controls` = .data$n_negative,
      Specificity = scales::percent(.data$specificity, accuracy = 0.1),
      `False-positive rate` = scales::percent(.data$false_positive_rate, accuracy = 0.1),
      PPV = scales::percent(.data$ppv, accuracy = 0.1),
      NPV = scales::percent(.data$npv, accuracy = 0.1),
      Inconclusive = .data$inconclusive
    )
  DT::datatable(
    tableData, rownames = FALSE, filter = "top",
    class = "stripe hover compact",
    options = list(pageLength = 25, scrollX = TRUE, autoWidth = TRUE)
  )
}

cohortSymmetryPerformancePlot <- function(data, designFamily = c("windows", "blackouts")) {
  designFamily <- match.arg(designFamily)
  if (nrow(data) == 0) return(emptyEvidencePlot("No benchmark controls for these filters"))
  designMap <- if (identical(designFamily, "windows")) {
    c(window_30 = "30d", window_60 = "60d", window_90 = "90d", window_180 = "180d", primary = "365d")
  } else {
    c(blackout_0 = "0d", blackout_1 = "1d", primary = "7d", blackout_14 = "14d")
  }
  plotData <- data |>
    dplyr::filter(.data$analysis_id %in% names(designMap)) |>
    dplyr::select(
      "cdm_name", "marker_label", "analysis_id", "sensitivity", "specificity"
    ) |>
    tidyr::pivot_longer(
      cols = c("sensitivity", "specificity"),
      names_to = "metric", values_to = "value"
    ) |>
    dplyr::mutate(
      metric = dplyr::recode(
        .data$metric,
        sensitivity = "Sensitivity", specificity = "Specificity"
      ),
      design = factor(unname(designMap[.data$analysis_id]), levels = unname(designMap)),
      analysis_group = paste0(.data$cdm_name, " - ", .data$marker_label),
      text = paste0(
        .data$analysis_group, "<br>", .data$design,
        "<br>", .data$metric, ": ", scales::percent(.data$value, accuracy = 0.1)
      )
    )
  if (nrow(plotData) == 0) return(emptyEvidencePlot("No benchmark controls for these filters"))
  ggplot2::ggplot(
    plotData,
    ggplot2::aes(
      .data$design, .data$value, colour = .data$analysis_group,
      group = .data$analysis_group, text = .data$text
    )
  ) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~metric, ncol = 1) +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(), limits = c(0, 1),
      expand = ggplot2::expansion(mult = c(0.045, 0.045))
    ) +
    ggplot2::labs(x = NULL, y = "Classification rate", colour = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.margin = ggplot2::margin(12, 18, 22, 12)
    )
}

emptyEvidencePlot <- function(message) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = message, size = 5) +
    ggplot2::xlim(-1, 1) +
    ggplot2::ylim(-1, 1) +
    ggplot2::theme_void()
}

proxyPairTargetMap <- function() {
  tibble::tribble(
    ~proxy_pair_id, ~condition_pair_id,
    "amiodarone_thyroid_hormones", "amiodarone_hypothyroidism",
    "dhp_ccb_loop_diuretics", "dhp_ccb_peripheral_edema",
    "opioids_laxatives", "opioids_constipation",
    "nsaids_proton_pump_inhibitors", "nsaids_upper_gi_bleeding_or_dyspepsia",
    "sglt2_antifungals", "sglt2_genital_fungal_infection",
    "glp1_antiemetics", "glp1_nausea_or_vomiting",
    "ace_inhibitors_antitussives", "ace_inhibitors_cough",
    "antipsychotics_antiparkinson", "antipsychotics_extrapyramidal",
    "ppi_antimigraine", "ppi_migraine",
    "nsaids_ophthalmologicals", "nsaids_glaucoma",
    "bisphosphonates_antidepressants", "bisphosphonates_depression",
    "ace_inhibitors_antiepileptics", "ace_inhibitors_seizure",
    "dhp_ccb_osteoporosis_medicines", "dhp_ccb_osteoporosis",
    "bisphosphonates_antimigraine", "bisphosphonates_migraine",
    "opioids_antigout", "opioids_gout_or_hyperuricemia",
    "nsaids_organic_nitrates", "nsaids_acute_mi_or_angina",
    "contraceptives_anticoagulants", "contraceptives_vte",
    "jak_inhibitors_organic_nitrates", "jak_inhibitors_mace_or_vte"
  )
}

cohortSymmetryPrimaryForestPlot <- function(data, protocolPairs) {
  conditionData <- data |>
    dplyr::filter(
      .data$marker_type == "diagnosis",
      .data$is_primary,
      !is.na(.data$point_estimate),
      is.finite(.data$point_estimate)
    ) |>
    dplyr::mutate(
      forest_pair_id = .data$pair_id,
      series = "Condition (primary)"
    )

  proxyData <- data |>
    dplyr::filter(
      .data$marker_type == "proxy",
      .data$is_primary,
      !is.na(.data$point_estimate),
      is.finite(.data$point_estimate)
    )
  proxyData <- proxyData |>
    dplyr::inner_join(
      proxyPairTargetMap(),
      by = c("pair_id" = "proxy_pair_id")
    ) |>
    dplyr::mutate(
      forest_pair_id = .data$condition_pair_id,
      series = "Drug proxy (primary)"
    )

  forestLabels <- protocolPairs |>
    dplyr::filter(.data$marker_type == "diagnosis") |>
    dplyr::transmute(
      forest_pair_id = .data$pair_id,
      forest_pair = paste(
        protocolDisplayName(.data$index_cohort_name),
        protocolDisplayName(.data$marker_cohort_name),
        sep = " \u2192 "
      )
    )

  plotData <- dplyr::bind_rows(conditionData, proxyData) |>
    dplyr::left_join(forestLabels, by = "forest_pair_id") |>
    dplyr::mutate(
      forest_pair = stats::reorder(.data$forest_pair, .data$point_estimate),
      tooltip = paste0(
        .data$forest_pair, "<br>", .data$series,
        "<br>Actual marker: ", protocolDisplayName(.data$marker_cohort_name),
        "<br>Estimate: ", .data$estimate,
        "<br>Observed: ", .data$observed_direction,
        "<br>Validation: ", .data$validated_status
      )
    )
  if (nrow(plotData) == 0) {
    return(emptyEvidencePlot("No primary estimates for the selected filters"))
  }
  seriesColours <- stats::setNames(
    ifelse(grepl("^Condition", unique(plotData$series)), "#0b6e69", "#7a5195"),
    unique(plotData$series)
  )

  ggplot2::ggplot(
    plotData,
    ggplot2::aes(
      x = .data$forest_pair,
      y = .data$point_estimate,
      ymin = .data$lower_CI,
      ymax = .data$upper_CI,
      colour = .data$series,
      group = .data$series,
      text = .data$tooltip
    )
  ) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", colour = "#64747c") +
    ggplot2::geom_pointrange(
      linewidth = 0.45,
      position = ggplot2::position_dodge(width = 0.58)
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_log10() +
    ggplot2::scale_colour_manual(
      values = seriesColours
    ) +
    ggplot2::labs(x = NULL, y = "Sequence ratio (95% CI, log scale)", colour = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "top",
      panel.grid.major.y = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(10, 24, 10, 10)
    )
}

cohortSymmetrySensitivityMap <- function(data) {
  plotData <- data |>
    dplyr::filter(!is.na(.data$point_estimate), .data$point_estimate >= 0) |>
    dplyr::mutate(
      pair = factor(.data$pair, levels = rev(unique(.data$pair))),
      analysis_short = factor(
        .data$analysis_short,
        levels = c(
          "Primary", "30d window", "60d window", "90d window", "180d window",
          "0d blackout", "1d blackout", "14d blackout", "180d history"
        )
      ),
      ci_excludes_one = .data$observed_direction %in% c("Above 1", "Below 1"),
      colour_value = pmin(.data$point_estimate, 4),
      outlier_class = dplyr::case_when(
        is.infinite(.data$point_estimate) | .data$point_estimate > 10 ~ "Extreme >10",
        .data$point_estimate > 4 ~ "High >4",
        TRUE ~ NA_character_
      ),
      tooltip = paste0(
        .data$pair, "<br>", .data$analysis_short,
        "<br>Estimate: ", .data$estimate,
        "<br>", .data$validated_status
      )
    )
  if (nrow(plotData) == 0) {
    return(emptyEvidencePlot("No sensitivity estimates for the selected filters"))
  }

  ggplot2::ggplot(
    plotData,
    ggplot2::aes(
      x = .data$analysis_short,
      y = .data$pair,
      fill = .data$colour_value,
      text = .data$tooltip
    )
  ) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.35) +
    ggplot2::geom_point(
      data = dplyr::filter(
        plotData, .data$ci_excludes_one, is.na(.data$outlier_class)
      ),
      shape = 21, size = 1.7, stroke = 0.35, fill = "white", colour = "#263238"
    ) +
    ggplot2::geom_point(
      data = dplyr::filter(plotData, !is.na(.data$outlier_class)),
      ggplot2::aes(colour = .data$outlier_class),
      shape = 23, size = 3.2, stroke = 0.8, fill = "white"
    ) +
    ggplot2::scale_fill_gradient2(
      low = "#31688e", mid = "#f7f8f8", high = "#d1495b", midpoint = 1,
      limits = c(0, 4), oob = scales::squish,
      name = "Sequence ratio"
    ) +
    ggplot2::scale_colour_manual(
      values = c("High >4" = "#ff8c00", "Extreme >10" = "#ff00a8"),
      name = "Outlier"
    ) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 35, hjust = 1),
      legend.position = "right",
      plot.margin = ggplot2::margin(10, 24, 10, 10)
    )
}

cohortSymmetryConfigurablePlot <- function(data, facetBy = NULL,
                                            colourBy = "analysis_short",
                                            showCI = FALSE) {
  plotData <- data |>
    dplyr::filter(
      !is.na(.data$point_estimate),
      is.finite(.data$point_estimate),
      .data$point_estimate > 0
    ) |>
    dplyr::mutate(
      pair = stats::reorder(.data$pair, .data$point_estimate),
      expected_association = factor(
        .data$expected_association,
        levels = c("positive", "negative", "unknown"),
        labels = c("Positive control", "Negative control", "Exploratory / unknown")
      ),
      tooltip = paste0(
        .data$pair, "<br>", .data$analysis_short,
        "<br>", .data$marker_label,
        "<br>Estimate: ", .data$estimate,
        "<br>", .data$validated_status
      )
    )
  if (nrow(plotData) == 0) {
    return(emptyEvidencePlot("No finite estimates for the selected filters"))
  }

  colourBy <- if (
    length(colourBy) == 1 && !is.na(colourBy) && colourBy %in% names(plotData)
  ) colourBy else "analysis_short"
  facetBy <- if (
    length(facetBy) == 1 && !is.na(facetBy) &&
      nzchar(facetBy) && !facetBy %in% c("none", "null") &&
      facetBy %in% names(plotData)
  ) facetBy else NULL
  dodge <- ggplot2::position_dodge(width = 0.62)

  plot <- ggplot2::ggplot(
    plotData,
    ggplot2::aes(
      x = .data$point_estimate,
      y = .data$pair,
      colour = .data[[colourBy]],
      group = interaction(.data$analysis_id, .data$marker_type),
      text = .data$tooltip
    )
  ) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", colour = "#64747c") +
    ggplot2::geom_point(position = dodge, size = 2.1) +
    ggplot2::scale_x_log10() +
    ggplot2::labs(
      x = "Sequence ratio (log scale)", y = NULL,
      colour = gsub("_", " ", colourBy, fixed = TRUE)
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.major.y = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold")
    )

  if (isTRUE(showCI)) {
    finiteCI <- plotData |>
      dplyr::filter(
        !is.na(.data$lower_CI), !is.na(.data$upper_CI),
        is.finite(.data$lower_CI), is.finite(.data$upper_CI),
        .data$lower_CI > 0
      )
    plot <- plot + ggplot2::geom_errorbar(
      data = finiteCI,
      ggplot2::aes(xmin = .data$lower_CI, xmax = .data$upper_CI),
      orientation = "y", width = 0.15, linewidth = 0.35, position = dodge
    )
  }
  if (!is.null(facetBy)) {
    plot <- plot + ggplot2::facet_grid(
      rows = ggplot2::vars(.data[[facetBy]]),
      scales = "free_y", space = "free_y"
    )
  }
  plot
}

sequenceRatioDatabaseComparisonData <- function(data, databaseA, databaseB) {
  keys <- c("pair_id", "analysis_id", "variable_name")
  comparisonColumns <- c(
    keys, "pair", "marker_label", "analysis_short", "point_estimate",
    "lower_CI", "upper_CI", "estimate", "observed_direction",
    "validated_status"
  )

  databaseData <- function(database, suffix) {
    selected <- data |>
      dplyr::filter(.data$cdm_name == database) |>
      dplyr::select(dplyr::all_of(comparisonColumns)) |>
      dplyr::distinct(dplyr::across(dplyr::all_of(keys)), .keep_all = TRUE)
    names(selected)[!names(selected) %in% keys] <- paste0(
      names(selected)[!names(selected) %in% keys], suffix
    )
    selected
  }

  databaseData(databaseA, "_a") |>
    dplyr::inner_join(databaseData(databaseB, "_b"), by = keys) |>
    dplyr::mutate(
      comparable_direction = .data$observed_direction_a %in%
        c("Above 1", "Below 1", "Includes 1") &
        .data$observed_direction_b %in%
          c("Above 1", "Below 1", "Includes 1"),
      direction_agrees = dplyr::if_else(
        .data$comparable_direction,
        .data$observed_direction_a == .data$observed_direction_b,
        NA
      ),
      agreement = dplyr::case_when(
        is.na(.data$direction_agrees) ~ "Not comparable",
        .data$direction_agrees ~ "Concordant",
        TRUE ~ "Discordant"
      ),
      fold_difference = dplyr::if_else(
        .data$point_estimate_a > 0 & .data$point_estimate_b > 0,
        pmax(
          .data$point_estimate_a / .data$point_estimate_b,
          .data$point_estimate_b / .data$point_estimate_a
        ),
        NA_real_
      )
    ) |>
    dplyr::select(-"comparable_direction") |>
    dplyr::arrange(
      factor(.data$agreement, levels = c("Discordant", "Not comparable", "Concordant")),
      dplyr::desc(.data$fold_difference), .data$pair_a, .data$analysis_short_a
    )
}

cohortSymmetryDatabaseComparisonPlot <- function(data, databaseA, databaseB) {
  plotData <- data |>
    dplyr::filter(
      !is.na(.data$point_estimate_a), !is.na(.data$point_estimate_b),
      is.finite(.data$point_estimate_a), is.finite(.data$point_estimate_b),
      .data$point_estimate_a > 0, .data$point_estimate_b > 0
    ) |>
    dplyr::mutate(
      tooltip = paste0(
        .data$pair_a, "<br>", .data$analysis_short_a,
        "<br>", databaseA, ": ", .data$estimate_a,
        "<br>", databaseB, ": ", .data$estimate_b,
        "<br>Direction agreement: ", .data$agreement,
        "<br>Fold difference: ", sprintf("%.2f×", .data$fold_difference)
      )
    )
  if (nrow(plotData) == 0) {
    return(emptyEvidencePlot("No matched positive estimates for these databases"))
  }

  limits <- range(c(plotData$point_estimate_a, plotData$point_estimate_b))
  limits <- c(limits[[1]] / 1.12, limits[[2]] * 1.12)

  ggplot2::ggplot(
    plotData,
    ggplot2::aes(
      x = .data$point_estimate_a,
      y = .data$point_estimate_b,
      colour = .data$agreement,
      shape = .data$marker_label_a,
      text = .data$tooltip
    )
  ) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = "#60747c") +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", colour = "#9aa8ae") +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", colour = "#9aa8ae") +
    ggplot2::geom_point(size = 2.7, alpha = 0.82) +
    ggplot2::scale_x_log10(limits = limits) +
    ggplot2::scale_y_log10(limits = limits) +
    ggplot2::scale_colour_manual(
      values = c(
        Concordant = "#17845c", Discordant = "#c43d4b",
        `Not comparable` = "#7b8790"
      )
    ) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      x = paste0(databaseA, " sequence ratio (log scale)"),
      y = paste0(databaseB, " sequence ratio (log scale)"),
      colour = "Direction", shape = "Marker"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")
}

sequenceRatioDatabaseComparisonTable <- function(data, databaseA, databaseB) {
  tableData <- data |>
    dplyr::transmute(
      Pair = .data$pair_a,
      Marker = .data$marker_label_a,
      Analysis = .data$analysis_short_a,
      Estimate_A = .data$estimate_a,
      Estimate_B = .data$estimate_b,
      Direction_A = .data$observed_direction_a,
      Direction_B = .data$observed_direction_b,
      Agreement = .data$agreement,
      `Fold difference` = round(.data$fold_difference, 2)
    ) |>
    dplyr::rename(
      !!databaseA := "Estimate_A",
      !!databaseB := "Estimate_B",
      !!paste0(databaseA, " direction") := "Direction_A",
      !!paste0(databaseB, " direction") := "Direction_B"
    )

  DT::datatable(
    tableData,
    rownames = FALSE,
    filter = "top",
    class = "stripe hover compact",
    options = list(
      pageLength = 25,
      lengthMenu = c(10, 25, 50, 100),
      scrollX = TRUE,
      deferRender = TRUE,
      order = list(list(7, "desc"), list(8, "desc")),
      columnDefs = list(list(className = "dt-nowrap", targets = 1:8))
    )
  ) |>
    DT::formatStyle(
      "Agreement",
      color = DT::styleEqual(
        c("Concordant", "Discordant", "Not comparable"),
        c("#176b46", "#a23b3f", "#667985")
      ),
      fontWeight = DT::styleEqual("Discordant", "600")
    )
}

temporalSymmetryEvidenceData <- function(result) {
  result |>
    omopgenerics::tidy() |>
    addProtocolPairMetadata() |>
    dplyr::filter(!is.na(.data$pair_id)) |>
    dplyr::mutate(
      day = suppressWarnings(as.integer(.data$variable_level)),
      count = suppressWarnings(as.numeric(.data$count)),
      pair = paste(
        protocolDisplayName(.data$index_name),
        protocolDisplayName(.data$marker_name),
        sep = " \u2192 "
      ),
      marker_label = dplyr::if_else(
        .data$marker_type == "proxy", "Drug proxy", "Condition"
      ),
      analysis_short = analysisShortLabel(
        .data$analysis_id, .data$changed_parameter,
        suppressWarnings(as.integer(.data$window_days)),
        suppressWarnings(as.integer(.data$blackout_days)),
        suppressWarnings(as.integer(.data$protocol_prior_observation))
      )
    ) |>
    dplyr::arrange(.data$pair, .data$analysis_id, .data$day)
}

cohortSymmetryTemporalProfilePlot <- function(data) {
  plotData <- data |>
    dplyr::filter(!is.na(.data$day), !is.na(.data$count), .data$day != 0) |>
    dplyr::mutate(
      is_primary = .data$analysis_id == "primary",
      tooltip = paste0(
        .data$pair, "<br>", .data$analysis_short,
        "<br>Day: ", .data$day, "<br>Count: ", format(.data$count, big.mark = ",")
      )
    )
  if (nrow(plotData) == 0) {
    return(emptyEvidencePlot("No temporal counts for the selected filters"))
  }

  ggplot2::ggplot(
    plotData,
    ggplot2::aes(
      x = .data$day,
      y = .data$count,
      colour = .data$analysis_short,
      group = .data$analysis_id,
      linewidth = .data$is_primary,
      alpha = .data$is_primary,
      text = .data$tooltip
    )
  ) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "#65777f") +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(ggplot2::vars(.data$pair), scales = "free_y") +
    ggplot2::scale_linewidth_manual(values = c(`TRUE` = 1.05, `FALSE` = 0.45), guide = "none") +
    ggplot2::scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.55), guide = "none") +
    ggplot2::labs(x = "Days from index initiation", y = "Individuals", colour = "Design") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold")
    )
}

temporalSymmetryEvidenceTable <- function(data) {
  tableData <- data |>
    dplyr::transmute(
      Database = .data$cdm_name,
      `Index → marker` = .data$pair,
      Marker = .data$marker_label,
      Analysis = .data$analysis_short,
      Day = .data$day,
      Count = .data$count
    )

  DT::datatable(
    tableData,
    rownames = FALSE,
    filter = "top",
    class = "stripe hover compact",
    options = list(
      pageLength = 25,
      lengthMenu = c(10, 25, 50, 100),
      scrollX = TRUE,
      deferRender = TRUE,
      order = list(list(1, "asc"), list(3, "asc"), list(4, "asc"))
    )
  ) |>
    DT::formatRound("Count", digits = 0, mark = ",")
}
