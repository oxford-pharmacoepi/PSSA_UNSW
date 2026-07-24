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
    return(dplyr::left_join(
      data, pairs,
      by = c("index_cohort_name", "marker_cohort_name")
    ))
  }
  if (all(c("index_name", "marker_name") %in% names(data))) {
    return(dplyr::left_join(
      data, pairs,
      by = c(
        "index_name" = "index_cohort_name",
        "marker_name" = "marker_cohort_name"
      )
    ))
  }
  data
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
  functionNames <- switch(
    resultType,
    temporal = c("tableTemporalSymmetry"),
    adjusted = c("tableAdjustedSequenceRatios", "tableAdjustedSequenceRatio", "tableSequenceRatios"),
    c("tableSequenceRatios", "tableSequenceRatio")
  )
  
  table <- cohortSymmetryCall(
    functionNames = functionNames,
    result = result,
    args = list(header = header, groupColumn = group, hide = hide, type = "gt")
  )
  
  if (!is.null(table)) {
    return(table)
  }
  
  simpleTable(result, header = header, group = group, hide = hide)
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
