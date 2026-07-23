# Study runner

Run `CodeToRun.R` after filling in the database connection.

The protocol inputs are:

- `Cohorts/*.csv`: one csv for each drug or disease cohort;
- `inst/analysis_pairs.csv`: diagnosis/proxy pairs, controls, tiers, and
  expected directions; and
- `inst/analysis_settings.csv`: the design and inputs for main and
  sensitivity analyses.

The runner creates a timestamped result folder, exports concept-set expressions,
PhenotypeR diagnostics, sequence ratios, temporal symmetry, a combined
suppressed result file, and a complete Shiny report.

The primary design is:

- symmetric 365-day sequence window;
- 7-day initiation blackout;
- 365 days of prior observation;
- 365-day incident-event washout; and
- monthly temporal-symmetry bins.

The current CohortSymmetry API applies `washoutWindow` to both the index and
marker. This implementation therefore uses a symmetric 365-day washout while
retaining the marker-specific incident-event requirement intended by the
protocol.
