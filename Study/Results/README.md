# Results

After execution, this folder will contain a subfolder `Results/<database>/<YYYYMMDD_HHMMSS>/` with:

- `summarised_result/`: importable result CSVs for the CDM snapshot,
  PhenotypeR diagnostics, sequence ratios, and temporal symmetry;
- `codelists/`: exported final concept-set expressions;
- a combined suppressed `pssa_safety_<database>.csv`; and
- the run log.

The generated Shiny report is written to `Study/shiny/`.
