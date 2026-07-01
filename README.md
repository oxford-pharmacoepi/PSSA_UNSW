# PSSA UNSW + Oxf
<img src="https://img.shields.io/badge/Study%20Status-Started-blue.svg" alt="Study Status: Started">

- **Study title**: Prescription sequence symmetry analysis using CohortSymmetry
- **Study start date**:
- **Study leads**:
- **Study end date**:
- **Publications**:

---

This repository contains the study code for a prespecified prescription sequence
symmetry analysis (PSSA). The workflow:

1. reads prespecified drug/diagnosis pairs from `Study/inst/mock_codelists.csv`;
2. instantiates those concept sets as OMOP cohorts;
3. runs sequence ratio and adjusted sequence ratio analyses for the settings in
   `Study/inst/analysis_settings.csv`;
4. exports summarised results with `omopgenerics`; and
5. creates a static OmopViewer Shiny app from the exported results.

Partner sites should interact only with `Study/CodeToRun.R`, fill
in their connection details, replace the mock codelists/settings, and run the
file.
