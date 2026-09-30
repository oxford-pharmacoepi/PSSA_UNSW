---
header: "GNN-PSSA safety signal study"
---

This report implements the PSSA component of the prespecified GNN-PSSA safety
protocol. It reports diagnosis-based outcomes and drug proxies separately,
includes known positive and negative controls, and retains emerging signals as
exploratory results.

The primary design uses a 365-day symmetric sequence window, a 7-day blackout
around initiation, 365 days of prior observation, and a 365-day incident-event
washout. Eight one-at-a-time sensitivity analyses change the window to 30, 60,
90, or 180 days; the blackout to 0, 1, or 14 days; or prior observation to 180
days while retaining every other primary setting.

Use **Sequence ratios** for crude and adjusted sequence-ratio estimates,
confidence intervals, the primary forest plot, and the one-at-a-time
sensitivity plot.
Use **Temporal symmetry** to inspect the distribution of marker timing before
and after the index medicine. PhenotypeR panels document cohort counts,
characteristics, overlap, code use, and other diagnostics generated at the
participating data source.

All results are for signal detection and prioritisation. Concordance does not
establish causality or clinical actionability. Small cells are suppressed using
the site-defined minimum cell count.
