# Repository Cleanup Notes

Cleanup performed from the available sources:

- Public GitHub repository: `moquedace/ro_soil_carbon_stock`
- Zenodo record: `10.5281/zenodo.10543942`
- Article PDF: `stock_ro.pdf`
- Script snapshots:
  - `scripts2.zip`
  - `scripts_b.zip`
  - `scripts.zip`

## Selection rationale

The article describes a final 30 m workflow using soil profile harmonization,
SCORPAN covariates, correlation filtering, RFE, model comparison, Random Forest
prediction, and uncertainty summaries. `scripts2.zip` most closely matches that
workflow and includes the cleanest publication-era script set.

`scripts_b.zip` was treated as historical context. It contains useful traces of
earlier work, but also many obsolete paths, alternative models, 200 m workflows,
future-scenario scripts, and experiments that would make the public repository
harder to understand if copied wholesale.

## Recommended next technical pass

1. Replace hard-coded paths with a single project-root variable.
2. Add missing local helper scripts or remove those dependencies.
3. Create a lightweight example dataset for smoke testing.
4. Add `renv.lock` once the package versions used in the final environment are
   known.
5. Confirm license metadata before release/archive.

See `docs/portability_notes.md` for the current non-portable assumptions.
