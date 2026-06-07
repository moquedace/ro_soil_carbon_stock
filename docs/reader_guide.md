# Reader Guide

This guide is for readers who arrive here from the article and want to quickly
understand what the repository contains.

## I want the final maps

Use Zenodo, not GitHub:

- Final maps: https://doi.org/10.5281/zenodo.10543942
- Products: mean, Q05, Q95, and coefficient of variation for the standard
  depth intervals used in the article.

## I want to understand the method

Start with the article. Then use:

- `docs/article_traceability.md`: maps article sections and figures to scripts.
- `docs/workflow.md`: explains the computational workflow.
- `scripts/README.md`: explains the script folders.

## I want to inspect the code behind a figure

Use `docs/article_traceability.md`. It links the main article figures to their
supporting scripts.

## I want to rerun everything

The scripts are coherent with the article workflow, but the repository is not
yet a turnkey pipeline. The complete workflow depends on large local geospatial
inputs, intermediate model objects, and helper scripts that were not present in
the recovered archives.

Read first:

- `docs/portability_notes.md`
- `data/README.md`

## I want to cite this work

Cite the article as the canonical scientific reference:

Moquedace, C. M., Baldi, C. G. O., Siqueira, R. G., Cardoso, I. M., de Souza,
E. F. M., Fontes, R. L. F., Francelino, M. R., Gomes, L. C., &
Fernandes-Filho, E. I. (2024). High-resolution mapping of soil carbon stocks in
the western Amazon. Geoderma Regional, 36, e00773.
https://doi.org/10.1016/j.geodrs.2024.e00773

Also cite the Zenodo record when using the final maps:

https://doi.org/10.5281/zenodo.10543942
