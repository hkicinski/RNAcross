# test_data_2026: cross-species tissue atlas
Source: EMBL-EBI Expression Atlas **E-MTAB-3716 / 3717 / 3718 / 3719**,
"RNA-seq of 6 tissues to investigate the evolution of gene expression".
Orthology from **Ensembl BioMart** (human-anchored homologs).

```
Rscript test_data_2026/build_test_data.R   # rebuilds everything in raw/
```

## What is in it

| Code | Species | Genes | Samples |
|---|---|---|---|
| `hs` | Homo sapiens | 31,557 | 21 |
| `mml` | Macaca mulatta | 20,462 | 14 |
| `mm` | Mus musculus | 27,240 | 20 |
| `md` | Monodelphis domestica | 18,770 | 15 |

Axis: `Tissue` = brain, cerebellum, heart, kidney, liver, testis.
Orthogroups: 17,335, of which 10,762 span all four species and 425 are 1:many
into mouse.

## Testing characteristic

- **Nominal axis.** Tissues have no order and no spacing, so interpolation and
  trajectory arrows must stay off. The stock app assumed an ordered time axis.
- **The axis column is called `Tissue`.** Nothing is named `Timepoint`.
- **Orthology requirement.** The four species share zero gene IDs
  (`ENSG…`, `ENSMMUG…`, `ENSMUSG…`, `ENSMODG…`)
- **~160 My of divergence**, including 1:many orthologs from vertebrate
  duplications.
- **Level vocabularies genuinely differed at source.** Human was annotated
  `frontal lobe` / `prefrontal cortex` / `temporal lobe` where the others said
  `brain`; the builder collapses those to the shared vocabulary.

## Upload mapping

- `<code>_expr.csv` -> Step 2, expression matrices
- `<code>_samples.csv` -> Step 3, sample metadata (`Sample`, `Tissue`, `Replicate`)
- `<code>_anno.csv` -> Step 4, annotations
- `orthogroups.tsv` -> Step 5, custom orthology
- Step 6: axis `Tissue`, type **nominal**, reference `brain`

No gene trees for this set, so the Gene Explorer tree panel stays empty.

## Provenance note

Atlas ships raw counts for these experiments, so the builder computes log2 CPM
(`log2(CPM + 1)`, genes with total count < 10 dropped). That is a convenience
normalization for testing, not the authors' published pipeline.
