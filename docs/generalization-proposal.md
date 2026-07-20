# From Time-Course Viewer to General Comparative Transcriptomics Platform

**A design-generalization proposal for RNAcross**

Status: draft for discussion · Scope: architecture & phasing (no code changes yet)

---

## 0. Executive summary

RNAcross today assumes one experimental design: a cross-species **time course**. That
assumption is not spread thin across the app — it is concentrated in a single hardcoded
level vector (`TIME_POINTS`) and a handful of semantic shortcuts built on top of it. The
app already treats time as an *ordered categorical factor* on a categorical axis, which
means the generalization is more conceptual than mechanical.

This proposal makes RNAcross **agnostic to experimental design** by three moves:

1. **Represent design as data, not code.** Replace the fixed `(Sample, Timepoint, Replicate)`
   schema with a `colData`-style sample table (arbitrary typed covariates) plus a declared
   **Design Spec** that assigns columns to *roles*. The current time course becomes one
   configuration of the general model, so nothing breaks.

2. **Type the condition axis by measurement scale.** A "condition" secretly plays three
   independent roles — **grouping, order, metric**. Formalized as Stevens scales
   (nominal / ordinal / interval), each visualization becomes a *function of the scale*:
   gated or substituted, never assumed.

3. **Adopt a handle-or-refuse-loudly contract.** The goal is not "support every design."
   It is to represent enough design structure to render a figure **correctly or refuse it
   with a specific reason.** The failure mode we are engineering against is not a crash — it
   is a figure that renders fine and is *wrong* (a marginal mean over a hidden interaction).

A load-bearing boundary makes all of this achievable: **RNAcross is a visualization and
exploration layer over precomputed normalized counts. It is not a modeling engine.** It must
group, facet, aggregate, and align correctly, and know when a display is invalid. It does not
need to fit every design.

---

## 1. The problem, reframed

### 1.1 What we are actually generalizing

The naive framing — "make the app accept more designs" — is a trap. Every design you admit
multiplies the space of what a plot *means*, and the dangerous outcome is silent: a
`species × genotype × treatment` factorial whose genotype axis has no slot in the data model
gets collapsed into a facet label, and the app quietly renders a marginal mean over a factor
that carries an interaction. Nobody sees the error.

So the real question is **not** "which designs do we support" but:

> Which *properties* of a design must the app represent, so that it can either handle the
> design correctly or refuse it loudly?

That is a tractable engineering target. This document enumerates those properties (§6), the
representation that captures them (§4), and the contract that enforces correct-or-refuse (§5).

### 1.2 The boundary that makes generality possible

RNAcross renders and explores **precomputed, normalized expression matrices**. It is not
DESeq2/limma. This boundary is deliberate and protective:

- We must represent enough structure to **group, facet, aggregate, align, and normalize-to-a-reference** correctly.
- We do **not** fit design formulas, estimate interaction contrasts, or model random effects.
- Analyses that require a model (formal `~ genotype * treatment` contrasts, continuous
  covariate regression, mixed models) are explicitly **out of v1** and, where useful, are a
  later phase or a clearly-labeled export to a modeling tool.

Keeping this line is what lets us claim broad design coverage without boiling the ocean.

---

## 2. The core insight: time plays three roles at once

The current code treats `Timepoint` as one thing, but time happens to satisfy three
*independent* properties. Different designs satisfy different subsets:

| Property     | Meaning                                                              | Designs that have it |
|--------------|---------------------------------------------------------------------|----------------------|
| **Grouping** | which samples belong together                                       | all designs          |
| **Order**    | the levels have a sequence → connecting them with a line means something | ordered designs  |
| **Metric**   | the levels have real numeric spacing → distance is meaningful (interpolation, proportional arrows, regression) | interval designs |

Time has all three, which is why every visualization could assume "ordered factor with
interpolation." The generalization is to **stop assuming all three** and let each dataset
declare which it has. This maps exactly onto the **Stevens measurement-scale taxonomy**:

| Scale             | Examples                                              | Order? | Metric? |
|-------------------|-------------------------------------------------------|:------:|:-------:|
| **Nominal**       | treatments, tissues, genotypes, stress conditions     | no     | no      |
| **Ordinal**       | developmental stages, severity grades, passage number | yes    | no      |
| **Interval/ratio**| time, dose, concentration, age                        | yes    | yes     |

RNAcross's current data is purely the **interval** case (`condition = Timepoint`,
`levels = TIME_POINTS`, with numeric minutes available). Build the general model and existing
behavior is one configuration of it. **Backward-compatibility is a design goal, not an
afterthought** — the Phase-0 refactor must reproduce current figures byte-for-byte.

---

## 3. What the code assumes today (grounding)

The good news first: the app is ~70% of the way here without knowing it. The x-axis is
already an **ordered categorical** axis (`categoryorder = "array", categoryarray = TIME_POINTS`,
`R/11_server.R:6841`), PCA orders points by factor **rank** rather than real minutes, and
`Contrast_Series` is already a hand-rolled *second factor*. The time assumption is
concentrated, not diffuse:

| Coupling | Kind | Representative sites | Generalizes to |
|---|---|---|---|
| `TIME_POINTS` global level vocabulary + order | **Structural** | `R/02_constants_themes.R:6`; ~15 `factor(…, levels = TIME_POINTS)` sites | `design.primary_axis.levels` (per-dataset) |
| Hardcoded reference `"0min"` / terminal `"8h"` | **Structural** | `R/11_server.R:3311, 4362-4364, 4735`; `R/06_data_process.R:349`; `R/08_visualization_heatmaps.R:21` | `design.primary_axis.reference` |
| Cross-species alignment (3 modes already present) | **Structural** | `standardize_timepoints()` `R/08_…:665`; `Reduce(intersect,…)` `R/11_server.R:5138, 5293` | condition-correspondence (§4.4) |
| Numeric interpolation (similarity shape-search) | **Structural** | `tp_to_minutes()` `R/06_data_process.R:779`; `approx()` `:877` | gated on `scale == interval` |
| PCA trajectory arrows / gradient | Semi-structural | `R/07_…core.R:938, 1459, 1494` | gated on `scale ∈ {ordinal, interval}` |
| `paste(Species, Timepoint, sep = "_")` + `strsplit("_")` | Fragility | `R/08_…heatmaps.R:582, 910` | safe composite key |
| Upload coerces timepoints into min/h grammar | **Limiting bug** | `R/11_server.R:6308-6337` | accept arbitrary labels; rebuild levels from data |
| Axis titles "Timepoint" / "Temporal…" / hover "Time:" | Cosmetic | `R/07_…core.R:480, 792, 1529, 1703` | `design.primary_axis.label` |
| `Contrast_Series` / `Condition` (partial 2nd factor) | **Precedent** | assigned `R/11_server.R:2407-2428`; swapped in for Species `:3320-3323` | first-class `secondary_factors` (§4.2) |

The last row matters: the 2026 "Contrast Modes" feature already carries a genotype contrast
(`WT 2026` vs `Mutant 2026`) and a `Condition` filter (`noPi`), and when present it *swaps the
contrast in for Species as the compared entity*. That is precisely the generalization —
implemented ad hoc for one dataset. We are making it first-class and data-driven.

---

## 4. Target architecture

### 4.1 Data contract: samples × typed covariates

Adopt the shape the entire field converged on — Bioconductor `SummarizedExperiment`, Python
`AnnData`, EBI MAGE-TAB/SDRF, DESeq2 `colData` + design: **a numeric assay matrix + a
free-form sample-metadata table + a declaration of which columns are experimental factors and
of what type.** Concretely:

- **assay**: features × samples numeric matrix (as today: `lcpm` / `rlog`).
- **colData**: sample table keyed by `sample_id`, with **arbitrary typed covariate columns**
  (replaces the fixed `Timepoint, Replicate`).
- **rowData + unit map**: feature table plus the cross-unit correspondence (orthology today;
  see §4.5).

**Decouple genome from sample** (schema-level, and the item most likely to break structurally
if deferred). The current `gene_lookup` schema (`gene_id, species, …`) assumes each sample
maps to exactly one genome. That assumption fails for the canonical *cis/trans* comparative
designs:

- **Interspecies hybrids with allele-specific expression** — one library, two subgenomes,
  reads assigned to parental alleles.
- **Allele swaps / humanized / chimeric strains** — a gene from species A in a species-B
  background; the sample-level species label is wrong for a subset of genes.

The fix is to make the join key **`(sample_id, genome_id)`**, not `sample_id → species`. This
is cheap to introduce now and structural surgery later. Even if hybrid *analysis views* come
later, the *schema* must be able to say "this sample contains two genomes," and the app must
**refuse** multi-genome samples until those views exist rather than silently mislabel them.

### 4.2 The Design Spec (the "tenant")

A dataset declares a Design Spec that maps colData columns to roles. This object is also the
**reproducibility artifact** we serialize (YAML), so it earns its keep twice.

```yaml
dataset:
  id: yeast_pho_timecourse_2026

  units:                       # the entities being compared (was hardwired = species)
    align: orthology           # orthology | identity(same genome) | none
    members: [sc, cg, kl, ca]

  assay:
    matrix: lcpm
    normalization:             # provenance — stored, displayed, and co-plot-guarded (§5)
      method: TMM_log2CPM      # TPM | VST | rlog | TMM_log2CPM | raw_counts | intensity
      space: log2

  samples:                     # colData
    key: sample_id
    genome_col: genome_id      # decoupled from unit; may list >1 genome (hybrids)

  design:
    primary_axis:              # 0 or 1 — the ordered/continuous "condition axis"
      column: timepoint
      scale: interval          # nominal | ordinal | interval
      levels: [0min, 15min, ..., 8h]     # order (ordinal + interval)
      values: {0min: 0, 15min: 15, ...}  # numeric metric (interval only; else null)
      reference: 0min          # baseline level, any scale (null ⇒ no baseline)
      per_unit_scaling:        # cross-species commensurability (see §4.4)
        mode: none             # none | generation_normalized | stage_mapped

    secondary_factors:         # 0..n — enables factorial display (facet / color)
      - {column: genotype, scale: nominal, reference: WT, aesthetic: facet}

    replicate: replicate       # averaging unit
    blocking:  [batch, run]    # covariates for annotation / coloring / (future) correctness
    pairing:   null            # subject/culture id linking samples across the axis

  features:
    paralog_policy: eigengene  # mean|median|eigengene|max_expr|sum|strict_1to1|explode
    coverage_filter: {present_in: 4, of: 4}   # k-of-N; gene loss ≠ missing (§5)

  condition_correspondence:    # categorical analog of orthology (§4.4)
    mode: by_label             # by_label(default) | declared
    map: null                  # user table when level labels differ across units
```

Every downstream operation — faceting, aggregation, z-scoring scope, baseline normalization,
heatmap column order, the similarity correlation, PCA centering — **reads the spec, not
`TIME_POINTS`**. The current dataset is exactly this spec with `scale: interval`,
`align: orthology`, no secondary factors.

### 4.3 Scale-aware visualization dispatch

Each visualization becomes a function of the primary-axis scale — **gated or substituted,
never assumed.** Correctness gates (from the refuse-loudly contract) are folded in.

| Feature (current) | Interval | Ordinal | Nominal |
|---|---|---|---|
| Line plot connecting points | x = numeric value; unequal spacing honored | step/line in declared level order, equal spacing | **substitute** bar / box / violin (no connecting line) |
| Heatmap column order | by numeric value | by declared level order | arbitrary or **clustered** |
| PCA trajectory arrows | proportional spacing | sequential, equal spacing | **no arrows** — points + per-condition ellipses |
| Similarity "shape" search | interpolate to common grid | align by matched level (no interp) | pattern-vector correlation across levels |
| Baseline normalization ("vs 0 min") | vs reference level | vs first/earliest stage | **vs control / WT** |
| Ridgeline / distributions | per condition | per condition | per condition |

Two payoffs worth naming:

- The baseline row is a gift: `"log2FC vs 0 min"` generalizes to `"log2FC vs a chosen
  reference level,"` and for nominal designs that is **vs control / vs WT** — the single most
  common contrast in all of transcriptomics. Generalizing hands us the most-requested workflow
  for free.
- Interpolation and proportional arrows must be **disabled**, not faked, for ordinal/nominal.
  Faking numeric spacing is exactly the silent-wrong-figure failure we are engineering against.

### 4.4 Cross-unit condition correspondence = categorical orthology

RNAcross already has the mental model: **orthology maps genes across species so they can be
compared.** General designs need the same thing for *conditions*. If species A has stages
`{egg, larva, pupa, adult}` and species B has `{embryo, juvenile, adult}`, that alignment is a
**scientific claim the user asserts** — just like an ortholog assignment. So expose it as a
user-declared **condition-correspondence table**, with **auto-match by identical label** as the
easy default (which covers most real cases).

This unifies the three alignment modes already in the code (`standardized` rank, `intersection`,
`raw` union) under one concept and generalizes the similarity search almost verbatim: replace
"interpolate to a common time grid" with "align by matched condition level," and the
z-score + Pearson + permutation test all still hold.

**Cross-species time commensurability** is a first-class case of this. *Kluyveromyces lactis*
and *Candida albicans* do not share a doubling time; plotting "30 min" for both on one x-axis
silently asserts that 30 min is the same biological interval in each — a modeling claim the app
currently makes on the user's behalf. The primary axis therefore carries an optional
**per-unit scaling** (`absolute` / `generation_normalized` / `stage_mapped`), and **the chosen
mode must be visible on the figure, never hidden.**

### 4.5 Decoupling "comparison" from "cross-species"

The `units.align` role frees the app from requiring the four-species ortholog scaffold:

- `align: orthology` — cross-species; genes align via the HOG map (today's behavior).
- `align: identity` — **within one genome** (strains, tissues, conditions, patient groups);
  genes align 1:1 by ID, **no ortholog map needed**. This is a huge class of public
  comparative data the app currently cannot ingest at all.
- `align: none` — a single unit; ordinary within-dataset exploration.

---

## 5. The handle-or-refuse-loudly contract

Support is not the only good outcome. **A refusal with a reason is a feature; a marginal mean
over a hidden interaction is a retraction.** The app must detect and block, with a specific
message, at minimum:

- **Marginalization over a real factor** — two+ primary grouping factors when the requested
  view would average over one. Offer facet/subset instead.
- **Mixed normalization types** co-plotted across units (TPM vs VST vs raw). Guarded by stored
  normalization provenance (§4.2).
- **Multi-genome samples**, until the schema-aware views exist (§4.1).
- **Cross-species shared x-axis with commensurability unset** when unit doubling times differ
  (§4.4).
- **OG-level aggregation without a declared paralog policy** (§4.2).

And it must **annotate**, not hide, structure it can render but that changes interpretation:

- **Balance** — a line plot where one species is n=1 and another n=4 looks identical and is
  not. Surface replicate counts per point.
- **Coverage / missingness** — an orthogroup absent in a species is **gene loss, which is
  biology, not missing data. Never impute it.** Provide explicit *present-in-k-of-N* filters,
  and every PCA/clustering view must state which OG subset it ran on. (At 4 species you can
  require 4/4; at 15 you cannot — this dominates the analysis as unit count grows.)
- **Paralog disagreement** — when a policy collapses paralogs, surface a disagreement statistic
  so the user knows when the collapse hid divergence.

---

## 6. The orthogonal property axes (completeness argument)

Designs are *combinations* of a small set of orthogonal properties; enumerating designs is
hopeless, enumerating properties is not. The model above has a representation slot for each,
with an explicit v1 disposition (**support** / **annotate** / **refuse** / **defer**):

| # | Property | Representation slot | v1 disposition |
|---|---|---|---|
| 1 | Number & type of factors (incl. crossed factorial) | `primary_axis` + `secondary_factors` | support ≤2 factors (facet); **refuse** invalid marginalization; **defer** formula contrasts |
| 2 | Axis scale (nominal/ordinal/interval) + per-unit time scaling | `primary_axis.scale`, `.per_unit_scaling` | **support** all three scales; scaling visible |
| 3 | Sample ↔ genome (hybrids, allele-specific, chimeric) | `samples.genome_col`, `(sample_id, genome_id)` | **schema now**; **refuse** multi-genome views until built |
| 4 | Ortholog cardinality / paralog aggregation | `features.paralog_policy` (propagated) | **support** as declared param + disagreement stat |
| 5 | Coverage / missingness (gene loss ≠ missing) | `features.coverage_filter` | **support** k-of-N filter; **never impute**; annotate |
| 6 | Blocking / pairing / batch | `design.blocking`, `.pairing` | **represent + annotate**; correctness use later |
| 7 | Balance (unequal n, n=1 arms) | derived from colData | **annotate** on every view |
| 8 | Unit count & phylogenetic structure | `units.members` | fine ≤~8; **warn** past the point ignoring it is indefensible |
| 9 | Normalization provenance | `assay.normalization` | **store + display**; **refuse** mismatched co-plot |
| 10 | Unit of observation (bulk / pseudobulk / tissue panel) | dataset-level; `primary_axis` optional (0 or 1) | **support** bulk & tissue panels (no-axis designs); single-cell via pseudobulk later |

Property 10 has a concrete consequence: the primary axis is **optional (0 or 1)**. A
Brawand-style `species × tissue` atlas has no time and no condition axis at all — `primary_axis`
is null, `tissue` is the grouping. If the data model *requires* a timepoint column, that entire
literature cannot use the tool.

---

## 7. Coverage: archetypes as property combinations

If every archetype is expressible as a role/property assignment, the space is covered. Each row
is just a different spec:

| Archetype | units.align | primary_axis (scale) | secondary | reference | v1 |
|---|---|---|---|---|:--:|
| Time course (today) | orthology | time (interval) | — | 0min | ✅ |
| Dose / concentration | identity | dose (interval) | — | 0 | ✅ |
| Developmental / stage series | orthology | stage (ordinal) | — | earliest | ✅ |
| 2-group contrast (treat vs ctrl) | identity | treatment (nominal) | — | control | ✅ |
| Tissue atlas (Brawand-style) | orthology | — (none) | — | — | ✅ |
| Genotype panel | identity | genotype (nominal) | — | WT | ✅ |
| **Genotype × time (factorial)** | orthology | time (interval) | genotype (facet) | WT, 0min | ✅ display + refuse-marginalize |
| Paired pre/post treatment | identity | treatment (nominal) | — (pairing set) | pre | ✅ display; paired stats defer |
| Interspecies hybrid (cis/trans) | orthology | condition (any) | allele | — | schema ✅ / analysis defer |
| Cross-study meta-analysis | (mixed) | any | any | any | **defer** (batch) |

---

## 8. Phased plan

Reconciling the phasings from all inputs, and honoring the rule that **schema-level and
correctness-level items cannot be deferred** (representing a factorial or a hybrid, and refusing
to mismarginalize, is near-term; *analyzing* them with formal models is later):

**Phase 0 — Introduce the abstraction; change nothing.**
Add the Design Spec + `colData` sample table; build one spec from today's `TIME_POINTS`
(`scale: interval`, `align: orthology`); route the existing time course through it. Add
accessors (`axis_levels`, `axis_reference`, `axis_label`, `axis_scale`). Neutralize the label
surface area ("Timepoint" → axis label; "vs 0 min" → "vs reference"). **Acceptance: current
figures reproduce byte-for-byte.** *(Pure refactor; proves backward-compat.)*

**Phase 1 — Schema-level correctness (cheap now, expensive later).**
Decouple genome from sample → `(sample_id, genome_id)`; **refuse** multi-genome samples with a
clear message. Store + display **normalization provenance**; guard mismatched co-plots. Make
**paralog policy** a declared parameter that propagates into every view and export, with a
disagreement statistic. *(These are the items that are structural surgery if postponed.)*

**Phase 2 — Scale-aware dispatch + reference-level normalization.**
Implement nominal/ordinal handling in the line/bar and heatmap paths (substitute geometry,
order by declared levels); ship `"vs reference level"` normalization (unlocks treatment /
tissue / genotype designs **and** the vs-control/WT contrast). Wire the first **refuse-loudly**
guards (marginalization, mixed normalization). *(This single phase covers most non-time
designs.)*

**Phase 3 — Cross-unit correspondence + generalized similarity/PCA.**
Add the condition-correspondence table (auto-match by label; user-declared when labels differ);
generalize the similarity search and PCA to level-matching; gate interpolation/arrows on scale;
add per-unit time-commensurability scaling with visible labeling.

**Phase 4 — Factorial display + coverage/balance.**
Secondary-factor faceting (genotype × time), with **refusal** when the requested view would
marginalize invalidly. Coverage `k-of-N` filters and balance annotations (replicate counts,
n=1 flags) across all views.

**Phase 5+ — Horizon (demand-driven, explicitly out of v1).**
Design-formula contrasts (`~ genotype * treatment`, limma/DESeq2 territory); continuous-covariate
regression (`expression ~ age`); phylogeny-aware handling past ~8–12 units; cross-dataset
integration with batch correction; direct public-repo import (GEO / Expression Atlas / recount3);
single-cell via pseudobulk.

---

## 9. Scope boundaries (honest edges)

- **Not a modeling engine.** Formal interaction contrasts and continuous regression are
  deferred or exported, not built into v1.
- **Within-dataset, not cross-dataset.** Comparing across independent studies needs batch
  correction and ID harmonization — deferred. Within one dataset (including built-in
  cross-species) is v1.
- **Representable ≠ comparable.** Normalization provenance and time commensurability are made
  *explicit and visible*, never silently reconciled.
- **Bulk (and tissue panels / pseudobulk), not native single-cell.**
- **The upload wizard is the linchpin.** Universality is won or lost on the "Describe your
  design" step (§10). Do it well and the app ingests anything; do it rigidly and we have only
  moved the limitation.

---

## 10. The "Describe your design" ingest wizard

The one genuinely new UX surface — a miniature MAGE-TAB/SDRF capture, with inference and
validation:

1. Upload assay matrix + sample table (colData) + optional annotations/orthology.
2. **Pick the condition variable(s)** from the covariate columns (or "none" for atlases).
3. **Declare the scale** (nominal / ordinal / interval); for ordinal, drag to order; for
   interval, confirm numeric values.
4. **Pick the reference level** (or "none").
5. **Map replicate / blocking / pairing** columns.
6. **Declare genome(s)** per sample (defaults to unit; flag multi-genome).
7. **Declare paralog policy** and **normalization provenance**.
8. **Cross-unit correspondence** (auto-matched by label; editable when labels differ).
9. **Validate** → infer sensibly, warn on imbalance/coverage, and **refuse with reasons** where
   the contract requires it.

The wizard's output *is* the Design Spec YAML (§4.2) — the same artifact serialized for
reproducibility.

---

## 11. Open decisions / richest threads to dig into next

1. **Condition-spec schema + wizard capture** (§4.2, §10) — anchors everything else.
2. **Cross-unit condition correspondence** (§4.4) — does auto-match-by-label cover most real
   cases, and what's the minimal declared-map UI for when it doesn't?
3. **Visualization dispatch** (§4.3) — the graceful-degradation design so plots substitute
   rather than error on non-time data.
4. **Scope calls** — bulk-only vs single-cell; within-dataset vs cross-dataset; upload vs
   repo-import (each shifts phase boundaries).

Recommended entry point: **(1)**, since the spec is the contract every other thread reads.
