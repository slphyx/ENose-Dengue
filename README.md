# Dengue e-nose — analysis code

Wolfram Language code accompanying the revision of a single-centre proof-of-concept study of breath-derived electronic-nose responses in **60 NS1-positive dengue patients** and **58 healthy controls** (118 participants). The prototype uses **four metal-oxide semiconductor sensors** (TGS2600, TGS2602, TGS2610 and TGS2620).

The repository documents sensor-feature reconstruction, leakage-aware classification, clinical summaries, and sensitivity analyses. It does **not** establish dengue-specific diagnostic performance: participant characteristics and collection periods differ between groups, and no independent cohort or non-dengue febrile control group was evaluated.

## Wolfram code files to retain

| File | Purpose | Required inputs / dependencies |
| --- | --- | --- |
| `Dengue_Enose_Feature_Audit.wl` | Reconstruct the original signal transformations and selected feature positions (11 and 9); compare the algebraic expressions with the original notebook. Includes a synthetic-data check. | Can run synthetic check independently; original evaluated raw-data and feature variables are needed for the participant-level audit. |
| `Dengue_Enose_WithinFold_FeatureSelection.wl` | **Primary corrected machine-learning analysis**: select two of 18 candidate features separately within each training fold; evaluate logistic regression and SVM using four repetitions of stratified 5-fold CV. | Evaluate the original notebook through `normal2nd` and `dengue2nd` first. |
| `Dengue_Enose_Additional_Sensitivity.wl` | Additional CV strategies excluding explicit additive-baseline terms and sensor-boundary features; date-only negative-control analysis when filename dates are available. | Run **after** `Dengue_Enose_WithinFold_FeatureSelection.wl` in the **same kernel**. |
| `Dengue_Enose_Revision_Analysis.wl` | Match the selected recordings to the clinical workbook; produce Table 1 and descriptive/confounding analyses. Contains optional feature-based analyses when the original notebook variables are available. | Local original notebook **and** confidential clinical Excel workbook. |
| `Dengue_Enose_Sensor_Temporal_Audit_CORRECTED.wl` | Examine sensor reference readings, response amplitudes, normalized response profiles and associations with collection date; produce temporal supplementary plots. | Evaluate original notebook variables for raw recordings, filenames, and difference profiles first. Use this **CORRECTED** script rather than earlier versions. |
| `Dengue_Enose_Revised_Figures.wl` | Produce revised held-out ROC and confusion-matrix figures from the primary CV outputs. | Run the within-fold script first; its local `FeatureSelection_Results/PRIVATE_CV_OutOfFold_Predictions.csv` file must exist. |

**Retain these six `.wl` files.** Earlier `Dengue_Enose_Sensor_Temporal_Audit.wl` and `_FIXED.wl`, and the duplicate `Dengue_Enose_Revised_Figures_download.wl`, are superseded/duplicate versions and need not be published. The original notebook is an indispensable local analysis input but should **not** automatically be published.

## Recommended repository layout

```text
Dengue-Enose/
├── README.md
├── code/
│   ├── Dengue_Enose_Feature_Audit.wl
│   ├── Dengue_Enose_WithinFold_FeatureSelection.wl
│   ├── Dengue_Enose_Additional_Sensitivity.wl
│   ├── Dengue_Enose_Revision_Analysis.wl
│   ├── Dengue_Enose_Sensor_Temporal_Audit_CORRECTED.wl
│   └── Dengue_Enose_Revised_Figures.wl
└── .gitignore
```

**Local-only inputs:** `anal27Mar26.nb` (original notebook with embedded recordings/filenames) and `sum breath data-22 December 2025.xlsx` (clinical workbook). Keep these outside the public repository or within an ignored local folder. Because some scripts look for these files beside the active notebook, the simplest way to run them is to copy the six `.wl` files into a separate **private working directory** alongside the original `.nb` and `.xlsx`, without adding those inputs to Git.

## Software

Developed for **Wolfram Language / Mathematica 14.3**. The classifiers are implemented with Wolfram `Classify`; the scripts do not claim an independently tuned hyperparameter search. Results may be sensitive to software versions or defaults. Capture the Mathematica version and any relevant method options when reproducing results.

## How to run

1. In a **private** working directory, open `anal27Mar26.nb` and evaluate the original import/processing cells defining the needed variables. For the main analysis these include `normal2nd` and `dengue2nd`, with dimensions `{58,19}` and `{60,19}`. Where available, also evaluate `NormalfileList`, `DenguefileList`, `NormalrawData`, `DenguerawData`, `normalPoints` and `denguePoints`. Do not reorder participants or clear the kernel between dependent steps.
2. Run the feature audit (optional but recommended):

   ```wolfram
   Get[FileNameJoin[{NotebookDirectory[], "Dengue_Enose_Feature_Audit.wl"}]];
   ```

3. Run the **primary** feature-selection and cross-validation analysis:

   ```wolfram
   Get[FileNameJoin[{NotebookDirectory[], "Dengue_Enose_WithinFold_FeatureSelection.wl"}]];
   ```

   The main output is `FeatureSelection_Results/CV_Performance_Summary.csv`. The predesignated primary strategy is `All18_2`, which uses training-fold selection of two among 18 candidates. Four stratified 5-fold repetitions yield **472 held-out predictions per model/strategy**, but there are only **118 unique participants**. Means and SDs summarize **four repetition-level** metrics, not 472 independent observations.

4. In the **same session**, run the additional sensitivity analyses:

   ```wolfram
   Get[FileNameJoin[{NotebookDirectory[], "Dengue_Enose_Additional_Sensitivity.wl"}]];
   ```

   The date-only negative control is run only if filename dates and participant IDs can be validated against the sensor arrays. Collection date is **not** a clinical predictor for dengue diagnosis; this is a confounding check.

5. Produce revised figures from local, participant-level held-out predictions:

   ```wolfram
   Get[FileNameJoin[{NotebookDirectory[], "Dengue_Enose_Revised_Figures.wl"}]];
   ```

6. Run clinical matching/participant summaries with both local inputs present:

   ```wolfram
   Get[FileNameJoin[{NotebookDirectory[], "Dengue_Enose_Revision_Analysis.wl"}]];
   ```

   Outputs are written to `Revision_Results/`. Check the matching audit locally before using reported characteristics.

7. Run the corrected temporal sensor audit after evaluating all necessary raw recording variables:

   ```wolfram
   Get[FileNameJoin[{NotebookDirectory[], "Dengue_Enose_Sensor_Temporal_Audit_CORRECTED.wl"}]];
   ```

   Outputs are written to `Sensor_Temporal_Supplement/`. These comparisons describe *between-participant and between-period* differences; they cannot independently demonstrate or correct physical sensor drift without calibration/reference measurements.

**Note on paths:** Scripts generally use `NotebookDirectory[]` or `Directory[]`. Run from a notebook located in the private working directory (or adjust their directory configuration). `Dengue_Enose_Revised_Figures.wl` requires `PRIVATE_CV_OutOfFold_Predictions.csv` generated by the within-fold script.

## Link to Supplementary Methods

| Supplementary methods topic | Primary code |
| --- | --- |
| Signal preprocessing and original features | `Dengue_Enose_Feature_Audit.wl` and original `anal27Mar26.nb` |
| Within-fold candidate selection, training-fold standardization and classification | `Dengue_Enose_WithinFold_FeatureSelection.wl` |
| Additive-baseline, sensor-boundary and date-only sensitivity checks | `Dengue_Enose_Additional_Sensitivity.wl` |
| Participant matching, demographics and covariates | `Dengue_Enose_Revision_Analysis.wl` |
| Raw sensor/date association and supplementary time-series figures | `Dengue_Enose_Sensor_Temporal_Audit_CORRECTED.wl` |
| ROC and confusion-matrix figure generation | `Dengue_Enose_Revised_Figures.wl` |

The original exploratory features correspond to **candidate positions 11 and 9**. The revision addresses outcome-informed feature selection by ranking candidate features **inside training folds**, but it does not remove cohort or collection-date confounding. The original fixed-feature comparison should be labelled exploratory rather than a leakage-free estimate.

## Privacy and sharing

**Do not commit** identifiable raw recordings, recording filenames containing participant identifiers/dates, the original notebook if it embeds those data, the clinical workbook, or files beginning with `PRIVATE_` without ethics and institutional data-sharing approval. Inspect all other generated CSVs and graphics before publication; some contain dates, participant-level measurements or small-group information. The scripts themselves should also undergo an institutional disclosure review before the repository becomes public.

Suggested `.gitignore` contents:

```gitignore
# Confidential local source files
anal27Mar26.nb
sum breath data-22 December 2025.xlsx
*.odr

# Private predictions, matched records, and generated outputs
PRIVATE_*
**/PRIVATE_*
FeatureSelection_Results/
Revision_Results/
Sensor_Temporal_Supplement/

# Mathematica local/autosave files
*.nb~
*.wl~
.DS_Store
```

To publish selected aggregate figures or tables, review and copy explicitly approved files into a dedicated public `results/` or `figures/` directory rather than making all generated outputs public by default.

## Data availability and limitations

The full analyses require confidential participant data and the original recording-derived notebook and therefore **cannot be rerun from public source code alone**. Subject to ethics/institutional permission, an anonymized or synthetic input dataset could be made available separately to provide a fully executable public demonstration. Do not state that the GitHub repository alone reproduces the manuscript results unless suitable input data are also released.

## Citation

If you use these scripts, cite the associated dengue e-nose manuscript after publication. Add the final paper citation and DOI here when available.
