
# Dengue E-nose: Breath Sensor Analysis

This repository contains Wolfram Language code supporting a **single-centre, proof-of-concept study** of dengue classification using breath-derived responses from a custom-built electronic nose (e-nose).

The study included **60 participants with a positive dengue NS1 antigen test** and **58 healthy controls** (118 participants in total). The device used four metal-oxide semiconductor (MOS) gas sensors: **Figaro TGS2600, TGS2602, TGS2610 and TGS2620**. Sensor responses were used as multivariate signals; the device did not identify or quantify individual volatile organic compounds in breath.

The analyses include reconstruction of the original signal transformations, within-training-fold feature selection, cross-validated classification, participant-characteristic summaries, and sensitivity analyses for feature definitions and differences in sample collection periods.

> **Study scope:** This is an exploratory case–control analysis, not a clinically validated dengue diagnostic test. Participants were not recruited as a representative diagnostic population; non-dengue febrile controls and independent external validation were not included. Differences in participant characteristics and collection periods may contribute to the observed discrimination.

## Repository contents

```text
Dengue-Enose/
├── README.md
├── .gitignore
└── code/
    ├── Dengue_Enose_Feature_Audit.wl
    ├── Dengue_Enose_WithinFold_FeatureSelection.wl
    ├── Dengue_Enose_Additional_Sensitivity.wl
    ├── Dengue_Enose_Revision_Analysis.wl
    ├── Dengue_Enose_Sensor_Temporal_Audit_CORRECTED.wl
    └── Dengue_Enose_Revised_Figures.wl
```

| Script | Description |
| --- | --- |
| `Dengue_Enose_Feature_Audit.wl` | Reconstructs the sensor-signal transformations and examines the original exploratory feature definitions. Includes a synthetic-data check. |
| `Dengue_Enose_WithinFold_FeatureSelection.wl` | Performs the primary analysis: training-fold selection of two features from 18 candidates and repeated stratified five-fold cross-validation for logistic regression and SVM classifiers. |
| `Dengue_Enose_Additional_Sensitivity.wl` | Assesses alternative feature sets and the influence of recording date, when the required date metadata are available. |
| `Dengue_Enose_Revision_Analysis.wl` | Matches sensor recordings with clinical records and summarizes participant characteristics and exploratory associations. |
| `Dengue_Enose_Sensor_Temporal_Audit_CORRECTED.wl` | Examines sensor reference values, response amplitudes and associations with collection date. |
| `Dengue_Enose_Revised_Figures.wl` | Creates receiver operating characteristic (ROC) and confusion-matrix figures from held-out model predictions. |

## Analytical approach

### Signal processing and feature extraction

For each of four sensor channels, five signal values were selected at recording indices 100, 200, 300, 400 and 500. The original analysis applied reference-based transformations and numerical scaling, concatenated the sensor-derived sequences and calculated differences to obtain **18 candidate features**. Exact transformations are documented in the accompanying Supplementary Methods and feature-audit code.

The original exploratory analysis inspected differences between study groups and selected candidate feature positions 11 and 9. Because those positions were chosen using the complete dataset, performance estimates based on their fixed selection may be optimistically biased.

### Classification and internal validation

The corrected primary analysis selects features **within each training fold**, preventing held-out observations from informing feature selection. Models are evaluated using **four repetitions of stratified five-fold cross-validation**. The primary feature-selection strategy, `All18_2`, retains the two highest-ranked features from the 18 candidates using a standardized between-group difference calculated from training data only.

Logistic regression and support vector machine (SVM) classifiers are evaluated using accuracy, sensitivity, specificity and area under the ROC curve (AUC). Reported means and standard deviations are based on the **four cross-validation repetitions**. Each participant contributes one held-out prediction per repetition; these repeated predictions are *not* independent participants.

### Sensitivity and temporal analyses

Additional scripts examine restrictions on feature definitions and associations between sensor responses and measurement dates. A date-only classification analysis investigates potential confounding due to different recruitment or collection periods. These checks do **not** establish physical sensor drift or eliminate cohort/date-related confounding; that would require additional measurements and prospective validation.

## Requirements

- Wolfram Mathematica / Wolfram Language (developed using **Mathematica 14.3**).
- The original sensor-processing notebook and its source data, available to authorised study investigators (analyse.nb).
- The study clinical workbook for participant-level matching and descriptive analyses.

Some scripts depend on symbols created by the original analysis notebook (analyse.nb) rather than importing a standalone, public dataset. Consequently, **the code in this repository is not fully executable against public inputs alone**.

## Running the analyses

The following example assumes the scripts have been copied into a **private working directory** alongside the approved local input files, and that the Wolfram notebook has been opened in that directory. Files containing identifiable or potentially identifiable information should remain outside the public repository.

1. Evaluate the relevant cells in the original processing notebook (analyse.nb) to create the sensor feature matrices `normal2nd` and `dengue2nd` (58 and 60 observations, respectively), and any other variables needed by the chosen analysis.

2. Run the primary classification analysis:

   ```wolfram
   Get[FileNameJoin[{NotebookDirectory[], "Dengue_Enose_WithinFold_FeatureSelection.wl"}]];
   ```

3. For sensitivity analyses, use the **same Wolfram kernel session**:

   ```wolfram
   Get[FileNameJoin[{NotebookDirectory[], "Dengue_Enose_Additional_Sensitivity.wl"}]];
   ```

4. To recreate ROC and confusion-matrix figures after the primary classification script has produced its local held-out prediction file:

   ```wolfram
   Get[FileNameJoin[{NotebookDirectory[], "Dengue_Enose_Revised_Figures.wl"}]];
   ```

5. The remaining scripts can be evaluated as needed once their required source variables and, where relevant, the clinical workbook have been loaded:

   ```wolfram
   Get[FileNameJoin[{NotebookDirectory[], "Dengue_Enose_Feature_Audit.wl"}]];
   Get[FileNameJoin[{NotebookDirectory[], "Dengue_Enose_Revision_Analysis.wl"}]];
   Get[FileNameJoin[{NotebookDirectory[], "Dengue_Enose_Sensor_Temporal_Audit_CORRECTED.wl"}]];
   ```

**Execution notes:** Some scripts use `NotebookDirectory[]` or the current working directory to locate inputs or write outputs. When running scripts from the `code/` directory, adjust paths to match the local private workspace. The revised figures script requires the private prediction CSV generated by the within-fold feature-selection script. Wolfram classifier defaults and numerical results may depend on the installed software version.

## Relationship to the Supplementary Methods

| Supplementary Methods topic | Relevant scripts |
| --- | --- |
| Signal processing and exploratory feature reconstruction | `Dengue_Enose_Feature_Audit.wl` |
| Within-fold feature selection and cross-validation | `Dengue_Enose_WithinFold_FeatureSelection.wl` |
| Feature-set and date-related sensitivity analyses | `Dengue_Enose_Additional_Sensitivity.wl` |
| Participant characteristics and clinical matching | `Dengue_Enose_Revision_Analysis.wl` |
| Temporal sensor-response assessments | `Dengue_Enose_Sensor_Temporal_Audit_CORRECTED.wl` |
| ROC and confusion-matrix visualization | `Dengue_Enose_Revised_Figures.wl` |

## Data availability and privacy

The source clinical records and participant-level sensor recordings are not included in this repository. The original processing notebook contains embedded recording-derived information and is also not distributed here. Access to study data is subject to applicable ethics approvals, institutional permissions and data-sharing arrangements.

Because the necessary participant-level inputs are not publicly available here, this repository provides **analytical transparency and code documentation**, not independent end-to-end reproduction of the manuscript results. Publicly shareable aggregate outputs or a synthetic demonstration dataset may be added separately if approved.

## Citation

Please cite the associated dengue e-nose study when using or adapting this code. The publication citation and DOI will be added when available.

## Contact

For questions concerning the methods, code or access to research data, please contact the corresponding author through the associated publication.
