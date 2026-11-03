(* ::Title:: *)
(* Dengue e-nose revision: participant matching and sensitivity analyses *)
(* Wolfram Language 14.3. Created 31 October 2026. *)
(* Clinical data are confidential. Run locally; review outputs before sharing. *)

(* ::Section:: *)
(* 0. Configuration and input checks *)

ClearAll[analysisDir, clinicalFile, originalNotebookFile, resultsDir];
analysisDir = If[StringQ[Quiet[Check[NotebookDirectory[], $Failed]]],
   NotebookDirectory[], Directory[]];
clinicalFile = FileNameJoin[{analysisDir, "sum breath data-22 December 2025.xlsx"}];
originalNotebookFile = FileNameJoin[{analysisDir, "anal27Mar26.nb"}];
resultsDir = FileNameJoin[{analysisDir, "Revision_Results"}];
If[!DirectoryQ[resultsDir], CreateDirectory[resultsDir]];
If[!FileExistsQ[clinicalFile] || !FileExistsQ[originalNotebookFile],
  Print["ERROR: Put the Excel and .nb files in analysisDir: ", analysisDir];
  Abort[]];

(* ::Section:: *)
(* 1. Recover selected recording IDs, in original notebook order *)

nbText = Import[originalNotebookFile, "Text"];
filenamePattern = RegularExpression[
  "(?:DE[0-9]{3}(?:-[0-9]+)?|N[0-9]{3})_[0-9]{2}-[0-9]{2}-[0-9]{2}-[0-9]{2}-[0-9]{2}\\.odr"];
allRecordingFiles = DeleteDuplicates[StringCases[nbText, filenamePattern]];
getID[file_String] := First[StringCases[file, RegularExpression["^(?:DE|N)[0-9]{3}"]]];
recordingIDs = getID /@ allRecordingFiles;
dengueFiles = Select[allRecordingFiles, StringStartsQ[#, "DE"] &];
healthyFiles = Select[allRecordingFiles, StringStartsQ[#, "N"] &];
selectedDengueIDs = getID /@ dengueFiles;
selectedHealthyIDs = getID /@ healthyFiles;
If[Length[dengueFiles] != 60 || Length[healthyFiles] != 58 ||
   !DuplicateFreeQ[recordingIDs],
  Print["ERROR: Unexpected recording IDs; verify the source notebook.",
    {Length[dengueFiles], Length[healthyFiles]}]; Abort[]];
Print["Selected recordings: ", Length[dengueFiles], " dengue + ",
  Length[healthyFiles], " healthy = ", Length[recordingIDs]];
Print["Important: DE035-2 recording is matched to participant DE035."];

(* Recording dates are taken from the .odr filenames, not from the mixed-format
   spreadsheet date cells (which include Thai Buddhist years/numeric values). *)
recordingDate[file_String] := Module[{a},
  a = StringCases[file, RegularExpression[
     "_([0-9]{2})-([0-9]{2})-([0-9]{2})-([0-9]{2})-([0-9]{2})\\.odr"] ->
       {"$1", "$2", "$3", "$4", "$5"}];
  If[a === {}, Return[Missing["InvalidDate"]]];
  With[{v = ToExpression /@ First[a]},
    DateObject[{2000 + v[[3]], v[[2]], v[[1]], v[[4]], v[[5]]}]]];

(* ::Section:: *)
(* 2. Read participant questionnaire sheets and match by canonical ID *)

healthySheet = Import[clinicalFile, {"Data", 1}];
dengueSheet = Import[clinicalFile, {"Data", 2}];
getSheetRows[data_, prefix_String] := Select[Drop[data, 2],
   Length[#] >= 2 && StringQ[#[[2]]] &&
   StringMatchQ[StringTrim[#[[2]]],
    If[prefix == "N", "N" ~~ DigitCharacter ~~ DigitCharacter ~~ DigitCharacter,
      "DE" ~~ DigitCharacter ~~ DigitCharacter ~~ DigitCharacter]] &];
healthySource = getSheetRows[healthySheet, "N"];
dengueSource = getSheetRows[dengueSheet, "DE"];
healthyLookup = Association[Map[StringTrim[#[[2]]] -> # &, healthySource]];
dengueLookup = Association[Map[StringTrim[#[[2]]] -> # &, dengueSource]];
If[Length[healthyLookup] != Length[healthySource] ||
   Length[dengueLookup] != Length[dengueSource],
  Print["ERROR: Duplicate IDs detected in questionnaire sheets."]; Abort[]];

missingHealthy = Complement[selectedHealthyIDs, Keys[healthyLookup]];
missingDengue = Complement[selectedDengueIDs, Keys[dengueLookup]];
If[missingHealthy =!= {} || missingDengue =!= {},
  Print["ERROR: selected IDs absent from spreadsheet: ",
    {missingHealthy, missingDengue}]; Abort[]];

yn[value_] := ToUpperCase[StringTrim[ToString[value]]] == "Y";
num[value_] := If[NumberQ[value], N[value], Missing["NotRecorded"]];

makeClinicalRow[id_String, file_String, group_String] := Module[
  {r, dengueQ, ageCol, sexCol, wtCol, htCol, smokeCol, alcCol,
   previousCol, chronicCol, medicineCol, date},
  dengueQ = group == "Dengue";
  r = If[dengueQ, dengueLookup[id], healthyLookup[id]];
  r = PadRight[r, 29, ""];
  {ageCol, sexCol, wtCol, htCol, smokeCol, alcCol, previousCol,
    chronicCol, medicineCol} = If[dengueQ,
     {7, 8, 9, 10, 11, 14, 23, 25, 27},
     {4, 5, 6, 7, 8, 11, 20, 22, 24}];
  date = recordingDate[file];
  <|"ID" -> id, "Group" -> group,
    "Dengue01" -> Boole[dengueQ],
    "Age" -> num[r[[ageCol]]],
    "Sex" -> ToUpperCase[StringTrim[ToString[r[[sexCol]]]]],
    "Male01" -> Boole[ToUpperCase[StringTrim[ToString[r[[sexCol]]]]] == "M"],
    "Weight" -> num[r[[wtCol]]], "Height" -> num[r[[htCol]]],
    "CurrentSmoking" -> yn[r[[smokeCol + 1]]],
    "FormerSmoking" -> yn[r[[smokeCol + 2]]],
    "AlcoholRegular" -> yn[r[[alcCol + 2]]],
    "PreviousDengue" -> yn[r[[previousCol]]],
    "ChronicDisease" -> yn[r[[chronicCol]]],
    "MedicationUse" -> yn[r[[medicineCol]]],
    "NS1" -> If[dengueQ, ToUpperCase[StringTrim[ToString[r[[4]]]]],
       "Not tested/recorded"],
    "IgG" -> If[dengueQ, ToUpperCase[StringTrim[ToString[r[[5]]]]],
       "Not tested/recorded"],
    "IgM" -> If[dengueQ, ToUpperCase[StringTrim[ToString[r[[6]]]]],
       "Not tested/recorded"],
    "RecordFile" -> file,
    "RecordingDate" -> date,
    "RecordingMonth" -> DateString[date, {"Year", "-", "Month"}],
    "RecordingDays" -> AbsoluteTime[date]/86400.
  |>
];

(* Keep original recording order within each group for joining to original
   Mathematica normalPoints/denguePoints in the optional section below. *)
healthyCohort = Map[makeClinicalRow[getID[#], #, "Healthy"] &, healthyFiles];
dengueCohort = Map[makeClinicalRow[getID[#], #, "Dengue"] &, dengueFiles];
cohort = Join[healthyCohort, dengueCohort];
If[Length[cohort] != 118 ||
   !AllTrue[Lookup[cohort, "Age"], NumericQ] ||
   !AllTrue[Lookup[cohort, "Sex"], MemberQ[{"M", "F"}, #] &],
  Print["ERROR: Cohort size or age/sex data are inconsistent."]; Abort[]];
If[!AllTrue[Lookup[dengueCohort, "NS1"], # == "Y" &],
  Print["WARNING: One or more selected cases are not NS1-positive."]];

Print["Matched 118/118 selected IDs to clinical questionnaire rows."];
Print["Workbook records NOT used in the sensor analysis (counts only): ",
  <|"Healthy" -> Length[Complement[Keys[healthyLookup], selectedHealthyIDs]],
    "Dengue" -> Length[Complement[Keys[dengueLookup], selectedDengueIDs]]|>];

(* ::Section:: *)
(* 3. Descriptive statistics, Welch test, and exact 2x2 Fisher p-values *)

countYes[data_, key_] := Count[Lookup[data, key], True];
fmtPct[n_, total_] := ToString[n] <> " (" <>
  ToString[NumberForm[100. n/total, {4, 1}]] <> "%)";
fmtMeanSD[v_] := ToString[NumberForm[Mean[v], {5, 2}]] <> " +/- " <>
  ToString[NumberForm[StandardDeviation[v], {5, 2}]];

welchP[x_List, y_List] := Module[{nx, ny, vx, vy, se2, t, df},
  nx = Length[x]; ny = Length[y];
  vx = Variance[x]; vy = Variance[y];
  se2 = vx/nx + vy/ny;
  t = (Mean[x] - Mean[y])/Sqrt[se2];
  df = se2^2/((vx/nx)^2/(nx - 1) + (vy/ny)^2/(ny - 1));
  N[2 CDF[StudentTDistribution[df], -Abs[t]]]
];

(* Two-sided Fisher p-value defined as the probability of all tables with
   conditional hypergeometric mass <= the observed table's mass. *)
fisherP[a_Integer, b_Integer, c_Integer, d_Integer] := Module[
  {n, cases, positive, dist, lo, hi, obs, probs},
  n = a + b + c + d; cases = a + b; positive = a + c;
  dist = HypergeometricDistribution[cases, positive, n];
  lo = Max[0, cases - (n - positive)]; hi = Min[cases, positive];
  obs = PDF[dist, a]; probs = PDF[dist, #] & /@ Range[lo, hi];
  N[Total[Select[probs, # <= obs + 10^-12 &]]]
];

ageH = Lookup[healthyCohort, "Age"]; ageD = Lookup[dengueCohort, "Age"];
sexH = Count[Lookup[healthyCohort, "Sex"], "M"];
sexD = Count[Lookup[dengueCohort, "Sex"], "M"];
ageP = welchP[ageD, ageH];
sexP = fisherP[sexD, 60 - sexD, sexH, 58 - sexH];
printP[p_] := If[p < 0.001, "<0.001", ToString[NumberForm[p, {4, 3}]]];

ageBins = {
  {"<20", Function[a, a < 20]},
  {"20-29", Function[a, 20 <= a < 30]},
  {"30-39", Function[a, 30 <= a < 40]},
  {"40-49", Function[a, 40 <= a < 50]},
  {">=50", Function[a, a >= 50]}
};
Table1 = Join[
  {{"Characteristic", "Healthy controls (n=58)",
      "NS1-positive dengue (n=60)", "p-value"},
   {"Age (years), mean +/- SD", fmtMeanSD[ageH], fmtMeanSD[ageD],
     printP[ageP]},
   {"Age range (years)", ToString[Min[ageH]] <> "-" <> ToString[Max[ageH]],
      ToString[Min[ageD]] <> "-" <> ToString[Max[ageD]], ""},
   {"Age groups, n (%)", "", "", ""}},
  Table[With[{fn = bin[[2]], name = bin[[1]]},
    {name, fmtPct[Count[ageH, _?(fn)], 58],
      fmtPct[Count[ageD, _?(fn)], 60], ""}], {bin, ageBins}],
  {{"Sex, n (%)", "", "", printP[sexP]},
   {"Male", fmtPct[sexH, 58], fmtPct[sexD, 60], ""},
   {"Female", fmtPct[58 - sexH, 58], fmtPct[60 - sexD, 60], ""}},
  (* Additional prespecified characteristics considered as potential
     confounders; these are questionnaire fields, not verified pre-sampling
     exposures or treatment dates. *)
  Table[With[{key = v[[2]], label = v[[1]]},
    Module[{h = countYes[healthyCohort, key],
      d = countYes[dengueCohort, key]},
      {label, fmtPct[h, 58], fmtPct[d, 60],
       printP[fisherP[d, 60 - d, h, 58 - h]]}]],
    {v, {{"Current smoking, n (%)", "CurrentSmoking"},
      {"Chronic disease recorded, n (%)", "ChronicDisease"},
      {"Medication use recorded, n (%)", "MedicationUse"}}}]
];
Export[FileNameJoin[{resultsDir, "Table1_revised.csv"}], Table1, "CSV"];
Print["Age Welch p = ", ageP, "; sex Fisher p = ", sexP];
Print[Grid[Table1, Frame -> All]];

(* Additional covariates: exploratory comparisons only. *)
extraVars = {
  {"Current smoking", "CurrentSmoking"},
  {"Former smoking", "FormerSmoking"},
  {"Regular alcohol intake", "AlcoholRegular"},
  {"Chronic disease recorded", "ChronicDisease"},
  {"Medication use recorded", "MedicationUse"},
  {"Previous dengue reported", "PreviousDengue"}
};
extraTable = Prepend[Table[
    With[{key = item[[2]], title = item[[1]]},
      Module[{h, d},
        h = countYes[healthyCohort, key]; d = countYes[dengueCohort, key];
        {title, fmtPct[h, 58], fmtPct[d, 60],
         fisherP[d, 60 - d, h, 58 - h]}]], {item, extraVars}],
   {"Characteristic", "Healthy n=58", "Dengue n=60", "Fisher p"}];
Export[FileNameJoin[{resultsDir, "Additional_characteristics.csv"}],
  extraTable, "CSV"];

(* ::Section:: *)
(* 4. Recording month and selection audit *)

monthCounts = Counts[Lookup[cohort, "RecordingMonth"]];
allMonths = Sort[Keys[monthCounts]];
monthTable = Prepend[Table[{m,
     Count[Lookup[healthyCohort, "RecordingMonth"], m],
     Count[Lookup[dengueCohort, "RecordingMonth"], m]}, {m, allMonths}],
   {"Measurement month", "Healthy", "Dengue"}];
Export[FileNameJoin[{resultsDir, "Recording_months.csv"}], monthTable, "CSV"];
Print["IMPORTANT: Group and recording month are strongly confounded."];
Print[Grid[monthTable, Frame -> All]];

(* Confidential ID-only selection audit: do not upload with public manuscript
   repository unless specifically permitted by ethics/data-sharing approval. *)
matchAudit = Prepend[
   Map[{#["ID"], #["Group"], #["RecordFile"], #["NS1"]} &, cohort],
   {"Participant ID", "Analysis group", "Recording filename", "NS1"}];
Export[FileNameJoin[{resultsDir, "PRIVATE_Matched_ID_Audit.csv"}],
  matchAudit, "CSV"];

(* ::Section:: *)
(* 5. OPTIONAL: sensor-feature association / cross-validation *)
(* Evaluate the ORIGINAL notebook's data-loading and feature-extraction
   cells FIRST, such that normal2nd, dengue2nd, normalPoints and
   denguePoints hold the original numerical lists in this kernel.
   If they are not available, the clinical analyses above still run. *)

sensorReady = And[ValueQ[normal2nd], ValueQ[dengue2nd],
  ValueQ[normalPoints], ValueQ[denguePoints]];
If[!sensorReady,
  Print["Sensor analyses skipped: evaluate the original notebook through the cells defining normal2nd, dengue2nd, normalPoints and denguePoints; then Get this .wl file again."],

  If[Length[normal2nd] != 58 || Length[dengue2nd] != 60 ||
      Length[normalPoints] != 58 || Length[denguePoints] != 60,
    Print["ERROR: Loaded sensor arrays do not have 58 healthy / 60 dengue samples."];
    Abort[]];

  (* Cross-check that the original selected features are: 12-11 and 10-9,
     from each 19-element, first-difference response vector. The latter
     straddles a sensor-concatenation boundary. *)
  derivativeRows = N[Join[normal2nd, dengue2nd]];
  originalPoints = N[Join[normalPoints, denguePoints]];
  reconstructedPoints = ({#[[12]] - #[[11]], #[[10]] - #[[9]]} & /@
     derivativeRows);
  maxFeatureDiscrepancy = Max[Abs[Flatten[originalPoints - reconstructedPoints]]];
  Print["Maximum discrepancy from original two-feature definitions: ",
    maxFeatureDiscrepancy];
  If[maxFeatureDiscrepancy > 10^-6,
    Print["ERROR: Feature definitions inconsistent; check notebook ordering."];
    Abort[]];

  featureRows = MapThread[Join[#1, <|
      "Feature1" -> #2[[1]], "Feature2" -> #2[[2]]|>] &,
    {cohort, originalPoints}];

  (* Within-group associations; descriptive, not a confounding cure. *)
  sexWelch[rows_, f_] := Module[{male, female},
     male = Lookup[Select[rows, #["Sex"] == "M" &], f];
     female = Lookup[Select[rows, #["Sex"] == "F" &], f];
     If[Min[Length[male], Length[female]] > 1,
       welchP[male, female], Missing["InsufficientData"]]];
  assocTable = Prepend[Flatten[Table[
      With[{sub = Select[featureRows, #["Group"] == grp &]},
        {grp, f,
          Correlation[Lookup[sub, "Age"], Lookup[sub, f]],
          sexWelch[sub, f],
          Correlation[Lookup[sub, "RecordingDays"], Lookup[sub, f]]}],
        {grp, {"Healthy", "Dengue"}},
        {f, {"Feature1", "Feature2"}}], 1],
      {"Group", "Feature", "Pearson r with age", "Sex Welch p",
       "Pearson r with recording date"}];
  Export[FileNameJoin[{resultsDir, "Feature_covariate_associations.csv"}],
     assocTable, "CSV"];

  (* Exploratory age/sex-adjusted associations between original selected
     features and study group. Linear regression is used here only as a
     covariate sensitivity check, not as validation of disease specificity. *)
  adjustedFeatureResults = Prepend[Flatten[Table[
      Module[{vars, design, lm, coefficients},
        vars = If[adjustment == "AgeSex",
          {"Age", "Male01", "Dengue01"},
          {"Age", "Male01", "ChronicDisease", "MedicationUse", "Dengue01"}];
        design = Table[Join[
           Table[With[{v = featureRows[[i]][key]},
             If[BooleanQ[v], Boole[v], v]], {key, vars}],
           {featureRows[[i]][feature]}], {i, Length[featureRows]}];
        lm = If[adjustment == "AgeSex",
          LinearModelFit[design, {1, a, s, d}, {a, s, d}],
          LinearModelFit[design, {1, a, s, c, m, d}, {a, s, c, m, d}]];
        coefficients = lm["ParameterTableEntries"];
        {feature, adjustment, coefficients[[-1, 1]], coefficients[[-1, 2]],
          coefficients[[-1, 4]]}],
      {feature, {"Feature1", "Feature2"}},
      {adjustment, {"AgeSex", "AgeSexChronicMedication"}}], 1],
    {"Feature", "Adjustment", "Dengue coefficient", "SE", "p value"}];
  Export[FileNameJoin[{resultsDir, "Feature_adjusted_regressions.csv"}],
     adjustedFeatureResults, "CSV"];

  (* Response-vector diagnostics: 4 sensors x 5 retained values yields
     19 first differences. Boundary first-difference indices are 5,10,15.
     The discrete second difference has 18 positions, and positions
     4,5,9,10,14,15 touch at least one boundary. *)
  secondDifferenceRows = N[Differences /@ derivativeRows];
  candidateColumns = Complement[Range[18], {4, 5, 9, 10, 14, 15}];
  Print["Original Feature2 uses second-difference position 9 (a sensor boundary)."];
  Print["Non-boundary candidate second-difference indices: ", candidateColumns];

  (* Fold creation: each patient is held out once per repetition, with
     the same folds used across model types. *)
  stratifiedFolds[labels_List, k_Integer, seed_Integer] := Module[
    {folds = Table[{}, {k}], classes, groupIndices, shuffled},
    SeedRandom[seed]; classes = DeleteDuplicates[labels];
    Do[
      groupIndices = Flatten[Position[labels, c]];
      shuffled = RandomSample[groupIndices];
      Do[folds[[Mod[i - 1, k] + 1]] =
         Append[folds[[Mod[i - 1, k] + 1]], shuffled[[i]]],
        {i, Length[shuffled]}], {c, classes}];
    Sort /@ folds
  ];

  safeAUC[truth_List, probs_List] := Module[{pos, neg},
    pos = Pick[probs, truth, 1]; neg = Pick[probs, truth, 0];
    If[Length[pos] == 0 || Length[neg] == 0, Return[Missing["AUC"]]];
    N[Total[Flatten[Outer[
        If[#1 > #2, 1., If[#1 == #2, 0.5, 0.]] &,
        pos, neg]]]/(Length[pos] Length[neg])]
  ];

  scoreColumn[mat_, classLabels_, training_, c_Integer] := Module[
    {a, b, pooled},
    a = mat[[Select[training, classLabels[[#]] == 1 &], c]];
    b = mat[[Select[training, classLabels[[#]] == 0 &], c]];
    pooled = Sqrt[(Variance[a] + Variance[b])/2. + 10^-12];
    Abs[Mean[a] - Mean[b]]/pooled
  ];

  (* Repeated, stratified CV. Crucially, the two sensor-feature positions
     are selected afresh FROM TRAINING DATA ONLY in every fold. Two original
     pre-selected positions are not reused here. Classify handles model
     fitting on training-only predictors. *)
  labels01 = Lookup[featureRows, "Dengue01"];
  runModels = {"SensorFoldwise2", "DemographicsOnly", "CombinedFoldwise2"};
  methods = {"LogisticRegression", "SupportVectorMachine"};
  predictions = Reap[
     Do[
       folds = stratifiedFolds[labels01, 5, 12345 + 100 run];
       Do[
         testIdx = folds[[fold]];
         trainIdx = Complement[Range[118], testIdx];
         ranked = Reverse[SortBy[candidateColumns,
           scoreColumn[secondDifferenceRows, labels01, trainIdx, #] &]];
         chosen = Take[ranked, 2];
         Do[
           predictorData = Switch[model,
             "SensorFoldwise2", secondDifferenceRows[[All, chosen]],
             "DemographicsOnly", ({#["Age"], #["Male01"]} & /@ featureRows),
             "CombinedFoldwise2", MapThread[Join[#1, #2] &,
               {secondDifferenceRows[[All, chosen]],
                ({#["Age"], #["Male01"]} & /@ featureRows)}]
           ];
           trainingRules = Map[Rule[predictorData[[#]],
               If[labels01[[#]] == 1, "Dengue", "Healthy"]] &, trainIdx];
           Do[
             trained = Classify[trainingRules, Method -> method];
             predictedProb = N[Lookup[trained[#, "Probabilities"],
                 "Dengue", 0.] & /@ predictorData[[testIdx]]];
             Do[Sow[<|"Run" -> run, "Fold" -> fold,
                "Method" -> method, "Model" -> model,
                "ID" -> featureRows[[testIdx[[i]]]]["ID"],
                "Truth" -> labels01[[testIdx[[i]]]],
                "Probability" -> predictedProb[[i]],
                "Prediction" -> Boole[predictedProb[[i]] >= 0.5],
                "SelectedCols" -> chosen|>],
               {i, Length[testIdx]}],
             {method, methods}],
           {model, runModels}],
         {fold, 5}],
       {run, 4}]
     ][[2, 1]];

  (* Exactly 118 x 4 predictions per model/method should appear. *)
  expectedEach = 118*4;
  countSummary = Counts[({#["Method"], #["Model"]} & /@ predictions)];
  Print["Prediction counts by analysis (expected ", expectedEach, "): ",
    countSummary];
  If[!AllTrue[Values[countSummary], # == expectedEach &],
    Print["WARNING: Incomplete out-of-fold predictions detected."]];

  perRunMetrics[sub_] := Module[{y, s, p, tp, tn, fp, fn},
    y = Lookup[sub, "Truth"]; s = Lookup[sub, "Probability"];
    p = Lookup[sub, "Prediction"];
    tp = Count[MapThread[And, {Map[# == 1 &, y], Map[# == 1 &, p]}], True];
    tn = Count[MapThread[And, {Map[# == 0 &, y], Map[# == 0 &, p]}], True];
    fp = Count[MapThread[And, {Map[# == 0 &, y], Map[# == 1 &, p]}], True];
    fn = Count[MapThread[And, {Map[# == 1 &, y], Map[# == 0 &, p]}], True];
    <|"Accuracy" -> N[(tp + tn)/Length[y]],
      "Sensitivity" -> N[tp/(tp + fn)],
      "Specificity" -> N[tn/(tn + fp)],
      "AUC" -> safeAUC[y, s],
      "TP" -> tp, "TN" -> tn, "FP" -> fp, "FN" -> fn|>
  ];
  metricTable = Prepend[Flatten[Table[
      Module[{m = perRunMetrics[Select[predictions,
          #["Run"] == run && #["Method"] == method &&
          #["Model"] == model &]]},
        Join[{run, method, model}, Lookup[m,
          {"Accuracy", "Sensitivity", "Specificity", "AUC",
           "TP", "TN", "FP", "FN"}]]],
       {run, 4}, {method, methods}, {model, runModels}], 2],
    {"Run", "Method", "Model", "Accuracy", "Sensitivity",
     "Specificity", "AUC", "TP", "TN", "FP", "FN"}];
  Export[FileNameJoin[{resultsDir, "FoldwiseCV_run_metrics.csv"}],
    metricTable, "CSV"];

  predictionTable = Prepend[Map[
      {#["Run"], #["Fold"], #["Method"], #["Model"],
       #["ID"], #["Truth"], #["Probability"], #["Prediction"],
       StringRiffle[ToString /@ #["SelectedCols"], ";"]} &,
      predictions],
    {"Run", "Fold", "Method", "Model", "ID", "Truth01",
     "PredictedProbability", "Predicted01", "SelectedColumns"}];
  Export[FileNameJoin[{resultsDir, "PRIVATE_FoldwiseCV_predictions.csv"}],
    predictionTable, "CSV"];
  Print["Sensor sensitivity analysis completed. This is a revised pipeline;"];
  Print["do NOT report old accuracy/AUC numbers as if they came from this pipeline."];
];

Print["Finished. Aggregate outputs saved to ", resultsDir];
