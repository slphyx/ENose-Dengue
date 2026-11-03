(* ::Title:: *)
(* Dengue e-nose: added sensor-baseline and collection-date sensitivity checks *)
(* Supplementary analysis for major revision; 31 October 2026 *)
(* REQUIRES: original notebook AND Dengue_Enose_WithinFold_FeatureSelection.wl
   already evaluated in this kernel. Do not clear the kernel between steps.
   No patient-level predictions are exported by this file. *)

If[!And[ValueQ[enCandidates], ValueQ[enY], ValueQ[enIDs],
  ValueQ[enOutput], DownValues[enMakeFolds] =!= {},
  DownValues[enRankTwo] =!= {}, DownValues[enFitProbabilities] =!= {},
  DownValues[enMetrics] =!= {}],
 Print["ERROR: Run Dengue_Enose_WithinFold_FeatureSelection.wl first."];
 Abort[]
];
If[Dimensions[enCandidates] =!= {118, 18} || Length[enY] != 118,
 Print["ERROR: Unexpected cohort or feature matrix dimensions."]; Abort[]];

(* In the ORIGINAL transformation, second-difference positions
   1, 6, 11, and 16 explicitly contain an ADDITIVE R(index100)/1000 term
   (from sensor channels 1,2,3,4 respectively). Other positions remain
   normalized by R(index100) and can still be susceptible to sensor drift.
   Thus these restrictions test an explicit baseline term, not all drift. *)
enAdditiveBaseline = {1, 6, 11, 16};
enBoundary = {4, 5, 9, 10, 14, 15};
enNoAdditive = Complement[Range[18], enAdditiveBaseline];
enNoAdditiveNoBoundary = Complement[enNoAdditive, enBoundary];
Print["Positions without explicit additive baseline term (14): ", enNoAdditive];
Print["Positions without that term or concatenation joins (8): ", enNoAdditiveNoBoundary];

enExtraStrategies = <|
 "NoAdditiveBaseline14_2" -> <|"Allowed" -> enNoAdditive, "NFeatures" -> 2|>,
 "NoAdditiveBaselineNoBoundary8_2" -> <|"Allowed" -> enNoAdditiveNoBoundary, "NFeatures" -> 2|>,
 "NoAdditiveBaselineNoBoundary8_1" -> <|"Allowed" -> enNoAdditiveNoBoundary, "NFeatures" -> 1|>
|>;

(* Filename-derived dates; a negative-control predictor that uses NO e-nose
   sensor measurements. Use when the original recording filename lists are
   present and ordered as the sensor arrays.  It is not a diagnostic model. *)
ClearAll[enFilenameDate, enFilenameID];
enFilenameDate[file_] := Module[{hits, values},
 hits = StringCases[ToString[file], RegularExpression[
   "_([0-9]{2})-([0-9]{2})-([0-9]{2})-([0-9]{2})-([0-9]{2})\\.odr"] ->
     {"$1", "$2", "$3", "$4", "$5"}];
 If[Length[hits] != 1, Return[Missing["InvalidFilenameDate"]]];
 values = ToExpression /@ First[hits];
 N[AbsoluteTime[DateObject[{2000 + values[[3]], values[[2]],
   values[[1]], values[[4]], values[[5]]}]]/86400.]
];
enFilenameID[file_] := Module[{hits = StringCases[ToString[file],
  RegularExpression["(?:DE|N)[0-9]{3}"]]},
 If[Length[hits] == 1, First[hits], "UNMATCHED"]];

enHasDates = False;
If[ValueQ[NormalfileList] && ValueQ[DenguefileList] &&
  Length[NormalfileList] == 58 && Length[DenguefileList] == 60,
 enFiles = Join[NormalfileList, DenguefileList];
 enExtractedIDs = enFilenameID /@ enFiles;
 enDateValues = enFilenameDate /@ enFiles;
 If[enExtractedIDs === enIDs && VectorQ[enDateValues, NumericQ],
  enHasDates = True;
  enDateMatrix = List /@ enDateValues;
  Print["Recording-date-only negative control enabled. (No sensor inputs)"];
  , Print["WARNING: recording IDs/date parsing did not match; date-only control SKIPPED."]
 ];
 , Print["WARNING: original filename lists not loaded; date-only control SKIPPED."]
];
If[enHasDates,
 enExtraStrategies = Append[enExtraStrategies,
   "DateOnly_NegativeControl" -> <|"Allowed" -> {1}, "NFeatures" -> 1|>]
];

enExtraRecords = {};
enExtraSelections = {};
enExtraMethods = {"LogisticRegression", "SupportVectorMachine"};
Do[
 folds = enMakeFolds[enY, 5, 20261031 + run];
 Do[
  test = folds[[fold]];
  train = Complement[Range[118], test];
  Do[
   If[strategy == "DateOnly_NegativeControl",
     chosen = {1}; sourceMatrix = enDateMatrix,
     chosen = enRankTwo[enCandidates, enY, train,
       enExtraStrategies[strategy]["Allowed"],
       enExtraStrategies[strategy]["NFeatures"]];
     sourceMatrix = enCandidates
   ];
   AppendTo[enExtraSelections, {run, fold, strategy,
     If[strategy == "DateOnly_NegativeControl", -1, chosen[[1]]],
     If[strategy == "DateOnly_NegativeControl" || Length[chosen] == 1,
       -1, chosen[[2]]]}];
   Do[
    probs = enFitProbabilities[sourceMatrix[[train, chosen]], enY[[train]],
      sourceMatrix[[test, chosen]], method];
    enExtraRecords = Join[enExtraRecords,
      Table[{run, fold, strategy, method, test[[i]], enY[[test[[i]]]],
        probs[[i]], Boole[probs[[i]] >= 0.5],
        If[strategy == "DateOnly_NegativeControl", -1, chosen[[1]]],
        If[strategy == "DateOnly_NegativeControl" || Length[chosen] == 1,
          -1, chosen[[2]]]}, {i, Length[test]}]];
   , {method, enExtraMethods}];
  , {strategy, Keys[enExtraStrategies]}];
 , {fold, 5}];
 Print["Extra sensitivity: completed repetition ", run, "/4"];
, {run, 4}];

(* Recalculate 118 held-out predictions per repetition and summarize across
   FOUR repetitions (not across 472 independent individuals). *)
enExtraRunMetrics = Flatten[Table[
 With[{r = Select[enExtraRecords, #[[1]] == run && #[[3]] == strategy &&
    #[[4]] == method &]},
   If[Length[r] != 118, Print["ERROR: incomplete repetition ",
     {run, strategy, method}, " n=", Length[r]]; Abort[]];
   Join[{run, strategy, method}, enMetrics[r]]],
 {run, 4}, {strategy, Keys[enExtraStrategies]},
 {method, enExtraMethods}], 2];

enExtraSummary = Flatten[Table[
 Module[{values, means, sds},
  values = (Take[#, {4, 7}] & /@ Select[enExtraRunMetrics,
   #[[2]] == strategy && #[[3]] == method &]);
  means = Mean /@ Transpose[values];
  sds = StandardDeviation /@ Transpose[values];
  {strategy, method, Sequence @@ Riffle[means, sds]}],
 {strategy, Keys[enExtraStrategies]}, {method, enExtraMethods}], 1];

enExtraCM = Flatten[Table[
 Module[{rows, m},
  rows = Select[enExtraRecords, #[[3]] == strategy && #[[4]] == method &];
  m = enMetrics[rows];
  {strategy, method, Length[rows], Sequence @@ Take[m, {5, 8}]}],
 {strategy, Keys[enExtraStrategies]}, {method, enExtraMethods}], 1];

(* Report how often each feature was selected, with no patient identifiers. *)
enExtraFreq = Flatten[Table[
 {strategy, j, Count[Flatten[Select[enExtraSelections,
   #[[3]] == strategy &][[All, {4, 5}]]], j]},
 {strategy, DeleteCases[Keys[enExtraStrategies], "DateOnly_NegativeControl"]},
 {j, Range[18]}], 1];

Export[FileNameJoin[{enOutput, "CV_AdditionalSensitivity_Performance.csv"}],
 Prepend[enExtraSummary, {"Strategy", "Method", "AccuracyMean",
   "AccuracySD", "SensitivityMean", "SensitivitySD", "SpecificityMean",
   "SpecificitySD", "AUCMean", "AUCSD"}], "CSV"];
Export[FileNameJoin[{enOutput, "CV_AdditionalSensitivity_RunMetrics.csv"}],
 Prepend[enExtraRunMetrics, {"Run", "Strategy", "Method", "Accuracy",
   "Sensitivity", "Specificity", "AUC", "TP", "TN", "FP", "FN"}], "CSV"];
Export[FileNameJoin[{enOutput, "CV_AdditionalSensitivity_Confusions.csv"}],
 Prepend[enExtraCM, {"Strategy", "Method", "RepeatedHeldOutN",
   "TP", "TN", "FP", "FN"}], "CSV"];
Export[FileNameJoin[{enOutput, "CV_AdditionalSensitivity_Selections.csv"}],
 Prepend[enExtraSelections, {"Run", "Fold", "Strategy", "Position1",
   "Position2"}], "CSV"];
Export[FileNameJoin[{enOutput, "CV_AdditionalSensitivity_FeatureFrequency.csv"}],
 Prepend[enExtraFreq, {"Strategy", "Position",
   "SelectedCountOf20Folds"}], "CSV"];

(* Descriptive within-group collection-time correlations for commonly
   selected sensor candidates, if dates are available. These are NOT a
   substitute for concurrent collection or external validation. *)
If[enHasDates,
 enDateCorrelations = Flatten[Table[
 Module[{idx},
  idx = Flatten[Position[enY, label]];
  {If[label == 1, "Dengue", "Healthy"], pos,
    Correlation[enDateValues[[idx]], enCandidates[[idx, pos]]]}],
 {label, {0, 1}}, {pos, {1, 9, 11, 17}}], 1];
 Export[FileNameJoin[{enOutput, "CV_AdditionalSensitivity_DateCorrelations.csv"}],
  Prepend[enDateCorrelations, {"Group", "FeaturePosition",
   "PearsonCorrelationWithRecordingDate"}], "CSV"];
];
Print["Additional sensitivity results written to: ", enOutput];
Print[Grid[Prepend[enExtraSummary, {"Strategy", "Method", "AccuracyMean",
 "AccuracySD", "SensitivityMean", "SensitivitySD", "SpecificityMean",
 "SpecificitySD", "AUCMean", "AUCSD"}], Frame -> All]];
Print["CAUTION: Even if no-additive-baseline models discriminate, group and date remain confounded; no clinical specificity or independent validity follows."];
