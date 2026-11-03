(* ::Title:: *)
(* Dengue e-nose: leakage-safe, within-training-fold feature selection *)
(* For Wolfram Language / Mathematica 14.3; 31 October 2026 *)
(*
   RUN AFTER the original anal27Mar26.nb cells defining normal2nd and dengue2nd.
   These are the original 19-element processed first-difference profiles.
   No participant-level private data leave the local computer.

   Scientific objective:
     Exploratory original features were selected with all labels visible.
     This analysis repeats selection INSIDE each training fold, making
     the held-out folds independent of feature selection and classifier fitting.

   The primary analysis allows 18 candidate adjacent differences (including
   sensor joins) to remain close to the original exploratory method.
   Sensitivity analyses exclude sensor-join candidates and compare one
   versus two selected features. A fixed-original-feature run is included
   only as an exploratory, potentially optimistic reference.

   Outcomes are 58 healthy (0) and 60 NS1-positive dengue (1).
   Four repetitions of 5-fold CV yield 472 out-of-fold predictions PER MODEL,
   from 118 distinct participants. Run-level metric mean +/- SD is reported
   across FOUR repetitions, not as 472 independent observations.
*)

(* ::Section:: *)
(* 0. Preconditions *)

If[!(ValueQ[normal2nd] && ValueQ[dengue2nd]),
  Print["ERROR: Evaluate the original notebook first, through the cells defining normal2nd and dengue2nd."];
  Abort[]
];

enHealthy = N[normal2nd];
enDengue = N[dengue2nd];
If[Dimensions[enHealthy] =!= {58, 19} ||
   Dimensions[enDengue] =!= {60, 19} ||
   !MatrixQ[enHealthy, NumericQ] || !MatrixQ[enDengue, NumericQ],
  Print["ERROR: Expected numeric matrices {58,19} and {60,19}. Actual dimensions: ",
    {Dimensions[enHealthy], Dimensions[enDengue]}]; Abort[]
];

enD = Join[enHealthy, enDengue];
enY = Join[ConstantArray[0, 58], ConstantArray[1, 60]];
enN = Length[enY];
(* Recover actual participant IDs from the original filename lists when present.
   DE035-2 is canonically DE035; DO NOT sort these lists, as row order matters. *)
enIDs = If[ValueQ[NormalfileList] && ValueQ[DenguefileList] &&
    Length[NormalfileList] == 58 && Length[DenguefileList] == 60,
  Map[With[{found = StringCases[ToString[#],
      RegularExpression["(?:DE|N)[0-9]{3}"]]},
      If[Length[found] == 1, First[found], "UNMATCHED"]] &,
      Join[NormalfileList, DenguefileList]],
  "ROW" <> IntegerString[#, 10, 3] & /@ Range[enN]
];
If[!DuplicateFreeQ[enIDs] || MemberQ[enIDs, "UNMATCHED"],
  Print["ERROR: participant IDs are duplicated or cannot be parsed. Check file list ordering."];
  Abort[]];

(* If the original features are in the kernel, verify exact correspondence.
   Original F1 = D[[12]]-D[[11]], i.e. candidate position 11.
   Original F2 = D[[10]]-D[[9]], i.e. candidate position 9. *)
enCandidates = N[Differences /@ enD];  (* 118 x 18 *)
enOriginal = enCandidates[[All, {11, 9}]];
If[ValueQ[normalPoints] && ValueQ[denguePoints],
  enSaved = N[Join[normalPoints, denguePoints]];
  If[Dimensions[enSaved] =!= {118, 2} ||
     Max[Abs[Flatten[enOriginal - enSaved]]] > 10^-7,
    Print["ERROR: Original two-feature values do not agree with notebook. Check sample ordering."];
    Abort[]
  ];
  Print["Audit: original features match candidate positions 11 and 9."]
];

(* Four five-point sensor segments generate 18 adjacent differences of
   the 19-element first-difference sequence. Positions 4,5,9,10,14,15
   touch the artificial joins between sensor channels. *)
enAllPositions = Range[18];
enBoundaryPositions = {4, 5, 9, 10, 14, 15};
enWithinPositions = Complement[enAllPositions, enBoundaryPositions];

(* Set output directory. *)
enFolder = Quiet[Check[NotebookDirectory[], $Failed]];
If[!StringQ[enFolder] || !DirectoryQ[enFolder], enFolder = Directory[]];
enOutput = FileNameJoin[{enFolder, "FeatureSelection_Results"}];
If[!DirectoryQ[enOutput], CreateDirectory[enOutput]];

(* ::Section:: *)
(* 1. Ranking criterion: standardized mean difference, TRAINING ONLY *)
(* score_j = |mean_D,j - mean_H,j| /
             sqrt[(variance_D,j + variance_H,j)/2]. *)

ClearAll[enRankScore, enRankTwo];
enRankScore[x_?MatrixQ, y_List, train_List, j_Integer] := Module[
  {dx, hx, vx, vy, denom},
  dx = Pick[x[[train, j]], y[[train]], 1];
  hx = Pick[x[[train, j]], y[[train]], 0];
  vx = Variance[dx]; vy = Variance[hx];
  denom = Sqrt[Max[0., N[(vx + vy)/2.]]];
  If[denom <= 10^-12, 0., N[Abs[Mean[dx] - Mean[hx]]/denom]]
];

enRankTwo[x_?MatrixQ, y_List, train_List, allowed_List, k_Integer] :=
  Take[SortBy[allowed, {-enRankScore[x, y, train, #], #} &], k];

(* ::Section:: *)
(* 2. Stratified folds: 5 folds, separately randomised for each repeat *)

ClearAll[enMakeFolds];
enMakeFolds[labels_List, k_Integer, seed_Integer] := BlockRandom[
  Module[{folds = Table[{}, {k}], shuffled, c},
    SeedRandom[seed];
    Do[
      shuffled = RandomSample[Flatten[Position[labels, c]]];
      Do[
        AppendTo[folds[[1 + Mod[i - 1, k]]], shuffled[[i]]],
        {i, Length[shuffled]}],
      {c, {0, 1}}];
    Sort /@ folds
  ]
];

(* ::Section:: *)
(* 3. Scale features from TRAINING data only, then fit and predict *)

ClearAll[enScaleFold, enFitProbabilities];
enScaleFold[xTrain_?MatrixQ, xTest_?MatrixQ] := Module[
  {mu, sd},
  mu = Mean /@ Transpose[xTrain];
  sd = StandardDeviation /@ Transpose[xTrain];
  sd = Map[If[!NumericQ[#] || # <= 10^-12, 1., #] &, sd];
  {Map[(# - mu)/sd &, xTrain], Map[(# - mu)/sd &, xTest]}
];

enFitProbabilities[xTrain_?MatrixQ, yTrain_List, xTest_?MatrixQ,
    method_String] := Module[{scaled, trainRules, clf, probabilities},
  scaled = enScaleFold[xTrain, xTest];
  trainRules = MapThread[Rule,
    {scaled[[1]], If[# == 1, "Dengue", "Healthy"] & /@ yTrain}];
  clf = Classify[trainRules, Method -> method];
  probabilities = N[Lookup[clf[#, "Probabilities"], "Dengue", Missing["Probability"]] & /@
    scaled[[2]]];
  If[!VectorQ[probabilities, NumericQ],
    Print["ERROR: A classifier did not return numerical dengue probabilities: ", method];
    Abort[]];
  probabilities
];

(* ::Section:: *)
(* 4. Four-repetition, 5-fold OUTER validation *)
(* One/two-feature counts are specified in advance; no test-fold tuning. *)

enStrategies = <|
  "All18_2" -> <|"Allowed" -> enAllPositions, "NFeatures" -> 2, "Fixed" -> False|>,
  "All18_1" -> <|"Allowed" -> enAllPositions, "NFeatures" -> 1, "Fixed" -> False|>,
  "WithinSensor12_2" -> <|"Allowed" -> enWithinPositions, "NFeatures" -> 2, "Fixed" -> False|>,
  "OriginalFixed_Exploratory" -> <|"Allowed" -> {11, 9}, "NFeatures" -> 2, "Fixed" -> True|>
|>;
enMethods = {"LogisticRegression", "SupportVectorMachine"};
enRecords = {};
enSelections = {};

enRandomSeed = 20261031;
Do[
  enFolds = enMakeFolds[enY, 5, enRandomSeed + run];
  If[Sort[Flatten[enFolds]] =!= Range[enN],
    Print["ERROR: Some participants were omitted or duplicated in a run."]; Abort[]];
  Do[
    enTest = enFolds[[fold]];
    enTrain = Complement[Range[enN], enTest];
    Do[
      allowed = enStrategies[strategy]["Allowed"];
      chosen = If[TrueQ[enStrategies[strategy]["Fixed"]], {11, 9},
        enRankTwo[enCandidates, enY, enTrain, allowed, enStrategies[strategy]["NFeatures"]]];
      AppendTo[enSelections, {run, fold, strategy, chosen[[1]],
        If[Length[chosen] == 2, chosen[[2]], Missing["OneFeature"]]}];
      Do[
        enProb = enFitProbabilities[
          enCandidates[[enTrain, chosen]], enY[[enTrain]],
          enCandidates[[enTest, chosen]], method];
        enRecords = Join[enRecords,
          Table[{run, fold, strategy, method, enTest[[j]],
            enY[[enTest[[j]]]], enProb[[j]], Boole[enProb[[j]] >= 0.5],
            chosen[[1]], If[Length[chosen] == 2, chosen[[2]], -1]},
            {j, Length[enTest]}]];
        ,{method, enMethods}];
      ,{strategy, Keys[enStrategies]}];
    ,{fold, 5}];
  Print["Completed CV repetition ", run, " / 4"];
  ,{run, 4}];

(* Each strategy/method MUST have exactly 4*118=472 predictions and each
   participant must be tested once in every repetition. *)
enCountCheck = Table[
  {strategy, method,
    Count[enRecords, {_, _, strategy, method, _, _, _, _, _, _}]},
  {strategy, Keys[enStrategies]}, {method, enMethods}];
If[!AllTrue[Flatten[enCountCheck, 1][[All, 3]], # == 472 &],
  Print["ERROR: Incomplete predictions: ", enCountCheck]; Abort[]];

(* ::Section:: *)
(* 5. Metrics per repetition; sample counts NOT independent across repeats *)

ClearAll[enAUC, enMetrics];
enAUC[y_List, scores_List] := Module[{p, n},
  p = Pick[scores, y, 1]; n = Pick[scores, y, 0];
  N[Total[Flatten[Outer[
     If[#1 > #2, 1., If[#1 == #2, .5, 0.]] &, p, n]]]/(Length[p] Length[n])]
];
enMetrics[rows_List] := Module[{y, score, pred, tp, tn, fp, fn},
  y = rows[[All, 6]];
  score = rows[[All, 7]];
  pred = rows[[All, 8]];
  tp = Count[Transpose[{y, pred}], {1, 1}];
  tn = Count[Transpose[{y, pred}], {0, 0}];
  fp = Count[Transpose[{y, pred}], {0, 1}];
  fn = Count[Transpose[{y, pred}], {1, 0}];
  {N[(tp + tn)/(tp + tn + fp + fn)], N[tp/(tp + fn)],
    N[tn/(tn + fp)], enAUC[y, score], tp, tn, fp, fn}
];

enRunMetrics = Flatten[Table[
    With[{r = Select[enRecords, #[[1]] == run && #[[3]] == strategy &&
         #[[4]] == method &]},
      Join[{run, strategy, method}, enMetrics[r]]],
    {run, 4}, {strategy, Keys[enStrategies]}, {method, enMethods}], 2];

enSummary = Flatten[Table[
    Module[{mat, means, sds},
      mat = (Take[#, {4, 7}] & /@ Select[enRunMetrics,
          #[[2]] == strategy && #[[3]] == method &]);
      means = Mean /@ Transpose[mat];
      sds = StandardDeviation /@ Transpose[mat];
      {strategy, method, Sequence @@ Riffle[means, sds]}
    ], {strategy, Keys[enStrategies]}, {method, enMethods}], 1];

Print["Run-level performance (mean +/- SD over four repeats):"];
Print[Grid[Prepend[enSummary,
  {"Strategy", "Method", "AccuracyMean", "AccuracySD", "SensitivityMean",
    "SensitivitySD", "SpecificityMean", "SpecificitySD", "AUCMean", "AUCSD"}],
  Frame -> All]];

(* ::Section:: *)
(* 6. Feature selection stability *)

enFreq = Table[
  {strategy, j, Count[
      Flatten[Select[enSelections, #[[3]] == strategy &][[All, {4, 5}]]], j],
    MemberQ[enBoundaryPositions, j], MemberQ[{11, 9}, j]},
  {strategy, Keys[enStrategies]}, {j, enAllPositions}];
enFreq = Flatten[enFreq, 1];

(* Expected 20 folds x k selected positions per strategy. *)
Do[
 Print["Strategy ", strategy, ": ",
   Total[Select[enFreq, #[[1]] == strategy &][[All, 3]]],
   " selected positions over 20 training folds (expected ",
   20 enStrategies[strategy]["NFeatures"], ")."],
 {strategy, Keys[enStrategies]}];

(* ::Section:: *)
(* 7. OPTIONAL participant-level OOF predictions: one row per participant *)
(* Each participant is held out four times; average their four probabilities.
   This is a reporting summary, not external validation. *)

enPerParticipant = Flatten[Table[
   Table[Module[{p = Select[enRecords,
      #[[3]] == strategy && #[[4]] == method && #[[5]] == i &]},
      If[Length[p] != 4,
        Print["ERROR: participant has <>4 OOF scores."]; Abort[]];
      With[{average = Mean[p[[All, 7]]]},
        {strategy, method, enIDs[[i]], i, enY[[i]], average, Boole[average >= .5]}]
     ], {i, enN}],
   {strategy, Keys[enStrategies]}, {method, enMethods}], 2];

(* Participant-level and repeat-aggregated confusion matrices are BOTH
   provided. The latter has 472 predictions and is not 472 people. *)
enConfusion = Flatten[Table[
  Module[{r, m},
    r = Select[enRecords, #[[3]] == strategy && #[[4]] == method &];
    m = enMetrics[r];
    {strategy, method, "472 repeated held-out predictions", Sequence @@ Take[m, {5, 8}]}
  ], {strategy, Keys[enStrategies]}, {method, enMethods}], 1];
enParticipantConfusion = Flatten[Table[
  Module[{r, tp, tn, fp, fn},
    r = Select[enPerParticipant, #[[1]] == strategy && #[[2]] == method &];
    tp = Count[r, {_, _, _, _, 1, _, 1}];
    tn = Count[r, {_, _, _, _, 0, _, 0}];
    fp = Count[r, {_, _, _, _, 0, _, 1}];
    fn = Count[r, {_, _, _, _, 1, _, 0}];
    {strategy, method, "118 participant-mean held-out predictions", tp, tn, fp, fn}
  ], {strategy, Keys[enStrategies]}, {method, enMethods}], 1];

(* ::Section:: *)
(* 8. Outputs. Do not publish the participant-index-level prediction files
   without prior data-sharing/ethics review. *)

Export[FileNameJoin[{enOutput, "CV_Run_Metrics.csv"}],
 Prepend[enRunMetrics, {"Run", "Strategy", "Method", "Accuracy",
   "Sensitivity", "Specificity", "AUC", "TP", "TN", "FP", "FN"}], "CSV"];
Export[FileNameJoin[{enOutput, "CV_Performance_Summary.csv"}],
 Prepend[enSummary, {"Strategy", "Method", "AccuracyMean", "AccuracySD",
   "SensitivityMean", "SensitivitySD", "SpecificityMean", "SpecificitySD",
   "AUCMean", "AUCSD"}], "CSV"];
Export[FileNameJoin[{enOutput, "CV_Confusion_Matrices.csv"}],
 Prepend[Join[enConfusion, enParticipantConfusion],
  {"Strategy", "Method", "Denominator", "TP", "TN", "FP", "FN"}], "CSV"];
Export[FileNameJoin[{enOutput, "CV_Feature_Selections.csv"}],
 Prepend[enSelections, {"Run", "Fold", "Strategy", "TopPosition", "SecondPosition"}], "CSV"];
Export[FileNameJoin[{enOutput, "CV_Feature_Frequency.csv"}],
 Prepend[enFreq, {"Strategy", "Position", "SelectedCountOf20Folds",
  "TouchesSensorBoundary", "OriginallySelected"}], "CSV"];
Export[FileNameJoin[{enOutput, "PRIVATE_CV_OutOfFold_Predictions.csv"}],
 Prepend[(Insert[#, enIDs[[#[[5]]]], 6] & /@ enRecords),
   {"Run", "Fold", "Strategy", "Method", "ParticipantRow", "ParticipantID",
   "Truth01", "DengueProbability", "Predicted01", "FeaturePosition1",
   "FeaturePosition2"}], "CSV"];
Export[FileNameJoin[{enOutput, "PRIVATE_CV_ParticipantMeanPredictions.csv"}],
 Prepend[enPerParticipant, {"Strategy", "Method", "ParticipantID", "ParticipantRow", "Truth01",
    "MeanOutOfFoldProbability", "Predicted01"}], "CSV"];

Print["Original selected candidate positions = {11, 9}; position 9 touches a sensor join."];
Print["IMPORTANT: OriginalFixed_Exploratory is biased by prior feature selection on the full labelled dataset; do not use it as the primary validation."];
Print["Results written to ", enOutput];
Print["CAUTION: Results are internally cross-validated only, and collection date is confounded with group."];
