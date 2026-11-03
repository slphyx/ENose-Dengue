(* ::Title:: *)
(* Dengue e-nose: exact feature reconstruction and algebraic audit *)
(* Based on the original anal27Mar26.nb (Wolfram Language 14.3). *)
(* This file does not alter the original notebook or perform feature selection. *)

ClearAll[dengueENoseFeaturesFromMatrix, dengueENoseFiveReadings];

(* r is a 5 x 4 numeric matrix: rows = indices 100,200,300,400,500;
   columns = the four sensor channels in the ORIGINAL notebook order. *)
dengueENoseFeaturesFromMatrix[r_?MatrixQ] := Module[
  {a, b, u, firstDifferences, d, fOriginal, fSimplified},
  If[Dimensions[r] =!= {5, 4} || !MatrixQ[r, NumericQ] ||
      !FreeQ[r[[1]], 0],
    Print["ERROR: expected a numeric 5 x 4 matrix with nonzero row-1 values."];
    Return[$Failed]];

  (* Mirror makeFeature: first value is raw R100, later values are
     consecutive increments relative to R100. *)
  a = Table[
    If[k == 1, r[[1, s]],
      (r[[k, s]] - r[[k - 1, s]])/r[[1, s]]],
    {k, 5}, {s, 4}];

  (* Mirror get2ndOrderFeature's makeY. *)
  b = Table[
    If[k == 1, 0, 20 a[[k, s]] - a[[1, s]]/1000],
    {s, 4}, {k, 5}];

  (* Same four-sensor join/offset construction. *)
  u = Flatten@Table[
    b[[s]] + Total[Take[b[[All, 5]], s - 1]], {s, 4}];

  firstDifferences = Differences[u];
  d = firstDifferences - First[firstDifferences];

  fOriginal = {d[[12]] - d[[11]], d[[10]] - d[[9]]};
  fSimplified = {
    20 (r[[3, 3]] - 3 r[[2, 3]] + 2 r[[1, 3]])/r[[1, 3]] +
      r[[1, 3]]/1000,
    20 (2 r[[4, 2]] - r[[3, 2]] - r[[5, 2]])/r[[1, 2]]
  };

  <|
    "SelectedRawReadings" -> r,
    "IntermediateRelativeIncrements" -> a,
    "ScaledSensorSequences" -> b,
    "Concatenated20" -> u,
    "ProcessedFirstDifferences19" -> d,
    "OriginalFeatures" -> fOriginal,
    "SimplifiedFeatures" -> fSimplified,
    "FeatureFormulaDiscrepancy" -> Max[Abs[N[fOriginal - fSimplified]]],
    "BoundaryDiscontinuity2to3" -> N[u[[11]] - u[[10]]]
  |>
];

(* Each rawRecord is one entry from the original `NormalrawData` or
   `DenguerawData`: a list of raw comma-separated rows, each of which
   contains four sensor-channel readings (possibly plus other columns). *)
dengueENoseFiveReadings[rawRecord_List] := Module[{r},
  If[Length[rawRecord] < 500,
    Print["ERROR: fewer than 500 raw records."]; Return[$Failed]];
  r = Table[
    Take[ToExpression /@ StringSplit[rawRecord[[k]], ","], 4],
    {k, {100, 200, 300, 400, 500}}];
  If[!MatrixQ[r, NumericQ] || Dimensions[r] =!= {5, 4},
    Print["ERROR: invalid raw record format."]; Return[$Failed]];
  N[r]
];

(* =================================================================== *)
(* Synthetic internal test. Expected maximum discrepancy = 0.            *)
(* =================================================================== *)
syntheticReadings = Table[
  8000 + 100 s + 60 k^2 + 14 s k + 15 k^3,
  {k, 5}, {s, 4}];
syntheticAudit = dengueENoseFeaturesFromMatrix[syntheticReadings];
Print["Synthetic test feature discrepancy: ",
  syntheticAudit["FeatureFormulaDiscrepancy"]];
Print["Synthetic test sensor-2/3 boundary discontinuity: ",
  syntheticAudit["BoundaryDiscontinuity2to3"]];

(* =================================================================== *)
(* OPTIONAL: evaluate ORIGINAL notebook cells creating:                *)
(*   NormalrawData, DenguerawData, normalPoints, denguePoints          *)
(* before evaluating this block.                                         *)
(* This should be run before modifying the original feature code.       *)
(* =================================================================== *)
If[And[ValueQ[NormalrawData], ValueQ[DenguerawData],
       ValueQ[normalPoints], ValueQ[denguePoints]],
  Module[{healthyAudit, dengueAudit, reconstructedHealthy,
      reconstructedDengue, originalHealthy, originalDengue,
      allError, summaryRows},

    healthyAudit = dengueENoseFeaturesFromMatrix @
        dengueENoseFiveReadings[#] & /@ NormalrawData;
    dengueAudit = dengueENoseFeaturesFromMatrix @
        dengueENoseFiveReadings[#] & /@ DenguerawData;

    reconstructedHealthy = Lookup[healthyAudit, "OriginalFeatures"];
    reconstructedDengue = Lookup[dengueAudit, "OriginalFeatures"];
    originalHealthy = N[normalPoints];
    originalDengue = N[denguePoints];

    If[Dimensions[reconstructedHealthy] =!= Dimensions[originalHealthy] ||
       Dimensions[reconstructedDengue] =!= Dimensions[originalDengue],
      Print["ERROR: The number/order of raw records and plotted feature rows differ."];
      Abort[]];

    allError = Max[Abs[Flatten[Join[
      reconstructedHealthy - originalHealthy,
      reconstructedDengue - originalDengue]]]];
    Print["Maximum difference vs original plotted features: ", allError];
    Print["Number of original observations: ",
      Length[reconstructedHealthy], " healthy + ",
      Length[reconstructedDengue], " dengue."];

    (* A within-group descriptive baseline association. WARNING: not
       a correction for measurement-date or feature-selection bias. *)
    summaryRows = Join[
      MapThread[{"Healthy", #1[[1, 3]], #2[[1]], #2[[2]]} &,
        {Lookup[healthyAudit, "SelectedRawReadings"],
         reconstructedHealthy}],
      MapThread[{"Dengue", #1[[1, 3]], #2[[1]], #2[[2]]} &,
        {Lookup[dengueAudit, "SelectedRawReadings"],
         reconstructedDengue}]
    ];
    Print["Pearson correlation of feature 1 with sensor-3 R(index 100): ",
      Correlation[summaryRows[[All, 2]], summaryRows[[All, 3]]]];
    If[StringQ[Quiet[Check[NotebookDirectory[], $Failed]]],
      Export[FileNameJoin[{NotebookDirectory[],
        "PRIVATE_Feature_Baseline_Audit.csv"}],
        Prepend[summaryRows, {"Group", "Sensor3_R_index100",
          "Feature1", "Feature2"}], "CSV"]];
  ],
  Print["Original raw data/feature variables are not in the kernel. ",
    "Only the synthetic test was run. To validate clinical features, ",
    "evaluate the original notebook first."]
];
