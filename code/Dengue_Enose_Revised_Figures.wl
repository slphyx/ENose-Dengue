(* ::Title:: *)
(* Dengue e-nose: revised held-out ROC and confusion-matrix figures *)
(* Requires FeatureSelection_Results/ from Dengue_Enose_WithinFold_FeatureSelection.wl. *)
(* Mathematica / Wolfram Language 14.3. No participant-level records are exported. *)

ClearAll[enStrToNumber, enROCPoints, enSelectRows, enFigureRoot];
enFigureRoot = Quiet[Check[NotebookDirectory[], $Failed]];
If[!StringQ[enFigureRoot] || !DirectoryQ[enFigureRoot], enFigureRoot = Directory[]];
enFigureDir = FileNameJoin[{enFigureRoot, "FeatureSelection_Results"}];
enFigureCSV = FileNameJoin[{enFigureDir, "PRIVATE_CV_OutOfFold_Predictions.csv"}];
If[!FileExistsQ[enFigureCSV], Print["ERROR: Unzip FeatureSelection_Results beside notebook, or edit enFigureDir."]; Abort[]];
enFigureRaw = Rest[Import[enFigureCSV, "CSV"]];
enStrToNumber[x_] := If[NumericQ[x], N[x], ToExpression[ToString[x]]];
enSelectRows[method_String, run_Integer] := Select[enFigureRaw,
    #[[3]] === "All18_2" && #[[4]] === method && enStrToNumber[#[[1]]] == run &];

enROCPoints[rows_List] := Module[{y, p, positives, negatives, thresholds, points},
  y = Round[enStrToNumber /@ rows[[All, 7]]];
  p = enStrToNumber /@ rows[[All, 8]];
  positives = Pick[p, y, 1]; negatives = Pick[p, y, 0];
  If[Length[positives] == 0 || Length[negatives] == 0,
    Print["ERROR: ROC needs both classes."]; Abort[]];
  thresholds = Reverse[Sort[DeleteDuplicates[p]]];
  points = Table[{
      N[Count[negatives, _?(# >= t &)]/Length[negatives]],
      N[Count[positives, _?(# >= t &)]/Length[positives]]},
    {t, thresholds}];
  DeleteDuplicates[Join[{{0., 0.}}, points, {{1., 1.}}]]
];

(* Figure 3: four independently held-out ROC traces from primary logistic regression. *)
enROCData = Table[enROCPoints[enSelectRows["LogisticRegression", run]], {run, 4}];
If[!AllTrue[Table[Length[enSelectRows["LogisticRegression", run]], {run, 4}], # == 118 &],
  Print["ERROR: Each repetition must contain 118 out-of-fold predictions."]; Abort[]];
enROCPlot = ListLinePlot[
  Append[enROCData, {{0., 0.}, {1., 1.}}],
  Frame -> True, Axes -> False, PlotRange -> {{0, 1}, {0, 1}},
  PlotLegends -> Placed[{"Repetition 1", "Repetition 2", "Repetition 3", "Repetition 4", "Chance"}, Right],
  FrameLabel -> {"False-positive rate (1 - specificity)", "True-positive rate (sensitivity)"},
  PlotStyle -> {Directive[Thickness[0.004]], Directive[Thickness[0.004]],
    Directive[Thickness[0.004]], Directive[Thickness[0.004]], Directive[GrayLevel[0.6], Dashed]},
  ImageSize -> 670, BaseStyle -> {FontFamily -> "Arial", FontSize -> 12},
  PlotLabel -> "Logistic regression: four out-of-fold ROC curves"
];
Export[FileNameJoin[{enFigureDir, "Figure3_Revised_ROC.png"}], enROCPlot, ImageResolution -> 300];
Export[FileNameJoin[{enFigureDir, "Figure3_Revised_ROC.pdf"}], enROCPlot];

(* Figure 4: 472 repeated out-of-fold predictions, not 472 unique participants. *)
enPrimaryRows = Join @@ Table[enSelectRows["LogisticRegression", run], {run, 4}];
enPairs = ({Round[enStrToNumber[#[[7]]]], Round[enStrToNumber[#[[9]]]]} & /@ enPrimaryRows);
enTP = Count[enPairs, {1, 1}]; enFN = Count[enPairs, {1, 0}];
enFP = Count[enPairs, {0, 1}]; enTN = Count[enPairs, {0, 0}];
If[{enTP, enFN, enFP, enTN} =!= {221, 19, 33, 199},
  Print["WARNING: Actual confusion matrix differs from the version reviewed: ",
    {enTP, enFN, enFP, enTN}];
];
enMatrix = Grid[{
   {Style["Actual / Predicted", Bold, 13], Style["Dengue", Bold, 13], Style["Healthy", Bold, 13]},
   {Style["Dengue", Bold, 13], Style[enTP, 18, Bold], Style[enFN, 18, Bold]},
   {Style["Healthy", Bold, 13], Style[enFP, 18, Bold], Style[enTN, 18, Bold]}
   }, Frame -> All, Alignment -> Center,
   ItemSize -> {{20, 15, 15}, {3.5, 5, 5}}, Spacings -> {1, 1},
   Background -> {None, {None, Lighter[Blue, 0.9], Lighter[Gray, 0.9]}}];
enCMFigure = Column[{
   Style["Logistic regression: repeated held-out predictions", Bold, 15],
   enMatrix,
   Style["Four repetitions: 240 dengue predictions and 232 healthy predictions; 118 unique participants.", 11]
  }, Alignment -> Center, Spacings -> 2];
Export[FileNameJoin[{enFigureDir, "Figure4_Revised_ConfusionMatrix.png"}],
  Rasterize[enCMFigure, "Image", ImageResolution -> 300]];
Export[FileNameJoin[{enFigureDir, "Figure4_Revised_ConfusionMatrix.pdf"}], enCMFigure];

(* Optional Supplementary Figure: one averaged held-out probability per participant. *)
enMeanCSV = FileNameJoin[{enFigureDir, "PRIVATE_CV_ParticipantMeanPredictions.csv"}];
If[FileExistsQ[enMeanCSV],
  enMeanRaw = Rest[Import[enMeanCSV, "CSV"]];
  enMeanRows = Select[enMeanRaw,
    #[[1]] === "All18_2" && #[[2]] === "LogisticRegression" &];
  enHealthyScores = enStrToNumber /@ Select[enMeanRows,
      Round[enStrToNumber[#[[5]]]] == 0 &][[All, 6]];
  enDengueScores = enStrToNumber /@ Select[enMeanRows,
      Round[enStrToNumber[#[[5]]]] == 1 &][[All, 6]];
  enHealthyPoints = MapIndexed[{1. + 0.055 Sin[2.4 First[#2]], #1} &,
    enHealthyScores];
  enDenguePoints = MapIndexed[{2. + 0.055 Sin[2.4 First[#2]], #1} &,
    enDengueScores];
  enProbPlot = ListPlot[{enHealthyPoints, enDenguePoints},
    Frame -> True, Axes -> False, PlotRange -> {{0.6, 2.4}, {-0.02, 1.02}},
    FrameTicks -> {{Automatic, None}, {{{1, "Healthy controls"}, {2, "NS1-positive dengue"}}, None}},
    FrameLabel -> {"Study group", "Mean held-out predicted dengue probability"},
    ImageSize -> 590, PlotMarkers -> {Automatic, 9},
    PlotLegends -> None,
    Epilog -> {Directive[Dashed, GrayLevel[0.5]], Line[{{0.65, 0.5}, {2.35, 0.5}}]},
    BaseStyle -> {FontFamily -> "Arial", FontSize -> 12}];
  Export[FileNameJoin[{enFigureDir, "Supplementary_OOF_Probabilities.png"}],
    enProbPlot, ImageResolution -> 300];
  Export[FileNameJoin[{enFigureDir, "Supplementary_OOF_Probabilities.pdf"}], enProbPlot];
];
Print["Figures exported to: ", enFigureDir];
Print["Confusion matrix (TP,FN,FP,TN): ", {enTP, enFN, enFP, enTN}];
