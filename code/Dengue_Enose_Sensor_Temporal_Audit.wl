(* ::Title:: *)
(* Supplementary e-nose sensor response / collection-period audit *)
(* Requires original anal27Mar26.nb cells defining NormalrawData, DenguerawData,
   NormalfileList, DenguefileList, normal2nd and dengue2nd. No clinical workbook needed. *)
ClearAll[staFolder,staOut,staDate,staNumericRow,staReadRecording,staRecords,staRaw,
 staDays,staIDs,staGroup,staMean,staSD,staExport,staPlot,staCorr];
If[!(ValueQ[NormalrawData] && ValueQ[DenguerawData] && ValueQ[NormalfileList] && ValueQ[DenguefileList] && ValueQ[normal2nd] && ValueQ[dengue2nd]),
 Print["ERROR: Run original notebook cells to create raw data, file lists, normal2nd and dengue2nd first."];Abort[]];
If[Dimensions[normal2nd]=!={58,19} || Dimensions[dengue2nd]=!={60,19} ||
 Length[NormalrawData]!=58 || Length[DenguerawData]!=60 ||
 Length[NormalfileList]!=58 || Length[DenguefileList]!=60,
 Print["ERROR: Data lengths differ from expected 58 controls and 60 dengue recordings."];Abort[]];
staFolder=Quiet[Check[NotebookDirectory[],$Failed]];
If[!StringQ[staFolder] || !DirectoryQ[staFolder],staFolder=Directory[]];
staOut=FileNameJoin[{staFolder,"Sensor_Temporal_Supplement"}];
If[!DirectoryQ[staOut],CreateDirectory[staOut]];
staDate[file_]:=Module[{hit,v},
 hit=StringCases[ToString[file],RegularExpression["_([0-9]{2})-([0-9]{2})-([0-9]{2})-([0-9]{2})-([0-9]{2})\\.odr"]->{"$1","$2","$3","$4","$5"}];
 If[Length[hit]!=1,Return[Missing["NoDate"]]];
 v=ToExpression/@First[hit];DateObject[{2000+v[[3]],v[[2]],v[[1]],v[[4]],v[[5]]}]];
staNumericRow[line_]:=Module[{v},
 v=If[StringQ[line],StringSplit[StringTrim[line],","],line];
 If[!ListQ[v] || Length[v]<4,Return[$Failed]];
 v=Quiet[Check[N[If[StringQ[#],ToExpression[StringTrim[#]],#]&/@Take[v,4]],$Failed]];
 If[v===$Failed || !VectorQ[v,NumericQ],$Failed,v]];
staReadRecording[r_]:=Module[{a},
 If[!ListQ[r] || Length[r]<500,Return[$Failed]];
 a=staNumericRow/@r;
 If[MemberQ[a,$Failed] || !MatrixQ[a,NumericQ],Return[$Failed]];
 a];
staRecords=Join[NormalrawData,DenguerawData];
staRaw=staReadRecording/@staRecords;
If[MemberQ[staRaw,$Failed],Print["ERROR: Raw recording format unexpected. Run Dimensions[NormalrawData[[1]]] and inspect rows manually."];Abort[]];
staFiles=ToString/@Join[NormalfileList,DenguefileList];
staDates=staDate/@staFiles;
If[!FreeQ[staDates,_Missing],Print["ERROR: Cannot parse recording dates from file names."];Abort[]];
staDays=N[AbsoluteTime[#]/86400&/@staDates];
staIDs=First[StringCases[#,RegularExpression["(?:DE|N)[0-9]{3}"]]]&/@staFiles;
staGroup=Join[ConstantArray["Healthy",58],ConstantArray["Dengue",60]];
staMonth=DateString[#, {"Year","-","Month"}]&/@staDates;
staLateCutoff=AbsoluteTime[DateObject[{2025,12,1}]];
staPeriod=MapThread[If[#1=="Dengue","Dengue",If[AbsoluteTime[#2]>=staLateCutoff,"Healthy late","Healthy early"]]&, {staGroup,staDates}];
Print["Raw recording lengths: ",Counts[Length/@staRaw]];
Print["Participant periods: ",Counts[staPeriod]];
If[Sort[Values[Counts[staPeriod]]] =!= Sort[{52,6,60}],Print["ERROR: Period labeling did not give expected 52 early controls, 6 late controls and 60 dengue patients."];Abort[]];
(* Important: reference index 100 is an operational reference and not a verified clean-air baseline. *)
staRef=N[#[[100,All]]&/@staRaw];
staAmplitude=Table[Max[staRaw[[i,All,s]]]-Min[staRaw[[i,All,s]]],{i,118},{s,4}];
staRelativeAmplitude=staAmplitude/Map[Max[Abs[#],10^-12]&,staRef,{2}];
staProfiles=Table[(staRaw[[i,All,s]]-staRef[[i,s]])/Max[Abs[staRef[[i,s]]],10^-12],{i,118},{s,4}];
staFeatures=N[Differences/@Join[normal2nd,dengue2nd]];
staSelected={1,3,9,11,17};
staCorr[x_List,y_List]:=If[Length[x]>2 && StandardDeviation[N[x]]>10^-12 && StandardDeviation[N[y]]>10^-12,N[Correlation[N[x],N[y]]],Missing["ConstantOrSmall"]];
(* One row per person / sensor. No participant names exported. *)
staRows=Flatten[Table[Join[{staIDs[[i]],staGroup[[i]],staMonth[[i]],staPeriod[[i]],s,
 N[staDays[[i]]]},N[{staRef[[i,s]],staAmplitude[[i,s]],staRelativeAmplitude[[i,s]]}]],{i,118},{s,4}],1];
Export[FileNameJoin[{staOut,"S1_RawSensor_Summary.csv"}],Prepend[staRows,{"ID","Group","YearMonth","Period","SensorChannel","RecordingDay","Index100Reference","FullRecordingAmplitude","RelativeAmplitude"}]];
staFeatureRows=Table[Join[{staIDs[[i]],staGroup[[i]],staMonth[[i]],N[staDays[[i]]]},staFeatures[[i,staSelected]]],{i,118}];
Export[FileNameJoin[{staOut,"S2_Feature_Date_Data.csv"}],Prepend[staFeatureRows,Join[{"ID","Group","YearMonth","RecordingDay"},"FeaturePos"<>ToString[#]&/@staSelected]]];
staCorRows=Flatten[Table[Module[{idx=Flatten@Position[staGroup,g]},
 Table[{g,s,staCorr[staDays[[idx]],staRef[[idx,s]]],staCorr[staDays[[idx]],staAmplitude[[idx,s]]],staCorr[staDays[[idx]],staRelativeAmplitude[[idx,s]]]},{s,4}]],{g,{"Healthy","Dengue"}}],1];
Export[FileNameJoin[{staOut,"S3_WithinGroup_Correlations.csv"}],Prepend[staCorRows,{"Group","SensorChannel","r_Date_Reference","r_Date_Amplitude","r_Date_RelativeAmplitude"}]];
staFeatureCorr=Flatten[Table[Module[{idx=Flatten@Position[staGroup,g]},Table[{g,p,staCorr[staDays[[idx]],staFeatures[[idx,p]]]},{p,staSelected}]],{g,{"Healthy","Dengue"}}],1];
Export[FileNameJoin[{staOut,"S4_Feature_Date_Correlations.csv"}],Prepend[staFeatureCorr,{"Group","CandidatePosition","PearsonR"}]];
(* Four sensor-level figures, with periods color-coded; plot dates as numerical days for cross-version reliability. *)
staColors=<|"Healthy early"->RGBColor[0.12,0.52,0.35],"Healthy late"->RGBColor[0.2,0.45,0.8],"Dengue"->RGBColor[0.83,0.2,0.23]|>;
staScatter[matrix_,label_,name_]:=Module[{panels},
 panels=Table[Module[{series},series=Table[Cases[Transpose[{staPeriod,staDays,matrix[[All,s]]}],{group_,day_,value_}/;group===cat:>{day,value}],{cat,{"Healthy early","Healthy late","Dengue"}}];
 ListPlot[series,PlotStyle->(Directive[#,PointSize[0.017]]&/@Values[staColors]),
 PlotLegends->If[s==1,Placed[{"Healthy early","Healthy late","Dengue"},Below],None],
 Frame->True,Axes->False,FrameLabel->{"Recording date (days since epoch)",label},
 PlotLabel->"Sensor channel "<>ToString[s],ImageSize->420]],{s,4}];
 Export[FileNameJoin[{staOut,name<>".png"}],GraphicsGrid[Partition[panels,2],ImageSize->1050],ImageResolution->160]];
staScatter[staRef,"Reading at index 100","Figure_S3_Index100_Reference_vs_Date"];
staScatter[staAmplitude,"Full-recording range","Figure_S4_Amplitude_vs_Date"];
staScatter[staRelativeAmplitude,"Range / |index-100 reference|","Figure_S5_RelativeAmplitude_vs_Date"];
(* Feature figure: 5 panels each with corresponding dated scatter. *)
staFeaturePanels=Table[ListPlot[Table[Cases[Transpose[{staPeriod,staDays,staFeatures[[All,p]]}],{group_,day_,value_}/;group===cat:>{day,value}],{cat,{"Healthy early","Healthy late","Dengue"}}],
 PlotStyle->(Directive[#,PointSize[0.018]]&/@Values[staColors]),
 Frame->True,Axes->False,FrameLabel->{"Recording date (days since epoch)","Feature value"},
 PlotLabel->"Candidate position "<>ToString[p],ImageSize->430],{p,staSelected}];
Export[FileNameJoin[{staOut,"Figure_S6_Features_vs_Date.png"}],GraphicsGrid[Partition[PadRight[staFeaturePanels,6,Graphics[{}]],2],ImageSize->1000],ImageResolution->160];
(* Mean normalized response curves, by sensor and period. Each group has different participants. *)
staCurvePanels=Table[Module[{curves,idx},
 curves=Table[idx=Flatten@Position[staPeriod,cat];
 Table[{t,Mean[staProfiles[[idx,s,t]]]},{t,1,Length[staProfiles[[1,s]]]}],{cat,{"Healthy early","Healthy late","Dengue"}}];
 ListLinePlot[curves,Frame->True,Axes->False,PlotStyle->Values[staColors],
 PlotLabel->"Sensor channel "<>ToString[s],FrameLabel->{"Recording index","Relative change vs index 100"},
 PlotLegends->If[s==1,Placed[{"Healthy early","Healthy late","Dengue"},Below],None],ImageSize->420]],{s,4}];
Export[FileNameJoin[{staOut,"Figure_S7_Normalized_Response_Profiles.png"}],GraphicsGrid[Partition[staCurvePanels,2],ImageSize->1050],ImageResolution->160];
Print["Finished sensor temporal audit; output at: ",staOut];
Print["INTERPRETATION: observational associations only; no calibration standards or paired longitudinal reference measurements. Do not claim confirmed sensor drift."];
