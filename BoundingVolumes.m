(* ::Package:: *)

(* ::Title:: *)
(*BoundingVolumes*)


(* ::Section:: *)
(*Begin*)


Package["UTCG`"]


PackageExport["UTDimensions"]


UTDimensions::usage = "Compute measurements of various bounding regions of a mesh.";


(* ::Section:: *)
(*UTDimensions*)


(* ::Subsection::Closed:: *)
(*Main*)


UTDimensions[mr_, spec_:Automatic, rt_:Automatic] /; validDimensionInputQ[mr, spec] :=
	Block[{rtval, res},
		rtval = processBoundingDimensionsReturnType[rt, spec];
		(
			res = iMeshDimensions[mr, spec];
			(
				Switch[{AssociationQ[res], rt},
					{False, All}, Association[res],
					{True, All}, Dataset[Association /@ res],
					{False, _}, rtval /. res,
					_, (rtval /. #)& /@ res
				]
		
			) /; res =!= $Failed
			
		) /; rtval =!= $Failed
	]

UTDimensions[___] = $Failed;


iMeshDimensions[mr_, Automatic | "MinCuboid"] := minBoundingCuboid[mr]

iMeshDimensions[mr_, "MinVolumeOrientedCuboid"] := minVolumeOrientedCuboid[mr]

iMeshDimensions[mr_, "MinSidedBaseOrientedCuboid"] := minSidedBaseOrientedCuboid[mr]

iMeshDimensions[mr_, "MinSidedBaseCuboid"] := minSidedBaseCuboid[mr]

iMeshDimensions[mr_, "MinBall"] := minBoundingBall[mr]

iMeshDimensions[mr_, All] := 
	With[{keys = {"MinCuboid", "MinVolumeOrientedCuboid", "MinSidedBaseOrientedCuboid", "MinSidedBaseCuboid", "MinBall"}},
		With[{res = iMeshDimensions[mr, #]& /@ keys},
			AssociationThread[keys, res] /; FreeQ[res, $Failed, {1}]
		]
	]

iMeshDimensions[___] = $Failed;


processBoundingDimensionsReturnType[rt:Except[_List], spec_] := Replace[processBoundingDimensionsReturnType[{rt}, spec], {s_} :> s, {0}]

processBoundingDimensionsReturnType[{o1___, Automatic, o2___}, spec:"MinVolumeCuboid" | "MinSidedBaseCuboid" | "MinSidedBaseOrientedCuboid"] := 
	processBoundingDimensionsReturnType[{o1, "Dimensions", "AlignmentTransform", o2}, spec]

processBoundingDimensionsReturnType[{o1___, Automatic, o2___}, spec_] := 
	processBoundingDimensionsReturnType[{o1, "Dimensions", o2}, spec]

processBoundingDimensionsReturnType[{o1___, All, o2___}, spec_] := 
	processBoundingDimensionsReturnType[{o1, "Dimensions", "AlignmentTransform", "BoundingRegion", o2}, spec]

processBoundingDimensionsReturnType[rt_List, _] := Cases[rt, "Dimensions" | "AlignmentTransform" | "BoundingRegion"]

processBoundingDimensionsReturnType[___] = $Failed;


validDimensionInputQ[reg_, Automatic | "MinCuboid"] := ConstantRegionQ[reg]


validDimensionInputQ[mr_, _] := UTMesh3DQ[mr]


(* ::Subsection::Closed:: *)
(*Argument conform & completion*)


addCodeCompletion[UTDimensions][None, {"MinCuboid", "MinVolumeOrientedCuboid", "MinSidedBaseOrientedCuboid", "MinSidedBaseCuboid", "MinBall"}];


registerForDiscretization[UTDimensions, 1, False];


(* ::Subsection::Closed:: *)
(*minBoundingCuboid*)


minBoundingCuboid[reg_] :=
	With[{bds = RegionBounds[reg]},
		(
			{
				"Dimensions" -> Abs[Subtract @@@ bds], 
				"AlignmentTransform" -> TranslationTransform[-bds[[All, 1]]], 
				"BoundingRegion" -> Cuboid @@ Transpose[bds]
			}
			
		) /; MatrixQ[bds, NumericQ]
	]


minBoundingCuboid[___] = $Failed;


(* ::Subsection::Closed:: *)
(*minBoundingBall*)


minBoundingBall[reg_] :=
	With[{br = BoundingRegion[reg, "MinBall"], d = RegionEmbeddingDimension[reg]},
		(
			{
				"Dimensions" -> ConstantArray[2Last[br], d], 
				"AlignmentTransform" -> TranslationTransform[-RegionBounds[reg][[All, 1]]], 
				"BoundingRegion" -> br
			}
			
		) /; MatchQ[br, (Sphere|Ball)[_, _]]
	]


minBoundingBall[___] = $Failed;


(* ::Subsection::Closed:: *)
(*minVolumeOrientedCuboid*)


minVolumeOrientedCuboid[mr_] :=
	Block[{br = BoundingRegion[mr, "MinOrientedCuboid"], d, p, v, nv, cornersi, cornerso, \[CurlyEpsilon], tf},
		(
			d = RegionEmbeddingDimension[mr];
			{p, v} = List @@ br;
			v = SortBy[v, Norm];
			nv = (Norm /@ v)IdentityMatrix[d];
			
			cornersi = Table[p + t . v, {t, Tuples[{0, 1}, d]}];
			cornerso = Table[t . nv, {t, Tuples[{0, 1}, d]}];
			
			{\[CurlyEpsilon], tf} = FindGeometricTransform[cornerso, cornersi];
			
			{
				"Dimensions" -> Diagonal[nv], 
				"AlignmentTransform" -> tf, 
				"BoundingRegion" -> br
			}
			
		) /; MatchQ[Head[br], Parallelogram | Parallelepiped]
	]


minVolumeOrientedCuboid[___] = $Failed;


(* ::Subsection::Closed:: *)
(*minSidedBaseOrientedCuboid*)


(* ::Text:: *)
(*TODO -- re-implement!*)


Clear[minSidedBaseOrientedCuboid];
minSidedBaseOrientedCuboid[mr_] :=
	Block[{coords, rotdata, \[Alpha], \[Beta], \[Gamma], \[CurlyPhi], tf, cbds, dims, br},
		coords = MeshCoordinates[mr];
		rotdata = iMinSidedBaseOrientedCuboid[Quiet @ MeshCoordinates[ConvexHullMesh[coords]]];
		(
			{\[Alpha], \[Beta], \[Gamma], \[CurlyPhi]} = rotdata;
			tf = RotationTransform[\[CurlyPhi], {0, 0, 1}] @* AffineTransform[RollPitchYawMatrix[{\[Alpha], \[Beta], \[Gamma]}]];
			coords = tf[coords];
			cbds = CoordinateBounds[coords];
			
			dims = Abs[Subtract @@@ cbds];
			br = InverseTransformedRegion[BoundingRegion[coords], tf];
			tf = TranslationTransform[-cbds[[All, 1]]] @* tf;
			
			{
				"Dimensions" -> dims, 
				"AlignmentTransform" -> tf, 
				"BoundingRegion" -> br
			}
			
		) /; VectorQ[rotdata, NumericQ] && Length[rotdata] == 4
	]


minSidedBaseOrientedCuboid[___] = $Failed;


iMinSidedBaseOrientedCuboid[coords_?MatrixQ] :=
	Block[{\[Alpha], \[Beta], \[Gamma], \[CurlyPhi], mindata, rpyangles, smallrotdata},
		mindata = Quiet[NMinimize[{rotXYBounds[\[Alpha], \[Beta], \[Gamma], coords], -\[Pi]/2 < \[Alpha] < \[Pi]/2 && -\[Pi]/2 < \[Beta] < \[Pi]/2 && -\[Pi]/2 < \[Gamma] < \[Pi]/2}, {\[Alpha], \[Beta], \[Gamma]}]];
		(
			rpyangles = {\[Alpha], \[Beta], \[Gamma]} /. Last[mindata];
			smallrotdata = optimalXYRot[RollPitchYawMatrix[rpyangles] . Transpose[coords]];
			If[smallrotdata === $Failed || First[smallrotdata] > First[mindata],
				\[CurlyPhi] = 0.,
				\[CurlyPhi] = Last[smallrotdata];
			];
			
			Append[rpyangles, \[CurlyPhi]]
			
		) /; validFindMinimumResultQ[mindata]
	]


iMinSidedBaseOrientedCuboid[___] = $Failed;


With[{rot = RotationMatrix[\[CurlyPhi], {0, 0, 1}]},
	UTCompile[rotXYBoundsphiC, {{\[CurlyPhi], _Real}, {coords, _Real, 2}},
		Block[{data},
			data = Most[rot . coords];
			Subtract[Max[First[data]] + Min[Last[data]], Min[First[data]] + Max[Last[data]]]
		]
	];
];


optimalXYRot[coords_] :=
	Block[{f, plot, brackets, roots, xys},
		f[x_?NumericQ] := rotXYBoundsphiC[x, coords];
		
		plot = Visualization`Core`Plot[
			f[x], {x, 0, 2\[Pi]},
			Exclusions -> f[x] == 0,
			ExclusionsStyle -> Black,
			PlotStyle -> Green
		];
		
		brackets = First[Cases[plot, {___, Black, l__Line} :> {l}[[All, 1, All, 1]], \[Infinity]], {}];
		
		(
			roots = x /. FindRoot[f[x], {x, ##}]& @@@ brackets;
			(
				xys = Max[Most[Abs[Subtract @@@ MinMax /@ (RotationMatrix[#, {0, 0, 1}] . coords)]]]& /@ roots;
				
				First[MinimalBy[Transpose[{xys, roots}], First]]
				
			) /; VectorQ[roots, NumberQ]
			
		) /; Length[brackets] > 0
	]


optimalXYRot[___] = $Failed;


validFindMinimumResultQ[{_Real, {(_ -> _Real)..}}] = True;
validFindMinimumResultQ[___] = False;


With[{M = RollPitchYawMatrix[{\[Alpha], \[Beta], \[Gamma]}]},
	UTCompile[rotXYBoundsC, {{\[Alpha], _Real}, {\[Beta], _Real}, {\[Gamma], _Real}, {coords, _Real, 2}},
		Block[{data = Most[M . Transpose[coords]]},
			Max[Max[First[data]] - Min[First[data]], Max[Last[data]] - Min[Last[data]]]
		]
	]
];


rotXYBounds[\[Alpha]_?NumericQ, \[Beta]_, \[Gamma]_, coords_] := rotXYBoundsC[\[Alpha], \[Beta], \[Gamma], coords]


(* ::Subsection::Closed:: *)
(*minSidedBaseCuboid*)


(* ::Text:: *)
(*TODO -- re-implement!*)


minSidedBaseCuboid[mr_] :=
	Block[{coords, \[Theta], tf, cbds, dims, br},
		coords = MeshCoordinates[mr];
		\[Theta] = iMinSidedBaseCuboid[ConvexHullMesh[coords[[All, 1;;2]]]];
		(
			tf = RotationTransform[\[Theta], {0, 0, 1}];
			coords = tf[coords];
			cbds = CoordinateBounds[coords];
			
			dims = Abs[Subtract @@@ cbds];
			br = InverseTransformedRegion[BoundingRegion[coords], tf];
			tf = TranslationTransform[-cbds[[All, 1]]] @* tf;
			
			{
				"Dimensions" -> dims, 
				"AlignmentTransform" -> tf, 
				"BoundingRegion" -> br
			}
			
		) /; NumericQ[\[Theta]]
	]


minSidedBaseCuboid[___] = $Failed;


iMinSidedBaseCuboid[bmr_?BoundaryMeshRegionQ] :=
	Block[{coords, res},
		coords = MeshCoordinates[bmr];
		
		res = optimalXYRot2D[Transpose[coords]];
		
		Last[res] /; res =!= $Failed
	]


iMinSidedBaseCuboid[___] = $Failed;


With[{rot = RotationMatrix[\[Theta]]},
	UTCompile[rotXYBoundsphiC2D, {{\[Theta], _Real}, {coords, _Real, 2}},
		Block[{data},
			data = rot . coords;
			Subtract[Max[First[data]] + Min[Last[data]], Min[First[data]] + Max[Last[data]]]
		]
	];
];


optimalXYRot2D[coords_] :=
	Block[{f, plot, brackets, roots, xys},
		f[x_?NumericQ] := rotXYBoundsphiC2D[x, coords];
		
		plot = Visualization`Core`Plot[
			f[x], {x, 0, 2\[Pi]},
			Exclusions -> f[x] == 0,
			ExclusionsStyle -> Black,
			PlotStyle -> Green
		];
		
		brackets = First[Cases[plot, {___, Black, l__Line} :> {l}[[All, 1, All, 1]], \[Infinity]], {}];
		
		(
			roots = x /. FindRoot[f[x], {x, ##}]& @@@ brackets;
			(
				xys = Max[Most[Abs[Subtract @@@ MinMax /@ (RotationMatrix[#] . coords)]]]& /@ roots;
				
				First[MinimalBy[Transpose[{xys, roots}], First]]
				
			) /; VectorQ[roots, NumberQ]
			
		) /; Length[brackets] > 0
	]


optimalXYRot[___] = $Failed;
