(* ::Package:: *)

(* ::Title:: *)
(*Corners*)


(* ::Section:: *)
(*Begin*)


Package["UTCG`"]


PackageExport["UTCreaseEdges"]
PackageExport["UTCornerComponents"]
PackageExport["UTCornerComponentsCellIndices"]
PackageExport["UTHighlightCorners"]
PackageExport["UTCellAngles"]


UTCreaseEdges::usage = "Finds the edge indices that form a crease.";
UTCornerComponents::usage = "Partition a mesh based upon cycles of sharp edges.";
UTCornerComponentsCellIndices::usage = "Partition mesh cell indices based upon cycles of sharp edges.";
UTHighlightCorners::usage = "Highlight corners of a mesh.";
UTCellAngles::usage = "Computes all angles in a triangulated mesh.";


(* ::Section:: *)
(*Utilities*)


(* ::Subsection::Closed:: *)
(*iMeshCornerComponentsCellIndices*)


Options[iMeshCornerComponentsCellIndices] = {"IncludeBoundary" -> True, "TinyComponentThreshold" -> Automatic};


iMeshCornerComponentsCellIndices[mr_, opts:OptionsPattern[]] := iMeshCornerComponentsCellIndices[mr, Automatic, opts]


iMeshCornerComponentsCellIndices[mr_, \[Alpha]spec_, OptionsPattern[]] :=
	Block[{c12orig, c12, hbQ, tinythreshold, hedges1, hedges2, hedges3p, hedges, comps, areas, ord},
		hbQ = TrueQ[OptionValue["IncludeBoundary"]];
		
		{hedges1, hedges2, hedges3p, c12orig} = iMeshCornerEdges[mr, \[Alpha]spec, hbQ];
		hedges = Join[hedges1, hedges2, hedges3p];
		
		If[Length[hedges] == 0, Return[{Range[MeshCellCount[mr, 2]]}]];
		
		c12 = c12orig;
		c12[[hedges, All]] = 0;
		comps = SparseArray`StronglyConnectedComponents[Transpose[c12] . c12];
		
		areas = Total[PropertyValue[{mr, {2, #}}, MeshCellMeasure]]& /@ comps;
		ord = Reverse[Ordering[areas]];
		comps = comps[[ord]];
		areas = areas[[ord]];
		tinythreshold = parseTinyComponentThreshold[OptionValue["TinyComponentThreshold"], areas];
		
		comps = resolveTinyComponents[comps, c12orig, areas, tinythreshold];
		
		comps
	]


(* ::Subsection::Closed:: *)
(*iMeshCornerEdges*)


iMeshCornerEdges[mr_, \[Alpha]spec_, hbQ_, oC12_:None] :=
	Block[{c12, rp, rpts, normals, norms, zp, \[Alpha]lo, \[Alpha]hi, manfoldedges, nonmanfoldedges, ci, rpsub, p1, p2, cos\[Theta], mc, groups, lens, memQ, intervalmems, hedges1, hedges2, hedges3p},
		{\[Alpha]lo, \[Alpha]hi} = meshCornerAngles[\[Alpha]spec];
		
		c12 = If[oC12 === None, mr["ConnectivityMatrix"[1, 2]], oC12];
		rp = c12["RowPointers"];
		rpts = Differences[rp];
		
		normals = faceUnitNormals[mr];
		
		If[TrueQ[hbQ],
			hedges1 = Random`Private`PositionsOf[rpts, 1],
			hedges1 = {}
		];
		
		manfoldedges = Random`Private`PositionsOf[rpts, 2];
		If[Length[manfoldedges] > 0,
			ci = c12["ColumnIndices"];
			rpsub = rp[[manfoldedges + 1]];
			p1 = normals[[Flatten[ci[[rpsub - 1]]]]];
			p2 = normals[[Flatten[ci[[rpsub]]]]];
			
			cos\[Theta] = Total[p1 p2, {2}];
			
			hedges2 = manfoldedges[[Random`Private`PositionsOf[UnitStep[-cos\[Theta] - Cos[\[Alpha]hi]]*UnitStep[cos\[Theta] + Cos[\[Alpha]lo]], 1]]];,
			hedges2 = {};
		];
		
		If[TrueQ[hbQ] && Length[hedges1] + Length[manfoldedges] < MeshCellCount[mr, 1],
			nonmanfoldedges = Complement[Range[MeshCellCount[mr, 1]], hedges1, manfoldedges],
			nonmanfoldedges = {}
		];
		If[Length[nonmanfoldedges] > 0,
			mc = c12[[nonmanfoldedges]]["MatrixColumns"];
			groups = Subsets[#, {2}]& /@ mc;
			lens = Length /@ groups;
			{p1, p2} = Transpose[Partition[normals[[Flatten[groups]]], 2]];
			
			cos\[Theta] = Total[p1 p2, {2}] Power[Total[p1^2, {2}]*Total[p2^2, {2}], -0.5];
			memQ = UnitStep[-cos\[Theta] - Cos[\[Alpha]hi]]*UnitStep[cos\[Theta] + Cos[\[Alpha]lo]];
			memQ = Developer`ToPackedArray[Unitize[Total[Internal`PartitionRagged[memQ, lens], {2}]]];
			intervalmems = Random`Private`PositionsOf[memQ, 1];
			
			hedges3p = nonmanfoldedges[[intervalmems]];,
			hedges3p = {};
		];
		
		{hedges1, hedges2, hedges3p, If[oC12 === None, c12, Nothing]}
	]


meshCornerAngles[spec_] := Clip[iMeshCornerAngles[spec], {0, 2\[Pi]}]


iMeshCornerAngles[{\[Alpha]_?Internal`RealValuedNumericQ, \[Beta]_?Internal`RealValuedNumericQ}] := {\[Alpha], \[Beta]}


iMeshCornerAngles[{\[Alpha]_?Internal`RealValuedNumericQ}] := With[{\[Delta] = 0.001}, {\[Alpha] - \[Delta], \[Alpha] + \[Delta]}]


iMeshCornerAngles[\[Alpha]_?Internal`RealValuedNumericQ] := {0, \[Alpha]}


iMeshCornerAngles[_] = {0, 2.3};


(* ::Subsection::Closed:: *)
(*Tiny Component Thresholding*)


parseTinyComponentThreshold[Automatic, areas_] := 10^-5.


parseTinyComponentThreshold[threshold_, _] /; Quiet[Positive[threshold]] := Clip[threshold, {0, 1}]


parseTinyComponentThreshold[__] = 0;


resolveTinyComponents[comps_, __, t_] /; Positive[t] =!= True := comps


resolveTinyComponents[comps_, _, areas_, t_] /; Min[areas] > t*Max[areas] := comps


resolveTinyComponents[comps_, c12_, areas_, t_] := 
	Block[{smallQ, nbhdlookup, nzp, nbhd, c22, nothing, comps2, j},
		smallQ = Thread[areas < t*Max[areas]];
		nbhdlookup = (Join @@ MapIndexed[ConstantArray[First[#2], Length[#1]]&, comps])[[InversePermutation[Join @@ comps]]];
		
		nbhd = ConstantArray[{}, Length[comps]];
		c22 = Transpose[c12] . c12;
		Do[
			If[smallQ[[i]],
				nzp = c22[[comps[[i]]]]["NonzeroPositions"][[All, -1]];
				nbhd[[i]] = Select[Union[nbhdlookup[[nzp]]], LessThan[i]]
			], 
			{i, Length[comps]}
		];
		
		comps2 = Transpose[{Range[Length[comps]]}];
		Do[
			If[smallQ[[i]] && Length[nbhd[[i]]] > 0,
				j = First[nbhd[[i]]];
				comps2[[j]] = Join[comps2[[j]], comps2[[i]]];
				comps2[[i]] = {};
				nbhd[[i]] = {};
				nbhd = nbhd /. Dispatch[{i -> j}];
			],
			{i, Length[comps], 1, -1}
		];
		
		(Join @@ comps[[#]])& /@ Replace[comps2, {} -> Nothing, {1}]
	]


(* ::Section:: *)
(*UTCreaseEdges*)


(* ::Subsection::Closed:: *)
(*Main*)


Options[UTCreaseEdges] = {"IncludeSingularEdges" -> True, "EdgeFaceConnectivity" -> None};


UTCreaseEdges[mr_, opts:OptionsPattern[]] := UTCreaseEdges[mr, Automatic, opts]


UTCreaseEdges[mr_?UTSurfaceMesh3DQ, \[Alpha]spec_, opts:OptionsPattern[]] := 
	Block[{hbQ, C12, cornerdata},
		hbQ = TrueQ[OptionValue["IncludeSingularEdges"]];
		C12 = OptionValue["EdgeFaceConnectivity"];
		If[!MatrixQ[C12], C12 = None];
		
		cornerdata = iMeshCornerEdges[mr, \[Alpha]spec, hbQ, C12];
		
		(
			If[hbQ,
				Union @@ cornerdata[[2 ;; 3]],
				cornerdata[[2]]
			]
			
		) /; ListQ[cornerdata] && Length[cornerdata] >= 3
	]


UTCreaseEdges[___] = $Failed;


(* ::Section:: *)
(*UTCornerComponents*)


(* ::Subsection::Closed:: *)
(*Main*)


Options[UTCornerComponents] = {"IncludeBoundary" -> True, "TinyComponentThreshold" -> Automatic};


UTCornerComponents[mr_, opts:OptionsPattern[]] := UTCornerComponents[mr, Automatic, opts]


UTCornerComponents[mr_?UTSurfaceMesh3DQ, \[Alpha]spec_, opts:OptionsPattern[]] :=
	Block[{head, comps, coords, cells},
		comps = iMeshCornerComponentsCellIndices[mr, \[Alpha]spec, opts];
		(
			If[Length[comps] == 1, Return[{mr}]];
			
			head = Head[mr];
			coords = MeshCoordinates[mr];
			cells = Join @@ MeshCells[mr, 2, "Multicells" -> True][[All, 1]];
			
			Map[subCellMesh[MeshRegion, coords, {Polygon[cells[[#]]]}]&, comps]
			
		) /; ListQ[comps]
	]


UTCornerComponents[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Argument conform & completion*)


registerForDiscretization[UTCornerComponents, 1, False];


(* ::Section:: *)
(*UTCornerComponentsCellIndices*)


(* ::Subsection::Closed:: *)
(*Main*)


Options[UTCornerComponentsCellIndices] = {"IncludeBoundary" -> True, "TinyComponentThreshold" -> Automatic};


UTCornerComponentsCellIndices[mr_?UTSurfaceMesh3DQ, args___] := 
	With[{res = iMeshCornerComponentsCellIndices[mr, args]},
		res /; ListQ[res]
	]


UTCornerComponentsCellIndices[___] = $Failed;


(* ::Section:: *)
(*UTHighlightCorners*)


(* ::Subsection::Closed:: *)
(*Main*)


Options[UTHighlightCorners] = {"IncludeBoundary" -> True};


UTHighlightCorners[mr_, opts:OptionsPattern[]] := UTHighlightCorners[mr, Automatic, Automatic, opts]


UTHighlightCorners[mr_, \[Alpha]_, opts:OptionsPattern[]] := UTHighlightCorners[mr, \[Alpha], Automatic, opts]


UTHighlightCorners[mr_?UTSurfaceMesh3DQ, \[Alpha]spec_, stylespec_, OptionsPattern[]] :=
	Block[{style, hbQ, hedges1, hedges2, hedges3p},
		style = meshCornerStyle[stylespec];
		hbQ = TrueQ[OptionValue["IncludeBoundary"]];
		
		{hedges1, hedges2, hedges3p} = Most[iMeshCornerEdges[mr, \[Alpha]spec, hbQ]];
		
		If[Length[hedges1] > 0 || Length[hedges2] > 0 || Length[hedges3p] > 0,
			Head[mr][mr, MeshCellStyle -> {{1, Union[hedges1, hedges2, hedges3p]} -> style}],
			mr
		]
	]


UTHighlightCorners[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Argument conform & completion*)


registerForDiscretization[UTHighlightCorners, 1, False];


(* ::Subsection::Closed:: *)
(*Utilities*)


meshCornerStyle[Automatic] = Directive[Opacity[1], Black];


meshCornerStyle[spec_] := Directive[meshCornerStyle[Automatic], spec]


(* ::Section:: *)
(*UTCellAngles*)


(* ::Subsection::Closed:: *)
(*Main*)


UTCellAngles[mr_?UTTriangleMesh3DQ] :=
	Block[{prims, p1, p2, cos, cots, inds, spopt, L},
		prims = Join @@ MeshPrimitives[mr, 2, "Multicells" -> True][[All, 1]];
		
		p1 = prims - RotateRight[prims, {0, 1}];
		p2 = -RotateRight[p1, {0, 1}];
		
		ArcCos[Total[p1 p2, {3}] Power[Total[p1^2, {3}]*Total[p2^2, {3}], -0.5]]
	]
