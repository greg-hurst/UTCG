(* ::Package:: *)

(* ::Title:: *)
(*Repair*)


(* ::Section:: *)
(*Begin*)


Package["UTCG`"]


PackageExport["UTResolveIntersections"]
PackageExport["UTHull"]
PackageExport["UTPruneHangingFaces"]
PackageExport["UTPruneHangingComponents"]


UTResolveIntersections::usage = "Split self intersecting faces of multiple triangulated meshes.";
UTHull::usage = "Join multiple triangulated meshes by taking their hull.";
UTPruneHangingFaces::usage = "Delete components of a mesh with both nonmanifold and boundary edges.";
UTPruneHangingComponents::usage = "Delete small singular components of a mesh.";


(* ::Section:: *)
(*UTResolveIntersections*)


(* ::Subsection::Closed:: *)
(*Main*)


Options[UTResolveIntersections] = Join[
	Options[intersectedSurfaceJoin], 
	{"IntersectionStyle" -> {}},
	Options[MeshRegion]
]


UTResolveIntersections[mr__?UTTriangleMesh3DCollectionQ, opts:OptionsPattern[]] :=
	Block[{join = intersectedSurfaceJoin[Flatten[{mr}], FilterRules[{opts}, Options[intersectedSurfaceJoin]]], hStyle, se},
		(
			hStyle = OptionValue["IntersectionStyle"];
			If[!emptySeamStyleQ[hStyle], 
				se = Random`Private`PositionsOf[Differences[join["ConnectivityMatrix"[1, 2]]["RowPointers"]] - 3, Positive];
				join = MeshRegion[join, MeshCellStyle -> {{1, se} -> processHighlightStyle[hStyle]}, BaseStyle -> Opacity[.3]]
			];
			
			Head[join][join, FilterRules[{opts}, Options[MeshRegion]]]
			
		 ) /; RegionQ[join]
	]


UTResolveIntersections[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Argument conform & completion*)


registerForDiscretization[UTResolveIntersections];


(* ::Subsection::Closed:: *)
(*Utilities*)


emptySeamStyleQ[None] = True;
emptySeamStyleQ[False] = True;
emptySeamStyleQ[{}] = True;
emptySeamStyleQ[_] = False;


(* ::Section:: *)
(*UTHull*)


(* ::Subsection::Closed:: *)
(*Main*)


(* ::Text:: *)
(*TODO have option to delete nested parts*)


Options[UTHull] = Join[
	Options[intersectedSurfaceJoin], 
	{"RepairNormals" -> False, "IntersectionStyle" -> {}, "ReturnComplement" -> False},
	Options[MeshRegion]
];
SetOptions[UTHull, "ReturnSolid" -> False];


UTHull[mr__?UTTriangleMesh3DCollectionQ, opts:OptionsPattern[]] :=
	Block[{pre, join, hull, complementQ, solidQ, hStyle},
		If[TrueQ[OptionValue["RepairNormals"]],
			pre = repairNormals /@ Flatten[{mr}],
			pre = Flatten[{mr}]
		];
		join = intersectedSurfaceJoin[pre, FilterRules[{opts}, Options[intersectedSurfaceJoin]]];
		(
			complementQ = TrueQ[OptionValue["ReturnComplement"]];
			solidQ = OptionValue["ReturnSolid"];
			solidQ = TrueQ @ If[solidQ === Automatic, MinMax[Differences[join["ConnectivityMatrix"[1, 2]]["RowPointers"]]] == {2, 2}, solidQ];
			hStyle = OptionValue["IntersectionStyle"];
			
			hull = iMeshHull[join, complementQ, solidQ, hStyle];
			
			applyMeshOptions[hull, opts]
			
		) /; RegionQ[join]
	]


UTHull[___] = $Failed;


applyMeshOptions[expr_] := expr


applyMeshOptions[mr_?UTMeshQ, opts__] := Head[mr][mr, FilterRules[{opts}, Options[MeshRegion]]]


applyMeshOptions[expr_, ___] := expr


(* ::Subsection::Closed:: *)
(*Argument conform & completion*)


registerForDiscretization[UTHull];


(* ::Subsection::Closed:: *)
(*iMeshHull*)


iMeshHull[mr_, complementQ_, solidQ_:False, hStyle_:False] :=
	Block[{C12, C12sep, C22, seamedges, comps, normals, cells, clookup, seamlookup, seenEdge, seenCC, extremecc, keepinds, cents, 
		compsQueue, cc, edgeiters, next, cseen, eseen, faces, f, cf, nf, na, \[Alpha], data, adjacentcomponents, af, ab, group},
		C12 = mr["ConnectivityMatrix"[1, 2]];
		
		seamedges = Random`Private`PositionsOf[Subtract[2, Differences[C12["RowPointers"]]], NonPositive];
		If[Length[seamedges] == 0, Return[If[TrueQ[complementQ], EmptyRegion[3], mr]]];
		
		C12sep = Quiet[C12 * UnitStep[Subtract[2, Differences[C12["RowPointers"]]]]];
		C22 = Transpose[C12sep] . C12sep;
		comps = SparseArray`StronglyConnectedComponents[C22];
		
		normals = faceUnitNormals[mr];
		cells = Catenate[MeshCells[mr, 2, "Multicells" -> True][[All, 1]]];
		
		clookup = Association[Join @@ Thread /@ Thread[comps -> Range[Length[comps]]]];
		seamlookup = Flatten /@ Reap[Sow[seamedges[[#1]], clookup[#2]]& @@@ C12[[seamedges]]["NonzeroPositions"];, Range[Length[comps]]][[-1]];
		
		adjacentcomponents = ConnectedComponents[Union[Flatten[UndirectedEdge @@@ Subsets[Sort[Lookup[clookup, #]], {2}]& /@ C12[[seamedges]]["MatrixColumns"]]]];
		af = Association[Flatten[Thread /@ Thread[adjacentcomponents -> Range[Length[adjacentcomponents]]]]];
		ab = Association[Thread[Range[Length[adjacentcomponents]] -> adjacentcomponents]];
		
		seenEdge = AssociationThread[seamedges, 0];
		seenCC = AssociationThread[Range[Length[comps]], 0];
		
		cents = PropertyValue[{mr, 2}, MeshCellCentroid][[All, 1]];
		cents = Min[cents[[#]]]& /@ comps;
		cents = Sort[AssociationThread[Range[Length[comps]], cents]];
		
		keepinds = Union @@ Reap[
			Scan[(Sow[#]; seenCC[#] = 1; cents[#]=.)&, Random`Private`PositionsOf[Length /@ seamlookup, 0]];
			
			While[Length[cents] > 0,
			
			extremecc = First[Keys[cents[[1;;1]]]];
			
			seenCC[extremecc] = 1;
			cents[extremecc]=.;
			Sow[extremecc];
			compsQueue = <|extremecc -> extremecc|>;
			
			While[Length[compsQueue] != 0,
				cc = First[compsQueue];
				compsQueue = Rest[compsQueue];
				edgeiters = Pick[seamlookup[[cc]], Lookup[seenEdge, seamlookup[[cc]]], 0];
				{next, cseen, eseen} = hullReap @ Union[Flatten@Table[
					faces = C12[[e]]["NonzeroPositions"][[All, -1]];
					faces = RotateLeft[faces, Random`Private`PositionsOf[clookup /@ faces, cc][[1]] - 1];
					f = First[faces];
					cf = cells[[f]];
					nf = normals[[f]];
					na = normalsAligned[cf, cells[[Rest[faces]]]];
					\[Alpha] = MapThread[Mod[#2 ArcCos[Clip[#2 nf . normals[[#]] Power[Total[nf^2]*Total[normals[[#]]^2], -0.5], {-1., 1.}]], 2.0\[Pi]]&, {Rest[faces], na}];
					(*\[Alpha] = na/MapThread[nf.normals[[#]] Power[Total[nf^2]*Total[normals[[#]]^2], -0.5]&, {Rest[faces], na}];*)
					data = Sort[Transpose[{\[Alpha], na, Range[2, Length[faces]]}]];
					compSow[Lookup[clookup, faces]];
					If[data[[1, 2]] == -1, clookup[faces[[data[[1, 3]]]]], clookup[faces[[data[[-1, 3]]]]]]
					(*Switch[data[[{1, -1}, 2]],
						{-1, -1}, 
							compSow[Lookup[clookup, faces[[data[[{1, -1}, 3]]]]]],
						{-1, _}, 
							compSow[Lookup[clookup, faces]];
							edgeSow[e];
							clookup[faces[[data[[1, 3]]]]],
						_, 
							compSow[Lookup[clookup, faces]];
							edgeSow[e];
							clookup[faces[[data[[-1, 3]]]]]
					]*),
					{e, edgeiters}
				]];
				
				next = Pick[next, Lookup[seenCC, next], 0];
				Scan[(seenCC[#] = 1; cents[#]=.)&, cseen];
				Scan[(seenEdge[#] = 1)&, edgeiters];
				Do[AppendTo[compsQueue, n -> n], {n, next}];
				Scan[Sow, next];
			];
			
			group = af[extremecc];
			Scan[(seenCC[#] = 1; cents[#]=.)&, ab[group]];
			
			]
			
		][[-1]];
		
		If[TrueQ[complementQ], 
			keepinds = Complement[Range[Length[comps]], keepinds]
		];
		constructHullMesh[mr, Catenate[comps[[keepinds]]], solidQ, hStyle, seamedges]
	]


UTCompile[normalsAligned, {{c1, _Integer, 1}, {c2, _Integer, 1}},
	Module[{int, x, y, c3 = c1, c4 = c2},
		int = Intersection[c1, c2];
		x = int[[1]];
		y = int[[2]];
		While[c3[[3]] == x || c3[[3]] == y,
			c3 = c3[[{2, 3, 1}]]
		];
		While[c4[[3]] == x || c4[[3]] == y,
			c4 = c4[[{2, 3, 1}]]
		];
		
		(-1)^Boole[c3[[1]] != c4[[1]]]
	],
	RuntimeAttributes -> {Listable},
	Parallelization -> False
];


(* ::Subsection::Closed:: *)
(*constructHullMesh*)


(* ::Text:: *)
(*TODO actually map the non-manifold edges to their new indices when styling...*)


constructHullMesh[mr_, cells_, solidQ_, _?emptySeamStyleQ, ___] := 
	UTLaxBlock[
		Quiet @ If[TrueQ[solidQ], BoundaryMeshRegion, MeshRegion][MeshCoordinates[mr], MeshCells[mr, {2, cells}, "Multicells" -> True]]
	]


constructHullMesh[mr_, cells_, solidQ_, hStyle_, seamedges_] :=
	Block[{res = constructHullMesh[mr, cells, solidQ, None, {}], nmedgesnew, directive},
		(
			nmedgesnew = Nearest[PropertyValue[{res, 1}, MeshCellCentroid] -> {"Index", "Distance"}, PropertyValue[{mr, {1, seamedges}}, MeshCellCentroid]][[All, 1]];
			nmedgesnew = Pick[#1, Unitize[Threshold[#2]], 1]& @@ Transpose[nmedgesnew];
			
			directive = processHighlightStyle[hStyle];
			
			Head[res][res, MeshCellStyle -> {{1, nmedgesnew} -> directive}, BaseStyle -> Opacity[.3], PlotTheme -> "Minimal"]
			
		) /; RegionQ[res]
	]


constructHullMesh[___] = $Failed;


processHighlightStyle[True] := processHighlightStyle[Black]


processHighlightStyle[spec_List] := processHighlightStyle[Directive @@ spec]


processHighlightStyle[spec_] := Flatten[Directive[Opacity[1], spec], 1, Directive]


(* ::Subsection::Closed:: *)
(*repairNormals*)


repairNormals[mr_MeshRegion] /; closedMeshQ[mr] := 
	With[{bmr = UTToBMR[mr]},
		FastMeshRegion[MeshCoordinates[bmr], MeshCells[bmr, 2, "Multicells" -> True]] /; BoundaryMeshRegionQ[bmr]
	]


repairNormals[mr_MeshRegion] := RepairMesh[mr, "FlippedFaces"]


repairNormals[bmr_BoundaryMeshRegion] := bmr


closedMeshQ[mr_] := MinMax[Differences[mr["ConnectivityMatrix"[1, 2]]["RowPointers"]]] === {2, 2}


(* ::Subsection::Closed:: *)
(*Reap / Sow*)


$comp = "comp";
$edge = "edge";


SetAttributes[hullReap, HoldFirst];


hullReap[code_] := Prepend[#2, #1]& @@ Reap[code, {$comp, $edge}, Union[Flatten[#2]]&]


compSow[expr_] := Sow[expr, $comp]
edgeSow[expr_] := Sow[expr, $edge]


(* ::Subsection::Closed:: *)
(*parallelUTHull \[LongDash] Scratch*)


parallelUTHull[meshes_, opts___] /; initializeParallelUT[] :=
	Block[{kercnt, binsz, binspec, binrng, phulls, hull},
		kercnt = $KernelCount;
		(
			binsz = Quotient[Length[meshes], kercnt];
			binspec = ReplacePart[Range[binsz, kercnt*binsz, binsz], -1 -> -1];
			binrng = (#1+1 ;; #2)& @@@ Partition[Prepend[binspec, 0], 2, 1];
			
			Print[AbsoluteTiming[phulls = ParallelMap[UTHull[UTLaxBlock[#], "ReturnSolid" -> False, opts]&, UTJoin[meshes[[#]]]& /@ binrng, Method -> "CoarsestGrained"];]];
			(
				Print[AbsoluteTiming[hull = UTHull[phulls, opts];]];
				
				hull /; RegionQ[hull]
				
			) /; VectorQ[phulls, RegionQ]
			
		) /; kercnt > 1 && Length[meshes] >= 2kercnt
	]

parallelUTHull[___] = $Failed;


(* ::Text:: *)
(*Loader broken now. Does not load the full library on the subkernels.*)


With[{path = $InputFileName},
	initializeParallelUT[] :=
		Block[{},
			If[$KernelCount == 0, Quiet[LaunchKernels[]]];
			
			If[$KernelCount == 0, Return[False]];
			ParallelEvaluate[Get[path]];
			
			True
		]
	]


(* ::Section:: *)
(*UTPruneHangingFaces*)


(* ::Subsection::Closed:: *)
(*Main*)


UTPruneHangingFaces[mr_?UTSurfaceMesh3DQ, areapercentage_:0.025, iter_:Infinity] /; areapercentage >= 0 && iter >= 0 := 
	With[{res = iPruneHangingFaces[mr, areapercentage, iter]},
		res[[1]] /; res =!= $Failed
	]


UTPruneHangingFaces[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Argument conform & completion*)


registerForDiscretization[UTPruneHangingFaces, 1];


(* ::Subsection::Closed:: *)
(*iPruneHangingFaces*)


iPruneHangingFaces[mr_, fac_, iter_] :=
	Block[{cnt = 0, C12, border, comps, cassoc, ccands, borderfaces, delete, measures, area},
		C12 = mr["ConnectivityMatrix"[1, 2]];
		border = Random`Private`PositionsOf[Differences[C12["RowPointers"]], 1];
		If[Length[border] == 0, Return[{mr, False}]];
		
		measures = PropertyValue[{mr, 2}, MeshCellMeasure];
		area = Total[measures];
		
		With[{partition = Quiet[C12 * UnitStep[2 - Differences[C12["RowPointers"]]]]},
			comps = SparseArray`StronglyConnectedComponents[Transpose[partition] . partition];
			cassoc = Association[Flatten[Thread /@ Thread[comps -> Range[Length[comps]]]]];
		];
		
		delete = Flatten @ Last @ Reap[
			While[cnt++ < iter,
				borderfaces = C12[[border]]["NonzeroPositions"][[All, -1]];
				C12[[All, borderfaces]] = 0;
				
				Scan[
					If[Total[measures[[comps[[#]]]]] < fac*area, Sow[#]]&,
					Union[Lookup[cassoc, borderfaces]]
				];
			
				border = Complement[Random`Private`PositionsOf[Differences[C12["RowPointers"]], 1], border];
				If[Length[border] == 0, Break[]];
			]
		];
		
		If[Length[delete] == 0, Return[{mr, False}]];
		
		comps = Join @@ Delete[comps, List /@ delete];
		If[Length[comps] == 0, 
			{EmptyRegion[3], True},
			{MeshRegion[MeshCoordinates[mr], MeshCells[mr, {2, comps}, "Multicells" -> True], Method -> {"DeleteDuplicateCoordinates" -> False, "DeleteDuplicateCells" -> False}], True}
		]
	]


iPruneHangingFaces[___] = $Failed;


(* ::Section:: *)
(*UTPruneHangingComponents*)


(* ::Subsection::Closed:: *)
(*Main*)


UTPruneHangingComponents[mr_?UTSurfaceMesh3DQ, areapercentage_:0.025] /; Internal`RealValuedNumericQ[areapercentage] := 
	With[{res = iPruneHangingComponents[mr, areapercentage]},
		res /; RegionQ[res]
	]


UTPruneHangingComponents[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Argument conform & completion*)


registerForDiscretization[UTPruneHangingComponents, 1];


(* ::Subsection::Closed:: *)
(*iPruneHangingComponents*)


iPruneHangingComponents[mr_, _?NonPositive] := mr


iPruneHangingComponents[mr_, _?(GreaterThan[1])] = EmptyRegion[3];


iPruneHangingComponents[mr_, areapercentage_] := 
	Block[{C12, nonSingQ, C22, comps, coords, cells, areas, fac, subcells},
		C12 = mr["ConnectivityMatrix"[1, 2]];
		nonSingQ = UnitStep[Subtract[2, Differences[C12["RowPointers"]]]];
		
		(* short circuit clean meshes *)
		If[Min[nonSingQ] === 1,
			Return[mr]
		];
		
		C12 = Quiet[C12 * nonSingQ];
		C22 = Transpose[C12] . C12;
		comps = SparseArray`StronglyConnectedComponents[C22];
		
		areas = Total[PropertyValue[{mr, {2, #}}, MeshCellMeasure]]& /@ comps;
		fac = areapercentage * Total[areas];
		
		coords = MeshCoordinates[mr];
		cells = Join @@ MeshCells[mr, 2, "Multicells" -> True][[All, 1]];
		subcells = cells[[Join @@ Pick[comps, UnitStep[areas - fac], 1]]];
		
		subCellMesh[MeshRegion, coords, {Polygon[subcells]}]
	]
