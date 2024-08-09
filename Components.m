(* ::Package:: *)

(* ::Title:: *)
(*Components*)


(* ::Section:: *)
(*Begin*)


Package["UTCG`"]


PackageExport["UTSingularComponents"]
PackageExport["UTBoundaryEdges"]
PackageExport["UTSubMesh"]
PackageExport["UTHighlightBoundaryEdges"]


UTSingularComponents::usage = "Splits a mesh into multiple components at singular seams.";
UTBoundaryEdges::usage = "Finds the 1D topological boundary of a surface.";
UTSubMesh::usage = "Take subparts of a mesh.";
UTHighlightBoundaryEdges::usage = "Highlight the 1D topological boundary of a mesh.";


(* ::Section:: *)
(*UTSingularComponents*)


(* ::Subsection::Closed:: *)
(*Main*)


UTSingularComponents[mr_MeshRegion?MeshRegionQ, n_:All] /; RegionEmbeddingDimension[mr] > 1 && RegionDimension[mr] < 3 :=
	Block[{comps, splits},
		comps = dimensionalComponents[mr];
		splits = singularComponents[#2, #1, n]& @@@ comps;
		(
			If[Length[splits] == 1,
				First[splits],
				Join @@ Reverse[splits]
			]
			
		) /; MatchQ[splits, {{___?RegionQ}..}]
	]


UTSingularComponents[bmr_BoundaryMeshRegion?BoundaryMeshRegionQ, n_:All] /; RegionEmbeddingDimension[bmr] > 1 :=
	Block[{splits},
		splits = singularComponents[bmr, RegionEmbeddingDimension[bmr]-1, n];
		splits /; MatchQ[splits, {___?RegionQ}]
	]


UTSingularComponents[mr_MeshRegion?MeshRegionQ, n_:All] /; RegionEmbeddingDimension[mr] > 1 && RegionDimension[mr] === 3 := UTSingularComponents[RegionBoundary[mr], n]


UTSingularComponents[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Argument conform & completion*)


registerForDiscretization[UTSingularComponents, 1, False];


(* ::Subsection::Closed:: *)
(*iSingularMeshSplit*)


singularComponents[_EmptyRegion, ___] = {};


singularComponents[mr_, 0, n_] := 
	With[{comps = ConnectedMeshComponents[mr]}, 
		If[IntegerQ[n] && n > 0,
			comps[[1 ;; Min[Length[comps], n]]],
			comps
		]
	]


singularComponents[mr_, 1, n_] :=
	Block[{C01, C11, comps, coords, m, lens, ord},
		C01 = mr["ConnectivityMatrix"[0, 1]];
		C01 = Quiet[C01 * UnitStep[Subtract[2, Differences[C01["RowPointers"]]]]];
		
		C11 = Transpose[C01] . C01;
		comps = SparseArray`StronglyConnectedComponents[C11];
		
		m = If[IntegerQ[n] && n > 0, 
			Min[Length[comps], n],
			Length[comps]
		];
		lens = Total[PropertyValue[{mr, {1, #}}, MeshCellMeasure]]& /@ comps;
		ord = Ordering[lens, -m];
		
		coords = MeshCoordinates[mr];
		constructPathCurve[coords, MeshCells[mr, {1, #}, "Multicells" -> True][[1, 1]]]& /@ comps[[ord]]
	]


singularComponents[mr_, 2, n_] :=
	Block[{C12, C22, comps, coords, cells, m, areas, ord},
		C12 = mr["ConnectivityMatrix"[1, 2]];
		C12 = Quiet[C12 * UnitStep[Subtract[2, Differences[C12["RowPointers"]]]]];
		
		C22 = Transpose[C12] . C12;
		comps = SparseArray`StronglyConnectedComponents[C22];
		
		(* short circuit clean mesh with a single component *)
		If[Length[comps] == 1 && (!IntegerQ[n] || n == 1),
			Return[{mr}]
		];
		
		m = If[IntegerQ[n] && n > 0, 
			Min[Length[comps], n],
			Length[comps]
		];
		areas = Total[PropertyValue[{mr, {2, #}}, MeshCellMeasure]]& /@ comps;
		ord = Ordering[areas, -m];
		
		coords = MeshCoordinates[mr];
		cells = Join @@ MeshCells[mr, 2, "Multicells" -> True][[All, 1]];
		Map[subCellMesh[MeshRegion, coords, {Polygon[cells[[#]]]}]&, comps[[ord]]]
	]


singularComponents[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Utilities*)


dimensionalComponents[mr_] :=
	Switch[Union[Flatten[mr["MeshCellTypes"]][[1 ;; -1 ;; 3]]],
		{Polygon}, {2 -> mr},
		{Line}, {1 -> mr},
		{Point}, {0 -> mr},
		_, Thread[Range[0, Length[#]-1] -> #]&[DimensionalMeshComponents[mr]]
	]


constructPathCurve[coords_, segs_] :=
	Block[{g, pathQ, path},
		g = Graph[UndirectedEdge @@@ segs];
		pathQ = Min[VertexDegree[g]] === 1;
		
		path = If[pathQ,
			FindShortestPath[g, ##]& @@ GraphPeriphery[g],
			FindCycle[g][[1, All, 1]]
		];
		
		MeshRegion[coords[[path]], Line[If[pathQ, #, Append[#, 1]]&[Range[Length[path]]]]]
	]


(* ::Section:: *)
(*UTBoundaryEdges*)


(* ::Subsection::Closed:: *)
(*Main*)


Options[UTBoundaryEdges] = {"IncludeSingularEdges" -> True};


UTBoundaryEdges[mr_?UTSurfaceMesh3DQ, OptionsPattern[]] :=
	Block[{singQ, c12, rpts, hedges1, hedges3p},
		singQ = TrueQ[OptionValue["IncludeSingularEdges"]];
		
		c12 = mr["ConnectivityMatrix"[1, 2]];
		rpts = Differences[c12["RowPointers"]];
		
		hedges1 = Random`Private`PositionsOf[rpts, 1];
		If[singQ, 
			hedges3p = Random`Private`PositionsOf[rpts - 3, Positive],
			hedges3p = {}
		];
		
		If[Length[hedges1] > 0 || Length[hedges3p] > 0,
			MeshRegion[MeshCoordinates[mr], MeshCells[mr, {1, Union[hedges1, hedges3p]}]],
			EmptyRegion[3]
		]
	]


UTBoundaryEdges[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Argument conform & completion*)


registerForDiscretization[UTBoundaryEdges, 1, False];


(* ::Section:: *)
(*UTSubMesh*)


(* ::Subsection::Closed:: *)
(*PackageScopes*)


PackageScope["subCellMesh"]


(* ::Subsection::Closed:: *)
(*Main*)


UTSubMesh[mr_?UTMeshQ, icellspec_] :=
	Block[{coords, cellspec, cells, res},
		coords = MeshCoordinates[mr];
		cellspec = If[VectorQ[icellspec], 
			{RegionDimension[mr] - Boole[BoundaryMeshRegionQ[mr]], Union[icellspec]}, 
			icellspec
		];
		cells = MeshCells[mr, cellspec, "Multicells" -> True];
		
		res = subCellMesh[Head[mr], coords, cells];
		
		res /; RegionQ[res]
	]


UTSubMesh[___] = $Failed;


(* ::Subsection::Closed:: *)
(*subCellMesh*)


subCellMesh[h_, icoords_, icells_] :=
	Block[{coords, cells, res},
		If[emptyCellQ[icells], Return[EmptyRegion[Length[icoords[[1]]]]]];
		
		{coords, cells} = eliminateUnusedCoordinates[icoords, icells];
		
		res = makeMeshRegion[h, coords, cells];
		
		If[RegionQ[res], 
			res, 
			res = makeMeshRegion[MeshRegion, coords, cells]
		];
		
		res /; RegionQ[res]
	]


subCellMesh[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Utilities*)


emptyCellQ[cellspec_] := Catch[iEmptyCellQ[cellspec]]


iEmptyCellQ[{}] = True;
iEmptyCellQ[l_List] := If[IntegerQ[First[l]], Throw[False], AllTrue[l, iEmptyCellQ]];
iEmptyCellQ[_[arg_]] := iEmptyCellQ[arg];
iEmptyCellQ[_] = True;


makeMeshRegion[h_, coords_, cells_] :=
	Quiet @ h[
		coords, 
		cells, 
		Method -> {
			"EliminateUnusedCoordinates" -> False, "DeleteDuplicateCoordinates" -> False, "DeleteDuplicateCells" -> False,
			"TJunction" -> False, "CheckIntersections" -> False, "CoplanarityTolerance" -> 14
		}
	]


(* ::Section:: *)
(*UTHighlightBoundaryEdges*)


(* ::Subsection::Closed:: *)
(*Main*)


Options[UTHighlightBoundaryEdges] = {"IncludeSingularEdges" -> True};


UTHighlightBoundaryEdges[mr_, opts:OptionsPattern[]] := UTHighlightBoundaryEdges[mr, Automatic, opts]


UTHighlightBoundaryEdges[mr_?UTSurfaceMesh3DQ, stylespec_, OptionsPattern[]] :=
	Block[{style, singQ, c12, rpts, hedges1, hedges3p},
		style = meshBoundaryStyle[stylespec];
		singQ = TrueQ[OptionValue["IncludeSingularEdges"]];
		
		c12 = mr["ConnectivityMatrix"[1, 2]];
		rpts = Differences[c12["RowPointers"]];
		
		hedges1 = Random`Private`PositionsOf[rpts, 1];
		If[singQ, 
			hedges3p = Random`Private`PositionsOf[rpts - 3, Positive],
			hedges3p = {}
		];
		
		If[Length[hedges1] > 0 || Length[hedges3p] > 0,
			Head[mr][mr, MeshCellStyle -> {{1, Join[hedges1, hedges3p]} -> style}],
			mr
		]
	]


UTHighlightBoundaryEdges[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Argument conform & completion*)


registerForDiscretization[UTHighlightBoundaryEdges, 1, False];


(* ::Subsection::Closed:: *)
(*Utilities*)


meshBoundaryStyle[Automatic] = Directive[Opacity[1], Black];


meshBoundaryStyle[spec_] := Directive[meshBoundaryStyle[Automatic], spec]
