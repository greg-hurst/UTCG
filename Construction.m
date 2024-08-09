(* ::Package:: *)

(* ::Title:: *)
(*Construction*)


(* ::Section:: *)
(*Begin*)


Package["UTCG`"]


PackageExport["UTJoin"]
PackageExport["UTLaxBlock"]
PackageExport["UTFastBlock"]
PackageExport["UTToBMR"]


UTJoin::usage = "Join multiple meshes into the same scene.";
UTLaxBlock::usage = "Temporarily relaxes mesh constraints such as coplanarity tolerances and non self-intersection requirements.";
UTFastBlock::usage = "Construct a mesh without unused points, duplicate points/cells, and self-intersection tests.";
UTToBMR::usage = "Attempts to convert a 3D surface mesh into a solid BoundaryMeshRegion.";


(* ::Section:: *)
(*UTJoin*)


(* ::Subsection::Closed:: *)
(*Main*)


Options[UTJoin] = Options[MeshRegion];


UTJoin[mr___?UTJoinableQ, opts:OptionsPattern[]] :=
	Block[{flat, join},
		flat = Select[Flatten[{mr}], UTMeshQ];
		(
			flat = UTFastBlock @ Replace[flat, bmr_BoundaryMeshRegion :> RegionBoundary[bmr], {1}];
			
			join = Switch[Length[flat],
				0, EmptyRegion[3],
				1, First[flat],
				_, Quiet[UTLaxBlock[Region`Mesh`MeshRegionJoin[##, opts]& @@ flat]]
			];
			
			(
				If[UTMeshQ[join],
					Head[join][join, opts],
					join
				]
			) /; RegionQ[join]
		
		) /; SameQ @@ RegionEmbeddingDimension /@ flat
	]


UTJoin[___] = $Failed;


UTJoinableQ[l_List] := VectorQ[l, UTJoinableQ]
UTJoinableQ[_EmptyRegion|_FullRegion] = True;
UTJoinableQ[expr_] := UTMeshQ[expr]


(* ::Subsection::Closed:: *)
(*Argument conform & completion*)


registerForDiscretization[UTJoin, {{}}, False];


(* ::Section:: *)
(*UTLaxBlock*)


(* ::Subsection::Closed:: *)
(*Main*)


SetAttributes[UTLaxBlock, HoldFirst];


UTLaxBlock[code_] :=
	Block[{bmethod},
		Internal`WithLocalSettings[
			bmethod = OptionValue[BoundaryMeshRegion, Method];
			SetOptions[BoundaryMeshRegion, Method -> Join[{"CheckIntersections" -> False}, Replace[bmethod, Except[_List] -> {}, {0}]]];,
			
			code,
			
			SetOptions[BoundaryMeshRegion, Method -> bmethod];
		]
	]


(* ::Section:: *)
(*UTFastBlock*)


(* ::Subsection::Closed:: *)
(*Main*)


SetAttributes[UTFastBlock, HoldFirst];


UTFastBlock[code_] :=
	Block[{moff, mmethod, bmethod},
		moff = {"EliminateUnusedCoordinates" -> False, "DeleteDuplicateCoordinates" -> False, "DeleteDuplicateCells" -> False, "TJunction" -> False};
		Internal`WithLocalSettings[
			mmethod = OptionValue[MeshRegion, Method];
			bmethod = OptionValue[BoundaryMeshRegion, Method];
			SetOptions[MeshRegion, Method -> Join[moff, Replace[mmethod, Except[_List] -> {}, {0}]]];
			SetOptions[BoundaryMeshRegion, Method -> Join[moff, {"CheckIntersections" -> False}, Replace[bmethod, Except[_List] -> {}, {0}]]];,
			
			code,
			
			SetOptions[MeshRegion, Method -> mmethod];
			SetOptions[BoundaryMeshRegion, Method -> bmethod];
		]
	]


(* ::Section:: *)
(*UTToBMR*)


(* ::Subsection::Closed:: *)
(*Main*)


UTToBMR[bmr_BoundaryMeshRegion?BoundaryMeshRegionQ, ___] := bmr


UTToBMR[mr_MeshRegion?UTLineMesh2DQ, intQ_:False, ___] := 
	With[{res = Quiet @ BoundaryMeshRegion[MeshCoordinates[mr], MeshCells[mr, RegionDimension[mr], "Multicells" -> True], 
		Method -> {"EliminateUnusedCoordinates" -> False, "DeleteDuplicateCoordinates" -> False, "DeleteDuplicateCells" -> False, "TJunction" -> False, "CheckIntersections" -> TrueQ[intQ]}]},
		res /; BoundaryMeshRegionQ[res]
	]


UTToBMR[mr_MeshRegion?UTSurfaceMesh3DQ, intQ_:False, nestingQ_:True] := 
	Block[{coords, C12, C22, comps, cells, nesting, res},
		coords = MeshCoordinates[mr];
		
		C12 = mr["ConnectivityMatrix"[1, 2]];
		(
			C22 = Transpose[C12] . C12;
			comps = SparseArray`StronglyConnectedComponents[C22];
			
			cells = MeshCells[mr, {2, #}, "Multicells" -> True]& /@ comps;
			
			nesting = If[!TrueQ[nestingQ],
				ConstantArray[0, {Length[comps], 2}],
				boundaryNestingArrays[coords, cells]
			];
			
			res = BoundaryMeshRegion[
				coords,
				Sequence @@ cells,
				Method -> {
					"EliminateUnusedCoordinates" -> False, 
					"DeleteDuplicateCoordinates" -> False, 
					"DeleteDuplicateCells" -> False, 
					"TJunction" -> False, 
					"CheckIntersections" -> TrueQ[intQ],
					"SeparateBoundaries" -> False,
					"BoundaryNesting" -> nesting
				}
			];
			
			res /; RegionQ[res]
			
		) /; Max[Mod[DeleteDuplicates[Differences[C12["RowPointers"]]], 2]] == 0
	]


UTToBMR[e_EmptyRegion, ___] := e
UTToBMR[f_FullRegion, ___] := f


UTToBMR[___] = $Failed;


(* ::Subsection::Closed:: *)
(*boundaryNestingArrays*)


(* ::Text:: *)
(*Uses the same idea as Region`Mesh`BoundaryNestingArrays, but only tests one point for crossing count instead of all points. This assumes no intersecting facets.*)


polygonBoundingBox[Polygon[coords_]] := CoordinateBoundingBox[coords]
polygonBoundingBox[data_List] := CoordinateBoundingBox[polygonBoundingBox /@ data]


polygonCoordinate[Polygon[data_]] := iPolygonCoordinate[data];
polygonCoordinate[data_List] := polygonCoordinate[First[data]];


iPolygonCoordinate[lis_List] := 
	If[Length[lis] == 3 && VectorQ[lis, NumericQ], 
		lis, 
		iPolygonCoordinate[lis[[1]]]
	]


boundingBoxNesting[{min1_, max1_}, {min2_, max2_}] := 
	Module[{minless, maxless},
	
		If[Or @@ MapThread[Less, {max1, min2}],
			Return["Disjoint"]
		];
		
		If[Or @@ MapThread[Less, {max2, min1}],
			Return["Disjoint"]
		];
		
		minless = Union[MapThread[Less, {min1, min2}]];
		maxless = Union[MapThread[Less, {max1, max2}]];
		
		If[Length[minless] > 1 || Length[maxless] > 1 || minless === maxless,
			Return[Indeterminate]
		];
		
		If[TrueQ[First[minless]],
			{"Inside", 2, 1},
			{"Inside", 1, 2}
		]
	]


pointInsideQ[polys_, pt_] := OddQ[Region`Mesh`CrossingCount[polys, pt]]


(* ::Text:: *)
(*Returns {{i1, j1}, {i2, j2}, ...} where the i1 component contains the j1 component, etc.*)


boundaryNestingAdjacency[coords_, cells_] := 
	Block[{np, polycomps, bb, pts, nesting},
		
		np = Length[cells];
		polycomps = Region`Mesh`ToCoordinates[cells, coords];
		bb = polygonBoundingBox /@ polycomps;
		pts = polygonCoordinate /@ polycomps;
		
		nesting = Reap[
			Do[
				Switch[
					boundingBoxNesting[bb[[j]], bb[[i]]],
					Indeterminate,
						If[pointInsideQ[polycomps[[i]], pts[[j]]],
							Sow[{i, j}],
							If[pointInsideQ[polycomps[[j]], pts[[i]]],
								Sow[{j, i}]
							]
						],
					{"Inside", 1, 2},
						If[pointInsideQ[polycomps[[i]], pts[[j]]],
							Sow[{i, j}]
						],
					{"Inside", 2, 1},
						If[pointInsideQ[polycomps[[j]], pts[[i]]],
							Sow[{j, i}]
						],
					"Disjoint",
						Null,
					_,
						Throw[0]
				],
				{i, 1, np - 1},
				{j, i + 1, np}
			]
		][[-1]];
		
		Sort[Flatten[nesting, 1]]
]


nestingLevels[np_Integer, adj_] :=
	Block[{depths, roots, depthlist},
	
		(* The number of times a component appears in the second location is its depth in the inclusion tree. *)
		depths = SplitBy[SortBy[Tally[adj[[All, 2]]], Last], Last];
		
		roots = Complement[Range[np], adj[[All, 2]]];
		
		(* creates {{i01, i02, i03, ...}, {i11, i12, i13, ...}, {i21, i22, i23, ...}, ...}, where idj means the index has depth d. *)
		Prepend[depths[[All, All, 1]], roots]
	]


nestingDepths[np_Integer, adj_] :=
	Block[{depthlist, df},
		depthlist = nestingLevels[np, adj];
		
		df[_] = 0;
		Do[Scan[(df[#] = i)&, depthlist[[i+1]]], {i, 0, Length[depthlist]-1}];
		
		df /@ Range[np]
	]


iboundaryNestingArrays[np_Integer, {}] := ConstantArray[0, {np, 2}]


iboundaryNestingArrays[np_Integer, adj_] := 
	Block[{depthlist, df, dnesting, samelvlnestings, samelvlnestingsgrouped, reps},
	
		(* creates {{i01, i02, i03, ...}, {i11, i12, i13, ...}, {i21, i22, i23, ...}, ...}, where idj means the index has depth d. *)
		depthlist = nestingLevels[np, adj];
		
		(* look up table for the depth of an index *)
		Do[Scan[(df[#] = i)&, depthlist[[i+1]]], {i, 0, Length[depthlist]-1}];
		
		(* Only select i \[Subset] j pairs if there is no k such that j \[Subset] k \[Subset] i. This is true iff the depth of j is one more than the depth of j, i.e. df[j] \[Equal] df[i]+1 *)
		dnesting = Select[adj, (Subtract @@ df /@ # == -1)&];
		
		(* gather the nestings by their depth *)
		samelvlnestings = GatherBy[dnesting, df[First[#]]&];
		
		(* for each level, gather the nestings by their starting node *)
		samelvlnestingsgrouped = GatherBy[#, First]& /@ samelvlnestings;
		
		(* i -> {i, s} means the ith component of the list has parent s *)
		reps = Flatten @ Map[
			#[[2]] -> Reverse[#]&,
			samelvlnestingsgrouped,
			{3}
		];
		
		(* If the ith component of the list is {i, s}, then i \[Subset] s with no other subset in between, and {0, 0} means i is a root node and is not contained in any other component. *)
		ReplacePart[ConstantArray[0, {np, 2}], reps]
	]


boundaryNestingArrays[coords_, cells_] := iboundaryNestingArrays[Length[cells], boundaryNestingAdjacency[coords, cells]]


(* ::Section:: *)
(*Mesh Construction*)


(* ::Subsection::Closed:: *)
(*PackageScopes*)


PackageScope["eliminateUnusedCoordinates"]
PackageScope["deleteDuplicateCoordinates"]


(* ::Subsection::Closed:: *)
(*eliminateUnusedCoordinates*)


eliminateUnusedCoordinates[coords_, multicells_] :=
	With[{inds = Union[Join @@ flattenCell /@ multicells]},
		With[{min = Min[inds]-1, max = Max[inds]},
			{
				coords[[inds]],
				Region`Mesh`ReplaceIncidents[multicells, Join[Range[max-min+1, max], inversePartialPermutation[max-min, inds-min]]]
			}
		]
	]


flattenCell = Flatten[If[ListQ[#], Identity, First][#]]&;


inversePartialPermutation = InversePermutation[DeleteDuplicates[Join[#2, Range[#1]]]]&;


(* ::Subsection::Closed:: *)
(*deleteDuplicateCoordinates*)


deleteDuplicateCoordinates[coords_, cells_] :=
	Block[{coordsnew, rep},
		{coordsnew, rep} = Region`Mesh`DeleteDuplicateCoordinates[coords];
		If[ListQ[rep],
			{coordsnew, Region`Mesh`ReplaceIncidents[cells, rep]},
			{coordsnew, cells}
		]
	]
