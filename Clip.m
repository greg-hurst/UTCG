(* ::Package:: *)

(* ::Title:: *)
(*Clip*)


(* ::Section:: *)
(*Begin*)


Package["UTCG`"]


PackageExport["UTClip"]
PackageExport["UTSlice"]
PackageExport["UTZSlices"]
PackageExport["UTDynamicZSlice"]


UTClip::usage = "Clips a mesh about certain convex planar regions.";
UTSlice::usage = "Creates a 2D slice of a mesh from an arbitrary clip plane.";
UTZSlices::usage = "Creates 2D z\[Hyphen]slices of a mesh at a specified step size.";
UTDynamicZSlice::usage = "Displays an interface that dynamically displays an arbitrary Z slice of a mesh.";


(* ::Section:: *)
(*Utilities*)


(* ::Subsection::Closed:: *)
(*Interval tree slicing utilities*)


(* ::Subsubsection::Closed:: *)
(*meshZIntervalTreeData*)


meshZIntervalTreeData[mr_] := meshZIntervalTreeData[MeshCoordinates[mr], Join @@ MeshCells[mr, 2, "Multicells" -> True][[All, 1]]]


meshZIntervalTreeData[coords_, cells_] :=
	Block[{intstagged, tree},
	
		intstagged = zRange[coords, cells, Range[Length[cells]]];
		tree = intervalTree[intstagged];

		{coords, cells, tree}
	]


UTCompile[zRange, {{coords, _Real, 2}, {cell, _Integer, 1}, {tag, _Real}},
	Block[{prim},
		prim = coords[[cell, 3]];
		{Min[prim], Max[prim], tag}
	],
	RuntimeAttributes -> {Listable}
];


(* ::Subsubsection::Closed:: *)
(*intervalTree*)


intervalTree[intstagged_] :=
	Block[{$RecursionLimit = \[Infinity]},
		iIntervalTree[intstagged[[Ordering[intstagged[[All, 1]]]]]]
	]


iIntervalTree[intstagged_] :=
	Block[{len, n, x, Sleft, Scent, Sright, node},
		len = Length[intstagged];
		If[len <= 100, Return[centerNode[Null, intstagged]]];
		
		n = Quotient[len+1, 2];
		x = Mean[intstagged[[n, 1;;2]]];
		
		{Sleft, Scent, Sright} = partitionIntervals[intstagged, x];
		
		node = centerNode[x, Scent];
		If[Length[Sleft] > 0, node["SetLeft", iIntervalTree[Sleft]];];
		If[Length[Sright] > 0, node["SetRight", iIntervalTree[Sright]];];
		
		node
	]


centerNode[x_, intstagged_] := CreateDataStructure["BinaryTree", {x, #1, #2, Developer`ToPackedArray[#3, Integer], #1[[1]], Max[#2]}& @@ Transpose[intstagged]]


partitionIntervals[intstagged_, x_] :=
	Block[{data, spec},
		data = Partition[partitionIntervalsC[intstagged, x], 3];
		spec = Round[data[[-1]]];
		
		{data[[1 ;; spec[[1]]]], data[[spec[[1]]+1 ;; spec[[2]]]], intstagged[[spec[[3]] ;; -1]]}
	]


UTCompile[partitionIntervalsC, {{intstagged, _Real, 2}, {x, _Real}},
	Block[{cright = 0., ccenter = 0., bagright = Internal`Bag[Most[{0.}]], bagcenter = Internal`Bag[Most[{0.}]], lcount = Length[intstagged]+1.0},
		Do[
			If[Compile`GetElement[i, 2] < x,
				Internal`StuffBag[bagright, i, 1];
				cright++,
				If[Compile`GetElement[i, 1] > x,
					lcount = cright+ccenter+1;
					Break[],
					Internal`StuffBag[bagcenter, i, 1];
					ccenter++
				]
			],
			{i, intstagged}
		];
		
		Join[Internal`BagPart[bagright, All], Internal`BagPart[bagcenter, All], {cright, cright + ccenter, lcount}]
	]
];


(* ::Subsubsection::Closed:: *)
(*intersectingIntervals*)


intersectingIntervals[tree_?DataStructureQ, x_, \[CurlyEpsilon]_:0.] :=
	Block[{bag = Internal`Bag[], ds = tree, sgn, d1, d2, d3, d4, d5, d6},
		While[True,
			{d1, d2, d3, d4, d5, d6} = ds["Data"];
			If[Subtract[d5, \[CurlyEpsilon]] <= x <= d6+\[CurlyEpsilon],
				Internal`StuffBag[bag, pickBetween[Subtract[d2, \[CurlyEpsilon]], d3+\[CurlyEpsilon], d4, x], 1];
			];
			
			If[d1 === Null, 
				Break[While], 
				sgn = Sign[Subtract[x, d1]]
			];
			
			Which[
				sgn === -1, 
					If[ds["LeftNullQ"],
						Break[While],
						ds = ds["Left"]
					],
				sgn === 1,
					If[ds["RightNullQ"],
						Break[While],
						ds = ds["Right"]
					],
				True,
					Break[While]
			];
		];
		
		Internal`BagPart[bag, All]
	]


intersectingIntervals[___] = $Failed;


UTCompile[pickBetween, {{lefts, _Real, 1}, {rights, _Real, 1}, {tags, _Integer, 1}, {x, _Real}},
	Select[Times[tags, UnitStep[Subtract[x, lefts]], UnitStep[Subtract[rights, x]]], Positive]
];


(* ::Subsubsection::Closed:: *)
(*iZSlice*)


iZSlice[coords_List, cells_, tree_, z_, \[CurlyEpsilon]_, closeQ_, lowdimQ_] :=
	Block[{intinds, intcoords, sgns, slicecoords, segs},
		
		intinds = intersectingIntervals[tree, z, \[CurlyEpsilon]];
		If[!ListQ[intinds] || Length[intinds] == 0, Return[{}]];
		
		intcoords = coords[[Flatten[cells[[intinds]]]]];
		intcoords[[All, 3]] -= z;
		
		sgns = Sign[Threshold[intcoords[[All, 3]], {"Hard", \[CurlyEpsilon]}]];
		
		segs = crossTriangleHorizontal[Partition[sgns, 3], Partition[intcoords, 3]];
		
		processSliceSegments[segs, closeQ, lowdimQ]
	]


iZSlice[___] = $Failed;


(* ::Subsubsection::Closed:: *)
(*crossTriangleHorizontal*)


UTCompile[zSegPoint, {{coord1, _Real, 1}, {coord2, _Real, 1}},
	Block[{z1, z2},
		z1 = Compile`GetElement[coord1, 3];
		z2 = Compile`GetElement[coord2, 3];
		
		Divide[
			{
				Subtract[Compile`GetElement[coord2, 1] z1, Compile`GetElement[coord1, 1] z2], 
				Subtract[Compile`GetElement[coord2, 2] z1, Compile`GetElement[coord1, 2] z2]
			}, 
			Subtract[z1, z2]
		]
	]
];


UTCompile[crossTriangleHorizontal, {{sgns, _Integer, 1}, {coords, _Real, 2}},
	Block[{s1, s2, s3, p1, p2, p3, tot},
		s1 = Compile`GetElement[sgns, 1];
		s2 = Compile`GetElement[sgns, 2];
		s3 = Compile`GetElement[sgns, 3];
		
		p1 = Compile`GetElement[coords, 1];
		p2 = Compile`GetElement[coords, 2];
		p3 = Compile`GetElement[coords, 3];
		
		tot = Abs[s1] + Abs[s2] + Abs[s3];
		Which[
			tot === 3,
				Which[
					s1 =!= s2 && s1 =!= s3, {zSegPoint[p1, p2], zSegPoint[p1, p3]},
					s1 =!= s2 && s2 =!= s3, {zSegPoint[p1, p2], zSegPoint[p2, p3]},
					True, {zSegPoint[p1, p3], zSegPoint[p2, p3]}
				],
			Min[s1, s2, s3] === -1 && Max[s1, s2, s3] === 1,
				Which[
					s1 === 0, {Most[p1], zSegPoint[p2, p3]},
					s2 === 0, {Most[p2], zSegPoint[p1, p3]},
					True, {Most[p3], zSegPoint[p1, p2]}
				],
			tot === 2, 
				Which[
					s1 === 0, {Most[p1]},
					s2 === 0, {Most[p2]},
					True, {Most[p3]}
				],
			tot === 1,
				Which[
					s1 === 0 === s2, {Most[p1], Most[p2], {s3, s3}},
					s1 === 0 === s3, {Most[p1], Most[p3], {s2, s2}},
					True, {Most[p2], Most[p3], {s1, s1}}
				],
			True,
				{Most[p1], Most[p2], Most[p3], Most[p3]}
		]
	],
	RuntimeAttributes -> {Listable}
];


(* ::Subsection::Closed:: *)
(*processSliceSegments*)


(* ::Subsubsection::Closed:: *)
(*Main*)


processSliceSegments[segs_, closeQ_, lowdimQ_] :=
	Block[{pts, lines, blines, faces, prims},
		
		(* 
			pts:    isolated points that don't lie on a segment or face
			lines:  segments in a component with singular or open endpoints
			blines: segments that will form a BoundaryMeshRegion
			faces:  triangles that lie in the horizontal z plane
		*)
		
		{pts, lines, blines, faces} = preProcessSliceSegments[segs, closeQ, lowdimQ];
		
		prims = Join @@ MapThread[
			If[Length[#2] == 0, {}, #1 @@ deleteDuplicateMultiCoordinates[#2]]&, 
			{
				{pointPrimitives, linePrimitives, BMRPrimitives, facePrimitives},
				{pts, lines, blines, faces}
			}
		];
		If[closeQ && !lowdimQ, prims = Select[prims, #[[1, 1]] === 2&]];
		
		mergePrimitives[prims]
	]


processSliceSegments[___] = {};


(* ::Subsubsection::Closed:: *)
(*preProcessSliceSegments*)


preProcessSliceSegments[segs_?Developer`PackedArrayQ, closeQ_, lowdimQ_] /; Length[segs[[1]]] =!= 3 || !closeQ :=
	Switch[Length[segs[[1]]],
		2, If[closeQ, {{}, {}, segs, {}}, {{}, segs, {}, {}}],
		1, If[lowdimQ, {segs, {}, {}, {}}, {{}, {}, {}, {}}],
		3, If[lowdimQ || !closeQ, {{}, segs[[All, 1;;2]], {}, {}}, {{}, {}, {}, {}}],
		4, {{}, {}, {}, segs}
	]


preProcessSliceSegments[segs_, closeQ_, lowdimQ_] :=
	Block[{lens, pts, dsegs, dsgns, psegs, faces, ipts, lines, blines},

		(* -------- prepare for handling of any degenerate cases: points, horizontal segments, horizontal faces --------
			
			pts:   points from a triangle that intersects z at exactly one of its vertices
			dsegs: (degenerage) segments from a triangle that intersects z at exactly two of its vertices
			psegs: (proper) segments from a triangle that intersects z in between all of its vertices
			faces: triangles that lie in the horizontal z plane
			
			dsgns: indicates for each segment in dsegs if its corresponding triange lies above or below z
		*)
		lens = Length /@ segs;
		
		pts   = If[lowdimQ, Developer`ToPackedArray[Pick[segs, lens, 1]], {}];
		dsegs = Developer`ToPackedArray[Pick[segs, lens, 3]];
		psegs = Developer`ToPackedArray[Pick[segs, lens, 2]];
		faces = Developer`ToPackedArray[Pick[segs, lens, 4]][[All, 1;;3]];
		
		(* 
			ipts:   isolated points that don't lie on a segment or face
			lines:  segments in a component with singular or open endpoints
			blines: segments that will form a BoundaryMeshRegion
		*)
		
		ipts = isolatedPoints[pts, dsegs, psegs, faces];
		
		If[!closeQ,
			dsgns = {};
			lines = Join[dsegs[[All, 1;;2]], psegs];
			blines = {},
			
			dsgns = dsegs[[All, 3, 1]];
			lines = dsegs[[All, 1;;2]];
			blines = psegs;
			
			(* segments with triangles above and below z should belong to blines *)
			{lines, blines} = gatherHangingSegments[lines, dsgns, blines];
			If[!lowdimQ, lines = {}]
			
		];
		
		(* 
			address segments adjacent to horizontal faces 
			
			for lines:  _\[FilledSquare] \[LongRightArrow] _ + \[FilledSquare] and not \[RightFloor] + \[FilledSquare]
			for blines: \[EmptySquare]\[FilledSquare] \[LongRightArrow] \[EmptySquare] + \[FilledSquare] and not \[SquareSubset] + \[FilledSquare]
		*)
		{lines, blines, faces} = closeWithAdjacentFaces[lines, blines, faces];
		
		{ipts, lines, blines, faces}
	]


(* ::Subsubsection::Closed:: *)
(*Degenerate utilities*)


isolatedPoints[p:{}, ___] := p


isolatedPoints[pts_, {}..] := pts


isolatedPoints[opts_, lines_, bds_, faces_] := 
	Block[{pts, dists},
		pts = Developer`ToPackedArray[Union @@ opts];
		dists = Nearest[Join[Join @@ lines, Join @@ bds, Join @@ faces] -> "Distance", pts][[All, 1]];
		pts = Pick[pts, Unitize[dists], 1];
		
		Partition[pts, 1]
	]


gatherHangingSegments[{}, _, blines_] := {{}, blines}


gatherHangingSegments[lines_, dsgns_, blines_] :=
	Block[{linereap},
		linereap = Reap[
			Scan[
				Sow[#[[1, 1]], ringEdgeType[#[[All, 2]]]]&,
				GatherBy[Transpose[{Sort /@ lines, dsgns}], First]
			],
			{"boundaryedge", "edge", "discard"}
		][[-1]];
		{
			Developer`ToPackedArray[First[linereap[[2]], {}]],
			Join[blines, Developer`ToPackedArray[First[linereap[[1]], {}]]]
		}
	]


ringEdgeType = 
	With[{len = Length[#], len4 = Mod[Length[#], 4], min = Min[#], max = Max[#]},
		Which[
			min != -1., "edge",
			max != 1., "edge",
			len === 2, "boundaryedge", (* short circuit *)
			len4 === 2, If[Total[#] == 0., "boundaryedge", "discard"],
			len4 === 0, If[Total[#] != 0., "boundaryedge", "discard"],
			True, "edge"
		]
	]&;


closeWithAdjacentFaces[{}.., _] = {{}, {}, {}};


closeWithAdjacentFaces[lines_, bds_, {}] := {lines, bds, {}}


closeWithAdjacentFaces[lines_, {}, faces_] := {Complement[lines, sortedBoundarySegments[faces]], {}, faces}


(* ::Text:: *)
(*Incorrect behavior for SeedRandom[1];arr=ArrayMesh[rand=RandomInteger[{0,1},{5,5,5}]];*)


(* ::Text:: *)
(*Perhaps the best approach is more brute force:*)
(** prune hanging segments*)
(** find all faces (with IGFaces?)*)
(** select the faces that have an interior point that is in faces or has an even crossing count with the original mesh*)
(** merge adjacent faces*)
(** all segments of faces will be blines*)
(** all hanging segments will be lines*)
(** no faces will be returned*)


closeWithAdjacentFaces[lines_, bds_, faces_] :=
	Block[{fbd, ll, intpts, g, path, seenQ, newbds},
		fbd = sortedBoundarySegments[faces];
		ll = Complement[lines, fbd];
		
		intpts = Intersection[Join @@ bds, Join @@ fbd];
		If[Length[intpts] == 0 || Mod[Length[intpts], 2] != 0, Return[{ll, bds, faces}]];
		
		g = Graph[UndirectedEdge @@@ fbd];
		
		newbds = Join @@ Table[
			path = {};
			If[seenQ[p] =!= True,
				seenQ[p] = True;
				Do[
					If[seenQ[q], Continue[]];
					path = FindPath[g, p, q]; (* TODO find path with smallest initial angle *)
					If[path =!= {}, Scan[(seenQ[#] = True)&, path]; Break[Do]],
					{q, intpts}
				]
			];
			Join @@ (Partition[#, 2, 1]& /@ path),
			{p, intpts}
		];
		
		{Complement[ll, newbds], Join[bds, newbds], faces}
	]


sortedBoundarySegments[{}] = {};


sortedBoundarySegments[faces_] := 
	Block[{segs},
		segs = Sort /@ Join @@ Map[Partition[#, 2, 1, 1]&, faces];
		segs = Gather[segs];
		Pick[segs, Unitize[Subtract[Length /@ segs, 2]], 1][[All, 1]]
	]


(* ::Subsubsection::Closed:: *)
(*Primitive generation*)


deleteDuplicateMultiCoordinates[{}] = {{}, {}};


deleteDuplicateMultiCoordinates[pts_] :=
	Block[{dims, coords, cells},
		dims = Dimensions[pts][[1 ;; 2]];
		{coords, cells} = Region`Mesh`DeleteDuplicateCoordinates[Join @@ pts];
		If[!ListQ[cells], cells = Range[Times @@ dims]];
		cells = Partition[cells, dims[[2]]];
		
		{coords, cells}
	]


pointPrimitives[coords_, cells_] := {{0 -> MeshRegion, coords, Range[Length[coords]]}}


linePrimitives[coords_, cells_] := {{1 -> MeshRegion, coords, cells}}


facePrimitives[coords_, cells_] := {{2 -> MeshRegion, coords, cells}}


BMRPrimitives[coords_, cells_] := 
	Block[{len, sa, g, C01, C11, comps, permcells, icells, lines, blines, mres = {}, bres = {}, inds},
		len = Length[coords];
		sa = SparseArray[cells -> _, {len, len}];
		g = Graph[Range[len], {Null, sa + Transpose[sa]}];
		
		If[MinMax[VertexDegree[g]] === {2, 2}, 
			Return[{{2 -> BoundaryMeshRegion, coords, Append[#, First[#]]& /@ FindFundamentalCycles[g][[All, All, 1]]}}];
		];
		
		If[Max[Mod[VertexDegree[g], 2]] === 0, 
			Return[{forceBMR[coords, cells]}];
		];
		
		C01 = Unitize[IncidenceMatrix[g]];
		permcells = Partition[Transpose[C01]["NonzeroPositions"][[All, 2]], 2];
		
		C01 = C01 * Subtract[1, Mod[Differences[C01["RowPointers"]], 2]];
		C11 = Transpose[C01] . C01;
		comps = SparseArray`StronglyConnectedComponents[C11];
		
		{lines, blines} = Reap[
			Scan[
				(
					icells = permcells[[#]];
					Sow[icells, Max[Mod[Tally[Join @@ icells][[All, 2]], 2]]]
				)&, 
				comps
			],
			{1, 0}
		][[-1]];
		
		If[Length[lines] =!= 0,
			lines = Join @@ First[lines];
			inds = Union @@ lines;
			mres = {1 -> MeshRegion, coords[[inds]], Replace[lines, AssociationThread[inds, Range[Length[inds]]], {2}]};
		];
		
		If[Length[blines] =!= 0,
			blines = Join @@ First[blines];
			inds = Union @@ blines;
			bres = forceBMR[coords[[inds]], Replace[blines, AssociationThread[inds, Range[Length[inds]]], {2}]];
		];
		
		Replace[{mres, bres}, {} -> Nothing, {1}]
	]


forceBMR[coords_, cells_] :=
	Block[{bmr},
		bmr = Quiet[UTFastBlock[BoundaryMeshRegion[coords, Line[cells]]]];
		If[RegionQ[bmr],
			{2 -> BoundaryMeshRegion, bmr},
			{1 -> MeshRegion, coords, cells}
		]
	]


mergePrimitives[prims_] := SortBy[mergeGroup /@ GatherBy[prims, First], First]


mergeGroup[{p_}] := p


mergeGroup[g_] :=
	Block[{lens},
		lens = Prepend[Most[Accumulate[Length /@ g[[All, 2]]]], 0];
		{g[[1, 1]], Join @@ g[[All, 2]], Join @@ (g[[All, 3]] + lens)}
	]


(* ::Section:: *)
(*UTClip*)


(* ::Subsection::Closed:: *)
(*Main*)


Clear[UTClip];


Options[UTClip] = {"CloseBoundary" -> Automatic};


UTClip[args__] :=
	Block[{res},
		res = clipCatchEmpty @ iClip[args];
		
		res /; RegionQ[res]
	]


UTClip[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Argument conform & completion*)


registerForDiscretization[UTClip, 1];


(* ::Subsection::Closed:: *)
(*iClip*)


Options[iClip] = Options[UTClip];


iClip[EmptyRegion[3], ___] = EmptyRegion[3];


iClip[mr_?UTTriangleMesh3DQ, spec:(Except[_?OptionQ]..), opts___?OptionQ] :=
	Block[{halfspaces, closeQ, coords, cells, reshead, res},
		halfspaces = canonicalizeHalfSpaceSpecs[mr, spec];
		(
			closeQ = processClipCloseBoundary[mr, opts];
			coords = MeshCoordinates[mr];
			cells = Join @@ MeshCells[mr, 2, "Multicells" -> True][[All, 1]];
			
			res = Fold[halfSpaceClip[##, closeQ]&, {coords, cells}, halfspaces];
			(
				If[closeQ =!= False && BoundaryMeshRegionQ[mr],
					reshead = BoundaryMeshRegion,
					reshead = MeshRegion
				];
				
				res = assembleClippedMesh[res, reshead];
				
				res /; RegionQ[res]
				
			) /; ListQ[res]
			
		) /; halfspaces =!= $Failed
	]


iClip[___] = $Failed;


processClipCloseBoundary[mr_, OptionsPattern[iClip]] := 
	Block[{closeQ},
		closeQ = OptionValue["CloseBoundary"];
		If[BooleanQ[closeQ],
			closeQ,
			BoundaryMeshRegionQ[mr]
		]
	]


(* ::Subsection::Closed:: *)
(*canonicalizeHalfSpaceSpecs*)


canonicalizeHalfSpaceSpecs[mr_, spec__] := 
	Block[{canonical},
		canonical = Flatten[{iCanonicalizeHalfSpaceSpecs[spec]}];
		(
			canonical = sortClipPlanes[mr, canonical];
			
			canonical /; canonical =!= $Failed
			
		) /; FreeQ[canonical, $Failed]
	]


canonicalizeHalfSpaceSpecs[___] = $Failed;


iCanonicalizeHalfSpaceSpecs[hs:HalfSpace[_, _?NumericQ]?ConstantRegionQ] /; RegionEmbeddingDimension[hs] == 3 := N[hs]


iCanonicalizeHalfSpaceSpecs[cb:Cuboid[lo_, hi_]?ConstantRegionQ] /; RegionEmbeddingDimension[cb] == 3 := 
	N @ {
		MapThread[HalfSpace, {-IdentityMatrix[3], -lo}],
		MapThread[HalfSpace, {IdentityMatrix[3], hi}]
	}


iCanonicalizeHalfSpaceSpecs[pp:Parallelepiped[p_, vv_]?ConstantRegionQ] /; RegionEmbeddingDimension[pp] == 3 := 
	Block[{ns, sgns},
		ns = Cross @@@ Partition[vv, 2, 1, 1];
		sgns = Sign[MapThread[Dot, {ns, RotateRight[vv]}]];
		
		N @ {
			With[{pt = p}, HalfSpace[#, # . pt]& /@ (-sgns*ns)],
			With[{pt = p + Total[vv]}, HalfSpace[#, # . pt]& /@ (sgns*ns)]
		}
	]


iCanonicalizeHalfSpaceSpecs[n_, p_] := iCanonicalizeHalfSpaceSpecs[HalfSpace[n, If[NumericQ[p], {0., 0., p}, p]]]


iCanonicalizeHalfSpaceSpecs[z_?NumericQ] := iCanonicalizeHalfSpaceSpecs[HalfSpace[{0, 0, -1}, -z]]


iCanonicalizeHalfSpaceSpecs[HalfSpace[n_List, p_List]?ConstantRegionQ] := iCanonicalizeHalfSpaceSpecs[HalfSpace[n, n . p]]


iCanonicalizeHalfSpaceSpecs[h:_Hyperplane?ConstantRegionQ] := iCanonicalizeHalfSpaceSpecs[HalfSpace @@ h]


iCanonicalizeHalfSpaceSpecs[InfinitePlane[{p1_, p2_, p3_}]?ConstantRegionQ] := iCanonicalizeHalfSpaceSpecs[HalfSpace[Cross[p1-p2, p3-p2], p1]]


iCanonicalizeHalfSpaceSpecs[InfinitePlane[p_, {v1_, v2_}]?ConstantRegionQ] := iCanonicalizeHalfSpaceSpecs[HalfSpace[Cross[v1, v2], p]]


iCanonicalizeHalfSpaceSpecs[pts:{p1_, p2_, p3_}] /; MatrixQ[pts, NumericQ] && Dimensions[pts] == {3, 3} := iCanonicalizeHalfSpaceSpecs[HalfSpace[Cross[p1-p2, p3-p2], p1]]


iCanonicalizeHalfSpaceSpecs[Cuboid[lo_]?ConstantRegionQ] := iCanonicalizeHalfSpaceSpecs[Cuboid[lo, lo+1]]


iCanonicalizeHalfSpaceSpecs[bds_List] /; MatrixQ[bds, NumericQ] && Dimensions[bds] == {3, 2} && And @@ Less @@@ bds := iCanonicalizeHalfSpaceSpecs[Cuboid @@ Transpose[bds]]


iCanonicalizeHalfSpaceSpecs[l_List] := iCanonicalizeHalfSpaceSpecs /@ l


iCanonicalizeHalfSpaceSpecs[(dir_Integer) -> {v1_?NumericQ, v2_?NumericQ}] /; 1 <= dir <= 3 := 
	iCanonicalizeHalfSpaceSpecs[{
		HalfSpace[-IdentityMatrix[3][[Abs[dir]]], -v1],
		HalfSpace[IdentityMatrix[3][[Abs[dir]]], v2]
	}]


iCanonicalizeHalfSpaceSpecs[(dir_Integer) -> v_?NumericQ] /; 1 <= Abs[dir] <= 3 := iCanonicalizeHalfSpaceSpecs[HalfSpace[-Sign[dir]*IdentityMatrix[3][[Abs[dir]]], -Sign[dir]v]]


iCanonicalizeHalfSpaceSpecs[___] = $Failed;


(* ::Subsection::Closed:: *)
(*sortClipPlanes*)


sortClipPlanes[mr_, hs:{__HalfSpace}] :=
	Block[{coords, len, djcnts},
		coords = MeshCoordinates[mr];
		len = Length[coords];
		djcnts = With[{cnt = halfSpaceDisjointTotal[coords, ##]& @@ #},
			If[cnt === len, clipThrowEmpty[]];
			{#, cnt}
		]& /@ hs;
		
		Replace[ReverseSortBy[djcnts, Last], {_, 0} -> Nothing, {1}][[All, 1]]
	]


$emptyclip = "Empty";


clipThrowEmpty[] := Throw[EmptyRegion[3], $emptyclip];


SetAttributes[clipCatchEmpty, HoldFirst];
clipCatchEmpty[code_] := Catch[code, $emptyclip]


halfSpaceDisjointTotal[coords_, n_, c_] :=
	Block[{proj, min, max, scale},
		proj = coords . n - c;
		{min, max} = MinMax[proj];
		scale = 2^6*$MachineEpsilon*Max[Abs[{min, max}]];
		
		If[Abs[min] < scale, min = 0.];
		If[Abs[max] < scale, max = 0.];
		
		Switch[Sign[{min, max}],
			{-1, 1}, Total[UnitStep[proj]],
			{-1, _}, 0,
			_, Length[coords]
		]
	]


(* ::Subsection::Closed:: *)
(*halfSpaceClip*)


halfSpaceClip[{{}, {}}, ___] = EmptyRegion[3];


halfSpaceClip[expr_, ___] /; !ListQ[expr] = $Failed;


halfSpaceClip[{coords_, cells_}, HalfSpace[n_, c_?NumericQ], args___] := halfSpaceClip[{coords, cells}, HalfSpace[n, (c*n)/(n . n)], args]


halfSpaceClip[mdata:{coords_, cells_}, HalfSpace[n_?cardinalDirectionQ, p_List], closeQ_] :=
	Block[{k, clip},
		k = Random`Private`PositionsOf[Unitize[n], 1][[1]];
		
		clip = clipCatchFull @ cardinalClip[coords, cells, p[[k]], k, Sign[n[[k]]], closeQ];
		
		(
			If[First[clip] === "FullyContained",
				mdata,
				clip
			]
		) /; clip =!= $Failed
	]


halfSpaceClip[mdata:{coords_, cells_}, HalfSpace[n:Except[_?cardinalDirectionQ], p_List], closeQ_] :=
	Block[{rt, rot, clip},
		rt = Quiet[RotationTransform[{n, {0, 0, -1}}] @* TranslationTransform[-p], RotationTransform::spln];
		
		clip = clipCatchFull @ cardinalClip[rt[coords], cells, 0., 3, -1, closeQ];
		
		(
			If[First[clip] === "FullyContained",
				mdata,
				MapAt[InverseFunction[rt], clip, {1}]
			]
		) /; clip =!= $Failed
	]


halfSpaceClip[___] = $Failed;


cardinalDirectionQ[n_] := VectorQ[n, Internal`RealValuedNumericQ] && Total[Unitize[n]] == 1


$fullclip = "Full";


clipThrowFull[mr_] := Throw[mr, $fullclip];


SetAttributes[clipCatchFull, HoldFirst];
clipCatchFull[code_] := Catch[code, $fullclip, If[# === $Failed, $Failed, {"FullyContained", #}]&]


(* ::Subsection::Closed:: *)
(*cardinalClip*)


cardinalClip[coords_, cells_, ord_, dim_, dir_, closeQ_] :=
	Block[{len, clipdata, metalen, cnt, l1, l2, l3, l4, l5, coordskeepinds, clipcoordsraw, clipcoords, clipcoordspos, coordsnew, cellsnewraw, rep1, rep2, rep, cellsnew, bdsraw, bds, cap},
		len = Length[coords];
		clipdata = cardinalClipC[coords, cells, ord, dim, dir];
		metalen = 6;
		{cnt, l1, l2, l3, l4, l5} = Round[clipdata[[1 ;; metalen]]];
		
		If[Max[l1, l2, l3, l4, l5] == 0, clipThrowEmpty[]];
		If[l2 == Length[cells] && l4 == 0, clipThrowFull[{coords, cells}]];
		
		coordskeepinds = Round[clipdata[[metalen+1 ;; l1+metalen]]];
		coordsnew = coords[[coordskeepinds]];
		cellsnewraw = cells[[Round[clipdata[[metalen+l1+1 ;; metalen+l1+l2]]]]];
		
		rep1 = ConstantArray[0, len];
		rep1[[coordskeepinds]] = Range[l1];
		
		If[l4 == 0,
			rep = rep1;,
			
			clipcoordsraw = Partition[clipdata[[metalen+l1+l2+1 ;; metalen+l1+l2+l3]], 3];
			clipcoords = DeleteDuplicates[clipcoordsraw];
			clipcoordspos = Nearest[clipcoords -> "Index", clipcoordsraw][[All, 1]];
			
			coordsnew = Join[coordsnew, clipcoords];
			cellsnewraw = Join[cellsnewraw, Partition[Round[clipdata[[metalen+l1+l2+l3+1 ;; metalen+l1+l2+l3+l4]]], 3]];
			
			rep2 = l1 + clipcoordspos;
			rep = Join[rep1, rep2];
		];
		cellsnew = Region`Mesh`ReplaceIncidents[cellsnewraw, rep];
		
		If[closeQ && l5 > 0,
			bdsraw = Partition[Round[clipdata[[metalen+l1+l2+l3+l4+1 ;; metalen+l1+l2+l3+l4+l5]]], 2];
			bds = Region`Mesh`ReplaceIncidents[bdsraw, rep];
			
			cap = closeCardinalClip[coordsnew, cellsnew, bds, ord, dim, dir];
			If[!MatchQ[cap, {{___}, {___}}], Return[$Failed]];
			
			coordsnew = Join[coordsnew, First[cap]];
			cellsnew = Join[cellsnew, Last[cap]];
			
			{coordsnew, cellsnew} = deleteDuplicateClipPlaneCoordinates[coordsnew, cellsnew, ord, dim];
		];
		
		{coordsnew, cellsnew}
	]

cardinalClip[___] = $Failed;


closeCardinalClip[coords_, cells_, bds_, ord_, dim_, dir_] :=
	Block[{cproj, bmr, tri, tricoords, tricells, cutpos, bdpos, bdcoordsinds, newcoordspos, newcoords, rep, newcells},
		cproj = coords[[All, Delete[Range[3], {dim}]]];
		bmr = Quiet @ BoundaryMeshRegion[cproj, Line[bds], Method -> {"CheckIntersections" -> False, "DeleteDuplicateCells" -> False}];
		(
			tri = Quiet @ UTLaxBlock[iTriangulateMesh[bmr]];
			(
				tricoords = MeshCoordinates[tri];
				tricells = Join @@ MeshCells[tri, 2, "Multicells" -> True][[All, 1]];
				
				newcoords = Transpose[Insert[Transpose[tricoords], ConstantArray[ord, Length[tricoords]], dim]];
				
				cutpos = Union[Flatten[bds]];
				bdpos = Union[Flatten[MeshCells[tri, {0, "Boundary"}, "MultiCells" -> True][[All, 1]]]];
				bdcoordsinds = cutpos[[Nearest[coords[[cutpos]] -> "Index", newcoords[[bdpos]]][[All, 1]]]];
				
				newcoordspos = Complement[Range[Length[tricoords]], bdpos];
				newcoords = newcoords[[newcoordspos]];
				
				rep = ConstantArray[0, Length[tricoords]];
				rep[[bdpos]] = bdcoordsinds;
				rep[[newcoordspos]] = Range[Length[coords]+1, Length[coords]+Length[newcoordspos]];
				
				newcells = Region`Mesh`ReplaceIncidents[tricells, rep];
				If[(dir < 0 && dim =!= 2) || (dir > 0 && dim === 2),
					newcells = Reverse[newcells, {2}];
				];
				
				{newcoords, newcells}
				
			) /; RegionQ[tri]
			
		) /; RegionQ[bmr]
	]

closeCardinalClip[___] = $Failed;


UTCompile[cardinalClipSegment, {{p1, _Real, 1}, {p2, _Real, 1}, {ord, _Real}, {dim, _Integer}},
	Module[{x1,y1,z1,x2,y2,z2},
		x1 = Compile`GetElement[p1, 1];
		y1 = Compile`GetElement[p1, 2];
		z1 = Compile`GetElement[p1, 3];
		x2 = Compile`GetElement[p2, 1];
		y2 = Compile`GetElement[p2, 2];
		z2 = Compile`GetElement[p2, 3];
		
		Which[
			dim === 1,
			{
				ord, 
				Divide[Subtract[y2 Subtract[x1, ord], y1 Subtract[x2, ord]], Subtract[x1, x2]], 
				Divide[Subtract[z2 Subtract[x1, ord], z1 Subtract[x2, ord]], Subtract[x1, x2]]
			},
			dim === 2,
			{
				Divide[Subtract[x2 Subtract[y1, ord], x1 Subtract[y2, ord]], Subtract[y1, y2]], 
				ord, 
				Divide[Subtract[z2 Subtract[y1, ord], z1 Subtract[y2, ord]], Subtract[y1, y2]]
			},
			True,
			{ 
				Divide[Subtract[x2 Subtract[z1, ord], x1 Subtract[z2, ord]], Subtract[z1, z2]],
				Divide[Subtract[y2 Subtract[z1, ord], y1 Subtract[z2, ord]], Subtract[z1, z2]], 
				ord
			}
		]
	]
];


UTCompile[cardinalClipC, {{coords, _Real, 2}, {cells, _Integer, 2}, {ord, _Real}, {dim, _Integer}, {sdir, _Integer}},
	Module[{cnt, coordskeep, cellskeepbag, coordsnewbag, cellsnewbag, bdbag, inds, pts, ords, min, max, sgns, tot, a, b, c, coord1, coord2, oldcoordsinds, oldccellinds, coordsnew, cellsnew, bds},
		cnt = Length[coords];
		coordskeep = Table[0, {cnt}];
		cellskeepbag = Internal`Bag[Most[{0}]];
		coordsnewbag = Internal`Bag[Most[{0.}]];
		cellsnewbag = Internal`Bag[Most[{0}]];
		bdbag = Internal`Bag[Most[{0}]];
		
		Do[
			inds = Compile`GetElement[cells, k];
			pts = coords[[inds]];
			ords = pts[[All, dim]];
			min = Min[ords];
			max = Max[ords];
			
			sgns = Sign[Subtract[ords, ord]];
			tot = Total[sgns];
			
			(* keep cells completely contained in the halfspace and ignore the disjoint cells *)
			If[sdir === -1,
				If[max <= ord, Continue[]];
				If[min >= ord, 
					coordskeep[[inds]] = inds;
					Internal`StuffBag[cellskeepbag, k];
					If[min == ord && min < max && Sort[ords][[2]] == ord,
						Do[If[Compile`GetElement[ords, i] == ord, Internal`StuffBag[bdbag, Compile`GetElement[inds, i]]];, {i, 3}]
					];
					Continue[]
				],
				If[min >= ord, Continue[]];
				If[max <= ord, 
					coordskeep[[inds]] = inds;
					Internal`StuffBag[cellskeepbag, k];
					If[max == ord && min < max && Sort[ords][[2]] == ord,
						Do[If[Compile`GetElement[ords, i] == ord, Internal`StuffBag[bdbag, Compile`GetElement[inds, i]]];, {i, 3}]
					];
					Continue[]
				]
			];
			
			If[tot =!= 0,
				(* exactly two segments of the triangle intersect with the clip plane *)
				(* a is the index of the coordinate that's the other side of the clip plane as b and c *)
				{a, b, c} = Which[
					Compile`GetElement[sgns, 1] =!= tot,
						{1, 2, 3},
					Compile`GetElement[sgns, 2] =!= tot,
						{2, 3, 1},
					True,
						{3, 1, 2}
				];
				
				Do[
					If[Compile`GetElement[sgns, i] =!= sdir, coordskeep[[Compile`GetElement[inds, i]]] = Compile`GetElement[inds, i]];,
					{i, 3}
				];
				
				(* store the new coordinates *)
				cnt += 2;
				coord1 = cardinalClipSegment[Compile`GetElement[pts, a], Compile`GetElement[pts, b], ord, dim];
				coord2 = cardinalClipSegment[Compile`GetElement[pts, a], Compile`GetElement[pts, c], ord, dim];
				Internal`StuffBag[coordsnewbag, coord1, 1];
				Internal`StuffBag[coordsnewbag, coord2, 1];
				
				(* store the new cell(s) with same orientation as the original triangle *)
				Which[
					(* b and c are outside the halfspace and a is inside -- this gives a single triangle *)
					tot === sdir, 
						Internal`StuffBag[cellsnewbag, Compile`GetElement[inds, a]];
						Internal`StuffBag[cellsnewbag, cnt-1];
						Internal`StuffBag[cellsnewbag, cnt];
						Internal`StuffBag[bdbag, cnt-1];
						Internal`StuffBag[bdbag, cnt],
					(* b and c are inside the halfspace and a is outside -- this gives 2 triangles and we pick the tris that have the smallest common diagonal *)
					Total[Subtract[Compile`GetElement[inds, b], coord2]^2] <= Total[Subtract[Compile`GetElement[inds, c], coord1]^2],
						(* triangle 1 *)
						Internal`StuffBag[cellsnewbag, Compile`GetElement[inds, b]];
						Internal`StuffBag[cellsnewbag, cnt];
						Internal`StuffBag[cellsnewbag, cnt-1];
						(* triangle 2 *)
						Internal`StuffBag[cellsnewbag, Compile`GetElement[inds, b]];
						Internal`StuffBag[cellsnewbag, Compile`GetElement[inds, c]];
						Internal`StuffBag[cellsnewbag, cnt];
						(* oriented boundary edge *)
						Internal`StuffBag[bdbag, cnt];
						Internal`StuffBag[bdbag, cnt-1],
					True,
						(* triangle 1 *)
						Internal`StuffBag[cellsnewbag, Compile`GetElement[inds, c]];
						Internal`StuffBag[cellsnewbag, cnt];
						Internal`StuffBag[cellsnewbag, cnt-1];
						(* triangle 2 *)
						Internal`StuffBag[cellsnewbag, Compile`GetElement[inds, b]];
						Internal`StuffBag[cellsnewbag, Compile`GetElement[inds, c]];
						Internal`StuffBag[cellsnewbag, cnt-1];
						(* oriented boundary edge *)
						Internal`StuffBag[bdbag, cnt];
						Internal`StuffBag[bdbag, cnt-1]
				],
				
				(* exactly one segment of the triangle intersects with the clip plane *)
				(* a is the index of the coordinate that lies on the clip plane -- we could canonicalize more by forcing b to lie above the plane, but this might not preserve orientation of the triangle *)
				{a, b, c} = Which[
					Compile`GetElement[sgns, 1] === 0,
						{1, 2, 3},
					Compile`GetElement[sgns, 2] === 0,
						{2, 3, 1},
					True,
						{3, 1, 2}
				];
				
				(* store the new coordinate *)
				cnt++;
				Internal`StuffBag[coordsnewbag, cardinalClipSegment[Compile`GetElement[pts, b], Compile`GetElement[pts, c], ord, dim], 1];
				
				(* store the new cell with same orientation as the original triangle *)
				If[Compile`GetElement[sgns, b] =!= sdir,
					coordskeep[[Compile`GetElement[inds, a]]] = Compile`GetElement[inds, a];
					coordskeep[[Compile`GetElement[inds, b]]] = Compile`GetElement[inds, b];
					Internal`StuffBag[cellsnewbag, Compile`GetElement[inds, a]];
					Internal`StuffBag[cellsnewbag, Compile`GetElement[inds, b]];
					Internal`StuffBag[cellsnewbag, cnt];
					Internal`StuffBag[bdbag, cnt];
					Internal`StuffBag[bdbag, Compile`GetElement[inds, a]];,
					coordskeep[[Compile`GetElement[inds, a]]] = Compile`GetElement[inds, a];
					coordskeep[[Compile`GetElement[inds, c]]] = Compile`GetElement[inds, c];
					Internal`StuffBag[cellsnewbag, Compile`GetElement[inds, a]];
					Internal`StuffBag[cellsnewbag, cnt];
					Internal`StuffBag[cellsnewbag, Compile`GetElement[inds, c]];
					Internal`StuffBag[bdbag, Compile`GetElement[inds, a]];
					Internal`StuffBag[bdbag, cnt];
				]
			],
			{k, Length[cells]}
		];
		
		oldcoordsinds = Select[coordskeep, Positive];
		oldccellinds = Internal`BagPart[cellskeepbag, All];
		coordsnew = Internal`BagPart[coordsnewbag, All];
		cellsnew = Internal`BagPart[cellsnewbag, All];
		bds = Internal`BagPart[bdbag, All];
		
		Join[{cnt, Length[oldcoordsinds], Length[oldccellinds], Length[coordsnew], Length[cellsnew], Length[bds]}, oldcoordsinds, oldccellinds, coordsnew, cellsnew, bds]
	]
];


(* ::Subsection::Closed:: *)
(*deleteDuplicateClipPlaneCoordinates*)


deleteDuplicateClipPlaneCoordinates[coords_, cells_, ord_, dim_] :=
	Block[{n, \[CurlyEpsilon], dupcands, coordsnew, cellsnew, m, todelete, tokeep, rep, coords2, cells2},
		n = Length[coords];
		\[CurlyEpsilon] = Max[Abs[Subtract @@@ CoordinateBounds[coords]]] * 10.^(Internal`$EqualTolerance - MachinePrecision);
		dupcands = Random`Private`PositionsOf[Threshold[Abs[coords[[All, dim]] - ord], \[CurlyEpsilon]], NonPositive];
		
		(* No duplicates for sure *)
		If[Length[dupcands] === 0, 
			Return[{coords, cells}]
		];
		
		{coordsnew, cellsnew} = Region`Mesh`DeleteDuplicateCoordinates[coords[[dupcands]]];
		
		(* No duplicates according to MeshRegion *)
		If[!ListQ[cellsnew],
			Return[{coords, cells}]
		];
		
		m = Length[dupcands];
		todelete = Partition[dupcands, 1];
		tokeep = Delete[Range[n], todelete];
		
		rep = ConstantArray[0, n];
		rep[[tokeep]] = Range[Length[tokeep]];
		rep[[dupcands]] = cellsnew + n - m;
		
		coords2 = Join[Delete[coords, todelete], coordsnew];
		cells2 = Region`Mesh`ReplaceIncidents[cells, rep];
		
		{coords2, cells2}
	]


(* ::Subsection::Closed:: *)
(*assembleClippedMesh*)


assembleClippedMesh[{___, {}, ___}, __] = EmptyRegion[3];


assembleClippedMesh[spec_, head_] := assembleClippedMesh[spec, head, Identity]


assembleClippedMesh[{coords_, cells_}, head_, tf_] := 
	Quiet @ head[
		tf @ coords, 
		Polygon[cells], 
		Method -> {"DeleteDuplicateCoordinates" -> False, "DeleteDuplicateCells" -> False, "EliminateUnusedCoordinates" -> False, "TJunction" -> False, "CheckIntersections" -> False}
	]


assembleClippedMesh[mr:(_MeshRegion|_BoundaryMeshRegion), _, tf_] := UTCoordinateApply[mr, tf]


assembleClippedMesh[reg_?ConstantRegionQ, _, tf_] := TransformedRegion[reg, tf]


assembleClippedMesh[___] = $Failed;


(* ::Section:: *)
(*UTSlice*)


(* ::Subsection::Closed:: *)
(*Main*)


Clear[UTSlice];


Options[UTSlice] = {"CloseBoundary" -> Automatic, "ProjectSlice" -> True};


UTSlice[args__, opts:OptionsPattern[]] :=
	Block[{res},
		res = clipCatchEmpty @ iSlice[args, opts];
		
		(
			If[TrueQ[OptionValue["ProjectSlice"]] && Head[res] === EmptyRegion,
				EmptyRegion[2],
				res
			]
		) /; RegionQ[res]
	]


UTSlice[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Argument conform & completion*)


registerForDiscretization[UTSlice, 1];


(* ::Subsection::Closed:: *)
(*iSlice*)


Options[iSlice] = Options[UTSlice];


iSlice[mr_?UTTriangleMesh3DQ, spec:(Except[_?OptionQ]..), opts:OptionsPattern[]] :=
	Block[{halfspaces, closeQ, head, projQ, res},
		halfspaces = canonicalizeHalfSpaceSpecs[mr, spec];
		(
			closeQ = processSliceCloseBoundary[mr, opts];
			head = If[closeQ, BoundaryMeshRegion, MeshRegion];
			projQ = TrueQ[OptionValue["ProjectSlice"]];
			res = halfSpaceSlice[mr, halfspaces, head, projQ];
			
			res /; RegionQ[res]
			
		) /; MatchQ[halfspaces, {_HalfSpace}|{}]
	]


iSlice[___] = $Failed;


processSliceCloseBoundary[mr_, OptionsPattern[iSlice]] := 
	Block[{closeQ},
		closeQ = OptionValue["CloseBoundary"];
		If[BooleanQ[closeQ],
			closeQ,
			BoundaryMeshRegionQ[mr] || Max[Mod[DeleteDuplicates[Differences[mr["ConnectivityMatrix"[1, 2]]["RowPointers"]]], 2]] == 0
		]
	]


(* ::Subsection::Closed:: *)
(*halfSpaceSlice*)


halfSpaceSlice[EmptyRegion[3], ___, projQ_] := If[projQ, EmptyRegion[2], EmptyRegion[3]];


halfSpaceSlice[_, {}, _, projQ_] := If[projQ, EmptyRegion[2], EmptyRegion[3]];


halfSpaceSlice[mr_, {HalfSpace[n_, c_?NumericQ]}, args___] := halfSpaceSlice[mr, {HalfSpace[n, (c*n)/(n . n)]}, args]


halfSpaceSlice[mr_, {HalfSpace[n_?cardinalDirectionQ, p_List]}, head_, projQ_] :=
	Block[{k, slice, dim},
		k = Random`Private`PositionsOf[Unitize[n], 1][[1]];
		slice = cardinalSlice[MeshCoordinates[mr], Join @@ MeshCells[mr, 2, "Multicells" -> True][[All, 1]], p[[k]], k, n[[k]], head];
		(
			If[!projQ,
				slice = cardinalEmbed3D[slice, p[[k]], k];
			];
			
			slice /; RegionQ[slice]
			
		 ) /; RegionQ[slice]
	]


halfSpaceSlice[mr_, {HalfSpace[n:Except[_?cardinalDirectionQ], p_List]}, head_, projQ_] :=
	Block[{rt, rot, slice},
		rt = Quiet[RotationTransform[{n, {0, 0, 1}}] @* TranslationTransform[-p], RotationTransform::spln];
		
		slice = cardinalSlice[rt[MeshCoordinates[mr]], Join @@ MeshCells[mr, 2, "Multicells" -> True][[All, 1]], 0., 3, -1, head];
		(
			If[!projQ,
				slice = embed3D[slice, rt];
			];
			
			slice /; RegionQ[slice]
			
		 ) /; RegionQ[slice]
	]


halfSpaceSlice[___] = $Failed;


(* ::Subsection::Closed:: *)
(*cardinalSlice*)


cardinalSlice[coords_, cells_, ordinal_, k_, dir_, head_, \[Delta]_:20000] :=
	Block[{bds, \[CurlyEpsilon], sgns, cnt, crossingcells, crossingpairs, segs, slicecoords, slicecells, tfunc, bmr},
		bds = CoordinateBounds[coords];
		\[CurlyEpsilon] = Max[Abs[Subtract @@@ bds]] * 10.^(Internal`$EqualTolerance - MachinePrecision);
		sgns = Sign[Threshold[coords[[All, k]] - ordinal, {"Hard", \[CurlyEpsilon]}]];
		
		cnt = Length[cells];
		crossingcells = Join @@ Table[
			Pick[#, faceCrosses[sgns, #], 1]&[cells[[Range[\[Delta](i-1)+1, Min[cnt, \[Delta]*i]]]]],
			{i, Ceiling[cnt/\[Delta]]}
		];
		If[Length[crossingcells] == 0, Return[EmptyRegion[2]]];
		
		crossingpairs = adjacentCrossingSegments[sgns, crossingcells];
		segs = Union @@ crossingpairs;
		
		slicecoords = crossSegments[[k]][coords, segs, \[CurlyEpsilon], ordinal];
		slicecells = Replace[crossingpairs, Dispatch[Thread[segs -> Range[Length[segs]]]], {2}];
		tfunc = canonicalProjTrans[bds, k, dir];
		
		bmr = Quiet @ UTLaxBlock @ head[tfunc[slicecoords], Line[slicecells], Method -> {"CheckIntersections" -> False}];
		
		If[!RegionQ[bmr], $Failed, bmr]
	]


(* ::Subsection::Closed:: *)
(*Compiled utilities*)


UTCompile[faceCrosses, {{sgns, _Integer, 1}, {face, _Integer, 1}},
	Block[{s1, s2, s3},
		s1 = Compile`GetElement[sgns, Compile`GetElement[face, 1]];
		s2 = Compile`GetElement[sgns, Compile`GetElement[face, 2]];
		s3 = Compile`GetElement[sgns, Compile`GetElement[face, 3]];
		
		Which[
			s1 === 0, Boole[Abs[Subtract[s2, s3]] === 2 || (s2 === 0 && Abs[s3] === 1) || (Abs[s2] === 1 && s3 === 0)],
			s2 === 0, Boole[s1 =!= s3],
			s3 === 0, Boole[s1 =!= s2],
			True, Boole[!SameQ[s1, s2, s3]]
		]
	],
	RuntimeAttributes -> {Listable}
];

With[{
	lookup1 = {{0,0},{1,3},{0,0},{2,3},{1,3},{1,2},{1,2},{2,1},{0,0},{1,3},{2,3},{1,2},{0,0},{1,2},{2,3},{1,3},{0,0},{2,1},{1,2},{1,2},{1,3},{2,3},{0,0},{1,3},{0,0},{0,0}},
	lookup2 = {{0,0},{2,3},{0,0},{3,2},{2,3},{3,2},{2,3},{3,1},{0,0},{3,1},{1,3},{2,1},{0,0},{2,1},{1,2},{3,1},{0,0},{3,1},{1,3},{3,2},{1,2},{3,2},{0,0},{2,3},{0,0},{0,0}},
	degenerate = {0,0,0,1,0,0,0,0,0,1,0,1,0,1,0,1,0,0,0,0,0,1,0,0,0,0}
},

UTCompile[adjacentCrossingSegments, {{sgns, _Integer, 1}, {face, _Integer, 1}},
	Block[{sig, seg1, seg2},
		sig = {9, 3, 1} . sgns[[face]] + 13;
		seg1 = face[[Compile`GetElement[lookup1, sig]]];
		seg2 = face[[Compile`GetElement[lookup2, sig]]];
		
		(*If[Compile`GetElement[degenerate, sig] == 0,
			seg1 = Sort[seg1];
			seg2 = Sort[seg2];
		];*)
		
		{seg1, seg2}
	],
	RuntimeAttributes -> {Listable}
]
];

crossSegments = {
UTCompile[crossSegmentsX, {{coords, _Real, 2}, {seg, _Integer, 1}, {\[CurlyEpsilon], _Real}, {ord, _Real}},
	Block[{x1,y1,z1,x2,y2,z2},
		{z1,x1,y1} = Compile`GetElement[coords, Compile`GetElement[seg, 1]];
		{z2,x2,y2} = Compile`GetElement[coords, Compile`GetElement[seg, 2]];
		
		If[Abs[z1-ord] <= \[CurlyEpsilon] && Abs[z2-ord] <= \[CurlyEpsilon],
			{x1, y1},
			Divide[{Subtract[x2 Subtract[z1, ord], x1 Subtract[z2, ord]], Subtract[y2 Subtract[z1, ord], y1 Subtract[z2, ord]]}, Subtract[z1, z2]]
		]
	],
	RuntimeAttributes -> {Listable}
],
UTCompile[crossSegmentsY, {{coords, _Real, 2}, {seg, _Integer, 1}, {\[CurlyEpsilon], _Real}, {ord, _Real}},
	Block[{x1,y1,z1,x2,y2,z2},
		{x1,z1,y1} = Compile`GetElement[coords, Compile`GetElement[seg, 1]];
		{x2,z2,y2} = Compile`GetElement[coords, Compile`GetElement[seg, 2]];
		
		If[Abs[z1-ord] <= \[CurlyEpsilon] && Abs[z2-ord] <= \[CurlyEpsilon],
			{x1, y1},
			Divide[{Subtract[x2 Subtract[z1, ord], x1 Subtract[z2, ord]], Subtract[y2 Subtract[z1, ord], y1 Subtract[z2, ord]]}, Subtract[z1, z2]]
		]
	],
	RuntimeAttributes -> {Listable}
],
UTCompile[crossSegmentsZ, {{coords, _Real, 2}, {seg, _Integer, 1}, {\[CurlyEpsilon], _Real}, {ord, _Real}},
	Block[{x1,y1,z1,x2,y2,z2},
		{x1,y1,z1} = Compile`GetElement[coords, Compile`GetElement[seg, 1]];
		{x2,y2,z2} = Compile`GetElement[coords, Compile`GetElement[seg, 2]];
		
		If[Abs[z1-ord] <= \[CurlyEpsilon] && Abs[z2-ord] <= \[CurlyEpsilon],
			{x1, y1},
			Divide[{Subtract[x2 Subtract[z1, ord], x1 Subtract[z2, ord]], Subtract[y2 Subtract[z1, ord], y1 Subtract[z2, ord]]}, Subtract[z1, z2]]
		]
	],
	RuntimeAttributes -> {Listable}
]
};


(* ::Subsection::Closed:: *)
(*Projection & embedding utilities*)


canonicalProjTrans[{{x1_, x2_}, {y1_, y2_}}, 1, _?Negative] := ReflectionTransform[{1, 1}, 0{x1, y1}] @* RotationTransform[\[Pi]]
canonicalProjTrans[{{x1_, x2_}, {y1_, y2_}}, 1, _] = RotationTransform[\[Pi]/2];
canonicalProjTrans[{_, {y1_, y2_}}, 2, _?Positive] := TranslationTransform[{0., -2y2}] @* ReflectionTransform[{0, 1}, {0, y2}]
canonicalProjTrans[{_, {y1_, y2_}}, 3, _?Negative] := TranslationTransform[{0., -2y2}] @* ReflectionTransform[{0, 1}, {0, y2}]
canonicalProjTrans[___] = Identity;


cardinalEmbed3D[_EmptyRegion, ___] = EmptyRegion[3];

cardinalEmbed3D[slice_, ordinal_, k_] :=
	Block[{slice3d, dim},
		slice3d = embedRegionProduct[slice, N[ordinal]];
		(
			dim = RegionDimension[slice];
			Switch[k, 
				1, slice3d = MeshRegion[MeshCoordinates[slice3d][[All, {3, 1, 2}]], MeshCells[slice3d, dim, "Multicells" -> True]],
				2, slice3d = MeshRegion[MeshCoordinates[slice3d][[All, {1, 3, 2}]], MeshCells[slice3d, dim, "Multicells" -> True]]
			];
			
			slice3d /; MeshRegionQ[slice3d]
			
		) /; MeshRegionQ[slice3d]
	]

cardinalEmbed3D[___] = $Failed;


embed3D[_EmptyRegion, ___] = EmptyRegion[3];

embed3D[slice_, forwardtrans_] :=
	Block[{slice3d},
		slice3d = embedRegionProduct[slice, 0.];
		(
			slice3d = UTFastBlock[InverseFunction[forwardtrans][slice3d]];
			
			slice3d /; MeshRegionQ[slice3d]
			
		) /; MeshRegionQ[slice3d]
	]

embed3D[___] = $Failed;


Clear[embedRegionProduct]


embedRegionProduct[_EmptyRegion, ___] = EmptyRegion[3];

embedRegionProduct[slice_, ordinal_] :=
	With[{res = Quiet @ RegionProduct[slice, Point[{N[ordinal]}]]},
		res /; MeshRegionQ[res]
	]

embedRegionProduct[___] = $Failed;


(* ::Section:: *)
(*UTZSlices*)


(* ::Subsection::Closed:: *)
(*Main*)


Options[UTZSlices] = {"CloseBoundary" -> True, "LowDimensionComponents" -> True, "ProjectSlice" -> True, "SliceFunction" -> (#&)};


UTZSlices[mr_?UTTriangleMesh3DQ, \[Delta]z_?Positive, opts:OptionsPattern[]] := 
	With[{bds = RegionBounds[mr][[3]]},
		UTZSlices[mr, {bds[[1]] + 0.5\[Delta]z, bds[[2]] - 0.5\[Delta]z, \[Delta]z}, opts]
	]


UTZSlices[mr_?UTTriangleMesh3DQ, {zmin_, zmax_, \[Delta]z_?Positive}, opts:OptionsPattern[]] /; zmin <= zmax :=
	Block[{closeQ, lowdimQ, projQ, func},
		closeQ = TrueQ[OptionValue["CloseBoundary"]];
		lowdimQ = TrueQ[OptionValue["LowDimensionComponents"]];
		projQ = TrueQ[OptionValue["ProjectSlice"]];
		func = OptionValue["SliceFunction"];
		
		iZSlices[mr, {zmin, zmax, \[Delta]z}, closeQ, lowdimQ, projQ, func]
	]


UTZSlices[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Argument conform & completion*)


registerForDiscretization[UTZSlices, 1];


(* ::Subsection::Closed:: *)
(*iZSlices*)


iZSlices[mr_?UTTriangleMesh3DQ, {zmin_, zmax_, \[Delta]z_}, closeQ_, lowdimQ_, projQ_, func_] :=
	Block[{coords, cells, tree, \[CurlyEpsilon], zs, slices},
		{coords, cells, tree} = meshZIntervalTreeData[mr];
		
		\[CurlyEpsilon] = 10.^(Internal`$EqualTolerance - MachinePrecision) * Max[Abs[Subtract @@@ RegionBounds[mr]]];
		zs = Range[N[zmin], zmax, \[Delta]z];
		
		slices = Table[
			func[
				sliceRegion[iZSlice[coords, cells, tree, zs[[i]], \[CurlyEpsilon], closeQ, lowdimQ], closeQ, lowdimQ],
				zs[[i]],
				i
			],
			{i, Length[zs]}
		];
		
		Clear[tree];
		
		If[!projQ,
			slices = Quiet @ UTFastBlock[UTJoin[Select[MapThread[RegionProduct[#1, Point[{#2}]]&, {slices, zs}], MeshRegionQ]]];
		];
		
		slices
	]


(* ::Subsection::Closed:: *)
(*sliceRegion*)


sliceRegion[slicedata_List, closeQ_, lowdimQ_] :=
	Block[{meshes, lowdimcomps, fulldimcomps, res},
		meshes = Flatten[iSliceRegion /@ slicedata];
		(
			fulldimcomps = Select[meshes, #["ComponentDimensions"] === {2}&];
			
			lowdimcomps = Which[
				Length[meshes] === Length[fulldimcomps], EmptyRegion[2],
				lowdimQ, UTFastBlock[Region`Mesh`MeshRegionJoin @@ Select[meshes, #["ComponentDimensions"] =!= {2}&]],
				closeQ, EmptyRegion[2],
				!closeQ && Max[RegionDimension /@ meshes] === 0, EmptyRegion[2],
				True, UTFastBlock[Region`Mesh`MeshRegionJoin @@ Select[meshes, #["ComponentDimensions"] === {1}&]]
			];
			
			fulldimcomps = Which[
				Length[fulldimcomps] === 1, First[fulldimcomps],
				Length[fulldimcomps] === 0, EmptyRegion[2],
				True, Quiet @ RegionUnion[fulldimcomps]
			];
			
			(
				res = Quiet @ Which[
					Head[lowdimcomps] === EmptyRegion, fulldimcomps,
					Head[fulldimcomps] === EmptyRegion, lowdimcomps,
					BoundaryMeshRegionQ[fulldimcomps], Region`Mesh`MeshRegionJoin[lowdimcomps, iTriangulateMeshRepair[fulldimcomps]],
					True, Region`Mesh`MeshRegionJoin[lowdimcomps, fulldimcomps]
				];
				
				res /; meshOrEmptyQ[res]
			
			) /; meshOrEmptyQ[fulldimcomps] && meshOrEmptyQ[lowdimcomps]
			
		) /; VectorQ[meshes, RegionQ]
	]


sliceRegion[___] = $Failed;


meshOrEmptyQ = MeshRegionQ[#] || BoundaryMeshRegionQ[#] || Head[#] === EmptyRegion&;


iSliceRegion[{2 -> BoundaryMeshRegion, pts_, cells_}] :=
	Block[{res},
		res = Quiet @ BoundaryMeshRegion[
			pts, 
			Sequence @@ Line /@ cells,
			Method -> {"EliminateUnusedCoordinates" -> False, "DeleteDuplicateCoordinates" -> False, "DeleteDuplicateCells" -> False, 
				"TJunction" -> False, "CheckIntersections" -> False, "SeparateBoundaries" -> False}
		];
		
		res /; RegionQ[res]
	];


iSliceRegion[{0 -> MeshRegion, pts_, cells_}] :=
	MeshRegion[pts, Point[cells], Method -> {"EliminateUnusedCoordinates" -> False, "DeleteDuplicateCoordinates" -> False, "DeleteDuplicateCells" -> False, "CheckIntersections" -> False}];


iSliceRegion[{1 -> MeshRegion, pts_, cells_}] := 
	MeshRegion[pts, Line[cells], Method -> {"EliminateUnusedCoordinates" -> False, "DeleteDuplicateCoordinates" -> False}];


iSliceRegion[{2 -> MeshRegion, pts_, cells_}] := 
	MeshRegion[pts, Polygon[cells], Method -> {"EliminateUnusedCoordinates" -> False, "DeleteDuplicateCoordinates" -> False}];


iSliceRegion[{2 -> BoundaryMeshRegion, bmr_BoundaryMeshRegion?BoundaryMeshRegionQ}] := bmr


iSliceRegion[{2 -> BoundaryMeshRegion, pts_, cells_}] :=
	Block[{mrcomps, bmrcomps, mres, bres, res},
		{mrcomps, bmrcomps} = Reap[
			Scan[Sow[#, If[Times @@ Subtract @@@ CoordinateBounds[pts[[#]]] == 0., "lines", "blines"]]&, cells],
			{"lines", "blines"}
		][[-1]];
		mrcomps = First[mrcomps, {}];
		bmrcomps = First[bmrcomps, {}];
		(
			mres = MeshRegion[pts, Line[mrcomps], Method -> {"DeleteDuplicateCoordinates" -> False, "DeleteDuplicateCells" -> False}];
			If[Length[bmrcomps] > 0,
				bres = Quiet @ BoundaryMeshRegion[
					pts,
					Sequence @@ Line /@ bmrcomps, 
					Method -> {"DeleteDuplicateCoordinates" -> False, "DeleteDuplicateCells" -> False, "TJunction" -> False, "CheckIntersections" -> False, "SeparateBoundaries" -> False}
				];
				res = {mres, bres},
				res = mres
			];

			res /; VectorQ[res, RegionQ] || RegionQ[res]
		
		) /; Length[mrcomps] > 0
];


iSliceRegion[___] = $Failed;


iTriangulateMeshRepair[bmr_] := 
	With[{mr = Quiet @ iTriangulateMesh[bmr, MaxCellMeasure -> \[Infinity], MeshQualityGoal -> 0]}, 
		mr /; MeshRegionQ[mr]
	]


iTriangulateMeshRepair[bmr_] := 
	Block[{repair, mr}, 
		repair = FastBoundaryMeshRegion[#1, Line[#2]]& @@ Region`Mesh`SplitIntersectingSegments[MeshPrimitives[bmr, 1]];
		(
			mr = Quiet @ iTriangulateMesh[repair, MaxCellMeasure -> \[Infinity], MeshQualityGoal -> 0];
			
			mr /; MeshRegionQ[mr]
			
		) /; BoundaryMeshRegionQ[repair]
	]


iTriangulateMeshRepair[___] = $Failed;


(* ::Section:: *)
(*UTDynamicZSlice*)


(* ::Subsection::Closed:: *)
(*Main*)


Clear[UTDynamicZSlice]


Options[UTDynamicZSlice] = {
	"CloseBoundary" -> True, "LowDimensionComponents" -> True,
	"SelfContained" -> Automatic, AppearanceElements -> Automatic,
	ImageSize -> Automatic, "SliceStyle" -> Automatic, Mesh -> None
};


UTDynamicZSlice[mr_?UTTriangleMesh3DQ, z_:Automatic, opts:OptionsPattern[]] /; !OptionQ[z] :=
	Module[{
			coords, cells, tree, bds, bds2d, p = 0.025, zlow, zhigh, \[CurlyEpsilon], closeQ, lowdimQ, sliceHead, img, rast, dims = {{0,0},{0,0}}, 
			w, h, sliderx, getSlice, UILine, thinLine, boldLine, selfContainedQ, size, sty, meshQ, controls, orientation, z0
		},
		{coords, cells, tree} = meshZIntervalTreeData[mr];
		
		{controls, orientation} = parseDZAppearanceElements[OptionValue[AppearanceElements]];
		
		bds = RegionBounds[mr];
		bds2d = bds[[1 ;; 2]];
		dims = bds[[{1, 3}]];
		{zlow, zhigh} = bds[[3]] + p{-1, 1}Abs[Abs[Subtract @@ bds[[3]]]];
		\[CurlyEpsilon] = 10.^(Internal`$EqualTolerance - MachinePrecision) * Max[Abs[Subtract @@@ bds]];
		z0 = If[TrueQ[Element[z, Reals]], z, Mean[bds[[3]]]];
		
		closeQ = OptionValue["CloseBoundary"];
		lowdimQ = TrueQ[OptionValue["LowDimensionComponents"]];
		If[!BooleanQ[closeQ], closeQ = BoundaryMeshRegionQ[mr]];
		sliceHead = If[closeQ, BoundaryMeshRegion, MeshRegion];
		
		If[controls["ModelSlider"],
		img = Rasterize[DZRenderingEngine[mr] @ Show[mr, 
				ViewPoint -> orientation, ViewVertical -> {0, 0, 1}, ViewProjection -> "Orthographic",
				PlotRangePadding -> None, Method -> {}
			], 
			Background -> None
		];
		img = Image[ImagePad[ImageCrop[img], {{0, 50}, {0, 0}}, RGBColor[1, 1, 1, 0]], "Byte"];
		
		{w, h} = ImageDimensions[img];
		dims = {{0, w Abs[Subtract @@ bds[[3]]]/h}, bds[[3]]};
		dims[[1]] += bds[[If[MatchQ[orientation, Left|Right], 2, 1], 1]];
		sliderx = dims[[1, 2]] - 16Abs[Subtract @@ bds[[3]]]/h;
		
		rast = Show[img] /. Raster[arr_, {_, {x_, y_}}, w___] :> Raster[arr, Transpose[dims], w];
		
		rast = Show[rast, 
			PlotRange -> dims, 
			PlotRangePadding -> {{Scaled[2p], None}, {Scaled[p], Scaled[p]}}, 
			Frame -> {{True, False}, {True, False}},
			FrameTicks -> {{Automatic, None}, {dzTicks[orientation, bds2d], None}}, 
			FrameLabel -> {If[MatchQ[orientation, Left|Right], "y", "x"], Rotate["z", -\[Pi]/2]}
		]];
		
		selfContainedQ = OptionValue["SelfContained"];
		If[!BooleanQ[selfContainedQ], selfContainedQ = TrueQ[Length[cells] <= $UTDZSliceCacheThreshold]];
		
		If[!selfContainedQ,
			coords = MakeDZSliceCache[coords, cells, tree];
			cells = Null;
			tree = Null;
		];
		
		size = OptionValue[ImageSize];
		sty = parseDZStyle[Flatten[{OptionValue["SliceStyle"]}]];
		
		meshQ = MatchQ[OptionValue[Mesh], True|All];
		
		iDynamicZSlice[coords, cells, tree, z0, bds2d, bds, p, \[CurlyEpsilon], closeQ, lowdimQ, sliceHead, dims, rast, size, sty, meshQ, controls, sliderx]
	]


UTDynamicZSlice[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Argument conform & completion*)


registerForDiscretization[UTDynamicZSlice, 1];


(* ::Subsection::Closed:: *)
(*iDynamicZSlice*)


iDynamicZSlice[coords_, cells_, tree_, z0_, bds2d_, bds_, p_, \[CurlyEpsilon]_, closeQ_, lowdimQ_, sliceHead_, dims_, rast_, size_, stys_, meshQ_, controls_, sliderx_] :=
	Manipulate[
		If[controls["ModelSlider"],
			Grid[{{side, Dynamic[getSlice[coords2, cells2, tree2]]}}, BaseStyle -> ImageSizeMultipliers -> 1, ItemSize -> Full, Spacings -> {3, 0}],
			Dynamic[getSlice[coords2, cells2, tree2]]
		], 
		{{z, z0, "z"}, #3[[1]], #3[[2]], ControlType -> If[controls["Slider"], Automatic, None]},
		{inclick, False, ControlType -> None},
		{in3D, False, ControlType -> None},
		{getSlice, None},
		{getStyledComplex, None},
		{thinLine, None},
		{boldLine, None},
		{UILine, None},
		{side, None},
		{coords2, None, ControlType -> None},
		{cells2, None},
		{tree2, None},
		AppearanceElements -> If[controls["Slider"], {}, None],
		Initialization :> (
			coords2 = coords;
			cells2 = cells;
			tree2 = tree;
			
			Clear[getSlice, getStyledComplex, thinLine, boldLine, UILine];
			getSlice[pts_, faces_, t_] := Quiet @ Block[{prims, graphics, head},
				prims = iDZSlice[pts, faces, t, z, \[CurlyEpsilon], sliceHead === BoundaryMeshRegion, lowdimQ];
				If[!ListQ[prims], prims = {}];
				
				graphics = getStyledComplex /@ prims;
				head = If[Length[graphics] == 0, Graphics, Show];
				
				head[
					graphics, 
					PlotRange -> bds2d, PlotRangePadding -> Scaled[0.05], 
					Frame -> True, FrameLabel -> {"x", Rotate["y", -\[Pi]/2]},
					ImageSize -> size
				]
			];
			
			getStyledComplex[{_, {}, _}] = Nothing;
			getStyledComplex[{dim_ -> type_, pts_, oprims_}] := Block[{prims},
				prims = Switch[{dim, type},
					{2, BoundaryMeshRegion}, FilledCurve[{Line[#]}& /@ oprims],
					{1, MeshRegion}, Line[oprims],
					{2, MeshRegion}, Polygon[oprims],
					_, Point[Flatten[oprims]]
				];
				Graphics[GraphicsComplex[pts, {{stys[dim], prims}, If[dim > 0 && meshQ, {stys[0], Point[Range[Length[pts]]]}, Nothing]}]]
			];
			getStyledComplex[{1 -> head_, mr_MeshRegion}] := getStyledComplex[{1 -> head, MeshCoordinates[mr], Join @@ MeshCells[mr, 1, "Multicells" -> True][[All, 1]]}];
			getStyledComplex[{dim_ -> head_, reg_?RegionQ}] := head[reg, MeshCellStyle -> {dim -> stys[dim], If[meshQ, 0 -> stys[0], Nothing]}];
			
			If[controls["ModelSlider"],
			thinLine[] := If[inclick, 
				boldLine[], 
				{
					With[{pos = MousePosition["Graphics"]}, If[!in3D || !ListQ[pos], {}, {GrayLevel[0.5], AbsoluteThickness[0.5], InfiniteLine[{0, Clip[pos[[2]], #3]}, {1, 0}]}]],
					Line[{{#1, z}, {#2, z}}],
					Inset[
						Graphics[{{Opacity[0],Disk[{0,0},1.5]},{GrayLevel[.3],Triangle[{{-.9,.5},{.9,.5},{0,1.3}}],Triangle[{{-.9,-.5},{.9,-.5},{0,-1.3}}]}}], 
						{sliderx, z}, Automatic, Offset[16]
					]
				}
			];
			
			boldLine[] := {
				AbsoluteThickness[2], Line[{{#1, z}, {#2, z}}],
				Inset[
					Graphics[{{Opacity[.2],Disk[{0,0},1.5]},{GrayLevel[.3],Triangle[{{-.9,.5},{.9,.5},{0,1.3}}],Triangle[{{-.9,-.5},{.9,-.5},{0,-1.3}}]}}], 
					{sliderx, z}, Automatic, Offset[16]
				]
			};
		
			UILine[] := MouseAppearance[Mouseover[thinLine[], boldLine[]], "Hand"];
		
			side = EventHandler[Show[rast, Epilog -> Dynamic[UILine[]]], {
				"MouseDown" :> (inclick = True; z = Clip[MousePosition["Graphics"][[2]], #3]), 
				"MouseDragged" :> (z = Clip[MousePosition["Graphics"][[2]], #3]), 
				"MouseUp" :> (inclick = False),
				"MouseEntered" :> (in3D = True),
				"MouseExited" :> (in3D = False)
			}]];
		),
		UnsavedVariables :> {coords2, cells2, tree2}
	]&[
		dims[[1, 1]] + 2p Subtract @@ dims[[1]], 
		dims[[1,2]] - 2p Subtract @@ dims[[1]], 
		dims[[2]] + {1, -1}p Subtract @@ dims[[2]]
	]


(* ::Subsection::Closed:: *)
(*Styling utilities*)


DZRenderingEngine[mr_] /; $OperatingSystem === "MacOSX" && MeshCellCount[mr, 2] > 30000000 = Style[#, RenderingOptions -> {"3DRenderingEngine" -> "OpenGL"}]&;


DZRenderingEngine[_] = Identity;


parseDZStyle[dirs:{(0|1|2 -> _)..}] := 
	Block[{assoc},
		assoc = Merge[{$defaultDZStyle, (iDirective @@ Flatten[{#}])& /@ Association[dirs]}, Apply[Join]];
		assoc[2] = assoc[2] /. None -> FaceForm[];
		assoc = assoc /. None -> Opacity[0];
		assoc = If[MatchQ[#, Directive[_]], #[[1]], #]& /@ assoc;
		
		assoc
	]


parseDZStyle[{Automatic}] := parseDZStyle[{2 -> {}}]


parseDZStyle[dirs_] := parseDZStyle[{2 -> dirs}]


$defaultDZStyle = <|0 -> Directive[Black], 1 -> Directive[Black], 2 -> Directive[EdgeForm[{AbsoluteThickness[0.5], Black}], FaceForm[Hue[0.6, 0.3, 0.95]]]|>;


iDirective[d_Directive] := d
iDirective[args___] := Directive[args]


$modelSlider = "ModelSlider";
$slider = "Slider";


parseDZAppearanceElements[Automatic] = parseDZAppearanceElements[$modelSlider]


parseDZAppearanceElements[All] = parseDZAppearanceElements[{$modelSlider, $slider}]


parseDZAppearanceElements[elems_List] :=
	Block[{controls, orientation},
		controls = <|$modelSlider -> !FreeQ[elems, $modelSlider|($modelSlider -> _)], $slider -> !FreeQ[elems, $slider|Slider]|>;
		orientation = First[Cases[elems, ($modelSlider -> (o:Front|Back|Left|Right|Automatic)) :> o], Front] /. Automatic -> Front;
		
		{controls, orientation}
	]


parseDZAppearanceElements[elem_] := parseDZAppearanceElements[{elem}]


dzTicks[orientation:Left|Back, bds_] := 
	Block[{ind, lo, hi, ticks, dpos},
		ind = If[orientation === Left, 2, 1];
		{lo, hi} = bds[[ind]];
		
		ticks = Charting`ScaledTicks[{Identity, Identity}][lo, hi, {6, 6}];
		
		dpos = Append[2] /@ Position[ticks, {_Real, _NumberForm | _?NumberQ, __}];
		ticks = ReplacePart[ticks, Thread[dpos -> Extract[ticks, Reverse[dpos]]]];
		
		ticks
	]


dzTicks[___] = Automatic;


(* ::Subsection::Closed:: *)
(*Caching mechanism*)


iDZSlice[uuid_String, _, _, rest___] := iDZSlice[dZSliceCoords[uuid], dZSliceCells[uuid], dZSliceTree[uuid], rest]


iDZSlice[args___] := iZSlice[args]


$UTDZSliceCacheThreshold = 2^20;


ClearDZSliceCache[] := Clear[dZSliceCoords, dZSliceCells, dZSliceTree]


MakeDZSliceCache[coords_, cells_, tree_] := 
	Block[{uuid = CreateUUID[]},
		dZSliceCoords[uuid] = coords;
		dZSliceCells[uuid] = cells;
		dZSliceTree[uuid] = tree;
		uuid
	]
