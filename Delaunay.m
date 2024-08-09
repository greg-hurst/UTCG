(* ::Package:: *)

(* ::Title:: *)
(*Delaunay*)


(* ::Section:: *)
(*Begin*)


Package["UTCG`"]


PackageExport["UTVoronoi3D"]
PackageExport["UTPowerTriangulation3D"]
PackageExport["UTPowerDiagram3D"]


UTVoronoi3D::usage = "Computes Voronoi cells of 3D points.";
UTPowerTriangulation3D::usage = "Computes the power triangulation of 3D spheres.";
UTPowerDiagram3D::usage = "Computes the power diagram of 3D spheres.";


(* ::Section:: *)
(*UTVoronoi3D*)


(* ::Subsection::Closed:: *)
(*Main*)


UTVoronoi3D[pts_List, ibds_:Automatic, returntypes_:Automatic] /; MatrixQ[pts, Internal`RealValuedNumericQ] && Length[pts[[1]]] == 3 && validVoronoiFormatQ[returntypes] :=
	Block[{bds, res, fmts, fmtlookup, reskeys},
		bds = voronoiBoundingBox[pts, ibds];
		(
			fmts = expandVoronoiFormats[returntypes];
			res = iVoronoi3D[pts, bds, fmts];
			
			(
				If[Length[fmts] > 1 || ListQ[returntypes],
					res[[fmts]],
					res[[fmts[[1]]]]
				]
			) /; AssociationQ[res]
			
		) /; MatrixQ[bds, NumericQ] && Dimensions[bds] === {3, 2}
	]


UTVoronoi3D[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Argument conform & completion*)


addCodeCompletion[UTVoronoi3D][None, None, {"Cells", "Faces", "Ridges", "Nodes", "Seeds", "BoundedCellQ", "ClippedCellQ", "ConnectivityMatrix", "CommonFaces", "Bounds", "Properties"}];


(* ::Subsection::Closed:: *)
(*iVoronoi3D*)


iVoronoi3D[pts_, bds_, fmts_] :=
	Block[{maxdim, vdata, res},
		maxdim = Max[vorDimNeeded /@ fmts];
		vdata = voronoi3DData[pts, bds, maxdim];
		(
			res = Table[makeVoronoiResult[fmt, vdata, bds], {fmt, fmts}];
			
			AssociationThread[fmts, res] /; FreeQ[res, $Failed, {1}]
		
		) /; AssociationQ[vdata]
	]


iVoronoi3D[___] = $Failed;


(* ::Subsection::Closed:: *)
(*voronoi3DData*)


voronoi3DData[__, -1] = <||>;


voronoi3DData[pts_, bds_?degenerateBoundingBoxQ, __] := <|"Seeds" -> pts, "Bounds" -> bds, "Nodes" -> {}, "Ridges" -> {}, "Faces" -> {}, "Cells" -> {}, "FaceCellIncidence" -> {}, "CheckDegeneracies" -> False|>


voronoi3DData[pts_, bds_, maxdimension_:3] :=
	Block[{ptsInQ, oseeds, seeds, numseeds, dmdata, dcells, conn, vnodes, vlines, mergedQ, dcheckQ = False, faces, fcincidence, cgath, cells},
		ptsInQ = And @@ MapThread[#2[[1]] < #1[[1]] <= #1[[2]] < #2[[2]]&, {CoordinateBounds[pts], bds}];
		oseeds = outsidePoints[pts, bds];
		seeds = Join[pts, oseeds];
		
		dmdata = delaunayData[seeds];
		(
			{seeds, dcells} = dmdata;
			numseeds = Length[seeds] - Length[oseeds];
			vnodes = voronoiNodes[seeds, dcells];
			(
				vlines = cells = faces = fcincidence = Missing[];
				If[maxdimension >= 1,
					vlines = voronoiRidges[dcells];
					
					{vnodes, vlines, dcells, mergedQ} = mergeDuplicateNodes[vnodes, vlines, dcells];
					dcheckQ = !ptsInQ || mergedQ;
					
					If[maxdimension >= 2,
						{faces, fcincidence} = voronoiFacesAndCellIncidence[vlines, dcells];
						
						If[maxdimension == 3,
							cells = voronoiCells[faces, fcincidence, numseeds];
						]
					]
				];
					
				AssociationThread[
					{"Seeds", "Bounds", "Nodes", "Ridges", "Faces", "Cells", "FaceCellIncidence", "CheckDegeneracies"}, 
					{seeds[[1 ;; numseeds]], bds, vnodes, vlines, faces, cells, fcincidence, dcheckQ}
				]
			
			) /; MatrixQ[vnodes, NumericQ]
			
		) /; ListQ[dmdata]
	]


voronoi3DData[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Construction utilities*)


(* ::Subsubsection::Closed:: *)
(*Bounding box*)


degenerateBoundingBoxQ[bds_] := NonNegative[Max[Chop[Subtract @@@ bds]]] =!= False


voronoiBoundingBox[pts_, bbox_List] /; Dimensions[bbox] === {3, 2} && MatrixQ[bbox, Internal`RealValuedNumericQ] := bbox


voronoiBoundingBox[pts_, Scaled[s_?Internal`RealValuedNumericQ]] := CoordinateBounds[pts, Scaled[s]]


voronoiBoundingBox[pts_, bbox_List] /; Dimensions[bbox] === {3, 2} := 
	Block[{bds, lens, res},
		bds = CoordinateBounds[pts];
		lens = Abs[Subtract @@@ bds];
		
		res = MapThread[voronoiPadding, Join @@@ {bds, Transpose[{Minus[lens], lens}], bbox}]
	]


voronoiBoundingBox[pts_, _] := 
	Block[{bds, lens, mx},
		bds = CoordinateBounds[pts, Scaled[0.05]];
		lens = Abs[Subtract @@@ bds];
		mx = Max[lens];
		If[mx == 0,
			0.5{{-1, 1}, {-1, 1}, {-1, 1}} + bds,
			MapThread[If[Chop[#2/mx] == 0, .1{-mx, mx} + Mean[#1], #1]&, {bds, lens}]
		]
	]


voronoiPadding[__, r_?Internal`RealValuedNumericQ] := r
voronoiPadding[x_, len_, Scaled[p_]] := x + p*len
voronoiPadding[x_, len_, _] := voronoiPadding[x, len, Scaled[0.05]]


outsidePoints[pts_, bbox_, perturbQ_:False] :=
	Block[{prec, bds, res, w},
		prec = Internal`EffectivePrecision[pts];
		bds = {Floor[#1], Ceiling[#2]}& @@@ CoordinateBounds[Join[pts, Tuples[scaledBoundingBox[SetPrecision[bbox, prec]]]], Scaled[2]];
		
		res = Join[
			Tuples[bds],
			With[{mx = Mean[#1], my = Mean[#2]}, {{mx, my, #3[[1]]}, {mx, my, #3[[2]]}}]& @@ bds,
			With[{mx = Mean[#1], mz = Mean[#3]}, {{mx, #2[[1]], mz}, {mx, #2[[2]], mz}}]& @@ bds,
			With[{my = Mean[#2], mz = Mean[#3]}, {{#1[[1]], my, mz}, {#1[[2]], my, mz}}]& @@ bds
		];
		
		res = N[res, prec];
		
		If[TrueQ[perturbQ],
			BlockRandom[
				w = 0.01*Min[Abs[Subtract @@@ bds]];
				res += SetPrecision[RandomReal[w*{-1, 1}, Dimensions[res]], prec];,
				RandomSeeding -> 1
			]
		];
		
		If[Developer`PackedArrayQ[pts],
			Developer`ToPackedArray[res, Head[pts[[1, 1]]]],
			res
		]
	]


scaledBoundingBox[bbox_] := 
	With[{d = Max[Abs[Subtract @@@ bbox]]/2},
		If[Chop[1.0d] == 0.0,
			{{-1, 1}, {-1, 1}, {-1, 1}}/2 + bbox[[All, 1]],
			d*{{-1, 1}, {-1, 1}, {-1, 1}} + Map[Mean, bbox]
		]
	]


(* ::Subsubsection::Closed:: *)
(*Delaunay*)


delaunayData[pts_] :=
	Block[{prec, dmdata, coords, cells, degenQ, conn},
		prec = Internal`EffectivePrecision[pts];
		dmdata = tetGenDelaunay[pts];
		(
			{coords, cells} = dmdata;
			
			If[prec =!= MachinePrecision,
				coords = Extract[pts, Nearest[realPack[pts] -> "Index", coords]];
			];
			
			degenQ = degenerateTet[coords, cells];
			If[Max[degenQ] == 1,
				cells = Pick[cells, degenQ, 0];
			];
			
			{coords, cells}
			
		) /; ListQ[dmdata]
	]


delaunayData[___] = $Failed;


realPack = Developer`ToPackedArray[#, Real]&;


UTCompile[degenerateTet, {{coords, _Real, 2}, {cells, _Integer, 1}},
	Block[{p1, p2, p3, p4},
		p1 = Compile`GetElement[coords, Compile`GetElement[cells, 1]];
		p2 = Compile`GetElement[coords, Compile`GetElement[cells, 2]];
		p3 = Compile`GetElement[coords, Compile`GetElement[cells, 3]];
		p4 = Compile`GetElement[coords, Compile`GetElement[cells, 4]];
		
		UnitStep[Subtract[6.`*^-10, Abs[Subtract[p1, p4] . Cross[Subtract[p2, p4], Subtract[p3, p4]]]]]
	],
	RuntimeAttributes -> {Listable}
];


tetGenDelaunay[pts_] /; loadTetGenQ :=
	Block[{dmdata},
		dmdata = Quiet @ TetGenLink`TetGenDelaunay[pts];
		
		dmdata /; ListQ[dmdata]
	]


tetGenDelaunay[___] = $Failed;


(* ::Subsubsection::Closed:: *)
(*Voronoi nodes*)


voronoiNodes[coords_, dcells_] /; Internal`EffectivePrecision[coords] === MachinePrecision := numericalCircumCenters[coords, dcells]


voronoiNodes[coords_, dcells_] := 
	Block[{ncents, \[CurlyEpsilon], ndists, suspect, scents},
		ncents = numericalCircumCenters[coords, dcells];
		
		(* look for nearby circumcenters that seem duplicate and compute then exactly *)
		\[CurlyEpsilon] = Min[Abs[Subtract @@@ CoordinateBounds[coords]]] * 10.^(Internal`$EqualTolerance - MachinePrecision + 3);
		ndists = Nearest[Union[ncents] -> "Distance", ncents, 2][[All, 2]];
		suspect = Random`Private`PositionsOf[ndists - \[CurlyEpsilon], Negative];
		
		If[Length[suspect] > 0,
			scents = realPack[carefulN[symbolicCircumCenters[coords, dcells[[suspect]]]]];
			ncents[[suspect]] = scents;
		];
		
		ncents
	]


numericalCircumCenters[coords_, dcells_] :=
	Block[{pcoords, tprims, len, blocks, vec, rtemplate, mat},
		pcoords = realPack[coords];
		tprims = pcoords[[#]]& /@ Transpose[dcells];
		
		blocks = Transpose[{Subtract[#1, #4], Subtract[#2, #4], Subtract[#3, #4]}& @@ tprims];
		vec = 0.5*Flatten[Transpose[{Subtract[#1, #4], Subtract[#2, #4], Subtract[#3, #4]}& @@ Total[tprims^2, {3}]]];
		
		len = Length[blocks];
		rtemplate = Mod[Range[9], 3, -2];
		mat = SparseArray[Automatic, {3len, 3len}, 0, {1, {Range[0, 9len, 3], ArrayReshape[ConstantArray[rtemplate, len] + Range[3, 3len, 3], {9len, 1}]}, Flatten[blocks]}];
		
		Partition[LinearSolve[mat, vec, Method -> "Banded"], 3]
	]


symbolicCircumCenters[coords_, dcells_] :=
	Block[{tprims, blocks, vec},
		tprims = coords[[#]]& /@ Transpose[dcells];
		
		blocks = Transpose[2{Subtract[#1, #4], Subtract[#2, #4], Subtract[#3, #4]}& @@ tprims];
		vec = Transpose[{Subtract[#1, #4], Subtract[#2, #4], Subtract[#3, #4]}& @@ Total[tprims^2, {3}]];
		
		MapThread[LinearSolve[##, Method -> "CofactorExpansion"]&, {blocks, vec}]
	]


carefulN = Quiet[N[N[#, $MachinePrecision]]]&;


(* ::Subsubsection::Closed:: *)
(*Voronoi ridges*)


UTCompile[voronoiRidges, {{dcells, _Integer, 2}},
	Block[{scells, faces, fordering, tags, scell, p1, p2, p3, p4, old, new, i, len, segs = Internal`Bag[Most[{0}]]},
		scells = Sort /@ dcells;
		faces = Partition[Flatten[scells[[All, {1,2,3, 1,2,4, 1,3,4, 2,3,4}]]], 3];
		fordering = Ordering[faces];
		
		tags = Quotient[4fordering + 12, 16];
		faces = faces[[fordering]];
		
		i = 1;
		len = Length[faces];
		old = Compile`GetElement[faces, 1];
		While[i < len,
			new = Compile`GetElement[faces, i+1];
			If[old =!= new,
				i++; old = new;,
				Internal`StuffBag[segs, Compile`GetElement[tags, i]];
				Internal`StuffBag[segs, Compile`GetElement[tags, i+1]];
				i += 2;
				old = Compile`GetElement[faces, i];
			];
		];
		
		Partition[Internal`BagPart[segs, All], 2]
	]
];


(* ::Subsubsection::Closed:: *)
(*Voronoi faces*)


(* ::Text:: *)
(*voronoiFacesAndCellIncidence will return a list {faces, cell adjacency list}. There is a 1-1 correspondence between the faces and the cell adjacency list. In fact element i of the faces corresponds to the common face shared by the ith cells in the adjacency list. *)


voronoiFacesAndCellIncidence[vlines_, dcells_] :=
	Block[{cellpairlines, cinds, cp, adj, faces, facedata, fcnt, flens, res},
		cellpairlines = cellTaggedVoronoiRidges[dcells, vlines];
		
		cinds = twoWayTieOrdering[cellpairlines];
		cp = inverseTieOrdering[cinds];
		cellpairlines = cellpairlines[[cp]];
		
		facedata = pathGraphScan[Partition[cinds, 2]];
		fcnt = facedata[[1]];
		flens = facedata[[2 ;; fcnt + 1]];
		
		res = cellpairlines[[facedata[[fcnt + 2 ;; -1]]]];
		{
			Pick[Statistics`Library`PartitionRagged[res[[All, 1]], Abs[flens]], Sign[flens], 1],
			Pick[res[[Accumulate[Abs[flens]], 2 ;; 3]], Sign[flens], 1]
		}
	]


UTCompile[twoWayTieOrdering, {{list, _Integer, 2}},
	Block[{ordering, ids, id = 1, k = 1, val, len, o},
		len = Length[list];
		ids = Table[0, {len}];
		
		ordering = Ordering[list];
		
		o = Compile`GetElement[ordering, k];
		While[k < len,
			val = Compile`GetElement[list, o];
			ids[[o]] = id;
			o = Compile`GetElement[ordering, ++k];
			If[Compile`GetElement[list, o] === val,
				k++;
				ids[[o]] = id;
				o = Compile`GetElement[ordering, k];
			];
			id++
		];
		If[k === len, ids[[Compile`GetElement[ordering, len]]] = id];
		
		ids
	]
];


UTCompile[inverseTieOrdering, {{list, _Integer, 1}},
	Block[{cpos},
		cpos = Table[0, {Max[list]}];
		Do[
			cpos[[Compile`GetElement[list, i]]] = i,
			{i, Length[list], 1, -1}
		];
		
		cpos
	]
];


(* ::Text:: *)
(*Returns all Voronoi ridges repeated and tagged once for every polyhedron it's a member of*)


cellTaggedVoronoiRidges[dcells_?MatrixQ, vlines_] /; Length[First[dcells]] == 4 := Flatten[taggedSharedTetSegments[dcells, vlines], 1]


(* ::Text:: *)
(*TODO speed up and make readable*)


cellTaggedVoronoiRidges[dcells_, vlines_] :=
	Flatten[Developer`ToPackedArray[Catenate[MapThread[Outer[Prepend, Subsets[#2, {2}], #1, 1]&, {vlines, Intersection @@@ Partition[dcells[[Flatten[vlines]]], 2]}]]], 1]


UTCompile[taggedSharedTetSegments, {{dcells, _Integer, 2}, {line, _Integer, 1}},
	Block[{s1, s2, ints, p1, p2, p3},
		s1 = Compile`GetElement[line, 1];
		s2 = Compile`GetElement[line, 2];
		
		ints = Intersection[Compile`GetElement[dcells, s1], Compile`GetElement[dcells, s2]];
		p1 = Compile`GetElement[ints, 1];
		p2 = Compile`GetElement[ints, 2];
		p3 = Compile`GetElement[ints, 3];
		
		{{s1, p1, p2}, {s2, p1, p2}, {s1, p1, p3}, {s2, p1, p3}, {s1, p2, p3}, {s2, p2, p3}}
	],
	RuntimeAttributes -> {Listable}
];


(* ::Text:: *)
(*Given a path graph represented as undirected edges ipos, pathGraphScan finds a depth first scan with no backtracking. It assumes elements of ipos are not repeated and if {v1, v2} is an element then {v2, v1} is not.*)


(* ::Text:: *)
(*The return value is of the form {n, l1, l2, ..., ln, i11, i12, ..., i1l1, i21, ..., i2l2, ..., iln1, ..., inln}.*)
(*Here, lj represents a scanned connected component ij1, ij2, ..., ijlj and if the sign of lj is 1, then these indices form a cycle and otherwise form an unclosed path.*)


(* ::Text:: *)
(*The input pathGraphScan[{{1, 2}, {2, 3}, {3, 4}, {4, 1}, {5, 6}, {6, 7}, {7, 8}}] returns {2, 4,-4, 1,2,3,4, 5,6,7,8}, indicating 2 connected components: 1,2,3,4 (forms a cycle) and 5,6,7,8 (does not form a cycle).*)


(* ::Text:: *)
(*This scan might break up open paths into 2 components since there is no backtracking, but this is not the case for cycles. For example, pathGraphScan[{{3, 1}, {1, 2}}] returns {2, -2,-2, 1,3, 2,1} *)


UTCompile[pathGraphScan, {{ipos, _Integer, 2}},
	Block[{mx, adjlist, i1, i2, lenbag, cycbag, cnt, lcnt = 0, old = 0, old2 = 0, next = 0},
		mx = 2Max[ipos];
		adjlist = Table[0, {mx}];
		Do[
			i1 = Compile`GetElement[p, 1];
			i2 = Compile`GetElement[p, 2];
			If[Compile`GetElement[adjlist, i1-1] === 0, adjlist[[i1-1]] = i2, adjlist[[i1]] = i2];
			If[Compile`GetElement[adjlist, i2-1] === 0, adjlist[[i2-1]] = i1, adjlist[[i2]] = i1],
			{p, 2ipos}
		];
		
		lenbag = Internal`Bag[Most[{0}]];
		cycbag = Internal`Bag[Most[{0}]];
		
		Do[
			If[Compile`GetElement[adjlist, i-1] =!= 0,
				cnt = 0;
				next = i;
				old = -1;
				While[True,
					cnt++;
					Internal`StuffBag[cycbag, next];
					old2 = old;
					old = next;
					next = Compile`GetElement[adjlist, old-1];
					If[next === old2, 
						next = Compile`GetElement[adjlist, old];
					];
					adjlist[[old-1]] = 0;
					adjlist[[old]] = 0;
					If[next === i || next === 0, Break[]];
				];
				lcnt++;
				If[next === i,
					Internal`StuffBag[lenbag, cnt],
					Internal`StuffBag[lenbag, Minus[cnt]]
				]
			],
			{i, 2, mx, 2}
		];
		
		Join[{lcnt}, Internal`BagPart[lenbag, All], Quotient[Internal`BagPart[cycbag, All], 2]]
	]
];


(* ::Subsubsection::Closed:: *)
(*Voronoi cells*)


voronoiCells[faces_, fcincidence_, numseeds_] :=
	Block[{cellinds, keep, fs, res, ord},
		cellinds = Join @@ Transpose[fcincidence];
		fs = Join[faces, faces];
		
		keep = Random`Private`PositionsOf[Subtract[cellinds, numseeds], NonPositive];
		cellinds = cellinds[[keep]];
		fs = fs[[keep]];
		
		res = GatherByList[fs, cellinds];
		ord = InversePermutation[DeleteDuplicates[cellinds]][[1 ;; numseeds]];
		
		res[[ord]]
	]


(* ::Subsubsection::Closed:: *)
(*Degenerate features*)


mergeDuplicateNodes[nodes_, lines_, cells_] :=
	Block[{nnodes, rep, nlines},
		{nnodes, rep} = Region`Mesh`DeleteDuplicateCoordinates[nodes];
		If[!ListQ[rep],
			{nodes, lines, cells, False},
			{
				nnodes,
				DeleteDuplicates[Map[Sort, Pick[#, Unitize[Subtract @@@ #], 1]]]& @ Region`Mesh`ReplaceIncidents[lines, rep],
				Union @@ cells[[#]]& /@ Values[PositionIndex[rep]],
				True
			}
		]
		
	]


Options[resolveVoronoiDegeneracies] = {"EliminateUnusedCoordinates" -> True, "DeleteDuplicateCoordinates" -> True, "ResolveDegenerateFaces" -> True};


resolveVoronoiDegeneracies[{}, __] = {{}, {}};


resolveVoronoiDegeneracies[_, {}, ___] = {{}, {}};


resolveVoronoiDegeneracies[coords_, cells_, opts:OptionsPattern[]] :=
	Block[{elimQ, dupQ, degQ, coordsnew, cellsnew},
		{elimQ, dupQ, degQ} = TrueQ /@ OptionValue[{"EliminateUnusedCoordinates", "DeleteDuplicateCoordinates", "ResolveDegenerateFaces"}];
		{coordsnew, cellsnew} = {coords, cells};
		
		If[elimQ,
			{coordsnew, cellsnew} = eliminateUnusedCoordinates[coordsnew, cellsnew];
		];
		
		If[dupQ,
			{coordsnew, cellsnew} = deleteDuplicateCoordinates[coordsnew, cellsnew];
		];
		
		If[degQ,
			cellsnew = resolveFaceDegeneracies /@ cellsnew;
		];
		
		{coordsnew, cellsnew}
	]


resolveFaceDegeneracies[cells_] :=
	With[{df = Select[Select[cells, Length[#] > 2&], DuplicateFreeQ]},
		If[Length[cells] == Length[df], 
			cells,
			Join[df, iResolveFaceDegeneracies /@ Select[cells, Not @* DuplicateFreeQ]]
		]
	]


iResolveFaceDegeneracies[cell_] :=
	Block[{df},
		df = Pick[cell, Prepend[Unitize[Differences[cell]], 1], 1];
		If[df[[1]] === df[[-1]],
			df = Most[df]
		];
		
		If[Length[df] < 3,
			Nothing,
			df
		]
	]


(* ::Subsection::Closed:: *)
(*Return type constructors*)


(* ::Subsubsection::Closed:: *)
(*Return type parsing*)


$vorFormats = {"Seeds", "Bounds", "Nodes", "Ridges", "Faces", "Cells", "BoundedCellQ", "ClippedCellQ", "ConnectivityMatrix", "CommonFaces"};


vorDimNeeded["Properties"] = -1;
vorDimNeeded["Seeds"] = vorDimNeeded["Bounds"] = vorDimNeeded["Nodes"] = 0;
vorDimNeeded["Ridges"] = 1;
vorDimNeeded["Faces"] = vorDimNeeded["ConnectivityMatrix"] = vorDimNeeded["CommonFaces"] = 2;
vorDimNeeded[_] = 3;


$validVoronoiFormats = Alternatives @@ Join[$vorFormats, {Automatic, All}];


validVoronoiFormatQ[Append[$validVoronoiFormats, "Properties"]] = True;
validVoronoiFormatQ[{Append[$validVoronoiFormats, "Properties"]..}] = True;
validVoronoiFormatQ["Properties"] = True;
validVoronoiFormatQ[___] = False;


expandVoronoiFormats[fmt:Except[_List]] := expandVoronoiFormats[{fmt}]
expandVoronoiFormats[fmts_] := DeleteDuplicates[Flatten[Replace[fmts, {Automatic -> "Cells", All -> $vorFormats}, {1}]]]


(* ::Subsubsection::Closed:: *)
(*makeVoronoiResult*)


makeVoronoiResult["Seeds", vdata_, ___] := vdata["Seeds"]


makeVoronoiResult["Bounds", vdata_, ___] := vdata["Bounds"]


makeVoronoiResult["Nodes", vdata_, bds_] := makeVoronoiNodes[vdata["Nodes"], bds]


makeVoronoiResult["Ridges", vdata_, bds_] := makeVoronoiRidges[vdata["Nodes"], vdata["Ridges"], bds]


makeVoronoiResult["Faces", vdata_, bds_] := makeVoronoiFaces[vdata["Nodes"], vdata["Faces"], bds, vdata["CheckDegeneracies"]]


makeVoronoiResult["Cells", vdata_, bds_] := makeVoronoiCells[vdata["Nodes"], vdata["Cells"], bds, vdata["CheckDegeneracies"]]


makeVoronoiResult["BoundedCellQ", vdata_, bds_] := makeVoronoiBoundedCellQ[vdata["FaceCellIncidence"], vdata["Cells"]]


makeVoronoiResult["ClippedCellQ", vdata_, bds_] := makeVoronoiClippedCellQ[vdata["Nodes"], vdata["Cells"], bds]


makeVoronoiResult["ConnectivityMatrix", vdata_, bds_] := makeVoronoiConnectivityMatrix[vdata["FaceCellIncidence"], Length[vdata["Seeds"]]]


makeVoronoiResult["CommonFaces", vdata_, bds_] := makeVoronoiCommonFaces[vdata["Nodes"], vdata["Faces"], vdata["FaceCellIncidence"], bds, Length[vdata["Seeds"]]]


makeVoronoiResult["Properties", ___] = $vorFormats;


(* ::Subsubsection::Closed:: *)
(*makeVoronoiNodes*)


makeVoronoiNodes[vnodes_, bds_] := pointsInBounds[vnodes, ##]& @@ Flatten[bds]


UTCompile[pointsInBounds, {{pts, _Real, 2}, {x1, _Real}, {x2, _Real}, {y1, _Real}, {y2, _Real}, {z1, _Real}, {z2, _Real}},
	Rest[Select[Prepend[pts, {x1, y1, z2}], x1 <= Compile`GetElement[#, 1] <= x2 && y1 <= Compile`GetElement[#, 2] <= y2 && z1 <= Compile`GetElement[#, 3] <= z2&]]
];


(* ::Subsubsection::Closed:: *)
(*makeVoronoiRidges*)


makeVoronoiRidges[vnodes_, vlines_, bds_] := 
	Block[{coords, cells, res},
		{coords, cells} = clipRidges[vnodes, vlines, bds];
		
		res = Quiet @ MeshRegion[coords, Line[cells]];
		
		res /; RegionQ[res]
	]


makeVoronoiRidges[___] = $Failed;


(* ::Subsubsection::Closed:: *)
(*makeVoronoiFaces*)


makeVoronoiFaces[vnodes_, cells_, bds_, dcheckQ_:True] :=
	Block[{multicells, coords, clippedcells, res},
		multicells = GatherByLength[cells];
		
		{coords, clippedcells} = clipRings[vnodes, multicells, bds];
		If[dcheckQ,
			{coords, clippedcells} = resolveVoronoiDegeneracies[coords, clippedcells];
		];
		
		res = voronoiFacesMR[coords, clippedcells, !dcheckQ];
		
		res /; RegionQ[res]
	]

makeVoronoiFaces[___] = $Failed;


voronoiFacesMR[_, {}, _] = EmptyRegion[3];

voronoiFacesMR[coords_, cells_, dcheckQ_] :=
	Block[{mr},
		mr = Quiet @ MeshRegion[
			coords, 
			Polygon /@ cells, 
			Method -> {"CoplanarityTolerance" -> 14, "EliminateUnusedCoordinates" -> dcheckQ, "DeleteDuplicateCoordinates" -> dcheckQ, "DeleteDuplicateCells" -> False, "TJunction" -> False}
		];
		
		(
			If[MeshCellCount[mr, 2] === 0 || (MeshCellCount[mr, 2] === 1 && Chop[Area[mr]] === 0),
				EmptyRegion[3],
				mr
			]
		) /; RegionQ[mr]
	]

voronoiFacesMR[___] = $Failed;


(* ::Subsubsection::Closed:: *)
(*makeVoronoiCells*)


makeVoronoiCells[vnodes_, cells_, bds_, dcheckQ_:True] :=
	With[{jj = Join @@ #}, 
		If[jj === {}, 
			EmptyRegion[3],
			makeVoroniCell[vnodes[[DeleteDuplicates[jj]]], Statistics`Library`PartitionRagged[integerIndices[jj], Length /@ #], bds, dcheckQ]
		]
	]& /@ cells


makeVoroniCell[coords_, cells_, bds_, dcheckQ_] :=
	Block[{cbds, clip, clippedcoords, clippedcells},
		If[Length[cells] == 0, Return[EmptyRegion[3]]];
		cbds = CoordinateBounds[coords];
		
		If[NonPositive[Max[#1]] && NonNegative[Min[#2]]& @@ Transpose[Subtract[bds, cbds]],
			voronoiCellBMR[coords, cells],
			
			{clippedcoords, clippedcells} = clipConvexCell[coords, cells, cbds, bds];
			If[dcheckQ,
				clippedcells = resolveFaceDegeneracies[clippedcells];
			];
			
			If[Length[clippedcells] == 0,
				EmptyRegion[3],
				voronoiCellBMR[clippedcoords, clippedcells]
			]
		]
	]


(* ::Text:: *)
(*Region`Mesh`FindNormalDirection is slow when generating many BoundaryMeshRegions. Instead, we exploit the fact that each cell is convex.*)


voronoiCellBMR[coords_, cells_] := 
	Block[{reg, Region`Mesh`FindNormalDirection = findNormalDirection},
		
		reg = Quiet @ BoundaryMeshRegion[coords, Polygon[cells], 
			Method -> {"CoplanarityTolerance" -> 14, "CheckIntersections" -> False, "SeparateBoundaries" -> False, "BoundaryNesting" -> {{0, 0}}, 
				"EliminateUnusedCoordinates" -> False, "DeleteDuplicateCoordinates" -> False, "DeleteDuplicateCells" -> False, "TJunction" -> False}];
		
		If[RegionQ[reg], reg, EmptyRegion[3]]
	]


findNormalDirection =
	If[ListQ[#], 
		findNormalDirectionC[#[[1, 1, 1]], Mean[#[[-1, 1, -1]]]],
		findNormalDirectionC[#[[1, 1]], Mean[#[[1, -1]]]]
	]&;


UTCompile[findNormalDirectionC, {{p, _Real, 2}, {test, _Real, 1}},
	Sign[Cross[Subtract[Compile`GetElement[p, 1], Compile`GetElement[p, 2]], Subtract[Compile`GetElement[p, 3], Compile`GetElement[p, 2]]] . Subtract[test, Compile`GetElement[p, 2]]]
];


UTCompile[integerIndices, {{ints, _Integer, 1}},
	Block[{len, ordering, inv, ids, id, k, val, o = 1},
		len = Length[ints];
		ids = Table[0, {len}];
		
		ordering = Ordering[ints];
		inv = ids;
		Do[inv[[Compile`GetElement[ordering, i]]] = i, {i, len}];
		
		id = 1;
		Do[
			If[Compile`GetElement[ids, j] === 0,
				val = Compile`GetElement[ints, j];
				ids[[j]] = id;
				k = Compile`GetElement[inv, j] + 1;
				While[k <= len && Compile`GetElement[ints, o = Compile`GetElement[ordering, k++]] === val,
					ids[[o]] = id
				];
				id++
			],
			{j, len}
		];
		
		ids
	]
];


(* ::Subsubsection::Closed:: *)
(*makeVoronoiBoundedCellQ*)


makeVoronoiBoundedCellQ[fcincidence_, cells_] :=
	Block[{conn, nbcnt},
		conn = makeVoronoiConnectivityMatrix[fcincidence, Length[cells]];
		nbcnt = Differences[conn["RowPointers"]];
		
		Clip[Subtract[nbcnt, Length /@ cells], {0, 1}]
	]


(* ::Subsubsection::Closed:: *)
(*makeVoronoiClippedCellQ*)


makeVoronoiClippedCellQ[vnodes_, cells_, bds_] :=
	Block[{outbdsQ},
		outbdsQ = outOfBounds[vnodes, ##]& @@ Flatten[bds];
		Unitize[Total[outbdsQ[[Union @@ #]]]& /@ cells]
	]


UTCompile[outOfBounds, {{pts, _Real, 1}, {x1, _Real}, {x2, _Real}, {y1, _Real}, {y2, _Real}, {z1, _Real}, {z2, _Real}},
	Boole[Not[x1 <= Compile`GetElement[pts, 1] <= x2 && y1 <= Compile`GetElement[pts, 2] <= y2 && z1 <= Compile`GetElement[pts, 3] <= z2]],
	RuntimeAttributes -> {Listable}
];


(* ::Subsubsection::Closed:: *)
(*makeVoronoiConnectivityMatrix*)


makeVoronoiConnectivityMatrix[fcincidence_, cellcnt_] :=
	Block[{nzp, arr},
		nzp = Pick[fcincidence, Total[UnitStep[fcincidence - cellcnt - 1], {2}], 0];
		arr = SparseArray[nzp -> ConstantArray[1, Length[nzp]], {cellcnt, cellcnt}];
		
		arr + Transpose[arr] + IdentityMatrix[cellcnt, SparseArray]
	]


(* ::Subsubsection::Closed:: *)
(*makeVoronoiCommonFaces*)


makeVoronoiCommonFaces[vnodes_, faces_, fcincidence_, bds_, cellcnt_] :=
	Block[{prims, primsclipped, coords, cells, clippedfaces, nzp, assoc},
		prims = Region`Mesh`ToCoordinates[faces, vnodes];
		primsclipped = boxClipPolygon[prims, ##]& @@ Flatten[bds];
		
		coords = Developer`ToPackedArray[Join @@ primsclipped, Real];
		cells = Statistics`Library`PartitionRagged[Range[Length[coords]], Length /@ primsclipped];
		
		{coords, clippedfaces} = resolveVoronoiDegeneracies[coords, cells,
			"EliminateUnusedCoordinates" -> False, "DeleteDuplicateCoordinates" -> True, "ResolveDegenerateFaces" -> False];
		
		nzp = Total[UnitStep[fcincidence - cellcnt - 1], {2}] + Subtract[1, Unitize[Developer`ToPackedArray[Length /@ clippedfaces, Integer]]];
		
		assoc = AssociationThread[Sort /@ Pick[fcincidence, nzp, 0], Pick[clippedfaces, nzp, 0]];
		
		<|"Coordinates" -> coords, "CommonFaceIndices" -> assoc|>
	]

makeVoronoiCommonFaces[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Convex Clipping*)


(* ::Subsubsection::Closed:: *)
(*clipRidges*)


clipRidges[coords_, icells_, bds_] :=
	Block[{prims, clipdata, cnt, sp, dp, mp, pp, lp, coordsnew, multiint, delete, lens, clippedcells, mclips, mcells},
		prims = Region`Mesh`ToCoordinates[icells, coords];
		clipdata = clipRidges6Sides[coords, icells, prims, bds];
		cnt = Round[clipdata[[1]]]+1;
		If[cnt === Length[coords]+1 && clipdata[[3]] == 0, Return[{coords, icells}]];
		
		{sp, pp, mp, dp, lp} = Accumulate[Prepend[Floor[clipdata[[2 ;; 5]]], 5]];
		
		coordsnew = Join[coords, Partition[clipdata[[sp+1 ;; pp]], 3]];
		multiint = Round[clipdata[[pp+1 ;; mp]]];
		delete = Round[clipdata[[mp+1 ;; dp]]];
		lens = Round[clipdata[[dp+1 ;; lp]]];
		
		clippedcells = Partition[Round[clipdata[[lp+1 ;; -1]]], 2];
		If[Length[icells] > Length[delete],
			clippedcells = Join[Delete[icells, Partition[delete, 1]], clippedcells];
		];
		
		If[Length[multiint] > 0, 
			mclips = Select[boxClipSegment[prims[[multiint]], ##]& @@ Flatten[bds], Length[#] > 1&];
			If[Length[mclips] > 0,
				coordsnew = Join[coordsnew, Join @@ mclips];
				mcells = Partition[Range[cnt, cnt + Total[Length /@ mclips]-1], 2];
				clippedcells = Join[clippedcells, mcells];
			]
		];
		
		{coordsnew, clippedcells}
	]


UTCompile[clipSegment, {{p1, _Real, 1}, {p2, _Real, 1}, {ord, _Real}, {dim, _Integer}},
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


UTCompile[clipRidges6Sides, {{coords, _Real, 2}, {cells, _Integer, 2}, {ptcells, _Real, 3}, {bds, _Real, 2}},
	Module[{
		x1, x2, y1, y2, z1, z2, 
		coordsbag, cellsbag, lensbag, deletebag, multiintbag, 
		coordsnew, cellsnew, lens, delete, multiint, multiintQ = False, 
		pts, ordsx, ordsy, ordsz, ords, mn, mx, 
		sgns = {1}, ord = 0., dim = 0, i, j, cnt},
		
		coordsbag = Internal`Bag[Most[{0.}]];
		cellsbag = Internal`Bag[Most[{0}]];
		lensbag = Internal`Bag[Most[{0}]];
		deletebag = Internal`Bag[Most[{0}]];
		multiintbag = Internal`Bag[Most[{0}]];
		
		{{x1, x2}, {y1, y2}, {z1, z2}} = bds;
		cnt = Length[coords];
		Do[
			dim = 0; multiintQ = False; i = 1; j = 2;
			pts = Compile`GetElement[ptcells, k];
			
			(* ------------ determine which plane to clip with, and if more than one, flag and continue ------------ *)
			
			{ordsx, ordsy, ordsz} = Transpose[pts];
			
			mn = Min[ordsx];
			mx = Max[ordsx];
			If[mx <= x1 || x2 <= mn, Internal`StuffBag[deletebag, k]; Continue[]];
			If[mn < x1, dim = 1; ord = x1; sgns = UnitStep[Subtract[x1, ordsx]]];
			If[x2 < mx, If[dim =!= 0, multiintQ = True, dim = 1; ord = x2; sgns = UnitStep[Subtract[ordsx, x2]]]];
			
			mn = Min[ordsy];
			mx = Max[ordsy];
			If[mx <= y1 || y2 <= mn, Internal`StuffBag[deletebag, k]; Continue[]];
			If[mn < y1, If[dim =!= 0, multiintQ = True, dim = 2; ord = y1; sgns = UnitStep[Subtract[y1, ordsy]]]];
			If[y2 < mx, If[dim =!= 0, multiintQ = True, dim = 2; ord = y2; sgns = UnitStep[Subtract[ordsy, y2]]]];
			
			mn = Min[ordsz];
			mx = Max[ordsz];
			If[mx <= z1 || z2 <= mn, Internal`StuffBag[deletebag, k]; Continue[]];
			If[mn < z1, If[dim =!= 0, multiintQ = True, dim = 3; ord = z1; sgns = UnitStep[Subtract[z1, ordsz]]]];
			If[z2 < mx, If[dim =!= 0, multiintQ = True, dim = 3; ord = z2; sgns = UnitStep[Subtract[ordsz, z2]]]];
			
			If[dim === 0, Continue[]];
			If[multiintQ, Internal`StuffBag[deletebag, k]; Internal`StuffBag[multiintbag, k]; Continue[]];
			
			(* ------------ clip segment along one plane ------------ *)
			
			cnt++;
			Internal`StuffBag[coordsbag, clipSegment[Compile`GetElement[pts, 1], Compile`GetElement[pts, 2], ord, dim], 1];
			Internal`StuffBag[lensbag, j+2];
			Internal`StuffBag[deletebag, k];
			If[Compile`GetElement[sgns, 1] === 0,
				Internal`StuffBag[cellsbag, cells[[k, 1]]];
				Internal`StuffBag[cellsbag, cnt],
				Internal`StuffBag[cellsbag, cnt];
				Internal`StuffBag[cellsbag, cells[[k, 2]]]
			];,
			{k, Length[cells]}
		];
		
		multiint = Internal`BagPart[multiintbag, All];
		delete = Internal`BagPart[deletebag, All];
		lens = Internal`BagPart[lensbag, All];
		coordsnew = Internal`BagPart[coordsbag, All];
		cellsnew = Internal`BagPart[cellsbag, All];
		
		Join[{cnt, Length[coordsnew], Length[multiint], Length[delete], Length[lens]}, coordsnew, multiint, delete, lens, cellsnew]
	]
];


UTCompile[clipRidge1Side, {{pts, _Real, 2}, {ord, _Real}, {dim, _Integer}, {dir, _Integer}},
	Module[{sgns, tot, i = 1, j = 2, ptsrot, cpts},
		sgns = If[dir === 1,
			UnitStep[Subtract[pts[[All, dim]], ord]],
			UnitStep[Subtract[ord, pts[[All, dim]]]]
		];
		
		tot = Total[sgns];
		Which[
			tot === 2, 
				{{-1.,-1.,-1.}},
			tot === 0, 
				pts,
			Compile`GetElement[sgns, 1] === 0,
				{Compile`GetElement[pts, 1], clipSegment[Compile`GetElement[pts, 1], Compile`GetElement[pts, 2], ord, dim]},
			True,
				{clipSegment[Compile`GetElement[pts, 1], Compile`GetElement[pts, 2], ord, dim], Compile`GetElement[pts, 2]}
		]
	],
	RuntimeAttributes -> {Listable}
];


UTCompile[boxClipSegment, {{seg, _Real, 2}, {x1, _Real}, {x2, _Real}, {y1, _Real}, {y2, _Real}, {z1, _Real}, {z2, _Real}},
	Module[{inbdsQ, clip},
		inbdsQ = {
			UnitStep[Subtract[Min[seg[[All, 1]]], x1]], 
			UnitStep[Subtract[x2, Max[seg[[All, 1]]]]],
			UnitStep[Subtract[Min[seg[[All, 2]]], y1]], 
			UnitStep[Subtract[y2, Max[seg[[All, 2]]]]],
			UnitStep[Subtract[Min[seg[[All, 3]]], z1]], 
			UnitStep[Subtract[z2, Max[seg[[All, 3]]]]]
		};
		
		If[Min[inbdsQ] === 1, 
			seg,
			clip = seg;
			If[Compile`GetElement[inbdsQ, 1] === 0 && Length[clip] > 1,
				clip = clipRidge1Side[clip, x1, 1, -1];
			];
			If[Compile`GetElement[inbdsQ, 2] === 0 && Length[clip] > 1,
				clip = clipRidge1Side[clip, x2, 1, 1];
			];
			If[Compile`GetElement[inbdsQ, 3] === 0 && Length[clip] > 1,
				clip = clipRidge1Side[clip, y1, 2, -1];
			];
			If[Compile`GetElement[inbdsQ, 4] === 0 && Length[clip] > 1,
				clip = clipRidge1Side[clip, y2, 2, 1];
			];
			If[Compile`GetElement[inbdsQ, 5] === 0 && Length[clip] > 1,
				clip = clipRidge1Side[clip, z1, 3, -1];
			];
			If[Compile`GetElement[inbdsQ, 6] === 0 && Length[clip] > 1,
				clip = clipRidge1Side[clip, z2, 3, 1];
			];
			If[Length[clip] > 1,
				clip,
				Most[{{0., 0., 0.}}]
			]
		]
	],
	RuntimeAttributes -> {Listable}
];


(* ::Subsubsection::Closed:: *)
(*clipRings*)


clipRings[coords_, multicells_, bds_] :=
	Block[{cnt, clipdata, coordsnew, cellsnew, rep},
		cnt = Length[coords]+1;
		clipdata = With[{proc = iRingClip[coords, #, bds, cnt]}, 
			cnt = First[proc];
			Rest[proc]
		]& /@ multicells;
		clipdata = Select[clipdata, Length[#[[1]]] > 0&];
		
		coordsnew = Join[coords, Flatten[clipdata[[All, 1]], 1]];
		cellsnew = Join @@@ GatherBy[Join @@ clipdata[[All, 2]], Length[#[[1]]]&];
		
		{coordsnew, cellsnew}
	]


iRingClip[coords_, icells_, bds_, icnt_] :=
	Block[{prims, clipdata, cnt, sp, dp, mp, pp, lp, coordsnew, multiint, delete, lens, clippedcells, tl, tlp, cells, mclips, mcells},
		prims = Region`Mesh`ToCoordinates[icells, coords];
		clipdata = clipRings6Sides[coords, icells, prims, bds, icnt];
		cnt = Round[clipdata[[1]]];
		
		{sp, pp, mp, dp, lp} = Accumulate[Prepend[Floor[clipdata[[2 ;; 5]]], 5]];
		
		coordsnew = Partition[clipdata[[sp+1 ;; pp]], 3];
		multiint = Round[clipdata[[pp+1 ;; mp]]];
		delete = Round[clipdata[[mp+1 ;; dp]]];
		lens = Round[clipdata[[dp+1 ;; lp]]];
		
		clippedcells = Statistics`Library`PartitionRagged[Round[clipdata[[lp+1 ;; -1]]], lens];
		clippedcells = GatherByLength[clippedcells];
		If[Length[icells] > Length[delete],
			tl = Length[icells[[1]]];
			cells = Delete[icells, Partition[delete, 1]];
			tlp = FirstPosition[Length /@ clippedcells[[All, 1]], tl, {Missing[]}, {1}, Heads -> False][[1]];
			If[IntegerQ[tlp],
				clippedcells[[tlp]] = Join[cells, clippedcells[[tlp]]];,
				PrependTo[clippedcells, cells];
			];
		];
		
		If[Length[multiint] > 0, 
			mclips = Select[boxClipPolygon[prims[[multiint]], ##]& @@ Flatten[bds], Length[#] > 2&];
			If[Length[mclips] > 0,
				coordsnew = Join[coordsnew, Join @@ mclips];
				mcells = Statistics`Library`PartitionRagged[Range[cnt, cnt + Total[#]-1], #]&[Length /@ mclips];
				mcells = Developer`ToPackedArray /@ GatherBy[mcells, Length];
				clippedcells = Join[clippedcells, mcells];
				cnt += Total[Length /@ mclips];
			]
		];
		
		{cnt, coordsnew, clippedcells}
	]


UTCompile[clipRings6Sides, {{coords, _Real, 2}, {cells, _Integer, 2}, {ptcells, _Real, 3}, {bds, _Real, 2}, {icnt, _Integer}},
	Module[{
		x1, x2, y1, y2, z1, z2, 
		coordsbag, cellsbag, lensbag, deletebag, multiintbag, 
		cc, cell, res, coordsnew, cellsnew, lens, delete, multiint, multiintQ = False, 
		pts, len, ordsx, ordsy, ordsz, ords, mn, mx, 
		sgns = {1}, ord = 0., dim = 0, i, j, ptsrot, cellsrot, cnt},
		
		coordsbag = Internal`Bag[Most[{0.}]];
		cellsbag = Internal`Bag[Most[{0}]];
		lensbag = Internal`Bag[Most[{0}]];
		deletebag = Internal`Bag[Most[{0}]];
		multiintbag = Internal`Bag[Most[{0}]];
		
		{{x1, x2}, {y1, y2}, {z1, z2}} = bds;
		cc = Length[coords]+1;
		cnt = icnt;
		Do[
			dim = 0; multiintQ = False; i = 1; j = 2;
			pts = Compile`GetElement[ptcells, k];
			
			(* ------------ determine which plane to clip with, and if more than one, flag and continue ------------ *)
			
			{ordsx, ordsy, ordsz} = Transpose[pts];
			
			mn = Min[ordsx];
			mx = Max[ordsx];
			If[mx <= x1 || x2 <= mn, Internal`StuffBag[deletebag, k]; Continue[]];
			If[mn < x1, dim = 1; ord = x1; sgns = UnitStep[Subtract[x1, ordsx]]];
			If[x2 < mx, If[dim =!= 0, multiintQ = True, dim = 1; ord = x2; sgns = UnitStep[Subtract[ordsx, x2]]]];
			
			mn = Min[ordsy];
			mx = Max[ordsy];
			If[mx <= y1 || y2 <= mn, Internal`StuffBag[deletebag, k]; Continue[]];
			If[mn < y1, If[dim =!= 0, multiintQ = True, dim = 2; ord = y1; sgns = UnitStep[Subtract[y1, ordsy]]]];
			If[y2 < mx, If[dim =!= 0, multiintQ = True, dim = 2; ord = y2; sgns = UnitStep[Subtract[ordsy, y2]]]];
			
			mn = Min[ordsz];
			mx = Max[ordsz];
			If[mx <= z1 || z2 <= mn, Internal`StuffBag[deletebag, k]; Continue[]];
			If[mn < z1, If[dim =!= 0, multiintQ = True, dim = 3; ord = z1; sgns = UnitStep[Subtract[z1, ordsz]]]];
			If[z2 < mx, If[dim =!= 0, multiintQ = True, dim = 3; ord = z2; sgns = UnitStep[Subtract[ordsz, z2]]]];
			
			If[dim === 0, Continue[]];
			If[multiintQ, Internal`StuffBag[deletebag, k]; Internal`StuffBag[multiintbag, k]; Continue[]];
			
			(* ------------ clip polygon along one plane ------------ *)
			
			len = Length[pts];
			If[Compile`GetElement[sgns, 1] === 0 && Compile`GetElement[sgns, len] === 0,
				i = len;
				While[Compile`GetElement[sgns, i] === 0,
					i--;
				]
			];
			
			While[Compile`GetElement[sgns, i] === 1,
				i++;
			];
			
			cell = Compile`GetElement[cells, k];
			cellsrot = RotateLeft[cell, i-1];
			ptsrot = RotateLeft[pts, i-1];
			sgns = RotateLeft[sgns, i-1];
			While[Compile`GetElement[sgns, j] === 0,
				j++;
			];
			j--;
			
			res = Join[
				clipSegment[Compile`GetElement[ptsrot, j], Compile`GetElement[ptsrot, j+1], ord, dim], 
				clipSegment[Compile`GetElement[ptsrot, len], Compile`GetElement[ptsrot, 1], ord, dim]
			];
			
			Internal`StuffBag[coordsbag, res, 1];
			Internal`StuffBag[lensbag, j+2];
			Internal`StuffBag[deletebag, k];
			Internal`StuffBag[cellsbag, Join[Take[cellsrot, j], {cnt, cnt+1}], 1];
			cnt += 2;,
			{k, Length[cells]}
		];
		
		multiint = Internal`BagPart[multiintbag, All];
		delete = Internal`BagPart[deletebag, All];
		lens = Internal`BagPart[lensbag, All];
		coordsnew = Internal`BagPart[coordsbag, All];
		cellsnew = Internal`BagPart[cellsbag, All];
		
		Join[{cnt, Length[coordsnew], Length[multiint], Length[delete], Length[lens]}, coordsnew, multiint, delete, lens, cellsnew]
	]
];


UTCompile[clipRing1Side, {{pts, _Real, 2}, {ord, _Real}, {dim, _Integer}, {dir, _Integer}},
	Module[{len = Length[pts], sgns, tot, i = 1, j = 2, ptsrot, cpts},
		sgns = If[dir === 1,
			UnitStep[Subtract[pts[[All, dim]], ord]],
			UnitStep[Subtract[ord, pts[[All, dim]]]]
		];
		
		tot = Total[sgns];
		Which[
			tot === 0, pts,
			tot === len, {{-1.,-1.,-1.}},
			
			True,
			If[Compile`GetElement[sgns, 1] === 0 && Compile`GetElement[sgns, len] === 0,
				i = len;
				While[Compile`GetElement[sgns, i] === 0,
					i--;
				]
			];
			
			While[Compile`GetElement[sgns, i] === 1,
				i++;
			];
			
			ptsrot = RotateLeft[pts, i-1];
			sgns = RotateLeft[sgns, i-1];
			While[Compile`GetElement[sgns, j] === 0,
				j++;
			];
			j--;
			
			cpts = {
				clipSegment[Compile`GetElement[ptsrot, j], Compile`GetElement[ptsrot, j+1], ord, dim], 
				clipSegment[Compile`GetElement[ptsrot, len], Compile`GetElement[ptsrot, 1], ord, dim]
			};
			
			Join[cpts, ptsrot[[1 ;; j]]]
		]
	],
	RuntimeAttributes -> {Listable}
];


UTCompile[boxClipPolygon, {{face, _Real, 2}, {x1, _Real}, {x2, _Real}, {y1, _Real}, {y2, _Real}, {z1, _Real}, {z2, _Real}},
	Module[{ft, inbdsQ, clip},
		ft = Transpose[face];
		inbdsQ = {
			Subtract[Min[Compile`GetElement[ft, 1]], x1], 
			Subtract[x2, Max[Compile`GetElement[ft, 1]]],
			Subtract[Min[Compile`GetElement[ft, 2]], y1], 
			Subtract[y2, Max[Compile`GetElement[ft, 2]]],
			Subtract[Min[Compile`GetElement[ft, 3]], z1], 
			Subtract[z2, Max[Compile`GetElement[ft, 3]]]
		};
		
		If[Min[inbdsQ] >= 0.0, 
			face,
			clip = face;
			If[Compile`GetElement[inbdsQ, 1] < 0.0 && Length[clip] > 2,
				clip = clipRing1Side[clip, x1, 1, -1];
			];
			If[Compile`GetElement[inbdsQ, 2] < 0.0 && Length[clip] > 2,
				clip = clipRing1Side[clip, x2, 1, 1];
			];
			If[Compile`GetElement[inbdsQ, 3] < 0.0 && Length[clip] > 2,
				clip = clipRing1Side[clip, y1, 2, -1];
			];
			If[Compile`GetElement[inbdsQ, 4] < 0.0 && Length[clip] > 2,
				clip = clipRing1Side[clip, y2, 2, 1];
			];
			If[Compile`GetElement[inbdsQ, 5] < 0.0 && Length[clip] > 2,
				clip = clipRing1Side[clip, z1, 3, -1];
			];
			If[Compile`GetElement[inbdsQ, 6] < 0.0 && Length[clip] > 2,
				clip = clipRing1Side[clip, z2, 3, 1];
			];
			If[Length[clip] > 2,
				clip,
				Most[{{0., 0., 0.}}]
			]
		]
	],
	RuntimeAttributes -> {Listable},
	Parallelization -> False
];


(* ::Subsubsection::Closed:: *)
(*clipConvexCell*)


clipConvexCell[coords_, cells_, cbds_, bds_] := 
	Block[{clip, clippedcoords, clippedcells},
		clip = Fold[iClipConvexCell, Region`Mesh`ToCoordinates[cells, coords], Select[Transpose[{cbds, bds, {1, 2, 3}}], #[[1, 1]] < #[[2, 1]] || #[[2, 2]] < #[[1, 2]]&]];
		
		If[Length[clip] == 0, Return[{{}, {}}]];
		
		{clippedcoords, clippedcells} = Region`Mesh`DeleteDuplicateCoordinates[Join @@ clip];
		If[!ListQ[clippedcells], clippedcells = Range[Length[coords]]];
		
		clippedcells = Statistics`Library`PartitionRagged[clippedcells, Length /@ clip];
		
		{clippedcoords, clippedcells}
	]


iClipConvexCell[prims_, {{plo_, phi_}, {lo_, hi_}, dim_}] :=
	Block[{clips, primskeep, bdsegslo, bdsegshi, cycslo = Nothing, cycshi = Nothing},
		If[Length[prims] == 0,
			Return[prims]
		];
		
		primskeep = prims;
		If[plo < lo,
			clips = clipRing1Side[primskeep, lo, dim, -1];
			
			bdsegslo = MapThread[If[Length[#2] > 2 && #1 != #2, #2[[1 ;; 2]], Nothing]&, {primskeep, clips}];
			If[Length[bdsegslo] > 0,
				cycslo = With[{uu = Union @@ bdsegslo},
					uu[[Ordering[ArcTan @@ Subtract[Transpose[#], Mean[#]]]]]&[uu[[All, Delete[{1, 2, 3}, dim]]]]
				];
			];
				
			primskeep = Select[clips, Length[#] > 2&];
			If[Length[primskeep] < 3, 
				Return[{}]
			];
		];
		
		If[hi < phi,
			clips = clipRing1Side[primskeep, hi, dim, 1];
			
			bdsegshi = MapThread[If[Length[#2] > 2 && #1 != #2, #2[[1 ;; 2]], Nothing]&, {primskeep, clips}];
			If[Length[bdsegshi] > 0,
				cycshi = With[{uu = Union @@ bdsegshi},
					uu[[Ordering[ArcTan @@ Subtract[Transpose[#], Mean[#]]]]]&[uu[[All, Delete[{1, 2, 3}, dim]]]]
				];
			];
			
			primskeep = Select[clips, Length[#] > 2&];
			If[Length[primskeep] < 3, 
				Return[{}]
			];
		];
		
		Join[primskeep, {cycslo, cycshi}]
	]


(* ::Section:: *)
(*UTPowerTriangulation3D*)


(* ::Subsection::Closed:: *)
(*Main*)


UTPowerTriangulation3D[centers_, radii_] /; validPowerTriangulation3DQ[centers, radii] :=
	Block[{cells, mr},
		cells = powerTriangulationCells3D[centers, radii];
		(
			(* make sure to eliminate unused coordinates since power triangulations might not contain all input circles *)
			mr = MeshRegion[
				centers, 
				Tetrahedron[cells], 
				Method -> {"EliminateUnusedCoordinates" -> True, "DeleteDuplicateCoordinates" -> False, "DeleteDuplicateCells" -> False, 
					"TJunction" -> False, "CheckIntersections" -> False, "CheckOrientation" -> False}
			];
			
			mr /; MeshRegionQ[mr]
			
		) /; cells =!= $Failed
	]


UTPowerTriangulation3D[___] = $Failed;


(* ::Subsection::Closed:: *)
(*powerTriangulationCells3D*)


powerTriangulationCells3D[c_, r_] :=
	Block[{nc = realPack[c], nr = realPack[r], pts4d, center, hull, normaldirs},
		pts4d = Transpose[Append[Transpose[nc], Subtract[NDSolve`FEM`MapThreadDot[nc, nc], nr^2]]];
		center = Mean[pts4d];
		
		hull = convexHull4D[pts4d];
		(
			normaldirs = delaunayNormal4DDirectionC[pts4d, hull, center];
			
			Pick[hull, normaldirs, -1]
			
		) /; hull =!= $Failed
	]


powerTriangulationCells3D[___] = $Failed;


(* ::Subsection::Closed:: *)
(*convexHull4D*)


quickHull := quickHull = If[$VersionNumber < 13,
	WolframCGL`WolframCGLDump`ConvexityDump`iQuickHull,
	WolframCGL`ConvexityDump`iQuickHull
];


convexHull4D[pts_ /; Length[pts] === 4] = Partition[Range[4], 4];


convexHull4D[pts_] /; quickHullQ && Length[pts] > 4 :=
	Block[{res},
		res = Quiet[quickHull[pts]];
		
		res /; MatrixQ[res, IntegerQ] && Last[Dimensions[res]] === 4
	]


convexHull4D[___] = $Failed;


quickHullQ := quickHullQ = 
	And[
		TrueQ[$VersionNumber >= 12.3],
		ConvexHullMesh[{{0, 0}, {1, 1}, {0, 1}}]; (* AutoLoad iQuickHull *)
		Head[quickHull] === LibraryFunction
	]


(* ::Subsection::Closed:: *)
(*delaunayNormal4DDirectionC*)


With[{cross = Cross[{m11, m12, m13, m14}, {m21, m22, m23, m24}, {m31, m32, m33, m34}]},
UTCompile[delaunayNormal4DDirectionC, {{coords, _Real, 2}, {cell, _Integer, 1}, {hullcenter, _Real, 1}},
	Block[{a, b, c, d, center, normal, orientation, v1, v2, v3, m11, m12, m13, m14, m21, m22, m23, m24, m31, m32, m33, m34},
		a = Compile`GetElement[coords, Compile`GetElement[cell, 1]];
		b = Compile`GetElement[coords, Compile`GetElement[cell, 2]];
		c = Compile`GetElement[coords, Compile`GetElement[cell, 3]];
		d = Compile`GetElement[coords, Compile`GetElement[cell, 4]];
		
		center = 0.25*(a + b + c + d);
		
		(* normal in 4D -- Cross is only compilable for 3D *)
		
		v1 = Subtract[b, a];
		v2 = Subtract[c, a];
		v3 = Subtract[d, a];
		
		m11 = Compile`GetElement[v1, 1];
		m12 = Compile`GetElement[v1, 2];
		m13 = Compile`GetElement[v1, 3];
		m14 = Compile`GetElement[v1, 4];
		m21 = Compile`GetElement[v2, 1];
		m22 = Compile`GetElement[v2, 2];
		m23 = Compile`GetElement[v2, 3];
		m24 = Compile`GetElement[v2, 4];
		m31 = Compile`GetElement[v3, 1];
		m32 = Compile`GetElement[v3, 2];
		m33 = Compile`GetElement[v3, 3];
		m34 = Compile`GetElement[v3, 4];
		
		normal = cross;
		
		(* multiplier to make sure we have an outward normal, which works because we know the hull is convex *)
		orientation = normal . Subtract[center, hullcenter];
		
		Sign[orientation * Compile`GetElement[normal, 4]]
	],
	RuntimeAttributes -> {Listable}
]
];


(* ::Subsection::Closed:: *)
(*validPowerTriangulation3DQ*)


validPowerTriangulation3DQ[centers_, radii_] :=
	And[
		MatrixQ[centers, Internal`RealValuedNumericQ],
		Length[centers] > 3, (* ignore degenerate inputs *)
		Length[centers[[1]]] === 3,
		VectorQ[radii, Internal`RealValuedNumericQ],
		Length[centers] === Length[radii]
	]


validPowerTriangulation3DQ[___] = False;


(* ::Section:: *)
(*UTPowerDiagram3D*)


(* ::Subsection::Closed:: *)
(*About*)


(* ::Text:: *)
(*Implementation and terminology are based off*)


(* ::Text:: *)
(*\[Bullet] Voronoi Diagram in the Laguerre Geometry and Its Applications*)
(*  https://doi.org/10.1137/0214006*)
(*  https://www2.cs.sfu.ca/~binay/2016/813/LaguerreGeometry.pdf*)


(* ::Text:: *)
(*\[Bullet] Power Diagrams: Properties, Algorithms and Applications*)
(*  https://doi.org/10.1137/0216006*)
(*  https://www.cs.jhu.edu/~misha/Spring20/Aurenhammer87.pdf*)


(* ::Subsection::Closed:: *)
(*Main*)


UTPowerDiagram3D[centers_, radii_, ibds_:Automatic, returntypes_:Automatic] /; validPowerDiagram3DQ[centers, radii, returntypes] :=
	Block[{bds, fmts, res},
		bds = powerDiagramBoundingBox[centers, radii, ibds];
		(
			fmts = expandPowerDiagramFormats[returntypes];
			res = iPowerDiagram3D[centers, radii, bds, fmts];
			(
				If[Length[fmts] > 1 || ListQ[returntypes],
					res[[fmts]],
					res[[fmts[[1]]]]
				]
			) /; AssociationQ[res]
			
		) /; MatrixQ[bds, NumericQ] && Dimensions[bds] === {3, 2}
	]


UTPowerDiagram3D[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Argument conform & completion*)


addCodeCompletion[UTPowerDiagram3D][
	None, None, None,
	{
		"Cells", "Faces", "Ridges", "Nodes", 
		"Centers", "Radii",  
		"BoundedCellQ", "ClippedCellQ", "ConnectivityMatrix", "CommonFaces", "Bounds",
		"SubstantialQ", "TrivialQ", "ProperQ", "ImproperQ", 
		"RadicalCenters", "RadicalAxes", "RadicalPlanes",
		"Properties"
	}
];


(* ::Subsection::Closed:: *)
(*iPowerDiagram3D*)


iPowerDiagram3D[centers_, radii_, bds_, fmts_] :=
	Block[{maxdim, vdata, res},
		maxdim = Max[pdDimNeeded /@ fmts];
		vdata = powerDiagram3DData[centers, radii, bds, maxdim];
		(
			res = makePowerDiagramResults[fmts, vdata];
			
			AssociationThread[fmts, res] /; FreeQ[res, $Failed, {1}]
		
		) /; AssociationQ[vdata]
	]


iPowerDiagram3D[___] = $Failed;


(* ::Subsection::Closed:: *)
(*powerDiagram3DData*)


powerDiagram3DData[___, -1] = <||>;


powerDiagram3DData[c_, r_, bds_?degenerateBoundingBoxQ, __] := 
	<|"Centers" -> c, "Radii" -> r, "Bounds" -> bds, "Nodes" -> {}, "Ridges" -> {}, "Faces" -> {}, "Cells" -> {}, "FaceCellIncidence" -> {}, "CheckDegeneracies" -> False|>


powerDiagram3DData[centers_, radii_, bds_, maxdimension_:3] :=
	Block[{ptsInQ, oseeds, seeds, weights, numseeds, ptcells, conn, pdnodes, pdlines, mergedQ, dcheckQ = False, faces, fcincidence, cgath, cells},
		ptsInQ = And @@ MapThread[#2[[1]] < #1[[1]] <= #1[[2]] < #2[[2]]&, {CoordinateBounds[centers], bds}];
		oseeds = outsidePoints[centers, bds, True];
		seeds = Join[centers, oseeds];
		weights = Join[radii, ConstantArray[N[0, Internal`EffectivePrecision[radii]], Length[oseeds]]];
		
		ptcells = powerTriangulationCells3D[seeds, weights];
		(
			numseeds = Length[seeds] - Length[oseeds];
			pdnodes = powerDiagramNodes[seeds, weights, ptcells];
			(
				pdlines = cells = faces = fcincidence = Missing[];
				If[maxdimension >= 1,
					pdlines = powerDiagramRidges[ptcells];
					
					{pdnodes, pdlines, ptcells, mergedQ} = mergeDuplicateNodes[pdnodes, pdlines, ptcells];
					dcheckQ = !ptsInQ || mergedQ;
					
					If[maxdimension >= 2,
						{faces, fcincidence} = powerDiagramFacesAndCellIncidence[pdlines, ptcells];
						
						If[maxdimension == 3,
							cells = powerDiagramCells[faces, fcincidence, numseeds];
						]
					]
				];
					
				AssociationThread[
					{"Centers", "Radii", "Bounds", "Nodes", "Ridges", "Faces", "Cells", "FaceCellIncidence", "CheckDegeneracies"}, 
					{seeds[[1 ;; numseeds]], radii, bds, pdnodes, pdlines, faces, cells, fcincidence, dcheckQ}
				]
			
			) /; MatrixQ[pdnodes, NumericQ]
			
		) /; ListQ[ptcells]
	]


powerDiagram3DData[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Construction utilities*)


(* ::Subsubsection::Closed:: *)
(*Bounding box*)


powerDiagramBoundingBox[_, _, bbox_List] /; Dimensions[bbox] === {3, 2} && MatrixQ[bbox, Internal`RealValuedNumericQ] := bbox


powerDiagramBoundingBox[centers_, _, Scaled[s_?Internal`RealValuedNumericQ]] := CoordinateBounds[centers, Scaled[s]]


powerDiagramBoundingBox[centers_, _, bbox_List] /; Dimensions[bbox] === {3, 2} := 
	Block[{bds, lens, res},
		bds = CoordinateBounds[centers];
		lens = Abs[Subtract @@@ bds];
		
		res = MapThread[voronoiPadding, Join @@@ {bds, Transpose[{Minus[lens], lens}], bbox}]
	]


powerDiagramBoundingBox[centers_, _, _] := 
	Block[{bds, lens, mx},
		bds = CoordinateBounds[centers, Scaled[0.05]];
		lens = Abs[Subtract @@@ bds];
		mx = Max[lens];
		If[mx == 0,
			0.5{{-1, 1}, {-1, 1}, {-1, 1}} + bds,
			MapThread[If[Chop[#2/mx] == 0, .1{-mx, mx} + Mean[#1], #1]&, {bds, lens}]
		]
	]


(* ::Subsubsection::Closed:: *)
(*Power diagram nodes*)


powerDiagramNodes[centers_, radii_, dcells_] /; Internal`EffectivePrecision[centers] === MachinePrecision := numericalRadicalCenters[centers, radii, dcells]


powerDiagramNodes[centers_, radii_, dcells_] := 
	Block[{ncents, \[CurlyEpsilon], ndists, suspect, scents},
		ncents = numericalRadicalCenters[centers, radii, dcells];
		
		(* look for nearby circumcenters that seem duplicate and compute then exactly *)
		\[CurlyEpsilon] = Min[Abs[Subtract @@@ CoordinateBounds[centers]]] * 10.^(Internal`$EqualTolerance - MachinePrecision + 3);
		ndists = Nearest[Union[ncents] -> "Distance", ncents, 2][[All, 2]];
		suspect = Random`Private`PositionsOf[ndists - \[CurlyEpsilon], Negative];
		
		If[Length[suspect] > 0,
			scents = realPack[carefulN[symbolicRadicalCenters[centers, radii, dcells[[suspect]]]]];
			ncents[[suspect]] = scents;
		];
		
		ncents
	]


numericalRadicalCenters[centers_, radii_, dcells_] :=
	Block[{pcoords, trdcells, tprims, trads, len, blocks, vec, rtemplate, mat},
		pcoords = realPack[centers];
		trdcells = Transpose[dcells];
		
		tprims = pcoords[[#]]& /@ trdcells;
		trads = radii[[#]]& /@ trdcells;
		
		blocks = Transpose[{Subtract[#1, #4], Subtract[#2, #4], Subtract[#3, #4]}& @@ tprims];
		vec = 0.5*Flatten[Transpose[{Subtract[#1, #4], Subtract[#2, #4], Subtract[#3, #4]}& @@ Subtract[Total[tprims^2, {3}], trads^2]]];
		
		len = Length[blocks];
		rtemplate = Mod[Range[9], 3, -2];
		mat = SparseArray[Automatic, {3len, 3len}, 0, {1, {Range[0, 9len, 3], ArrayReshape[ConstantArray[rtemplate, len] + Range[3, 3len, 3], {9len, 1}]}, Flatten[blocks]}];
		
		Partition[LinearSolve[mat, vec, Method -> "Banded"], 3]
	]


symbolicRadicalCenters[centers_, radii_, dcells_] :=
	Block[{trdcells, tprims, trads, blocks, vec},
		trdcells = Transpose[dcells];
		
		tprims = centers[[#]]& /@ trdcells;
		trads = radii[[#]]& /@ trdcells;
		
		blocks = Transpose[2{Subtract[#1, #4], Subtract[#2, #4], Subtract[#3, #4]}& @@ tprims];
		vec = Transpose[{Subtract[#1, #4], Subtract[#2, #4], Subtract[#3, #4]}& @@ Subtract[Total[tprims^2, {3}], trads^2]];
		
		MapThread[LinearSolve[##, Method -> "CofactorExpansion"]&, {blocks, vec}]
	]


(* ::Subsubsection::Closed:: *)
(*Power diagram ridges*)


powerDiagramRidges = voronoiRidges;


(* ::Subsubsection::Closed:: *)
(*Power diagram faces*)


powerDiagramFacesAndCellIncidence = voronoiFacesAndCellIncidence;


(* ::Subsubsection::Closed:: *)
(*Power diagram cells*)


powerDiagramCells[faces_, fcincidence_, numseeds_] :=
	Block[{trivialInds, tcount, vcells},
		trivialInds = Complement[Range[numseeds], Flatten[fcincidence]];
		tcount = Length[trivialInds];
		
		(* short circuit if all cells are substantial *)
		If[tcount === numseeds,
			Return[voronoiCells[faces, fcincidence, numseeds]]
		];
		
		(* insert virtual faces and cells *)
		vcells = voronoiCells[
			Join[faces, ConstantArray[Max[faces]+1, {tcount, 1}]], 
			Join[fcincidence, Transpose[{trivialInds, trivialInds}]], 
			numseeds
		];
		
		(* delete virtual cells *)
		vcells[[trivialInds]] = {};
		
		vcells
	]


(* ::Subsection::Closed:: *)
(*Return type constructors*)


(* ::Subsubsection::Closed:: *)
(*Return type parsing*)


$pdFormats = {
	"Centers", "Radii", "Bounds", 
	"Nodes", "Ridges", "Faces", "Cells", 
	"BoundedCellQ", "ClippedCellQ", "ConnectivityMatrix", "CommonFaces",
	"SubstantialQ", "TrivialQ", "ProperQ", "ImproperQ",
	"RadicalCenters", "RadicalAxes", "RadicalPlanes"
};


pdDimNeeded["Properties"] = -1;
pdDimNeeded["Centers"] = pdDimNeeded["Radii"] = pdDimNeeded["Bounds"] = pdDimNeeded["Nodes"] = pdDimNeeded["RadicalCenters"] = 0;
pdDimNeeded["Ridges"] = pdDimNeeded["RadicalAxes"] = 1;
pdDimNeeded["Faces"] = pdDimNeeded["ConnectivityMatrix"] = pdDimNeeded["CommonFaces"] = pdDimNeeded["SubstantialQ"] = pdDimNeeded["TrivialQ"] = pdDimNeeded["RadicalPlanes"] = 2;
pdDimNeeded[_] = 3;


$validPowerDiagramFormats = Alternatives @@ Join[$pdFormats, {Automatic, All}];


validPowerDiagramFormatQ[Append[$validPowerDiagramFormats, "Properties"]] = True;
validPowerDiagramFormatQ[{Append[$validPowerDiagramFormats, "Properties"]..}] = True;
validPowerDiagramFormatQ[___] = False;


expandPowerDiagramFormats[fmt:Except[_List]] := expandPowerDiagramFormats[{fmt}]
expandPowerDiagramFormats[fmts_] := DeleteDuplicates[Flatten[Replace[fmts, {Automatic -> "Cells", All -> $pdFormats}, {1}]]]


pdFormatLookup[fmts:Except[_List]] := pdFormatLookup[{fmts}]
pdFormatLookup[fmts_] := Join[AssociationThread[fmts, True], AssociationThread[Complement[$pdFormats, fmts], False]]


validPowerDiagram3DQ[centers_, radii_, returntypes_] :=
	And[
		MatrixQ[centers, Internal`RealValuedNumericQ],
		Length[centers] > 0,
		Length[centers[[1]]] === 3,
		VectorQ[radii, Internal`RealValuedNumericQ],
		Length[centers] === Length[radii],
		validPowerDiagramFormatQ[returntypes]
	]


validPowerDiagram3DQ[___] = False;


(* ::Subsubsection::Closed:: *)
(*makePowerDiagramResults*)


makePowerDiagramResults[fmts_List, vdata_] :=
	Block[{rtdata},
		rtdata[_] = Missing[];
		
		Table[
			powerDiagramResult[fmt, vdata, rtdata],
			{fmt, fmts}
		]
	]


makePowerDiagramResults[fmt_, args__] := makePowerDiagramResults[{fmt}, args]


powerDiagramResult["Properties", ___] = $pdFormats;


(* ::Subsubsection::Closed:: *)
(*Input parameters*)


powerDiagramResult["Centers", vdata_, rtdata_] := (rtdata["Centers"] = vdata["Centers"])


powerDiagramResult["Radii", vdata_, rtdata_] := (rtdata["Radii"] = vdata["Radii"])


powerDiagramResult["Bounds", vdata_, rtdata_] := (rtdata["Bounds"] = vdata["Bounds"])


(* ::Subsubsection::Closed:: *)
(*Geometric data*)


powerDiagramResult["Nodes", vdata_, rtdata_] := 
	Block[{nodes},
		nodes = rtdata["Nodes"];
		
		If[MissingQ[nodes],
			nodes = rtdata["Nodes"] = makeVoronoiNodes @@ Lookup[vdata, {"Nodes", "Bounds"}];
		];
		
		nodes
	]


powerDiagramResult["Ridges", vdata_, rtdata_] := 
	Block[{ridges},
		ridges = rtdata["Ridges"];
		
		If[MissingQ[ridges],
			ridges = rtdata["Ridges"] = makeVoronoiRidges @@ Lookup[vdata, {"Nodes", "Ridges", "Bounds"}];
		];
		
		ridges
	]


powerDiagramResult["Faces", vdata_, rtdata_] := 
	Block[{faces},
		faces = rtdata["Faces"];
		
		If[MissingQ[faces],
			faces = rtdata["Faces"] = makeVoronoiFaces @@ Lookup[vdata, {"Nodes", "Faces", "Bounds", "CheckDegeneracies"}];
		];
		
		faces
	]


powerDiagramResult["Cells", vdata_, rtdata_] := 
	Block[{cells},
		cells = rtdata["Cells"];
		
		If[MissingQ[cells],
			cells = rtdata["Cells"] = makeVoronoiCells @@ Lookup[vdata, {"Nodes", "Cells", "Bounds", "CheckDegeneracies"}];
		];
		
		cells
	]


powerDiagramResult["BoundedCellQ", vdata_, rtdata_] := 
	Block[{boundedQ},
		boundedQ = rtdata["BoundedCellQ"];
		
		If[MissingQ[boundedQ],
			boundedQ = rtdata["BoundedCellQ"] = makeVoronoiBoundedCellQ @@ Lookup[vdata, {"FaceCellIncidence", "Cells"}];
		];
		
		boundedQ
	]


powerDiagramResult["ClippedCellQ", vdata_, rtdata_] := 
	Block[{clippedQ},
		clippedQ = rtdata["ClippedCellQ"];
		
		If[MissingQ[clippedQ],
			clippedQ = rtdata["ClippedCellQ"] = makeVoronoiClippedCellQ @@ Lookup[vdata, {"Nodes", "Cells", "Bounds"}];
		];
		
		clippedQ
	]


(* ::Subsubsection::Closed:: *)
(*Topological data*)


powerDiagramResult["ConnectivityMatrix", vdata_, rtdata_] := 
	Block[{conn, substantialQ, fcincidence, cellcnt, nzp, arr},
		conn = rtdata["ConnectivityMatrix"];
		
		If[MissingQ[conn],
			substantialQ = powerDiagramResult["SubstantialQ", vdata, rtdata];
			
			fcincidence = vdata["FaceCellIncidence"];
			cellcnt = Length[vdata["Centers"]];
			
			nzp = Pick[fcincidence, Total[UnitStep[Subtract[fcincidence, cellcnt + 1]], {2}], 0];
			arr = SparseArray[nzp -> ConstantArray[1, Length[nzp]], {cellcnt, cellcnt}];
			
			conn = rtdata["ConnectivityMatrix"] = arr + Transpose[arr] + DiagonalMatrix[SparseArray[substantialQ]];
		];
		
		conn
	]


powerDiagramResult["CommonFaces", vdata_, rtdata_] := 
	Block[{common},
		common = rtdata["CommonFaces"];
		
		If[MissingQ[common],
			common = rtdata["CommonFaces"] = makeVoronoiCommonFaces[#1, #2, #3, #4, Length[#5]]& @@ Lookup[vdata, {"Nodes", "Faces", "FaceCellIncidence", "Bounds", "Seeds"}];
		];
		
		common
	]


(* ::Subsubsection::Closed:: *)
(*Substantialness*)


powerDiagramResult["SubstantialQ", vdata_, rtdata_] := 
	Block[{fcincidence, cellcnt, validcellpairs, subinds, substantialQ},
		substantialQ = rtdata["SubstantialQ"];
		If[MissingQ[substantialQ],
			fcincidence = vdata["FaceCellIncidence"];
			cellcnt = Length[vdata["Centers"]];
			
			validcellpairs = Pick[fcincidence, UnitStep[Subtract[cellcnt, Max /@ fcincidence]], 1];
			subinds = DeleteDuplicates[Flatten[validcellpairs]];
			
			substantialQ = ConstantArray[0, cellcnt];
			substantialQ[[subinds]] = 1;
			
			rtdata["SubstantialQ"] = substantialQ;
		];
			
		substantialQ
	]


powerDiagramResult["TrivialQ", vdata_, rtdata_] := Subtract[1, powerDiagramResult["SubstantialQ", vdata, rtdata]]


(* ::Subsubsection::Closed:: *)
(*Properness*)


powerDiagramResult["ProperQ", vdata_, rtdata_] := 
	Block[{properQ, nodes, celldata, cells, centers, radii, clippedQ},
		properQ = rtdata["ProperQ"];
		
		If[MissingQ[properQ],
			{nodes, celldata, centers, radii} = Lookup[vdata, {"Nodes", "Cells", "Centers", "Radii"}];
			cells = powerDiagramResult["Cells", vdata, rtdata];
			clippedQ = powerDiagramResult["ClippedCellQ", vdata, rtdata];
			
			properQ = Developer`ToPackedArray @ MapThread[
				properSphereQ[nodes, ##]&, 
				{celldata, cells, centers, radii, clippedQ}
			];
			
			rtdata["ProperQ"] = properQ;
		];
		
		properQ
	]


powerDiagramResult["ImproperQ", vdata_, rtdata_] := Subtract[1, powerDiagramResult["ProperQ", vdata, rtdata]]


properSphereQ[nodes_, celldata_, cell_, center_, radius_, clippedQ_] :=
	Block[{dist, jj, bmr},
		dist = SignedRegionDistance[cell, center];
		
		If[dist > radius && clippedQ === 1 && celldata =!= {},
			jj = Join @@ celldata;
			bmr = voronoiCellBMR[nodes[[DeleteDuplicates[jj]]], Statistics`Library`PartitionRagged[integerIndices[jj], Length /@ celldata]];
			dist = SignedRegionDistance[bmr, center]
		];
		
		UnitStep[Subtract[radius, dist]]
	]


(* ::Subsubsection::Closed:: *)
(*Radical hyperplanes*)


powerDiagramResult["RadicalCenters", vdata_, rtdata_] := rtdata["Nodes"]


powerDiagramResult["RadicalAxes", vdata_, rtdata_] := 
	Block[{ridges, prims},
		ridges = powerDiagramResult["Ridges", vdata, rtdata];
		
		If[!MeshRegionQ[ridges],
			{},
			prims = Join @@ MeshPrimitives[ridges, 1, "Multicells" -> True][[All, 1]];
			
			Transpose[{prims[[All, 1]], Subtract[prims[[All, 2]], prims[[All, 1]]]}]
		]
	]


powerDiagramResult["RadicalPlanes", vdata_, rtdata_] := 
	Block[{faces, normals, centroids},
		faces = powerDiagramResult["Faces", vdata, rtdata];
		
		If[!MeshRegionQ[faces],
			Return[{}]
		];
		
		normals = Region`Mesh`MeshCellNormals[faces, 2];
		If[normals === $Failed,
			Return[$Failed]
		];
		
		centroids = PropertyValue[{faces, 2}, MeshCellCentroid];
		
		Transpose[{normals, centroids}]
	]
