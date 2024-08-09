(* ::Package:: *)

(* ::Title:: *)
(*Discretization*)


(* ::Section:: *)
(*Begin*)


Package["UTCG`"]


PackageExport["UTTube"]
PackageExport["UTPathPoints"]
PackageExport["UTTorus"]
PackageExport["UTRevolutionMesh"]
PackageExport["UTSamplePoints"]
PackageExport["UTRoundedCuboidMesh"]
PackageExport["UTSplineWireframe"]
PackageExport["UTBodyCenteredCubicLattice"]


UTTube::usage = "Discretize a tube with where the faces and radii are parameterized by arclength.";
UTPathPoints::usage = "Finds a path parameterized by arclength.";
UTTorus::usage = "Discretize a torus.";
UTRevolutionMesh::usage = "Revolve points around an axis to create a surface mesh.";
UTSamplePoints::usage = "Samples a univariate function uniformly with respect to arclength.";
UTRoundedCuboidMesh::usage = "Discretize a cuboided with rounded edges.";
UTSplineWireframe::usage = "Express a wire frame as a union of spline wire frames.";
UTBodyCenteredCubicLattice::usage = "Generate a body centered cubic lattice over bounds or region.";


(* ::Section:: *)
(*UTTube*)


(* ::Subsection::Closed:: *)
(*Main*)


Clear[UTTube];


Options[UTTube] = DeleteDuplicates @ Join[
	{MaxRecursion -> 0, CapForm -> Automatic, "ReturnSolid" -> False, "Torsion" -> 0},
	{PlotPoints -> Automatic, "CrossSection" -> Automatic},
	{SplineDegree -> 1},
	Options[BSplineCurve],
	Options[MeshRegion]
];


UTTube[curve_, radii_, opts:OptionsPattern[]] /; !OptionQ[radii] := 
	Block[{tdata = tubeMeshData[curve, radii, opts], res},
		(
			res = iTubeMesh[tdata];
			(
				If[ListQ[res],
					res = If[TrueQ[OptionValue["ReturnSolid"]],
						UTHull[res, "ReturnSolid" -> True],
						UTJoin[res]
					]
				];
				(
					Head[res][res, FilterRules[{opts}, Options[MeshRegion]]]
				
				) /; RegionQ[res]
		
			) /; RegionQ[res] || VectorQ[res, RegionQ]
			
		) /; tdata =!= $Failed
	]


UTTube[mr_?UTLineMesh2DQ, r_?Positive, opts:OptionsPattern[]] /; RegionEmbeddingDimension[mr] == 2 :=
	Block[{mr3d, res},
		mr3d = RegionProduct[mr, Point[{0.}]];
		(
			res = UTTube[mr3d, r, opts];
			
			res /; RegionQ[res]
			
		) /; UTLineMesh3DQ[mr3d]
	]


UTTube[mr_?UTLineMesh3DQ, r_?Positive, opts:OptionsPattern[]] :=
	Block[{split, torsion, tubes, solidQ, hull},
		split = UTSingularComponents[mr];
		torsion = OptionValue["Torsion"];
		tubes = perturbedTorsionTube[#, r, torsion, opts]& /@ split;
		(
			solidQ = OptionValue["ReturnSolid"];
			hull = If[!TrueQ[solidQ] && Length[split] === 1,
				First[tubes],
				UTHull[tubes, "ReturnSolid" -> solidQ]
			];
			
			Head[hull][hull, FilterRules[{opts}, Options[MeshRegion]]] /; RegionQ[hull]
			
		) /; VectorQ[tubes, MeshRegionQ]
	]


UTTube[___] = $Failed;


perturbedTorsionTube[mr_, r_, t_, opts___] :=
	BlockRandom[
		SeedRandom[Hash[coords]];
		UTTube[MeshCoordinates[mr], r, SplineClosed -> Equal @@ MeshCellCount[mr], "Torsion" -> t + RandomReal[.01{-1, 1}], "ReturnSolid" -> False, opts]
	]


(* ::Subsection::Closed:: *)
(*iTubeMesh*)


iTubeMesh[tdatas_List] := iTubeMesh /@ tdatas


iTubeMesh[tdata_Association] :=
	Module[{samples, p, r, coords, cells, head, res},
		samples = sampleTubePoints[tdata];
		(
			{p, r} = samples;
			
			{coords, cells} = bishopTube[p, r, tdata];
			head = tdata["ReturnType"];
			res = Quiet[head[coords, cells, Method -> {"EliminateUnusedCoordinates" -> False, "DeleteDuplicateCells" -> False}]];
			
			If[head === BoundaryMeshRegion && !RegionQ[res],
				res = UTHull[FastMeshRegion[coords, cells], "ReturnSolid" -> True];
			];
			
			res /; RegionQ[res]
			
		) /; samples =!= $Failed
	]


iTubeMesh[___] = $Failed;


(* ::Subsection::Closed:: *)
(*tubeMeshData*)


(* ::Subsubsection::Closed:: *)
(*Main*)


Options[tubeMeshData] = Join[Options[UTTube], {"LineType" -> Automatic, "PathOnly" -> False}];


tubeMeshData[tspec_?multiTubeQ, args__] := tubeMeshData[#, args]& /@ tspec


tubeMeshData[Line[pts_List, o___], radii_, opts:OptionsPattern[]] := tubeMeshData[pts, radii, opts, SplineDegree -> 1, "LineType" -> Line]


tubeMeshData[BezierCurve[pts_List, o___], radii_, opts:OptionsPattern[]] := tubeMeshData[pts, radii, opts, o, SplineDegree -> 3, "LineType" -> BezierCurve]


tubeMeshData[BSplineCurve[pts_List, o___], radii_, opts:OptionsPattern[]] := tubeMeshData[pts, radii, opts, o, SplineDegree -> 3, "LineType" -> BSplineCurve]


tubeMeshData[Tube[pts:(_List|_Line|_BezierCurve|_BSplineCurve), radii_], opts:OptionsPattern[]] := tubeMeshData[pts, radii, opts]


tubeMeshData[pts_, r_?NumericQ, opts:OptionsPattern[]] := tubeMeshData[pts, ConstantArray[r, Length[pts]], opts]


tubeMeshData[pts_List, radii_List:{}, OptionsPattern[]] /; MatrixQ[pts, Internal`RealValuedNumericQ] && 2 <= Dimensions[pts][[2]] <= 3 && VectorQ[radii, Internal`RealValuedNumericQ] := 
	Catch @ Block[{npts = pts, nradii = radii, vals, assoc, scaledTorsionQ},
		vals = MapThread[Construct, {$pTMFunctions, OptionValue[$pTMOptions]}];
		assoc = AssociationThread[$pTMOptions, vals];
		
		If[Length[npts[[1]]] == 2 && !TrueQ[assoc["PathOnly"]],
			npts = Transpose[Append[Transpose[npts], ConstantArray[0., Length[npts]]]];
		];
		
		If[assoc["CrossSection"] === Automatic,
			assoc[CapForm] = assoc[CapForm] /. Automatic -> "Round",
			assoc[CapForm] = assoc[CapForm] /. {Automatic -> "Butt", "Round" -> "RoundApprox"}
		];
		
		scaledTorsionQ = Head[assoc["Torsion"]] === Scaled;
		assoc["ScaledTorsion"] = scaledTorsionQ;
		If[scaledTorsionQ, assoc["Torsion"] = First[assoc["Torsion"]]];
		
		assoc = Join[assoc, <|"CirclePoints" -> First[assoc[PlotPoints]], "TubePoints" -> Last[assoc[PlotPoints]]|>];
		If[assoc["TubePoints"] === Automatic, assoc["TubePoints"] = Scaled[(1/2 - 1/Sqrt[3])TriangleWave[assoc["Torsion"] + 1/4] + 1/2 + 1/Sqrt[3]]];
		
		assoc["ReturnType"] = assoc["ReturnSolid"];		
		If[assoc["ReturnType"] === BoundaryMeshRegion && assoc["Torsion"] == 0, assoc["Torsion"] = 0.001];
		
		assoc["LineType"] = Replace[assoc["LineType"], Automatic -> If[assoc[SplineDegree] === 1, Line, BSplineCurve], {0}];
		
		(* Close spline if endpoints match under the Automatic setting *)
		If[assoc["LineType"] =!= BezierCurve && assoc[SplineClosed] === Automatic, assoc[SplineClosed] = Chop[Norm[First[npts] - Last[npts]]] == 0];
		
		If[assoc["LineType"] =!= BezierCurve && assoc[SplineClosed] && Chop[Norm[First[npts] - Last[npts]]] == 0, 
			If[Length[npts] == 2, Throw[$Failed]];
			npts = Most[npts];
			nradii = Most[nradii]
		];
		
		If[assoc["LineType"] === BezierCurve, assoc[SplineClosed] = False];
		
		(* Can't fit too large of a polynomial *)
		If[NumericQ[assoc[SplineDegree]] || assoc[SplineDegree] === Infinity, 
			assoc[SplineDegree] = Min[assoc[SplineDegree], Length[npts]-1],
			assoc[SplineDegree] = 1
		];
		
		(* Must be a valid knot spec *)
		If[ListQ[assoc[SplineKnots]] && Length[assoc[SplineKnots]] <= Length[npts], assoc[SplineKnots] = Automatic];
		
		(* Must be a valid weight spec *)
		If[ListQ[assoc[SplineWeights]] && Length[assoc[SplineWeights]] != Length[pts], assoc[SplineWeights] = Automatic];
		If[ListQ[assoc[SplineWeights]] && Length[pts] != Length[npts], assoc[SplineWeights] = Most[assoc[SplineWeights]]];
		
		(* For speed just return a surface mesh *)
		If[assoc["ReturnType"] === Automatic, assoc["ReturnType"] = MeshRegion];
		
		(* Can't have a solid without caps *)
		If[!FreeQ[assoc[CapForm], None], assoc["ReturnType"] = MeshRegion];
		
		(* Ideally we shouldn't determine these values here... *)
		If[assoc["PathOnly"],
			If[NumericQ[assoc["CirclePoints"]] && !NumericQ[assoc["TubePoints"]],
				assoc["TubePoints"] = assoc["CirclePoints"];
			];
			assoc["TubePoints"] = Replace[assoc["CirclePoints"], Automatic -> 40, {0}];
		];
		assoc["CirclePoints"] = Replace[assoc["CirclePoints"], {Automatic -> $circlePointsDefault, Scaled[p_?Positive] :> Max[3, Round[p*$circlePointsDefault]]}, {0}];
		
		(* For nice caps, circle points must be equal to 3, or a multiple of 4, 5, or 6 *)
		If[!FreeQ[assoc[CapForm], "Round"], 
			assoc["CirclePoints"] = Replace[assoc["CirclePoints"], n_Integer /; n > 6 :> Nearest[{5, 6, 4}*Round[n/{5, 6, 4}], n][[1]], {0}]
		];
		
		assoc["CrossSectionPoints"] = parseCrossSection[assoc["CrossSection"], assoc["CirclePoints"]];
		If[assoc["CrossSectionPoints"] === $Failed, Return[$Failed]];
		
		With[{cp = assoc["CrossSectionPoints"]},
			assoc["CirclePoints"] = If[MatrixQ[cp], Length[cp], Round[Mean[Length /@ cp]]];
		];
		
		nradii = PadRight[nradii, Length[npts], Last[nradii, 0.]];
		assoc = Join[assoc, <|"Points" -> npts, "Radii" -> nradii|>];
		
		assoc
	]


tubeMeshData[___] = $Failed;


(* ::Subsubsection::Closed:: *)
(*Parsers*)


$pTMFunctions = {pTMDegree, pTMPlotPoints, pTMCrossSection, pTMMaxRecursion, pTMCaps, pTMClosed, pTMKnots, pTMWeights, pTMReturnSolid, pTMLineType, pTMTorsion, pTMPathOnly};


$pTMOptions = {SplineDegree, PlotPoints, "CrossSection", MaxRecursion, CapForm, SplineClosed, SplineKnots, SplineWeights, "ReturnSolid", "LineType", "Torsion", "PathOnly"};


pTMDegree[deg_Integer?Positive] := deg
pTMDegree[DirectedInfinity[1]] = DirectedInfinity[1];
pTMDegree[_] := OptionValue[UTTube, SplineDegree];


$circlePointsDefault = 16;


validTubePointsQ[Automatic] = True;
validTubePointsQ[tpts_Integer] /; tpts > 1 = True;
validTubePointsQ[Scaled[_?Positive]] = True;
validTubePointsQ[_] = False;


validCirclePointsQ[Automatic] = True;
validCirclePointsQ[cpts_Integer] /; cpts > 2 = True;
validCirclePointsQ[Scaled[_?Positive]] = True;
validCirclePointsQ[_] = False;


pTMPlotPoints[{cspec_, tspec_}] := {If[validCirclePointsQ[cspec], cspec, Automatic], If[validTubePointsQ[tspec], tspec, Automatic]}


pTMPlotPoints[cspec_?validCirclePointsQ] := {cspec, Automatic}


pTMPlotPoints[_] = {Automatic, Automatic};


pTMCrossSection[func_] := func


pTMMaxRecursion[mxr_Integer] /; 0 <= mxr <= 15 := mxr
pTMMaxRecursion[_] := OptionValue[UTTube, MaxRecursion]


pTMCaps[{cf1_, cf2_}] := 
	With[{p1 = iPTMCaps[cf1]},
		With[{p2 = iPTMCaps[cf2]},
			{p1, p2} /; p2 =!= $Failed
		] /; p1 =!= $Failed
	]
pTMCaps[cf_] := 
	With[{res = iPTMCaps[cf]},
		{res, res} /; res =!= $Failed
	]
pTMCaps[_] := pTMCaps[OptionValue[UTTube, CapForm]]


iPTMCaps[cform:(None|Automatic|"Round"|"Butt")] := cform
iPTMCaps[cform:("RoundApprox"|"Cone")] := parseDistanceBasedCap[cform]
iPTMCaps[{cform:("Round"|"RoundApprox"|"Cone"), args___}] := parseDistanceBasedCap[cform, args]
iPTMCaps[_] = $Failed;


pTMClosed[bool:(True|False|Automatic)] := bool
pTMClosed[_] := OptionValue[UTTube, SplineClosed]


pTMKnots[spec:(Automatic|"Unclamped"|"Clamped")] := spec
pTMKnots[vec_] /; VectorQ[vec, NumericQ] && LessEqual @@ vec := vec
pTMKnots[_] = Automatic;


pTMWeights[vec_] /; VectorQ[vec, Positive] := vec
pTMWeights[_] = Automatic;


pTMReturnSolid[True] = BoundaryMeshRegion;
pTMReturnSolid[_] = MeshRegion;


pTMLineType[type:(Line|BezierCurve|BSplineCurve)] := type
pTMLineType[_] = Automatic;


pTMTorsion[x_?Internal`RealValuedNumericQ] := x
pTMTorsion[Scaled[x_?Internal`RealValuedNumericQ]] := Scaled[x]
pTMTorsion[_] = 0;


pTMPathOnly[val_] := TrueQ[val]


multiTubeQ[tspec_] := VectorQ[tspec, validTubeSpecQ]
multiTubeQ[___] = False;


validTubeSpecQ[pts_List] := MatrixQ[pts, Internal`RealValuedNumericQ] && 2 <= Dimensions[pts][[2]] <= 3
validTubeSpecQ[(Line|BezierCurve|BSplineCurve)[pts_, ___]] := validTubeSpecQ[pts]
validTubeSpecQ[___] = False;


(* ::Subsubsection::Closed:: *)
(*parseDistanceBasedCap*)


parseDistanceBasedCap[cform_] := parseDistanceBasedCap[cform, \[Infinity], Automatic]


parseDistanceBasedCap[cform_, r_] := parseDistanceBasedCap[cform, r, Automatic]


parseDistanceBasedCap[cform_, r_, h_] := iParseDistanceBasedCap[r, h, If[cform === "Cone", 1, 2]]


parseDistanceBasedCap[_, r_, h_, p_] := iParseDistanceBasedCap[r, h, p]


parseDistanceBasedCap[___] = $Failed;


iParseDistanceBasedCap[Scaled[r_ /; r >= 1], args___] := iParseDistanceBasedCap[\[Infinity], args]


iParseDistanceBasedCap[Automatic, args___] := iParseDistanceBasedCap[\[Infinity], args]


iParseDistanceBasedCap[r_, h_, p_] := 
	Which[
		TrueQ[r < 10^-9.] || TrueQ[Abs[h] < 10^-9.],
			"Butt",
		Head[r] === Scaled && TrueQ[First[r, 0] < 10^-9.],
			"Butt",
		Head[h] === Scaled && TrueQ[Abs[First[h, 0]] < 10^-9.],
			"Butt",
		True,
			{"RoundApprox", r, h, p}
	]


(* ::Subsubsection::Closed:: *)
(*parseCrossSection*)


parseCrossSection[Automatic, n_] := CirclePoints[{1., 0.}, N[n]]


parseCrossSection[pts_List, n_] /; MatrixQ[pts, NumericQ] && Last[Dimensions[pts]] === 2 := rescaleCrossSectionPoints[repairCrossSection[subSamplePoints[pts, n]]]


parseCrossSection[mr_?BoundaryMeshRegionQ, n_] /; RegionEmbeddingDimension[mr] === 2 := rescaleCrossSectionPoints[repairCrossSection[subSamplePoints[#, n]& /@ meshComponents[mr]]]


parseCrossSection[reg_?ConstantRegionQ, n_] /; RegionEmbeddingDimension[reg] === 2 := 
	Block[{len, mr},
		Quiet @ Switch[RegionDimension[reg],
			2,
				len = Perimeter[reg];
				mr = BoundaryDiscretizeRegion[reg, MaxCellMeasure -> {1 -> len/n}];,
			1,
				len = ArcLength[reg];
				mr = DiscretizeRegion[reg, MaxCellMeasure -> {1 -> len/n}];,
			_, 
				Return[$Failed]
		];
		
		If[Length[#] === 1, First[#], #]& @ rescaleCrossSectionPoints[repairCrossSection[meshComponents[mr]]]
	]


parseCrossSection[{func_, {a_, b_}}, n_] /; a < b && MatchQ[func[a], {_?NumericQ, _?NumericQ}] := rescaleCrossSectionPoints[repairCrossSection[func /@ Most[Subdivide[N[a], b, n]]]]


parseCrossSection[func_, n_] /; MatchQ[func[0.], {_?NumericQ, _?NumericQ}] := rescaleCrossSectionPoints[repairCrossSection[func /@ Most[Subdivide[0., 1., n]]]]


parseCrossSection[t_Text, n_] :=
	Block[{bmr},
		bmr = Quiet[BoundaryDiscretizeGraphics[t, _Text]];
		
		parseCrossSection[bmr, n] /; BoundaryMeshRegionQ[bmr]
	]


parseCrossSection[___] = $Failed;


meshComponents[mr_] := MeshCoordinates /@ UTSingularComponents[mr]


subSamplePoints[pts_?MatrixQ, n_] :=
	Block[{diffs, lens, len, \[Delta], samples},
		diffs = Subtract[pts, RotateLeft[pts]];
		lens = Sqrt[Total[diffs^2, {2}]];
		len = Total[lens];
		\[Delta] = len/n;
		
		samples = MapThread[Subdivide[#[[1]], #[[2]], Max[1, Round[#2/\[Delta]]]]&, {Partition[pts, 2, 1, 1], lens}];
		
		Developer`ToPackedArray[If[Length[#] > 2 && #[[1]] == #[[-1]], Most[#], #]& @ Split[Catenate[samples]][[All, 1]], Real]
	]


subSamplePoints[pts_List, n_] /; Length[pts] >= 1 :=
	With[{res = subSamplePoints[#, n]& /@ pts},
		If[Length[pts] === 1,
			First[res],
			res
		]
	]


rescaleCrossSectionPoints[pts_] := 
	Block[{ball, tfunc},
		ball = BoundingRegion[If[MatrixQ[pts], pts, Join @@ pts], "MinDisk"];
		(
			tfunc = ScalingTransform[{1, 1}/Last[ball]] @* TranslationTransform[-First[ball]];
			If[MatrixQ[pts], 
				tfunc[pts],
				tfunc /@ pts
			]
			
		) /; Head[ball] === Disk
	]


rescaleCrossSectionPoints[___] = $Failed;


repairCrossSection[m_?MatrixQ] := 
	With[{res = splitPolygon[m]},
		If[MatrixQ[res],
			If[orientedPolygonQ[res], res, Reverse[res]],
			If[orientedPolygonQ[#], #, Reverse[#]]& /@ res
		]
	]


repairCrossSection[{m_?MatrixQ}] := repairCrossSection[m]


repairCrossSection[expr_] := 
	Block[{reps, io},
		reps = Join @@ (With[{res = repairCrossSection[#]}, If[MatrixQ[#], {#}, #]]& /@ expr);
		io = classifyInnerOuterBoundary[reps];
		
		MapThread[If[#1 === 0, #2, Reverse[#2]]&, {io, reps}]
	]


splitPolygon[pts_] :=
	Block[{res},
		res = Region`Mesh`SimplePolygonSplit[Polygon[pts]];
		
		If[!MatchQ[res, {__Polygon}], 
			pts,
			If[Length[#] === 1, #[[1]], #]&[Join @@ res[[All, 1]]]
		]
	]


orientedPolygonQ[pts_] := Total[Subtract[RotateRight[#1], RotateLeft[#1]] * #2& @@ Transpose[pts]] > 0


classifyInnerOuterBoundary[{_}] = {0};


classifyInnerOuterBoundary[bds_] := 
	Block[{coords, cells, acc, bmr, nesting, ncoords, nlvls},
		coords = Join @@ bds;
		cells = Mod[Range[Length[#]+1], Length[#], 1]& /@ bds;
		
		acc = Accumulate[Length /@ bds];
		cells += Prepend[Most[acc], 0];
		
		bmr = UTLaxBlock[BoundaryMeshRegion[coords, Line[cells]]];
		nesting = bmr["BoundaryNesting"];
		
		ncoords = (Union @@ Join @@ MeshPrimitives[bmr, {1, Range[#1, #2]}, "Multicells" -> True][[All, 1]])& @@@ nesting;
		nlvls = Join @@ (MapThread[ConstantArray[#2[[3]], Length[#1]]&, {ncoords, nesting}]);
		ncoords = Join @@ ncoords;
		
		Mod[Developer`ToPackedArray[Nearest[ncoords -> nlvls, bds[[All, 1]]][[All, 1]]], 2]
	]


(* ::Subsection::Closed:: *)
(*sampleTubePoints*)


(* ::Subsubsection::Closed:: *)
(*Main*)


sampleTubePoints[tdata_] := 
	Module[{pts, radii, closedQ, deg, knots, weights, ltype, tpts, cpts, mxr, bfhead, bfopts, bf, rad, y, t, s, sol, abf, lookup, pvals, rvals},
		
		{pts, radii, closedQ, deg, knots, weights, ltype, tpts, cpts, mxr} = Lookup[tdata, 
			{"Points", "Radii", SplineClosed, SplineDegree, SplineKnots, SplineWeights, "LineType", "TubePoints", "CirclePoints", MaxRecursion}];
		
		If[Length[pts] == Length[radii] == 2, Return[linearTubePoints[pts, radii, tpts, cpts]]];
		
		If[ltype === BezierCurve, 
			bfhead = BezierFunction;
			bfopts = Sequence[];, 
			bfhead = BSplineFunction;
			bfopts = Sequence[SplineDegree -> deg, SplineClosed -> closedQ, SplineKnots -> knots, SplineWeights -> weights]
		];
		
		bf = bfhead[pts, bfopts];
		rad = bfhead[radii, bfopts];
		
		sol = NDSolveValue[{y'[t] == Sqrt[bf'[t] . bf'[t]], y[0] == 0}, y, {t, 0, 1}];
		
		tpts = determineTubePoints[tpts, sol[1], cpts, radii];
		
		With[{len = sol[1]},
			abf[l_?NumericQ] := With[{r = Quiet[s /. FindRoot[sol[s] == len*l, {s, l, 0, 1}]]},
				Sow[bf[r] -> rad[r]];
				bf[r]
			];
		];
		
		{pvals, lookup} = parametricSamplePlotter[abf, tpts, {0, 1}, mxr, Length[pts[[1]]]];
		rvals = Nearest[lookup, pvals][[All, 1]];
		
		{pvals, rvals}
	]


(* ::Subsubsection::Closed:: *)
(*parametricSamplePlotter*)


parametricSamplePlotter[f_, n_, {tmin_, tmax_}, mxr_, dim_] := {#1, Flatten[#2]}& @@ Reap[iParametricSamplePlotter[f, n, {tmin, tmax}, mxr, dim]]


iParametricSamplePlotter[f_, n_, {tmin_, tmax_}, 0, _] := f /@ Subdivide[N@tmin, tmax, n-1.]


iParametricSamplePlotter[f_, n_, {tmin_, tmax_}, mxr_, dim_] := 
	Module[{parametricplot, mesh, t, coords, edges},
		parametricplot = If[dim === 3, Visualization`Core`ParametricPlot3D, Visualization`Core`ParametricPlot];
		
		mesh = parametricplot[f[t], {t, 0, 1},
			PlotPoints -> n, MaxRecursion -> mxr,
			PerformanceGoal -> "Speed", "ReturnMeshObject" -> True,
			Method -> {"BoundaryOffset" -> False}
		];
		
		coords = mesh["Coordinates"];
		edges = FindShortestPath[UndirectedEdge @@@ mesh["Edges"], 1, n];
		
		coords[[edges]]
	]


(* ::Subsubsection::Closed:: *)
(*Utilities*)


linearTubePoints[{p1_, p2_}, {r1_, r2_}, tpts_, cpts_] :=
	With[{n = determineTubePoints[tpts, Norm[p2-p1], cpts, {r1, r2}]},
		{Subdivide[N@p1, p2, n-1], Subdivide[N@r1, r2, n-1]}
	]


determineTubePoints[tpts_Integer, ___] := Clip[tpts, {3, \[Infinity]}]


determineTubePoints[Scaled[s_], args__] := Clip[Round[s * determineTubePoints[Automatic, args]], {3, \[Infinity]}]


determineTubePoints[Automatic, len_, cpts_, radii_] :=
	With[{r = Median[N[radii]]},
		Clip[Round[len/(2r Sin[\[Pi]/cpts])], {3, \[Infinity]}]
	]


(* ::Subsection::Closed:: *)
(*bishopTube*)


(* ::Subsubsection::Closed:: *)
(*Main*)


(* ::Text:: *)
(*Code originally by Henrik Schumacher: https://mathematica.stackexchange.com/a/170134/4346*)


bishopTube[pts_List, radii_List, optvals_] /; MatrixQ[optvals["CrossSectionPoints"]] :=
 Module[{p, r, \[Nu], mm, dp, \[Tau], b, e, \[Phi], w, u, cp, captorsion, 
   v, \[Tau]0, e0, A, q, u0, v0, fflist, Alist, rcap1, rcap2, fanCap1Q = False, fanCap2Q = False,
   angles, \[Lambda], \[Omega], most\[Tau], rest\[Tau], cform, csec, closedQ, nn, torsion, scaledTorsionQ, tangle},
  {cform, closedQ, cp, torsion, scaledTorsionQ, csec, \[Nu]} = Lookup[optvals, {CapForm, SplineClosed, "CirclePoints", "Torsion", "ScaledTorsion", "CrossSection", "CrossSectionPoints"}];
  p = pts;
  r = radii;
  nn = Length[\[Nu]];
  
  If[closedQ, p = Join[{p[[-2]]}, p, {p[[2]]}]; r = Join[{r[[-2]]}, r, {r[[2]]}]];
  dp = Differences[p];
  \[Tau] = cNormalize3[dp];
  most\[Tau] = Most[\[Tau]];
  rest\[Tau] = Rest[\[Tau]];
  If[Length[\[Tau]] > 1,
   b = cNormalize3[cCross3[most\[Tau], rest\[Tau]]];
   e = cCross3[b, Most[\[Tau]]];
   \[Phi] = cTripleAngle3[most\[Tau], rest\[Tau], b]
   ,
   b = {}; e = {}; \[Phi] = {};
   ];
  w = NDSolve`FEM`MapThreadDot[
    rotationMatrix3DAngleVector[0.5 \[Phi], b], e/Cos[0.5 \[Phi]]];
  \[Tau]0 = \[Tau][[1]] ;
  u0 = N[IdentityMatrix[3]][[Ordering[Abs[\[Tau][[1]]], 1][[1]]]];
  u0 = Normalize[u0 - \[Tau]0 \[Tau]0 . u0]; v0 = Cross[\[Tau]0, u0];
  Alist = If[Length[\[Tau]] > 1, rotationMatrix3DAngleVector[\[Phi], b], {}];
  If[Length[Alist] >= 1,
   {u, v} = Compile[{{u0, _Real, 1}, {A, _Real, 3}},
      Block[{U = u0}, Join[{u0}, Table[U = A[[i]] . U, {i, 1, Length[A]}]]],
      RuntimeAttributes -> {Listable},
      Parallelization -> True
      ][{u0, v0}, Alist];
      
    tangle = -2.0\[Pi]*torsion;
    If[scaledTorsionQ,
       If[Length[dp] =!= 0, tangle /= Length[dp]];,
       tangle /= cp;
    ];
    If[tangle != 0.0,
      angles = tangle*Range[Length[dp]];
      {u, v} = {Cos[angles] u - Sin[angles] v, Sin[angles] u + Cos[angles] v};
    ];
      
   (* {u,v} is now a Bishop frame. If the curve is closed, 
   we have to twist it a bit in order to get a continuous frame. *)      
    If[closedQ,
    \[Lambda] = Sqrt[Dot[dp^2, ConstantArray[1., 3]]];
    \[Lambda] = 0.5 (Most[\[Lambda]] + Rest[\[Lambda]]);
    \[Omega] = cTripleAngle3[u[[1]], u[[-2]], \[Tau][[1]]];
    angles = \[Omega] Join[ConstantArray[0., 1], Accumulate[\[Lambda]]/Total[Most[\[Lambda]]]];
    {u, v} = {Cos[angles] u - Sin[angles] v, Sin[angles] u + Cos[angles] v};
    ];
    
   A = With[{Part = Compile`GetElement},
     Compile[{{u, _Real, 1}, {v, _Real, 1}, {w, _Real, 1}, {e, _Real, 1}, {b, _Real, 1}, {\[Phi], _Real}},
       If[\[Phi] < 1.`*^-12,
        {u, v},
        {u, v} . Table[e[[i]] w[[j]] + b[[i]] b[[j]], {i, 1, 3}, {j, 1, 3}] ],
       RuntimeAttributes -> {Listable},
       Parallelization -> True
       ][u[[1 ;; Length[\[Phi]]]], v[[1 ;; Length[\[Phi]]]], 
      w[[1 ;; Length[\[Phi]]]], e[[1 ;; Length[\[Phi]]]], 
      b[[1 ;; Length[\[Phi]]]], \[Phi]]
     ];
   ,
   {u, v} = Developer`ToPackedArray[{{u0, u0}, {v0, v0}}];
   A = {u, v};
   ];
  q = Flatten[Table[ConstantArray[p[[i + 1]], nn] - r[[i+1]]\[Nu] . A[[i]], {i, 1, Length[\[Phi]]}], 1];
  If[Length[p] == 2, q = {}; u = {u0}; v = {v0}];
  If[closedQ,
   mm = Length[\[Phi]] - 1;
   q = q[[1 ;; -1 - nn]];
   fflist = Join[
     getOpenTubeFaces[mm, nn, 0, 0],
     ReplaceAll[
      ReplaceAll[
       getOpenTubeFaces[2, nn, 0, 0],
       Dispatch[ Thread[Range[nn] -> Range[1 + (mm - 1) nn, nn + (mm - 1) nn]]]],
      Dispatch[Thread[Range[nn + 1, 2 nn] -> Range[nn]]]
      ]
     ],
     mm = Length[\[Phi]] + 2;
     q = Join[
       ConstantArray[p[[1]], nn] - radii[[1]]\[Nu] . {Cos[tangle] u[[1]] + Sin[tangle] v[[1]], -Sin[tangle] u[[1]] + Cos[tangle] v[[1]]}, 
       q, 
       ConstantArray[p[[-1]], nn] - radii[[-1]]\[Nu] . {u[[-1]], v[[-1]]}
     ];
     fanCap1Q = cform[[1]] === "Butt" && csec === Automatic;
     fanCap2Q = cform[[2]] === "Butt" && csec === Automatic;
     fflist = getOpenTubeFaces[mm, nn, Boole[fanCap1Q], Boole[fanCap2Q]]
   ];
   
   Which[
      torsion == 0.0,
         captorsion = 0.0,
      scaledTorsionQ && !closedQ,
         captorsion = Divide[torsion, Total[Sqrt[Total[dp^2, {2}]]]],
      True,
         captorsion = Divide[Length[dp]*torsion, cp]
   ];
   
   If[!closedQ && !fanCap1Q,
      rcap1 = makeCap[cform[[1]], q[[1 ;; nn]], \[Nu], Mod[Range[nn+1], nn, 1], radii[[1]], -1, Length[q], captorsion];,
      rcap1 = $emptyCap;
   ];
   If[!closedQ && !fanCap2Q,
      rcap2 = makeCap[cform[[2]], q[[-1 ;; -nn ;; -1]], Reverse[\[Nu]], Mod[Range[nn+1], nn, 1], radii[[-1]], 1, Length[q] + Length[rcap1["CapData"][[1]]], captorsion, rcap1];,
      rcap2 = $emptyCap;
   ];
   
   rcap1 = rcap1["CapData"];
   rcap2 = rcap2["CapData"];
   
   {Join[q, rcap1[[1]], rcap2[[1]]], Polygon[Join[fflist, rcap1[[2]], rcap2[[2]]]]}
   
  ];


bishopTube[pts_List, radii_List, optvals_] /; VectorQ[optvals["CrossSectionPoints"], MatrixQ] :=
	Block[{optvals2 = optvals, res, lens, cap1, cap2, coords, cells},
		optvals2[CapForm] = {None, None};
		res = Table[
			optvals2["CrossSectionPoints"] = p;
			bishopTube[pts, radii, optvals2],
			{p, optvals["CrossSectionPoints"]}
		];
		
		lens = Length /@ res[[All, 1]];
		
		{cap1, cap2} = multiBishopTubeCaps[res, pts, radii, optvals];
		
		coords = Join[Join @@ res[[All, 1]], cap1[[1]], cap2[[1]]];
		cells = Join[
			Join @@ (res[[All, 2, 1]] + Prepend[Accumulate[Most[lens]], 0]),
			cap1[[2]], cap2[[2]]
		];
		
		{
			coords,
			Polygon[cells]
		}
	]


bishopTube[___] = $Failed;


multiBishopTubeCaps[__, optvals_] /; optvals[SplineClosed] = {{{}, {}}, {{}, {}}};


multiBishopTubeCaps[tubes_, pts3d_, radii_, optvals_] :=
	Block[{cform, cp, pts, torsion, scaledTorsionQ, captorsion, segs, pts3d1, pts3d2, n, cap1, cap2},
		
		{cform, cp, pts, torsion, scaledTorsionQ} = Lookup[optvals, {CapForm, "CirclePoints", "CrossSectionPoints", "Torsion", "ScaledTorsion"}];
		segs = With[{l = Length[#]}, Mod[Range[l+1], l, 1]]& /@ pts;
		
		pts3d1 = Join @@ MapThread[#1[[1, 1 ;; Length[#2]]]&, {tubes, pts}];
		pts3d2 = Join @@ MapThread[#1[[1, -1 ;; -Length[#2] ;; -1]]&, {tubes, pts}];
		
		segs += Prepend[Most[Accumulate[Length /@ pts]], 0];
		n = Length[Join @@ tubes[[All, 1]]];
		
		Which[
			TrueQ[torsion == 0.0],
				captorsion = 0.0,
			scaledTorsionQ,
				captorsion = Divide[torsion, Total[Sqrt[Total[Differences[pts3d]^2, {2}]]]],
			True,
				captorsion = Divide[(Length[pts3d] - 1)*torsion, cp]
		];
		
		cap1 = makeCap[cform[[1]], pts3d1, Join @@ pts, segs, radii[[1]], -1, n, captorsion];
		cap2 = makeCap[cform[[2]], pts3d2, Join @@ Reverse[pts, {2}], segs, radii[[-1]], 1, n + Length[cap1["CapData"][[1]]], captorsion, cap1];
		
		{cap1["CapData"], cap2["CapData"]}
	]


multiBishopTubeCaps[___] = $Failed;


(* ::Subsubsection::Closed:: *)
(*compiled bishopTube helpers*)


Block[{u, uu, v, vv, w, ww, angle},
uu = Table[Compile`GetElement[u, i], {i, 1, 3}];
vv = Table[Compile`GetElement[v, i], {i, 1, 3}];
ww = Table[Compile`GetElement[w, i], {i, 1, 3}];

With[{code = Sqrt[Total[uu^2]], \[Epsilon] = 10^5 $MachineEpsilon},
	UTCompile[cNormalize3, {{u, _Real, 1}},
		Block[{l = code},
			If[l < \[Epsilon], u 0., Divide[u, l]]
		],
		RuntimeAttributes -> {Listable}
	]
];

With[{code = ArcTan[uu . vv, Det[{uu, vv, ww}]]},
	UTCompile[cTripleAngle3, {{u, _Real, 1}, {v, _Real, 1}, {w, _Real, 1}},
		code,
		RuntimeAttributes -> {Listable}
	]
];

With[{
	\[Epsilon] = 1. 10^-14,
	r2 = uu . uu,
	id = N[IdentityMatrix[3]],
	code = Uncompress["1:eJztWN1ugyAU1mKTXe8N+iRtsmy72UVT9wC1CyYkKEul2eO1j9JH2AssK0ijwdkmKEOMxwsiys85H3wfh7PYsU0aBkFQPIji5ZB\
		/cMLyFMkvkSjeSMEbtZmszUXxTjJc1NU1+8L7uu2aHv78LKuPonhi2SehePuK+TPFGc65MkJ2jCnjRLYk8lP51nOcmaVxUDXYr3hU7/9zVcFbtk\
		XlOEWjY1g1t2SEdZzSSHNmH6jnNMlNozpKmm0SSbKEkh+xjdS/apljkje2wBW187Jt+XXk0J29YzCo1QUwQ8qqC4ZT+02leZvz38cuq9PFGaCsomx\
		ta1fKtmviCgAGTbxBvWE0sTbXW03UqeRCClGL+VNlan8p1C8WIIwgjEZIQbDojzBCjGhTGPWmk8RV5VhuuGqQG/ERGRfmQxJmoHP13iq7PVcHPCNH\
		kJu0lFDpHLwA/RzeIkDt4BYB6ZXxpJxBISHlPFpNhAhwHBFgvywJBH5Dy1qDZ1PE1ccsiYtprQN5AaxjZoY="][angle, u]},
	UTCompile[rotationMatrix3DAngleVector, {{angle, _Real}, {u, _Real, 1}},
		If[Abs[angle] < \[Epsilon] || r2 < \[Epsilon], id, code],
		RuntimeAttributes -> {Listable}
	]
];
];

UTCompile[cCross3, {{X, _Real, 1}, {Y, _Real, 1}},
	{
		Subtract[Compile`GetElement[X, 2]*Compile`GetElement[Y, 3], Compile`GetElement[X, 3]*Compile`GetElement[Y, 2]],  
		Subtract[Compile`GetElement[X, 3]*Compile`GetElement[Y, 1], Compile`GetElement[X, 1]*Compile`GetElement[Y, 3]], 
		Subtract[Compile`GetElement[X, 1]*Compile`GetElement[Y, 2], Compile`GetElement[X, 2]*Compile`GetElement[Y, 1]]
	},
	RuntimeAttributes -> {Listable}
];

UTCompile[getOpenTubeFaces, {{mm, _Integer}, {nn, _Integer}, {cap1Q, _Integer}, {cap2Q, _Integer}},
	Module[{res},
		res = Join[Flatten[Join[
			Table[
				{{i + nn (j - 1), i + 1 + nn (j - 1), i + nn j}, {i + nn j, i + 1 + nn (j - 1), i + 1 + nn j}},
				{i, 1, nn - 1}, {j, 1, mm - 1}
			],
			{Table[{{nn + nn (j - 1), 1 + nn (j - 1), nn + nn j}, {nn + nn j, 1 + nn (j - 1), 1 + nn j}}, {j, 1, mm - 1}]}
		], 2]];
		
		If[cap1Q == 1, res = Join[res, Table[{1, i+1, i}, {i, 2, nn-1}]]];
		If[cap2Q == 1, res = Join[res, Table[{mm nn - nn+1, i, i+1}, {i, 1 + mm nn - nn+1, mm nn - 1}]]];

		res
	]
];


(* ::Subsection::Closed:: *)
(*Cap Utilities*)


(* ::Subsubsection::Closed:: *)
(*makeCap*)


$emptyCap = <|"CapData" -> {{}, {}}|>;


makeCap["RoundApprox", args___] := makeCap[{"RoundApprox", \[Infinity]}, args]


makeCap[{"RoundApprox", r_, h_}, args___] := makeCap[{"RoundApprox", r, h, 2}, args]


makeCap[{"RoundApprox", r_}, args___] := makeCap[{"RoundApprox", r, Automatic, 2}, args]


makeCap[{"RoundApprox", rr_, h_, p_}, pts3d_, pts_, segs_, r_, orientation_, offset_:0, t_:0, data_:<||>] := roundCapApprox[pts3d, pts, segs, r, orientation, rr, offset, t, h, p, data]


makeCap["Round", pts3d_, _, _, r_, _, offset_:0, t_:0, data_:<||>] := roundCap[pts3d, r, offset]


makeCap["Butt", pts3d_, pts_, segs_, r_, orientation_, offset_:0, t_:0, data_:<||>] := buttCap[pts3d, pts, segs, r, orientation, offset, data]


makeCap[___] = $emptyCap;


(* ::Subsubsection::Closed:: *)
(*buttCap*)


buttCap[args___] := 
	UTLaxBlock @ Quiet[Block[{res},
		res = Catch[iButtCap[args]];
		
		If[AssociationQ[res],
			res,
			<|"CapData" -> {{}, {}}|>
		]
	]]


iButtCap[pts3d_, pts_, segs_, r_, orientation_, offset_:0, data_:<||>] :=
	UTLaxBlock @ Block[{bmr, len, tm, tfunc, bd, npts, coords, cells},
		{bmr, tm} = constructButtCap2DData[pts, segs, data];
		
		tfunc = N[Chop[FindGeometricTransform[pts3d, Append[0.] /@ (r*pts), TransformationClass -> "Similarity"][[2]]]];
		
		coords = tfunc[r*MeshCoordinates[tm]];
		bd = Union @@ Region`InternalBoundaryEdges[tm][[All, 1]];
		npts = Nearest[pts3d, coords[[bd]]][[All, 1]];
		coords = ReplacePart[coords, Thread[bd -> npts]];
		
		cells = MeshCells[tm, 2, "Multicells" -> True][[1, 1]] + offset;
		If[TrueQ[orientation == -1],
			cells = Reverse[cells, {2}]
		];
		
		<|
			"CapData" -> {coords, cells},
			"Butt" -> <|"BMR" -> bmr, "Triangulation" -> tm|>
		|>
	]


constructButtCap2DData[pts_, segs_, data_] /; KeyExistsQ[data, "Butt"] :=
	Block[{bmr, tm},
		bmr = data["Butt"]["BMR"];
		tm = data["Butt"]["Triangulation"];
		
		{bmr, tm} /; BoundaryMeshRegionQ[bmr] && MeshRegionQ[tm]
	]


constructButtCap2DData[pts_, segs_, _] :=
	Block[{bmr, tm},
		bmr = BoundaryMeshRegion[pts, Line[segs]];
		If[!BoundaryMeshRegionQ[bmr], Throw[$Failed]];
		
		tm = UTCoordinateTransposeApply[
			iTriangulateMesh[bmr, MaxCellMeasure -> \[Infinity], MeshQualityGoal -> 0], 
			{#1, #2, ConstantArray[0., Length[#1]]}&
		];
		If[!MeshRegionQ[tm], Throw[$Failed]];
		
		{bmr, tm}
	]


(* ::Subsubsection::Closed:: *)
(*roundCap*)


roundCap[pts_, r_, offset_:0] :=
	Block[{len, n, k, seed, loop, cap, coords, bd, npts, cells},
		len = Length[pts];
		n = SelectFirst[{5, 6, 4, 3}, Mod[len, #] == 0&];
		k = Divide[len, n];
		
		seed = MeshRegion[
			Append[Transpose[Transpose[pts[[1 ;; -1 ;; k]]] - Mean[pts]], r Normalize[Cross[pts[[1]]-pts[[2]], pts[[3]]-pts[[2]]]]], 
			Table[Polygon[{Mod[i+1, n, 1], i, n+1}], {i, n}]
		];
		If[IntegerQ[Log2[k]],
			loop = UTLoopSubdivide[seed, Log2[k], "EdgeWeight" -> .45, "VertexTransform" -> (r*(Normalize /@ #)&)];
			cap = MeshRegion[r*(Normalize /@ MeshCoordinates[loop]), MeshCells[loop, 2]],
			cap = MeshRegion @@ Region`Mesh`SphereMeshData[seed, k, r, None]
		];
		coords = Transpose[Mean[pts] + Transpose[MeshCoordinates[cap]]];
		
		bd = Union @@ Region`InternalBoundaryEdges[cap][[All, 1]];
		npts = Nearest[pts, coords[[bd]]][[All, 1]];
		
		coords = ReplacePart[coords, Thread[bd -> npts]];
		
		cells = MeshCells[cap, 2, "Multicells" -> True][[1, 1]] + offset;
		
		<|"CapData" -> {coords, cells}|>
	]


(* ::Subsubsection::Closed:: *)
(*roundCapApprox*)


(* ::Text:: *)
(*Very experimental code. The idea is to triangulate the cap in 2D, and make the height proportional to the sqrt of the distance to the medial axis. This generalizes (exactly) a round hemispherical cap.*)


(* ::Text:: *)
(*TODO add ability to clip the height of the cap in a smooth way. This is currently done by manually setting rr in the roundCapApprox definition.*)


(* ::Text:: *)
(*Needs proper implementations of the following: *)
(**)
(** approximate medial axis (approxMedialAxis2D)*)
(*  * current code just naively uses edges from the Voronoi diagram*)
(*  * this will create hairs for noisy data and return *)
(*  * a heuristic is used to add small segments to isolated points in the approximate medial axis (degenerate medial axes like from CirclePoints)*)
(*  *)
(** triangulation (sqrtIsoTriangulateMesh)*)
(*  * current code offsets vertices of the cap boundary inward until they go intersect the medial axis*)
(*  * this (ideally) stops propagation at places where erosion folds in on itself*)
(*  * need to introduce a new propagation ray once two vertices become too far apart (from large concave angles)*)
(*  * calls Triangle library, which seems to give a natural looking triangulation -- tends to give nice wireframes that blend nicely with the tube itself*)
(*  * triangulation can give poor quality triangles though*)
(*  * propagation step size is proportional to the sqrt of the distance to the boundary, so that when height is added, it appears to be equi-spaced iso-lines*)
(*  * the hope is for crisp corners at the medial axis once height is added*)


roundCapApprox[args___] := 
	UTLaxBlock @ Quiet[Block[{res},
		res = Catch[iRoundCapApprox[args]];
		
		If[AssociationQ[res],
			res,
			<|"CapData" -> {{}, {}}|>
		]
	]]


iRoundCapApprox[pts3d_, pts_, segs_, r_, orientation_, frraw_:\[Infinity], offset_:0, torsion_:0, h_:Automatic, p_:2, data_:<||>] :=
	Block[{bmr, tm, tmdists, dists, mx, fac, hemisphere, dt, tfunc, bd, npts, coords, cells},
		{bmr, tm, tmdists} = constructRoundApprox2DData[pts, segs, normalizeTubeRadius[r, frraw], data];
		
		tfunc = N[Chop[FindGeometricTransform[pts3d, Append[0.] /@ (r*pts), TransformationClass -> "Similarity"][[2]]]];
		dists = Abs[r*tmdists];
		
		mx = Max[dists];
		fac = Which[
			h === Automatic, 1,
			Head[h] === Scaled, First[h, 1],
			NumericQ[h] && mx > 0, h/mx,
			True, 1
		];
		
		hemisphere = fac * If[p === 2, Sqrt[2mx*dists - dists^2], dists];
		dt = MeshRegion[roundCap3DPoints[tm, orientation * hemisphere, r, tfunc, torsion], MeshCells[tm, 2, "Multicells" -> True]];
		
		coords = MeshCoordinates[dt];
		bd = Union @@ Region`InternalBoundaryEdges[dt][[All, 1]];
		npts = Nearest[pts3d, coords[[bd]]][[All, 1]];
		coords = ReplacePart[coords, Thread[bd -> npts]];
		
		cells = MeshCells[dt, 2, "Multicells" -> True][[1, 1]] + offset;
		If[TrueQ[orientation == -1],
			cells = Reverse[cells, {2}]
		];
		
		<|
			"CapData" -> {coords, cells},
			"RoundApprox" -> <|"BMR" -> bmr, "Triangulation" -> tm, "TriangulationDistances" -> tmdists, "RadialCutoff" -> frraw|>
		|>
	]


normalizeTubeRadius[r_, rmax_?NumericQ] := rmax/r


normalizeTubeRadius[r_, rmax_] := rmax


constructRoundApprox2DData[pts_, segs_, r_, data_] /; KeyExistsQ[data, "RoundApprox"] && data["RoundApprox"]["RadialCutoff"] === r :=
	Block[{bmr, tm, tmdists},
		bmr = data["RoundApprox"]["BMR"];
		tm = data["RoundApprox"]["Triangulation"];
		tmdists = data["RoundApprox"]["TriangulationDistances"];
		
		{bmr, tm, tmdists} /; BoundaryMeshRegionQ[bmr] && MeshRegionQ[tm] && VectorQ[tmdists]
	]


constructRoundApprox2DData[pts_, segs_, rmax_, _] :=
	Block[{bmr, len, tm, fr, tmdists},
		bmr = BoundaryMeshRegion[pts, Line[segs]];
		If[!BoundaryMeshRegionQ[bmr], Throw[$Failed]];
		
		len = Median[PropertyValue[{bmr, 1}, MeshCellMeasure]];
		{tm, fr} = sqrtIsoTriangulateMesh[bmr, len, rmax];
		If[!MeshRegionQ[tm], Throw[$Failed]];
		
		tmdists = SignedRegionDistance[bmr][MeshCoordinates[tm]];
		
		{bmr, tm, Clip[tmdists, {-1.0fr, 0.0}]}
	]


roundCap3DPoints[tm_, zs_, r_, tfunc_, torsion_] := tfunc[applyCapTorsion[r*MeshCoordinates[tm], zs, 2\[Pi]*torsion]]


UTCompile[applyCapTorsion, {{xy, _Real, 1}, {z, _Real}, {torsion, _Real}},
	Block[{\[Theta], cos, sin, R},
		\[Theta] = torsion*z;
		cos = Cos[\[Theta]];
		sin = Sin[\[Theta]];
		
		R = {{cos, Minus[sin]}, {sin, cos}};
		
		Append[R . xy, z]
	],
	RuntimeAttributes -> {Listable}
];


approxMedialAxis2D[bmr_] :=
	Block[{sdf, bdpts, vor, vprims, inQraw, inQ, inpos, ma1, extpos, extpts, ndists, \[CurlyEpsilon], nf, endpts, endpts1, ma2, ma},
		sdf = SignedRegionDistance[bmr];
		bdpts = MeshCoordinates[bmr];
		
		vor = VoronoiMesh[bdpts];
		If[!RegionQ[vor], (* temp workaround in case VoronoiMesh fails... *)
			BlockRandom[
				SeedRandom[1];
				vor = VoronoiMesh[bdpts + RandomReal[10^-6Max[Abs[Subtract @@@ CoordinateBounds[bdpts]]]{-1, 1}, Dimensions[bdpts]]];
			];
		];
		
		vprims = MeshPrimitives[vor, 1, "Multicells" -> True][[1, 1]];
		
		inQraw = Partition[UnitStep[Minus[sdf[Flatten[vprims, 1]]]], 2];
		
		inQ = Min /@ inQraw;
		inpos = Pick[Range[Length[vprims]], inQ, 1];
		
		ma1 = Replace[UTSubMesh[vor, {1, inpos}], $Failed -> EmptyRegion[2], {0}];
		ma1 = pruneOutsideMedialEdges[ma1, bmr];
		
		extpos = Pick[Range[Length[inQraw]], Total[inQraw, {2}], 1];
		If[Length[extpos] === 0,
			Return[ma1]
		];
		
		extpts = Pick[Flatten[vprims[[extpos]], 1], Flatten[inQraw[[extpos]], 1], 1];
		
		If[MeshRegionQ[ma1], 
			extpts = Complement[extpts, MeshCoordinates[ma1]];
		];
		
		If[Length[extpts] === 0, 
			Return[ma1];
		];
		
		ndists = Nearest[bdpts -> "Distance", extpts][[All, 1]];
		\[CurlyEpsilon] = 10^-7.;
		
		nf = Nearest[bdpts];
		endpts = MapThread[nf[#1, {All, #2}]&, {extpts, ndists+\[CurlyEpsilon]}];
		endpts1 = Nearest[#, Mean[#]][[1]]& /@ endpts;
		
		With[{w=0.98},
			ma2 = UTJoin[MapThread[
				MeshRegion[Prepend[Table[w*#1 + (1-w)*p, {p, #2}], #1], Line[Thread[{1, Range[2, Length[#2]+1]}]]]&,
				{extpts, endpts}
			]];
		];
		
		UTJoin[ma1, ma2]
	]


pruneOutsideMedialEdges[ma_?MeshRegionQ, bmr_] :=
	Block[{ints, dists, todel},
		ints = Region`Mesh`FindSegmentIntersections[Join @@ (MeshPrimitives[#, 1]& /@ {bmr, ma}), "Ignore" -> {"EndPointsTouching"}][[1]];
		If[Length[ints] === 0,
			Return[ma]
		];
		
		dists = RegionDistance[ma][ints];
		ints = Pick[ints, Unitize[Threshold[dists]], 0];
		If[Length[ints] === 0,
			Return[ma]
		];
		
		todel = Union[Region`Mesh`MeshNearestCellIndex[ma, ints][[All, 2]]];
		
		UTSubMesh[ma, Complement[Range[MeshCellCount[ma, 1]], todel]]
	]


pruneOutsideMedialEdges[ma_, _] := ma


(* experimental and currently not used *)
smoothApproxMedialAxis2D[bmr_] := 
	Block[{\[Delta], ccs, n = 25, fds, smoothpts, bmr2, res, sdists, keep},
		\[Delta] = Perimeter[bmr]/130;
		ccs = MeshCoordinates /@ UTSingularComponents[DiscretizeRegion[RegionBoundary[bmr], MaxCellMeasure -> {1 -> \[Delta]}]];
		
		SeedRandom[1];
		smoothpts = Developer`ToPackedArray[#, Real]& /@ (fourierSmoothContour[# + RandomReal[.01\[Delta]{-1, 1}, Dimensions[#]], 40]& /@ ccs);
		
		bmr2 = UTToBMR[DiscretizeGraphics[Line[Append[#, First[#]]& /@ smoothpts]]];
		
		res = approxMedialAxis2D[bmr2];
		
		(* heuristic to make sure smoothed medial axis is contained in bmr *)
		sdists = Max /@ Partition[SignedRegionDistance[bmr][Join @@ MeshPrimitives[res, 1][[All, 1]]], 2];
		keep = Pick[Range[MeshCellCount[res, 1]], UnitStep[Minus[sdists]], 1];
		res = UTSubMesh[res, {1, keep}];
		
		res
	]


fourierSmoothContour[contour_, n_] :=
	Block[{z},
		z = Fourier[contour . {1.0, 1.0I}];
		z[[Ceiling[n/2] ;; -Ceiling[n/2]]] = 0.;
		ReIm[InverseFourier[z]]
	]


sqrtIsoTriangulateMesh[bmr_, \[Delta]_, rraw_:\[Infinity]] :=
	Block[{sdf, coords, medial, medialprims, medial2, r, normals, d, rays, rayints, nears, epos, marchtos, marchtods, mx, pts, 
			segs, inst, outInst, meshCoords, meshElements, tri, cents, dists},
		sdf = SignedRegionDistance[bmr];
		coords = MeshCoordinates[bmr];
		
		medial = approxMedialAxis2D[bmr];
		medialprims = MeshPrimitives[medial, 1];
		
		r = Replace[rraw, Scaled[f_] :> (f*Max[Abs[sdf[MeshCoordinates[medial]]]]), {0}];

		normals = Minus[UTMeshCellNormals[bmr, 0, "Normalize" -> True]];
		d = 2.01Last[BoundingRegion[bmr, "MinBall"]];
		rays = Transpose[{coords, coords + d*normals}];
		rayints = With[{int = Intersection[MeshCoordinates[medial], #]},
			If[Length[int] === 1,
				int,
				Region`Mesh`FindSegmentIntersections[Append[medialprims, Line[#]], "Ignore" -> {"EndPointsTouching"}][[1]]
			]
		]& /@ rays;
		
		nears = RegionNearest[medial][coords];
		epos = Flatten[Position[rayints, {}, {1}, Heads -> False]];
		If[epos=!={}, 
			rayints[[epos]] = Partition[nears[[epos]], 1];
			normals[[epos]] = Normalize /@ (rayints[[epos,1]] - coords[[epos]]);
		];
		
		marchtos = Developer`ToPackedArray[MapThread[First@*Nearest, {rayints, coords}]];
		marchtods = Norm /@ (coords - marchtos);
		
		medial2 = deleteFarSegs[insertMedialSegPoints[medial, marchtos], sdf, r];
		If[MeshRegionQ[medial2],
			medial2 = Quiet[DiscretizeRegion[medial2, MaxCellMeasure -> {1 -> 1.5\[Delta]}]];
		];
		
		Do[
			If[Norm[coords[[i]] - nears[[i]]] < 0.5marchtods[[i]] && VectorAngle[nears[[i]] - coords[[i]], rayints[[i,1]] - coords[[i]]] < 10\[Degree],
				rayints[[i]] = {nears[[i]]};
				normals[[i]] = Normalize[rayints[[i,1]] - coords[[i]]];
				marchtos[[i]] = nears[[i]];
				marchtods[[i]] = Norm[coords[[i]] - marchtos[[i]]];
			],
			{i, Length[marchtods]}
		];

		pts = DeleteDuplicates[Join @@ MapThread[
			Table[#1 + d*#2, {d, makeSqrtIsoMarch[0.04\[Delta], #3]}]&,
			{coords, normals, Min[#, r]& /@ marchtods}
		]];
		pts = DeleteDuplicates[Developer`ToPackedArray[Mean /@ Nearest[pts, pts, {All, 10^-6}]]];
		pts = Pick[pts, UnitStep[-SignedRegionDistance[bmr][pts]], 1];
		
		If[MeshRegionQ[medial2],
			pts = Developer`ToPackedArray[MapThread[If[Norm[#1-#2] <= 10^-6., #1, #2]&, {Nearest[MeshCoordinates[medial2], pts][[All, 1]], pts}]];
			pts = DeleteDuplicates @ Join[MeshCoordinates[medial2], pts];
		];
		
		pts = DeleteDuplicates @ Join[coords, pts];
		
		segs = Join[
			MeshCells[bmr, 1][[All, 1]],
			If[MeshRegionQ[medial2], MeshCells[medial2, 1][[All, 1]] + MeshCellCount[bmr, 0], {}]
		];
		
		Needs["TriangleLink`"];
		inst = TriangleLink`TriangleCreate[];
		TriangleLink`TriangleSetPoints[inst, pts];
		TriangleLink`TriangleSetSegments[inst, segs];

		outInst = TriangleLink`TriangleTriangulate[inst, "pq0.1YY"];

		meshCoords = TriangleLink`TriangleGetPoints[outInst];
		meshElements = TriangleLink`TriangleGetElements[outInst];

		tri = MeshRegion[meshCoords, Polygon[meshElements]];

		cents = AnnotationValue[{tri, 2}, MeshCellCentroid];
		dists = sdf[cents];

		{UTSubMesh[tri, Pick[Range[Length[cents]], UnitStep[Minus[dists]], 1]], r}

	]


deleteFarSegs[mr_, _, \[Infinity]] := mr


deleteFarSegs[mr_, sdf_, d_] := 
	Block[{dists, keep0, keep1, C01},
		dists = sdf[MeshCoordinates[mr]];
		keep0 = Pick[Range[MeshCellCount[mr, 0]], UnitStep[dists + d], 1];
		
		If[Length[keep0] === 0, Return[EmptyRegion[2]]];
		
		C01 = mr["ConnectivityMatrix"[0, 1]][[keep0]];
		
		keep1 = Developer`ToPackedArray[Union @@ Values[KeyDrop[PositionIndex[Counts[C01["NonzeroPositions"][[All, 2]]]], 1]]];
		
		UTSubMesh[mr, {1, keep1}]
	]


makeSqrtIsoMarch[\[Delta]_, d_] :=
	Block[{dd = Max[d, \[Delta]], rng, next},
		rng = Range[Sqrt[\[Delta]], Sqrt[dd], Sqrt[\[Delta]]]^2;
		next = (Floor[Sqrt[dd]/Sqrt[\[Delta]]] + 1)^2 * \[Delta];
		
		If[Abs[Last[rng] - dd] < Abs[next - dd],
			Append[Most[rng], dd],
			Append[rng, dd]
		]
	]


insertMedialSegPoints[medial_, pts_] :=
	Block[{ninds, prims, newsegs, newmedial},
		ninds = Region`Mesh`MeshNearestCellIndex[medial, pts][[All, 2]];
		prims = MeshPrimitives[medial, 1, "Multicells" -> True][[1, 1]];
		newsegs = KeyValueMap[intersectMedialSeg[prims[[#1]], pts[[#2]]]&, PositionIndex[ninds]];
		
		newmedial = UTJoin[
			MeshRegion[
				Join @@ newsegs, 
				Line /@ Internal`PartitionRagged[Range[Length[Join @@ newsegs]], Length /@ newsegs]
			],
			UTSubMesh[medial, {1, Complement[Range[MeshCellCount[medial, 1]], Union[ninds]]}]
		];
		
		UTCoordinateApply[newmedial, Mean /@ Nearest[#, #, {All, 10^-7}]&]
	]

intersectMedialSeg[prim_, pts_] := With[{p = prim[[1]]}, DeleteDuplicates[SortBy[Join[prim, pts], Norm[#-p]&]]]


(* ::Subsection::Closed:: *)
(*Tests*)


(* ::Input:: *)
(*UTTube[{{0,0,0},{1,1,1},{1,0,0},{2,-1,3}},{.1,.15,.1,.025},SplineDegree->3,SplineClosed->True,MeshCellStyle->{1->Black},ViewPoint->{0.9628336337625536`,-0.44824748129566566`,3.212790934562609`},ViewVertical->{0.4147937848120057`,0.7099652271836815`,0.5691181707443149`}]*)


(* ::Input:: *)
(*UTTube[{{0,0,0},{1,1,1},{1,0,0},{2,-1,3}},{.1,.15,.1,.025},MaxRecursion->1,PlotPoints->{Automatic,200},SplineDegree->3,SplineClosed->True,MeshCellStyle->{1->Black},ViewPoint->{0.9628336337625536`,-0.44824748129566566`,3.212790934562609`},ViewVertical->{0.4147937848120057`,0.7099652271836815`,0.5691181707443149`},"ReturnSolid"->True]*)


(* ::Input:: *)
(*UTTube[Table[{Cos[u],Sin[u],Sqrt[u]+Sin[5u]/5},{u,0.,4\[Pi],\[Pi]/16}],.1,SplineDegree->3,MaxRecursion->3,"ReturnSolid"->True,MeshCellStyle->{1->Black}]*)


(* ::Input:: *)
(*UTTube[{{0,0,0},{1,0,0},4{1,1,1},2{1,0,0}},{1,1,1,.03},SplineDegree->3,PlotPoints->{50,50},MaxRecursion->3,"ReturnSolid"->False]*)


(* ::Input:: *)
(*UTTube[Table[{Sin[t]+2 Sin[2 t],Cos[t]-2 Cos[2 t],-Sin[3 t]},{t,0.,2\[Pi],\[Pi]/10}],Table[.15+.15t,{t,0.,2\[Pi],\[Pi]/10}],SplineDegree->3,(*MeshCellStyle\[Rule]{1\[Rule]Black},*)MaxRecursion->0,SplineClosed->True,"ReturnSolid"->False,PlotPoints->{Automatic,32}]//FindMeshDefects*)


(* ::Input:: *)
(*UTTube[Table[{Sin[t]+2 Sin[2 t],Cos[t]-2 Cos[2 t],-Sin[3 t]},{t,0.,2\[Pi],\[Pi]/10}],Table[.1+.1t,{t,0.,2\[Pi],\[Pi]/10}],SplineDegree->3,MeshCellStyle->{1->Black},MaxRecursion->0,SplineClosed->Automatic,"ReturnSolid"->False,PlotPoints->{Automatic,32}]*)


(* ::Input:: *)
(*UTTube[First@HilbertCurve[3,3],.1,SplineDegree->2,MeshCellStyle->{1->Black},PlotPoints->{Automatic,8}]*)


(* ::Input:: *)
(*UTTube[Table[Entity["SpaceCurve","Helix"]["NaturalParametricEquations"][2,1][s],{s,0.,30,3}],1,SplineDegree->3,MeshCellStyle->{1->Black},MaxRecursion->0,SplineClosed->True,"ReturnSolid"->True,PlotPoints->Scaled[1.5]]*)


(* ::Section:: *)
(*UTPathPoints*)


(* ::Subsection::Closed:: *)
(*Main*)


Options[UTPathPoints] = DeleteDuplicates @ Join[
	{MaxRecursion -> 0, PlotPoints -> Automatic},
	{SplineDegree -> 1},
	Options[BSplineCurve]
];


UTPathPoints[pts_List, n:(Automatic|_Integer?Positive), opts:OptionsPattern[]] := UTPathPoints[pts, PlotPoints -> n, opts]


UTPathPoints[pts_List, opts:OptionsPattern[]] /; MatrixQ[pts, NumericQ] && 2 <= Dimensions[pts][[2]] <= 3 := 
	Block[{tdata = tubeMeshData[pts, "PathOnly" -> True, opts], res},
		(
			res = sampleTubePoints[tdata];

			res[[1]] /; res =!= $Failed
			
		) /; AssociationQ[tdata]
	]


UTPathPoints[___] = $Failed;


(* ::Section:: *)
(*UTTorus*)


(* ::Subsection::Closed:: *)
(*Main*)


Options[UTTorus] = Options[UTTube];
SetOptions[UTTorus, {SplineDegree -> 3, SplineClosed -> True}];


UTTorus[opts:OptionsPattern[]] := UTTorus[$defaultTorusCenter, $defaultTorusInnerRadius, $defaultTorusOuterRadius, $defaultTorusNormal, opts]


UTTorus[p_List, opts:OptionsPattern[]] := UTTorus[p, $defaultTorusInnerRadius, $defaultTorusOuterRadius, $defaultTorusNormal, opts]


UTTorus[p_, r_, opts:OptionsPattern[]] /; ListQ[r] || NumericQ[r] := UTTorus[p, r, $defaultTorusOuterRadius, $defaultTorusNormal, opts]


UTTorus[p_, r_, R_, opts:OptionsPattern[]] /; ListQ[R] || NumericQ[R] := UTTorus[p, r, R, $defaultTorusNormal, opts]


UTTorus[p_, r_, R_, n_List, opts:OptionsPattern[]] /; validTorusQ[p, r, R, n] :=
	Block[{cnt, deg, closedQ, trans, rs, Rs, pts},
		cnt = Max[96, Length[r], Length[R]]+1;
		deg = OptionValue[SplineDegree];
		closedQ = OptionValue[SplineClosed];
		trans = Quiet[TranslationTransform[p] @* RotationTransform[{{0, 0, 1}, n}], RotationTransform::spln];
		
		rs = scalarSample[r, cnt, deg, closedQ];
		Rs = scalarSample[R, cnt, deg, closedQ];
		pts = trans[Rs * PadRight[CirclePoints[N[cnt]], {cnt, 3}, 0.]];
		
		UTTube[pts, rs, opts, SplineDegree -> 3, SplineClosed -> True]
	]


UTTorus[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Utilities*)


$defaultTorusCenter = {0, 0, 0};
$defaultTorusInnerRadius = 0.5;
$defaultTorusOuterRadius = 1;
$defaultTorusNormal = {0, 0, 1};


validTorusQ[p_, r_, R_, n_] := 
	And[
		ListQ[p] && Length[p] == 3 && VectorQ[p, Internal`RealValuedNumericQ],
		TrueQ[r > 0] || VectorQ[r, Positive],
		TrueQ[R > 0] || VectorQ[R, Positive],
		ListQ[n] && Length[n] == 3 && VectorQ[n, Internal`RealValuedNumericQ] && Total[Abs[n]] > 0
	]


scalarSample[x_?NumericQ, cnt_, ___] := ConstantArray[x, cnt]


scalarSample[{x_?NumericQ}, cnt_, ___] := ConstantArray[x, cnt]


scalarSample[vals_List, cnt_, deg_, closedQ_] := 
	Block[{data, n},
		data = If[TrueQ[closedQ] && vals[[1]] != vals[[-1]], PadRight[vals, Length[vals]+1, "Periodic"], vals];
		n = Min[9, Length[data], deg];
		
		ArrayResample[data, {cnt}, Resampling -> {"Spline", n}]
	]


(* ::Section:: *)
(*UTRevolutionMesh*)


(* ::Subsection::Closed:: *)
(*Main*)


(* ::Text:: *)
(*TODO: allow 3D points and arbitrary axis of revolution.*)


Options[UTRevolutionMesh] = {CapForm -> None, "Torsion" -> 0};


UTRevolutionMesh[pts_, n_:64, opts:OptionsPattern[]] /; MatrixQ[pts, NumericQ] && Dimensions[pts][[2]] == 2 && IntegerQ[n] && n > 2 :=
	Block[{c, t, oassoc, rpts, cells},
	
		c = pTMCaps[OptionValue[CapForm]];
		t = pTMTorsion[OptionValue["Torsion"]];
		
		oassoc = <|CapForm -> c, SplineClosed -> False, "CirclePoints" -> n, "Torsion" -> t, "CrossSection" -> ({Cos[2.\[Pi]*#], Sin[2.\[Pi]*#]}&)|>;
		
		{rpts, cells} = bishopTube[ArrayPad[pts[[All, 1;;1]], {{0, 0}, {1, 1}}, 0.], pts[[All, 2]], oassoc];
		
		FastMeshRegion[rpts, cells]
	]


UTRevolutionMesh[___] = $Failed;


(* ::Section:: *)
(*UTSamplePoints*)


(* ::Subsection::Closed:: *)
(*Main*)


SetAttributes[UTSamplePoints, HoldAll];


UTSamplePoints[fraw_, n_, {tmin_, tmax_}] := 
	Module[{f, tmid, val, d, y, t, s, sol, abf, vals},
		
		f = fraw[t];
		tmid = Mean[{tmin, tmax}];
		val = Quiet[f /. t -> tmid];
		
		If[Internal`RealValuedNumericQ[val],
			f = {t, f};
			val = {tmid, val};
		];
		If[!TrueQ[VectorQ[val, Internal`RealValuedNumericQ]], Return[$Failed]];
		
		d = D[f, t];
		Quiet @ If[!TrueQ[VectorQ[d /. t -> tmid, Internal`RealValuedNumericQ]], 
			d = centralDifferenceQuotient[f, {t, 10^-6}];
		];
		
		sol = NDSolveValue[{y'[t] == Sqrt[d . d], y[tmin] == 0}, y, {t, tmin, tmax}];
		
		With[{len = sol[tmax]},
			abf[0|0.] = N[tmin];
			abf[1|1.] = N[tmax];
			abf[l_?NumericQ] := Quiet[s /. FindRoot[sol[s] == len*l, {s, {1-l, l} . {tmin, tmax}, tmin, tmax}]];
		];
		
		vals = Table[f /. t -> abf[s], {s, Subdivide[0., 1, n-1]}];
		If[!MatrixQ[vals, Internal`RealValuedNumericQ], Return[$Failed]];
		
		vals
	]


UTSamplePoints[f_, n_, {t_, tmin_, tmax_}] := 
	Module[{x}, 
		UTSamplePoints[Function[#, f]&[x] /. Unevaluated[t] -> x, n, {tmin, tmax}]
	]


(* ::Subsection::Closed:: *)
(*Utilities*)


centralDifferenceQuotient[f_, {t_, h_}] := Together[((f /. t -> t+h) - (f /. t -> t-h))/(2h)]


(* ::Section:: *)
(*UTRoundedCuboidMesh*)


(* ::Subsection::Closed:: *)
(*Main*)


(* ::Text:: *)
(*TODO re-implement with prebuilt data and MaxCellMeasure option.*)


UTRoundedCuboidMesh[pmin_List, pmax_List, r_?Positive] /; Length[pmin] == 3 && MatrixQ[{pmin, pmax}, Internal`RealValuedNumericQ] && Min[Abs[pmax - pmin]] >= 2r :=
	Block[{octmr, ball, seamv, orthants, C20, coords, conv},
		octmr = RegionBoundary[BoundaryDiscretizeGraphics[Octahedron[Sqrt[2]]]];
		ball = Nest[sphericalProjectionMesh[UTSubdivide[#], r]&, octmr, 4];
		
		seamv = Random`Private`PositionsOf[Chop[Min /@ Abs[MeshCoordinates[ball]]], 0];
		ball = Nest[sphericalProjectionMesh[Region`Mesh`RelaxMesh[#, "NoisyMesh" -> {True, "DistortionTolerance" -> 0.}, "RestrictedVertices" -> seamv], r]&, ball, 2];
		
		orthants = PositionIndex[1 + UnitStep[PropertyValue[{ball, 2}, MeshCellCentroid]] . {4, 2, 1}];
		C20 = ball["ConnectivityMatrix"[2, 0]];
		
		coords = Join @@ Table[
			With[{nzp = Union[C20[[orthants[o]]]["NonzeroPositions"][[All, 2]]]}, 
				orthoTranslate[MeshCoordinates[ball][[nzp]], o, pmin, pmax, r]
			],
			{o, 8}
		];
		conv = RegionBoundary[ConvexHullMesh[coords]];
		
		Region`Mesh`TriangulateMeshCells[Region`Mesh`MergeCells[conv], MeshQualityGoal -> 1]
	]


UTRoundedCuboidMesh[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Utilities*)


sphericalProjectionMesh[mr_, r_] := FastMeshRegion[r*Chop[Normalize /@ MeshCoordinates[mr]], MeshCells[mr, 2, "Multicells" -> True]]


orthoTranslate[coords_, o_, pmin_, pmax_, r_] := TranslationTransform[pmin + r + oTransLookup[o](pmax - pmin - 2r)][coords]


oTransLookup[1] = {0, 0, 0};
oTransLookup[2] = {0, 0, 1};
oTransLookup[3] = {0, 1, 0};
oTransLookup[4] = {0, 1, 1};
oTransLookup[5] = {1, 0, 0};
oTransLookup[6] = {1, 0, 1};
oTransLookup[7] = {1, 1, 0};
oTransLookup[8] = {1, 1, 1};


(* ::Section:: *)
(*UTSplineWireframe*)


(* ::Subsection::Closed:: *)
(*Main*)


Options[UTSplineWireframe] = Options[UTPathPoints];


UTSplineWireframe[mr_?UTLineMesh3DQ, \[Delta]_, opts:OptionsPattern[]] :=
	Block[{n},
		UTJoin @ Table[
			n = Max[Round[Divide[Region`Mesh`MeshMeasure[seg], \[Delta]]], 2];
			FastMeshRegion[
				UTPathPoints[MeshCoordinates[seg], n, opts],
				Line[Range[n]]
			],
			{seg, UTSingularComponents[mr]}
		]
	]


(* ::Section:: *)
(*Lattice*)


(* ::Subsection::Closed:: *)
(*UTBodyCenteredCubicLattice*)


(* ::Subsubsection::Closed:: *)
(*Main*)


UTBodyCenteredCubicLattice[expr_] := UTBodyCenteredCubicLattice[expr, 1]


UTBodyCenteredCubicLattice[bds_List, scale_?Internal`RealValuedNumericQ] /; MatrixQ[bds, Internal`RealValuedNumericQ] :=
	Join @@ (Flatten[#, 2]& /@ separatedBodyCenteredCubicLattices[bds, scale])


UTBodyCenteredCubicLattice[bmr_BoundaryMeshRegion?UTTriangleMesh3DQ, scale_?Internal`RealValuedNumericQ] :=
	Block[{lattices, ldims, lbds, members},
		lattices = separatedBodyCenteredCubicLattices[RegionBounds[bmr], scale];
		ldims = Most[Dimensions[#]]-1& /@ lattices;
		lbds = CoordinateBounds[#, scale*Sqrt[2]/6]& /@ lattices;
		
		members = MapThread[Region`Mesh`GridMeshMember[bmr, ##]&, {ldims, lbds}];
		
		Join @@ MapThread[Pick[##, 1]&, {Flatten[Reverse[Transpose[#, {3, 2, 1}], {1, 2}], 2]& /@ lattices, Flatten /@ members}]
	]


UTBodyCenteredCubicLattice[___] = $Failed;


(* ::Subsubsection::Closed:: *)
(*Argument conform & completion*)


registerForDiscretization[UTBodyCenteredCubicLattice, 1];
registerForSolid[UTBodyCenteredCubicLattice, 1];


(* ::Subsubsection::Closed:: *)
(*Utilities*)


separatedBodyCenteredCubicLattices[bds_, scale_] :=
	Map[
		CoordinateBoundsArray[#, N[2Sqrt[2]/3scale]]&,
		Table[bds + scale*s*Sqrt[2]/3{{1, 0}, {1, 0}, {1, 0}}, {s, 2}]
	]
