(* ::Package:: *)

(* ::Title:: *)
(*Boolean*)


(* ::Section:: *)
(*Begin*)


Package["UTCG`"]


PackageExport["UTUnion"]
PackageExport["UTIntersection"]
PackageExport["UTDifference"]
PackageExport["UTSymmetricDifference"]
PackageExport["UTComplement"]
PackageExport["UTBooleanComponents"]


UTUnion::usage = "Union a collection of regions.";
UTIntersection::usage = "Intersect a collection of regions.";
UTDifference::usage = "Subtract a region from another.";
UTSymmetricDifference::usage = "Take the symmetric difference between two regions.";
UTComplement::usage = "Find the complement of a region.";
UTBooleanComponents::usage = "Cut and partition a group of meshes based upon cycles intersections.";


(* ::Section:: *)
(*Utilities*)


(* ::Subsection::Closed:: *)
(*PackageScopes*)


PackageScope["intersectedSurfaceJoin"]


(* ::Subsection::Closed:: *)
(*$BooleanOperationOptions*)


$BooleanOperationOptions = {"ReturnSolid" -> True, "DuplicateTolerance" -> 4, "MergeTolerance" -> 10^-8., WorkingPrecision -> MachinePrecision};


(* ::Subsection::Closed:: *)
(*validBooleanInputsQ*)


validBooleanInputsQ[bmrs_List] := AllTrue[bmrs, validBooleanInputsQ]


validBooleanInputsQ[bmr_BoundaryMeshRegion] := UTTriangleMesh3DQ[bmr]


validBooleanInputsQ[FullRegion[3]] = True;
validBooleanInputsQ[EmptyRegion[3]] = True;


validBooleanInputsQ[___] = False;


(* ::Subsection::Closed:: *)
(*validBooleanMeshInputQ*)


validBooleanMeshInputQ[bmr_] := BoundaryMeshRegionQ[bmr] && UTTriangleMesh3DQ[bmr]


validBooleanMeshInputQ[___] = False;


(* ::Subsection::Closed:: *)
(*candidateFace*)


candidateFace[mr_, comp_] := comp[[Ordering[PropertyValue[{mr, {2, comp}}, MeshCellMeasure], -1][[1]]]]


(* ::Subsection::Closed:: *)
(*intersectedSurfaceJoin*)


Options[intersectedSurfaceJoin] = Join[$BooleanOperationOptions, {"PruneHangingFaces" -> False}];


If[TrueQ[Quiet[Needs["TriangleLink`"]]; Region`Mesh`RepresentationAvailableQ["TriangleLinkMesh"]],
intersectedSurfaceJoin[bmrs_List, OptionsPattern[]] :=
	slimTriangleLinkBlock @ Block[{Region`Mesh`GetTrianglesFromSegments2d, wp, \[Delta], \[Mu], union, coords, cells, res},
		Region`Mesh`GetTrianglesFromSegments2d = Block[{on,n,m,d,XX,Xi,Xp=#1,pts,tinst1,tinst2,triangles,split,isegs,segs,si,temp,sindex},If[Length[#2]<=2,Return[{{},{}},Block]];If[Length[#2]==3,If[Length[#1]==0,Return[{{},{#2[[All,1]]}},Block]];If[Length[#1]==3,Return[{{},{Range[3]}},Block]];];{on,d}=Dimensions[Xp];{Xp,Xi}=Region`Mesh`DeleteDuplicateCoordinates[Xp,CoordinateBounds[Xp]];XX=Xp;If[Xi===Identity,Xi=Range[Length[Xp]]];isegs=Region`Mesh`ReplaceIncidents[#2,Xi];n=Length[Xp];segs=Region`Mesh`ToCoordinates[isegs,Xp];If[on===n&&Region`Mesh`IntersectionFreeSegmentsQ[segs,Method->Which[Length[segs]<30,"BruteForce",Min[Abs[Subtract@@Transpose[segs]]]<1.0*^-14,"OverlappingBounds",True,Automatic]],tinst1=TriangleLink`TriangleCreate[];TriangleLink`TriangleSetPoints[tinst1,Xp];TriangleLink`TriangleSetSegments[tinst1,isegs];tinst2=TriangleLink`TriangleTriangulate[tinst1,"pYYQ"];TriangleLink`TriangleDelete[tinst1];If[TrueQ[TriangleLink`TriangleExpressionQ[tinst2]],triangles=TriangleLink`TriangleGetElements[tinst2];TriangleLink`TriangleDelete[tinst2];If[Max[triangles]===n,Return[{{},triangles},Block]];];];pts=Complement[Xp,Flatten[segs,1]];If[Length[pts]>0,segs=Join[segs,Transpose[{pts,pts}]];temp=Flatten[(Position[Xp,#1]&)/@pts];];split=Region`Mesh`SplitIntersectingSegments[segs,Method->"OverlappingBounds"];If[Length[split]!=2,Return[$Failed,Block]];{Xp,si}=split;si=(Partition[#1,2,1]&)/@si;si=Join@@si;segs=Region`Mesh`ToCoordinates[si,Xp];sindex=Region`Mesh`FindMinimalIntersectingSegments[segs];If[!VectorQ[sindex,NumericQ],sindex={}];si=Delete[si,Partition[sindex,1]];tinst1=TriangleLink`TriangleCreate[];If[!TrueQ[TriangleLink`TriangleExpressionQ[tinst1]],Return[$Failed,Block]];TriangleLink`TriangleSetPoints[tinst1,Xp];TriangleLink`TriangleSetSegments[tinst1,si];tinst2=TriangleLink`TriangleTriangulate[tinst1,"pYYQ"];If[!TrueQ[TriangleLink`TriangleExpressionQ[tinst2]],TriangleLink`TriangleDelete[tinst1];Return[$Failed,Block]];Xp=TriangleLink`TriangleGetPoints[tinst2];triangles=TriangleLink`TriangleGetElements[tinst2];TriangleLink`TriangleDelete[tinst1];TriangleLink`TriangleDelete[tinst2];If[Length[Xp]<n,Return[$Failed,Block]];m=on-n;temp=(If[#1>n,#1+m,#1]&)/@Flatten[triangles];Xi=Nearest[Xp->Automatic,XX[[Xi]]];Xi=Flatten[Map[First,Xi,1]];temp=temp/. AssociationThread[Xi->Range[on]];triangles=Partition[temp,3];Xp=Drop[Xp,n];{Xp,triangles}]&;
		wp = Replace[OptionValue[WorkingPrecision], Except[_?Positive] -> MachinePrecision, {0}];
		\[Delta] = OptionValue["DuplicateTolerance"];
		\[Mu] = OptionValue["MergeTolerance"];
		
		If[Length[bmrs] == 1,
			union = First[bmrs],
			union = FastMeshRegion[
				Catenate[MeshCoordinates /@ bmrs], 
				Polygon[Catenate[MapThread[#2 + Catenate[MeshCells[#, 2, "Multicells" -> True][[All, 1]]]&, {bmrs, Most[Prepend[Accumulate[MeshCellCount[#, 0]& /@ bmrs], 0]]}]]]
			];
		];
		If[wp != MachinePrecision, union = SetPrecision[union, wp]];
		
		Quiet[{coords, cells} = Region`Mesh`SplitIntersectingTriangles[union, WorkingPrecision -> wp, "DuplicateTolerance" -> \[Delta]];];
		
		res = Which[
			!ListQ[coords], $Failed, 
			TrueQ[\[Mu] > 0], MeshRegion[Mean /@ Nearest[coords, coords, {All, \[Mu]}], Polygon[Flatten[cells, 1]]],
			True, MeshRegion[coords, Polygon[Flatten[cells, 1]]]
		];
		
		If[MeshRegionQ[res] && TrueQ[OptionValue["PruneHangingFaces"]],
			UTPruneHangingFaces[res],
			res
		]
	]
];


intersectedSurfaceJoin[___] = $Failed;


(* ::Text:: *)
(*Region`Mesh`SplitIntersectingTriangles uses TriangleLink on each intersection, this block tries to avoid a bunch of validity checks to speed up the code.*)
(*This is dangerous if future versions of TriangleLink change implementation. If that occurs, we can 1. remove the call to this block for a performance hit or 2. change the code based on $Version.*)
(*In this block, TriangleTriangulate will ignore the TriangleRefinement option.*)


(* ::Text:: *)
(*This definition was created with only intersectedSurfaceJoin in mind, it's not very extensible.*)


SetAttributes[slimTriangleLinkBlock, HoldAllComplete];


slimTriangleLinkBlock[body_] /; Region`Mesh`RepresentationAvailableQ["TriangleLinkMesh"] := (
	If[TriangleLink`Private`needInitialization =!= False, TriangleLink`LoadTriangle[]];
	With[{
		pointSetter = TriangleLink`Private`setPointsFun,
		segSetter = TriangleLink`Private`setSegmentsFun,
		triangulate = TriangleLink`Private`triangulateFun,
		deleter = TriangleLink`Private`deleteFun,
		pointGetter = TriangleLink`Private`getPointsFun,
		elemGetter = TriangleLink`Private`getElementsFun,
		order = Range[0, 5]
	},
	Block[
		{
			TriangleLink`TriangleCreate,
			TriangleLink`TriangleSetPoints,
			TriangleLink`TriangleSetSegments,
			TriangleLink`TriangleTriangulate,
			TriangleLink`TriangleDelete,
			TriangleLink`TriangleExpressionQ,
			TriangleLink`TriangleGetPoints,
			TriangleLink`TriangleGetElements
		},
		TriangleLink`TriangleCreate = CreateManagedLibraryExpression["TriangleManager", TriangleLink`TriangleExpression]&;
		TriangleLink`TriangleSetPoints = pointSetter[#1[[1]], #2]&;
		TriangleLink`TriangleSetSegments = segSetter[#1[[1]], #2]&;
		TriangleLink`TriangleDelete = deleter[#[[1]]]&;
		TriangleLink`TriangleExpressionQ = ManagedLibraryExpressionQ[#, "TriangleManager"]&;
		TriangleLink`TriangleGetPoints = pointGetter[#[[1]]]&;
		TriangleLink`TriangleGetElements = elemGetter[#[[1]], order]&;
		
		TriangleLink`TriangleTriangulate = 
			With[{out = TriangleLink`TriangleCreate[]},
				If[triangulate[#2 <> "u", #1[[1]], out[[1]]],
					out,
					$Failed
				]
			]&;
		
		body
	]]
)


slimTriangleLinkBlock[body_] := body


(* ::Subsection::Closed:: *)
(*booleanComponentIndices*)


booleanComponentIndices[type_, mr_, bmrs_] :=
	Block[{C12, C22, comps, samplepoints, res},
		C12 = mr["ConnectivityMatrix"[1, 2]];
		C12 = Quiet[C12 * UnitStep[Subtract[2, Differences[C12["RowPointers"]]]]];
		
		C22 = Transpose[C12] . C12;
		comps = SparseArray`StronglyConnectedComponents[C22];
		
		If[booleanTypeNeedsTestPointsQ[type], 
			samplepoints = PropertyValue[{mr, {2, candidateFace[mr, #]& /@ comps}}, MeshCellCentroid]
		];
		
		res = filterBooleanComponents[type, comps, samplepoints, bmrs];
		
		res /; ListQ[res]
	]

booleanComponentIndices[___] = $Failed;


(* ::Text:: *)
(*default filtering behavior, other behavior implemented in each Boolean operation section*)


booleanTypeNeedsTestPointsQ[_] = True;


filterBooleanComponents[___] = $Failed;


(* ::Subsection::Closed:: *)
(*joinBooleanComponents*)


joinBooleanComponents[_, {}, ___] := EmptyRegion[3];


joinBooleanComponents[join_, comps_, solidQ_, pruneQ_:False] := 
	Block[{coords, cells, head, res},
		coords = MeshCoordinates[join];
		cells = MeshCells[join, {2, Join @@ comps}, "Multicells" -> True];
		head = If[TrueQ[solidQ], BoundaryMeshRegion, MeshRegion];
		
		res = buildMesh[head, coords, cells];
		
		If[pruneQ,
			If[TrueQ[solidQ],
				If[!RegionQ[res], (* nothing to prune if already a valid bmr *)
					res = UTToBMR[UTPruneHangingFaces[buildMesh[MeshRegion, coords, cells]]];
				],
				res = UTPruneHangingFaces[res];
			]
		];
		
		res /; RegionQ[res]
	];


joinBooleanComponents[___] = $Failed;


buildMesh[head_, coords_, cells_] := Quiet @ head[coords, cells, Method -> {"CheckIntersections" -> False}]


(* ::Section:: *)
(*UTUnion*)


(* ::Subsection::Closed:: *)
(*Main*)


Options[UTUnion] = $BooleanOperationOptions;


UTUnion[{}, OptionsPattern[]] = EmptyRegion[3];


UTUnion[bmrs__?validBooleanInputsQ, opts:OptionsPattern[]] := 
	With[{union = iUnion[parseUnionArguments[{bmrs}], opts]},
		union /; union =!= $Failed
	]


UTUnion[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Argument conform & completion*)


registerForDiscretization[UTUnion];
registerForSolid[UTUnion];


(* ::Subsection::Closed:: *)
(*Boolean filter*)


filterBooleanComponents["Union", comps_, sp_, bmrs_] := Pick[comps, Count[#, True]& /@ Transpose[RegionMember[#][sp]& /@ bmrs], 1]


(* ::Subsection::Closed:: *)
(*parseUnionArguments*)


parseUnionArguments[bmrs_] := If[!FreeQ[bmrs, FullRegion[3]], {FullRegion[3]}, DeleteCases[Flatten[bmrs], EmptyRegion[3]]]


(* ::Subsection::Closed:: *)
(*iUnion*)


Options[iUnion] = Options[UTUnion];


iUnion[{}, ___] = EmptyRegion[3]


iUnion[{bmr_}, ___] := bmr


iUnion[bmrs_, opts:OptionsPattern[]] :=
	Block[{join, comps, res},
		join = intersectedSurfaceJoin[bmrs, opts, "PruneHangingFaces" -> True];
		(
			comps = booleanComponentIndices["Union", join, bmrs];
			(
				res = joinBooleanComponents[join, comps, OptionValue["ReturnSolid"]];
		
				res /; RegionQ[res]
				
			) /; ListQ[comps]
			
		) /; MeshRegionQ[join]
	]


iUnion[___] = $Failed;


(* ::Section:: *)
(*UTIntersection*)


(* ::Subsection::Closed:: *)
(*Main*)


Options[UTIntersection] = $BooleanOperationOptions;


UTIntersection[{}, OptionsPattern[]] = EmptyRegion[3];


UTIntersection[bmrs__?validBooleanInputsQ, opts:OptionsPattern[]] := 
	With[{int = iIntersection[parseIntersectionArguments[{bmrs}], opts]},
		int /; int =!= $Failed
	]


UTIntersection[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Argument conform & completion*)


registerForDiscretization[UTIntersection];
registerForSolid[UTIntersection];


(* ::Subsection::Closed:: *)
(*Boolean filter*)


filterBooleanComponents["Intersection", comps_, sp_, bmrs_] := 
	Catch @ Block[{keep},
		keep = Fold[
			If[Length[#1] == 0, Throw[{}], Pick[#1, RegionMember[#2][#1[[All, 2]]]]]&, 
			Transpose[{Range[Length[sp]], sp}], 
			bmrs
		][[All, 1]];
		
		comps[[keep]]
	]


(* ::Subsection::Closed:: *)
(*parseIntersectionArguments*)


parseIntersectionArguments[bmrs_] := If[!FreeQ[bmrs, EmptyRegion[3]], {}, Replace[DeleteCases[Flatten[bmrs], FullRegion[3]], {} -> {FullRegion[3]}, {0}]]


(* ::Subsection::Closed:: *)
(*iIntersection*)


Options[iIntersection] = Options[UTIntersection];


iIntersection[{}, ___] = EmptyRegion[3]


iIntersection[{bmr_}, ___] := bmr


iIntersection[bmrs_, opts:OptionsPattern[]] :=
	Block[{join, comps, res},
		join = intersectedSurfaceJoin[bmrs, opts, "PruneHangingFaces" -> True];
		(
			comps = booleanComponentIndices["Intersection", join, bmrs];
			(
				res = joinBooleanComponents[join, comps, OptionValue["ReturnSolid"], OptionValue["ReturnSolid"]];
		
				res /; RegionQ[res]
				
			) /; ListQ[comps]
			
		) /; MeshRegionQ[join]
	]


iIntersection[___] = $Failed;


(* ::Section:: *)
(*UTDifference*)


(* ::Subsection::Closed:: *)
(*Main*)


Options[UTDifference] = $BooleanOperationOptions;


UTDifference[bmr1_?validBooleanMeshInputQ, bmr2_?validBooleanMeshInputQ, opts:OptionsPattern[]] :=
	Block[{join, comps, res},
		join = intersectedSurfaceJoin[{bmr1, bmr2}, opts, "PruneHangingFaces" -> True];
		(
			comps = booleanComponentIndices["Difference", join, {bmr1, bmr2}];
			(
				res = joinBooleanComponents[join, comps, OptionValue["ReturnSolid"], True];
		
				res /; RegionQ[res]
				
			) /; ListQ[comps]
			
		) /; MeshRegionQ[join]
	]


UTDifference[reg_?RegionQ, EmptyRegion[3], ___] := reg
UTDifference[EmptyRegion[3], _?RegionQ, ___] = EmptyRegion[3];
UTDifference[_?RegionQ, FullRegion[3], ___] = EmptyRegion[3];


UTDifference[bmr1_?validBooleanInputsQ, bmr2__?validBooleanInputsQ, opts:OptionsPattern[]] /; !validBooleanMeshInputQ[bmr1] || !validBooleanMeshInputQ[bmr2] :=
	Block[{union1, union2},
		union1 = UTUnion[bmr1, "ReturnSolid" -> True, opts];
		(
			union2 = UTUnion[bmr2, "ReturnSolid" -> True, opts];
			
			UTDifference[union1, union2, opts] /; RegionQ[union2]
			
		) /; RegionQ[union1]
	]


UTDifference[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Argument conform & completion*)


registerForDiscretization[UTDifference];
registerForSolid[UTDifference];


(* ::Subsection::Closed:: *)
(*Boolean filter*)


filterBooleanComponents["Difference", comps_, sp_, {bmr1_, bmr2_}] := 
	Catch @ Block[{inbmr1, inbmr2, inbd2, keep},
		inbmr1 = Developer`ToPackedArray @ Boole[RegionMember[bmr1][sp]];
		inbmr2 = Developer`ToPackedArray @ Boole[RegionMember[bmr2][sp]];
		inbd2 = Developer`ToPackedArray @ Boole[RegionMember[RegionBoundary[bmr2]][sp]];
		
		keep = Pick[Range[Length[sp]], inbmr1 * Unitize[inbd2 + Subtract[1, inbmr2]], 1];
		
		comps[[keep]]
	]


(* ::Section:: *)
(*UTSymmetricDifference*)


(* ::Subsection::Closed:: *)
(*Main*)


Options[UTSymmetricDifference] = $BooleanOperationOptions;


UTSymmetricDifference[bmr1_?validBooleanMeshInputQ, bmr2_?validBooleanMeshInputQ, opts:OptionsPattern[]] :=
	Block[{join, comps, res},
		join = intersectedSurfaceJoin[{bmr1, bmr2}, opts, "PruneHangingFaces" -> True];
		(
			comps = booleanComponentIndices["SymmetricDifference", join, {bmr1, bmr2}];
			(
				res = joinBooleanComponents[join, comps, OptionValue["ReturnSolid"]];
		
				res /; RegionQ[res]
				
			) /; ListQ[comps]
			
		) /; MeshRegionQ[join]
	]


UTSymmetricDifference[bmr1_?validBooleanInputsQ, bmr2_?validBooleanInputsQ, opts:OptionsPattern[]] /; !validBooleanMeshInputQ[bmr1] || !validBooleanMeshInputQ[bmr2] :=
	Block[{union1, union2},
		union1 = UTUnion[bmr1, "ReturnSolid" -> True, opts];
		(
			union2 = UTUnion[bmr2, "ReturnSolid" -> True, opts];
			
			UTSymmetricDifference[union1, union2, opts] /; RegionQ[union2]
			
		) /; RegionQ[union1]
	]


UTSymmetricDifference[reg_?RegionQ, EmptyRegion[3], ___] := reg
UTSymmetricDifference[EmptyRegion[3], reg_?RegionQ, ___] := reg;


UTSymmetricDifference[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Argument conform & completion*)


registerForDiscretization[UTSymmetricDifference];
registerForSolid[UTSymmetricDifference];


(* ::Subsection::Closed:: *)
(*Boolean filter*)


filterBooleanComponents["SymmetricDifference", comps_, sp_, {bmr1_, bmr2_}] := 
	Catch @ Block[{inbmr1, inbmr2, inbd1, inbd2, keep},
		inbmr1 = Developer`ToPackedArray @ Boole[RegionMember[bmr1][sp]];
		inbmr2 = Developer`ToPackedArray @ Boole[RegionMember[bmr2][sp]];
		inbd1 = Developer`ToPackedArray @ Boole[RegionMember[RegionBoundary[bmr1]][sp]];
		inbd2 = Developer`ToPackedArray @ Boole[RegionMember[RegionBoundary[bmr2]][sp]];
		
		keep = Pick[Range[Length[sp]], Unitize[inbd1+inbd2-2] * Unitize[inbmr1 * Unitize[inbd2 + Subtract[1, inbmr2]] + inbmr2 * Unitize[inbd1 + Subtract[1, inbmr1]]], 1];
		
		comps[[keep]]
	]


(* ::Section:: *)
(*UTComplement*)


(* ::Subsection::Closed:: *)
(*Main*)


UTComplement[reg_] := UTComplement[reg, Scaled[0.05]]


UTComplement[bmr_BoundaryMeshRegion?UTTriangleMesh3DQ, bdspec_] :=
	Block[{cuboid, res},
		cuboid = parseComplementRegion[RegionBounds[bmr], bdspec];
		(
			res = iComplement[bmr, cuboid];
			
			res /; RegionQ[res]
		
		) /; cuboid =!= $Failed
	]


UTComplement[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Argument conform & completion*)


registerForDiscretization[UTComplement, 1];
registerForSolid[UTComplement, 1];


(* ::Subsection::Closed:: *)
(*iComplement*)


iComplement[bmr_, cuboid_] /; containsBounds[cuboid, RegionBounds[bmr]] :=
	With[{res = complementByNesting[bmr, cuboid]},
		res /; res =!= $Failed
	]


iComplement[bmr_, cuboid_] :=
	With[{res = complementByDifference[bmr, cuboid]},
		res /; res =!= $Failed
	]


iComplement[___] = $Failed;


(* ::Subsection::Closed:: *)
(*complementByNesting*)


complementByNesting[bmr_, cuboid_?BoundaryMeshRegionQ] :=
	Block[{res},
		res = UTToBMR[UTFastBlock @ UTJoin[RegionBoundary /@ {bmr, cuboid}]];
		
		res /; BoundaryMeshRegionQ[res]
	]

complementByNesting[___] = $Failed;


registerForDiscretization[complementByNesting, 2];


(* ::Subsection::Closed:: *)
(*complementByDifference*)


complementByDifference[bmr_, cuboid_] := 
	Block[{outer, bigdiff, res},
		outer = parseComplementBounds[regionUnionBounds[bmr, cuboid], Scaled[0.05]];
		(
			bigdiff = complementByNesting[bmr, outer];
			(
				res = UTClip[bigdiff, cuboid];
				
				res /; RegionQ[res]
			
			) /; BoundaryMeshRegionQ[bigdiff]
			
		) /; outer =!= $Failed
	]

complementByDifference[bmr_, cuboid_] := 
	With[{res = UTDifference[cuboid, bmr]},
		res /; RegionQ[res]
	]

complementByDifference[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Utilities*)


parseComplementRegion[_, bdspec_List] /; MatrixQ[bdspec, NumericQ] && Dimensions[bdspec] == {3, 2} := 
	With[{res = Cuboid @@ Transpose[bdspec]},
		res /; ConstantRegionQ[res] && MatchQ[res, Cuboid[_, _]] && Volume[res] > 0
	]

parseComplementRegion[bds_, x_?NumericQ] := parseComplementRegion[bds, x{{-1,1},{-1,1},{-1,1}} + bds]

parseComplementRegion[bds_, s_Scaled] := parseComplementRegion[bds, ConstantArray[s, {3, 2}]]

parseComplementRegion[bds_, {o1___, s_Scaled, o2___}] := parseComplementRegion[bds, {o1, {s, s}, o2}]

parseComplementRegion[bds_, bdspec_] /; MatrixQ[bds, NumericQ] && !FreeQ[bdspec, Scaled] && MatrixQ[bdspec, NumericQ[#] || MatchQ[#, Scaled[_?NumericQ]]&] && Dimensions[bdspec] == {3, 2} := 
	parseComplementRegion[bds, MapThread[dimBounds, {bds, bdspec}]]

parseComplementRegion[___] = $Failed;


dimBounds[{a_, b_}, {c_, d_}] := {Replace[c, Scaled[s_?NumericQ] :> a - s*(b-a), {0}], Replace[d, Scaled[s_?NumericQ] :> b + s*(b-a), {0}]}


containsBounds[Cuboid[p1_, p2_], bds_] := And @@ LessEqual @@@ Transpose[Join @@ {{p1}, Transpose[bds], {p2}}]

containsBounds[___] = False;


regionUnionBounds[regs__] :=
	Block[{bds},
		bds = RegionBounds /@ {regs};
		(
			{Min[#1], Max[#2]}& @@@ Transpose[bds, {3, 1, 2}]
			
		) /; ArrayQ[bds, 3, NumericQ]
	]

regionUnionBounds[___] = $Failed


(* ::Section:: *)
(*UTBooleanComponents*)


(* ::Subsection::Closed:: *)
(*Main*)


Options[UTBooleanComponents] = {"DuplicateTolerance" -> 4, "MergeTolerance" -> 10^-8., WorkingPrecision -> MachinePrecision};


UTBooleanComponents[{}, OptionsPattern[]] = {};


UTBooleanComponents[mr__?UTTriangleMesh3DCollectionQ, opts:OptionsPattern[]] :=
	Block[{mrs, join, comps, coords, cells},
		mrs = Flatten[{mr}];
		join = intersectedSurfaceJoin[mrs, opts];
		(
			comps = booleanComponentIndices["Components", join, mrs];
			(
				coords = MeshCoordinates[join];
				cells = MeshCells[join, 2, "Multicells" -> True][[1, 1]];
				
				subCellMesh[MeshRegion, coords, {Polygon[cells[[#]]]}]& /@ comps
				
			) /; ListQ[comps]
			
		) /; MeshRegionQ[join]
	]


UTBooleanComponents[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Argument conform & completion*)


registerForDiscretization[UTBooleanComponents, 1];


(* ::Subsection::Closed:: *)
(*Boolean filter*)


booleanTypeNeedsTestPointsQ["Components"] = False;


filterBooleanComponents["Components", comps_, ___] := ReverseSortBy[comps, Length]
