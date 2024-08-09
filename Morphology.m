(* ::Package:: *)

(* ::Title:: *)
(*Morphology*)


(* ::Section:: *)
(*Begin*)


Package["UTCG`"]


PackageExport["UTThicken"]
PackageExport["UTOffset"]
PackageExport["UTWireFrame"]
PackageExport["UTWireFrameDual"]
PackageExport["UTMorphologicalThinning3D"]


UTThicken::usage = "Add thickness to a surface mesh.";
UTOffset::usage = "Offset the boundary of a surface mesh.";
UTWireFrame::usage = "Computes the wire frame of a mesh.";
UTWireFrameDual::usage = "Computes the skeletal dual of a mesh.";
UTMorphologicalThinning3D::usage = "Computes the skeleton of a 3D image.";
(*UTMedialAxisMesh::usage = "Finds the medial axis of a mesh.";*)


(* ::Section:: *)
(*UTThicken*)


(* ::Subsection::Closed:: *)
(*Main*)


Options[UTThicken] = {"ReturnSolid" -> False};


UTThicken[mesh_, thickness_, OptionsPattern[]] /; Chop[thickness] == 0 && UTSurfaceMesh3DQ[mesh] := If[TrueQ[OptionValue["ReturnSolid"]], UTToBMR[mesh], mesh]


UTThicken[mesh_?UTSurfaceMesh3DQ, thickness_, OptionsPattern[]] :=
	Block[{solidQ, scoords, scells, t, normals, cnt, bdedges, coords, cells, res},
		solidQ = OptionValue["ReturnSolid"];
		scoords = MeshCoordinates[mesh];
		scells = MeshCells[mesh, 2, "Multicells" -> True][[1, 1]];
		
		t = If[NumericQ[thickness], thickness, thickness /@ scoords];
		(
			normals = (0.5t) * UTMeshCellNormals[mesh, 0, "Normalize" -> True];
			
			cnt = Length[scoords];
			bdedges = Developer`ToPackedArray[Region`InternalBoundaryEdges[mesh][[All, 1]]];
			
			coords = Join[
				Subtract[scoords, normals],
				Plus[scoords, normals]
			];
			
			cells = Join[
				Reverse[scells, {2}],
				scells + cnt,
				{#1, #2, #1+cnt}& @@@ bdedges,
				{#2, #2+cnt, #1+cnt}& @@@ bdedges
			];
			
			res = If[solidQ =!= False, BoundaryMeshRegion, MeshRegion][
				coords,
				Polygon[cells],
				Method -> {"CheckIntersections" -> False}
			];
			
			(
				If[solidQ,
					res = UTHull[res, "ReturnSolid" -> True];
				];
				
				res /; RegionQ[res]
			)
			
		) /; NumericQ[t] || (VectorQ[t, Internal`RealValuedNumericQ] && Length[t] == Length[scoords])
	]


UTThicken[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Argument conform & completion*)


registerForDiscretization[UTThicken, 1];


(* ::Section:: *)
(*UTOffset*)


(* ::Subsection::Closed:: *)
(*Main*)


Options[UTOffset] = {"ReturnSolid" -> False};


UTOffset[mesh_, thickness_, OptionsPattern[]] /; Chop[thickness] == 0 && UTSurfaceMesh3DQ[mesh] := If[TrueQ[OptionValue["ReturnSolid"]], UTToBMR[mesh], mesh]


UTOffset[mesh_?UTSurfaceMesh3DQ, thickness_, OptionsPattern[]] :=
	Block[{solidQ, t, normals, res},
		solidQ = OptionValue["ReturnSolid"];
		t = If[NumericQ[thickness], thickness, thickness /@ MeshCoordinates[mesh]];
		(
			normals = t * UTMeshCellNormals[mesh, 0, "Normalize" -> True];
			
			res = UTCoordinateApply[mesh, # + normals&];
			(
				If[solidQ,
					res = UTHull[res, "ReturnSolid" -> True];
				];
				
				res /; RegionQ[res]
				
			) /; RegionQ[res]
			
		) /; NumericQ[t] || (VectorQ[t, Internal`RealValuedNumericQ] && Length[t] == MeshCellCount[mesh, 0])
	]


UTOffset[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Argument conform & completion*)


registerForDiscretization[UTThicken, 1];


(* ::Section:: *)
(*UTWireFrame*)


(* ::Subsection::Closed:: *)
(*Main*)


Options[UTWireFrame] = Options[MeshRegion];


UTWireFrame[mr_?UTMeshQ, opts:OptionsPattern[]] :=
	Which[
		RegionDimension[mr] < 1, 
			EmptyRegion[RegionEmbeddingDimension[mr]],
		MatchQ[mr["MeshCellTypes"], {{{Line, _}..}..}], 
			MeshRegion[mr, opts],
		True, 
			MeshRegion[MeshCoordinates[mr], MeshCells[mr, 1, "Multicells" -> True], opts]
	]


(* ::Section:: *)
(*UTWireFrameDual*)


(* ::Subsection::Closed:: *)
(*Main*)


Options[UTWireFrameDual] = Join[
	{"JoinBoundaryEdges" -> False, "JoinSingularEdges" -> False, "CreaseSnappingAngle" -> 0.0, "SnappingDistance" -> \[Infinity]}, 
	Options[MeshRegion]
];


UTWireFrameDual[mr_?UTMeshQ, opts:OptionsPattern[]] /; RegionDimension[mr] > 1 :=
	Block[{mesh, dual, joinbdQ, joinseQ, csnap\[Alpha], snapd},
		Switch[mr["ComponentDimensions"],
			{2}, mesh = mr,
			{___, 3}, mesh = RegionBoundary[mr],
			_, mesh = Last[DimensionalMeshComponents[RegionBoundary[mr]]]
		];
		
		csnap\[Alpha] = parseCreaseSnappingAngle[OptionValue["CreaseSnappingAngle"]];
		snapd = parseSnappingDistance[OptionValue["SnappingDistance"]];
		joinbdQ = TrueQ[OptionValue["JoinBoundaryEdges"]];
		joinseQ = TrueQ[OptionValue["JoinSingularEdges"]];
		
		dual = iWireFrameDual[mesh, joinbdQ, joinseQ, csnap\[Alpha], snapd];
		(
			If[UTMeshQ[dual],
				Head[dual][dual, FilterRules[{opts}, Options[MeshRegion]]],
				dual
			]
		) /; RegionQ[dual]
	]


UTWireFrameDual[poly_?constantPolyhedronQ, opts:OptionsPattern[]] :=
	Block[{bmr, res},
		bmr = Quiet @ BoundaryDiscretizeGraphics[poly];
		(
			res = UTWireFrameDual[bmr, opts];
			
			res /; RegionQ[res]
		
		) /; BoundaryMeshRegionQ[bmr]
	]


UTWireFrameDual[___] = $Failed;


(* ::Subsection::Closed:: *)
(*iWireFrameDual*)


iWireFrameDual[mesh_, joinbdQ_, joinseQ_, csnap\[Alpha]_, snapd_] :=
	Block[{C12, coords, cells, main},
		C12 = mesh["ConnectivityMatrix"[1, 2]];
		{coords, cells} = iWireFrameDualConditioned[mesh, C12, joinseQ, csnap\[Alpha], snapd];
		
		main = If[Length[cells] > 0,
			Quiet @ MeshRegion[
				coords, 
				Line[cells],
				Method -> {"DeleteDuplicateCoordinates" -> False, "DeleteDuplicateCells" -> False}
			],
			EmptyRegion[RegionEmbeddingDimension[mesh]]
		];
		
		If[TrueQ[joinbdQ],
			UTJoin[main, iWireFrameDualBoundary[mesh, C12]],
			main
		]
	]


(* ::Subsection::Closed:: *)
(*iWireFrameDualConditioned*)


iWireFrameDualConditioned[mesh_, C12_, joinseQ_, csnap\[Alpha]_, snapd_] :=
	Block[{singp = {}, creasep = {}, distp = {}, splitp},
		
		(* -------- short circuit cases -------- *)
		If[!joinseQ && csnap\[Alpha] <= 0.0 && snapd === \[Infinity],
			Return[dualSplitNoEdges[mesh, C12]];
		];
		
		If[csnap\[Alpha] >= \[Pi] || snapd <= 0.0,
			Return[dualSplitAllEdges[mesh, C12]];
		];
		
		(* -------- extra splitting cases -------- *)
		
		(* ---- changing topology cases ---- *)
		If[joinseQ,
			singp = Random`Private`PositionsOf[Differences[C12["RowPointers"]] - 2, Positive];
		];
		
		(* ---- preserving topology cases ---- *)
		If[csnap\[Alpha] > 0,
			creasep = UTCreaseEdges[mesh, csnap\[Alpha], "IncludeSingularEdges" -> False, "EdgeFaceConnectivity" -> C12];
		];
		
		If[snapd < \[Infinity],
			distp = farDualEdges[mesh, C12, snapd];
		];
		
		(* -------- split wireframe mesh -------- *)
		splitp = If[Total[Unitize[Length /@ {singp, creasep, distp}]] <= 1, 
			Join[singp, creasep, distp],
			Union[singp, creasep, distp]
		];
		
		dualSplitEdges[mesh, splitp, C12]
	]


(* ::Subsection::Closed:: *)
(*dualSplitEdges*)


dualSplitEdges[mesh_, splitp_, C12_] /; Length[splitp] === MeshCellCount[mesh, 1] := dualSplitAllEdges[mesh, C12]


dualSplitEdges[mesh_, splitp_, C12_] /; Length[splitp] === 0 := dualSplitNoEdges[mesh, C12]


dualSplitEdges[mesh_, splitp_, C12_] :=
	Block[{coords, cells, C12split, C12sub, ci, rp, cnt, zz, splitcoords, splitcells},
		coords = AnnotationValue[{mesh, 2}, MeshCellCentroid];
		
		C12split = C12;
		C12split[[splitp, All]] = 0;
		cells = UpperTriangularize[Transpose[C12split] . C12split, 1]["NonzeroPositions"];
		
		C12sub = C12[[splitp]];
		ci = Flatten[C12sub["ColumnIndices"]];
		rp = C12sub["RowPointers"];
		
		zz = ConstantArray[0, Length[ci]];
		zz[[Most[rp] + 1]] = 1;
		cnt = MeshCellCount[mesh, 2];
		
		splitcoords = AnnotationValue[{mesh, {1, splitp}}, MeshCellCentroid];
		splitcells = Transpose[{ci, Accumulate[zz] + cnt}];
		
		coords = Join[coords, splitcoords];
		cells = Join[cells, splitcells];
		
		{coords, cells}
	]


dualSplitNoEdges[mesh_, C12_] :=
	Block[{coords, cells},
		coords = AnnotationValue[{mesh, 2}, MeshCellCentroid];
		cells = UpperTriangularize[Transpose[C12] . C12, 1]["NonzeroPositions"];
		
		{coords, cells}
	]


dualSplitAllEdges[mesh_, C12_] :=
	Block[{coords, cells, ci, rp, cnt, zz},
		coords = Join[
			AnnotationValue[{mesh, 2}, MeshCellCentroid],
			AnnotationValue[{mesh, 1}, MeshCellCentroid]
		];
		
		ci = Flatten[C12["ColumnIndices"]];
		rp = C12["RowPointers"];
		
		zz = ConstantArray[0, Length[ci]];
		zz[[Most[rp] + 1]] = 1;
		cnt = MeshCellCount[mesh, 2];
		
		cells = Transpose[{ci, Accumulate[zz] + cnt}];
		
		{coords, cells}
	]


(* ::Subsection::Closed:: *)
(*farDualEdges*)


farDualEdges[mesh_, C12_, snapd_] :=
	Block[{facecents, edgecents, ci, rp, rpts, manfoldedges, rpsub, dists2},
		facecents = AnnotationValue[{mesh, 2}, MeshCellCentroid];
		edgecents = AnnotationValue[{mesh, 1}, MeshCellCentroid];
		
		ci = C12["ColumnIndices"];
		rp = C12["RowPointers"];
		rpts = Differences[rp];
		
		manfoldedges = Random`Private`PositionsOf[rpts, 2];
		If[Length[manfoldedges] === 0, Return[{}]];
		
		rpsub = rp[[manfoldedges + 1]];
		
		dists2 = infiniteLinePointDistanceSquared[
			facecents[[Flatten[ci[[rpsub - 1]]]]],
			facecents[[Flatten[ci[[rpsub]]]]],
			edgecents[[manfoldedges]]
		];
		
		Pick[Range[Length[dists2]], UnitStep[dists2 - snapd^2], 1]
	]


UTCompile[infiniteLinePointDistanceSquared, {{p1, _Real, 1}, {p2, _Real, 1}, {q, _Real, 1}},
	Block[{v, vn2, w, u, h},
		v = Subtract[p2, p1];
		w = Subtract[q, p1];
		vn2 = v . v;
		
		If[vn2 != 0.0,
			h = Divide[w . v, vn2];
			u = Subtract[w, h*v];
			u . u,
			w . w
		]
	],
	RuntimeAttributes -> {Listable}
]


(* ::Subsection::Closed:: *)
(*iWireFrameDualBoundary*)


iWireFrameDualBoundary[mesh_, conn_] :=
	Block[{e, f, v, c, disp},
		e = Random`Private`PositionsOf[Differences[conn["RowPointers"]], 1];
		If[e === {}, Return[EmptyRegion[RegionEmbeddingDimension[mesh]]]];
		
		f = conn[[e]]["NonzeroPositions"];
		
		disp = Dispatch[Thread[Range[Length[e]] -> e]];
		{v, f} = {#1 /. disp, #2}& @@ Transpose[f];
		
		c = PropertyValue[{mesh, {2, f}}, MeshCellCentroid];
		v = MeshPrimitives[mesh, {1, e}][[All, 1]];
		
		DiscretizeGraphics[Line[Join @@ MapThread[{#1, {#1[[1]], #2}, {#1[[2]], #2}}&, {v, c}]]]
	]


(* ::Subsection::Closed:: *)
(*Utilities*)


constantPolyhedronQ = And[
	MatchQ[Head[#], Cube|Cuboid|Dodecahedron|Hexahedron|Icosahedron|Octahedron|Parallelepiped|Polyhedron|Prism|Pyramid|Simplex|Tetrahedron],
	ConstantRegionQ[#]
]&;


parseCreaseSnappingAngle[None] = 0.0;
parseCreaseSnappingAngle[All] = \[Pi];
parseCreaseSnappingAngle["NonPlanar"] = \[Pi] - 1.`*^-6;
parseCreaseSnappingAngle["NonShallow"] = 2.8;


parseCreaseSnappingAngle[x_] /; TrueQ[-\[Infinity] <= x <= \[Infinity]] := Clip[x, {0.0, \[Pi]}]


parseCreaseSnappingAngle[_] = 0.0;


parseSnappingDistance[None] = \[Infinity];
parseSnappingDistance[All] = 0.0;


parseSnappingDistance[x_] /; TrueQ[-\[Infinity] <= x <= \[Infinity]] := Clip[x, {0.0, \[Infinity]}]


parseSnappingDistance[_] = \[Infinity];


(* ::Section:: *)
(*UTMorphologicalThinning3D*)


(* ::Subsection::Closed:: *)
(*About*)


(* ::Text:: *)
(*Implements "Building Skeleton Models via 3-D Medial Surface Axis Thinning Algorithms", Lee T.C., Kashyap R.L., Chu C.N.*)


(* ::Text:: *)
(*https://www.sci.utah.edu/devbuilds/biomesh3d/FEMesher/references/lee94-3dskeleton.pdf*)


(* ::Text:: *)
(*I did not implement the function Octree_Labeling since it's wildly recursive and Compile does not like that. Instead I took a transitive closure approach.*)


(* ::Subsection::Closed:: *)
(*Main*)


UTMorphologicalThinning3D[im_Image3D?BinaryImageQ, 0] := im


UTMorphologicalThinning3D[im_Image3D?BinaryImageQ, iter_:Infinity] := 
  Block[{idata, ii, res},
    idata = ImageData[ImagePad[im, 1]];
    ii = Replace[iter, Except[_Integer?NonNegative] -> 10^9, {0}];

    res = morphologicalThinning3DC[idata, ii];

    ImagePad[Image3D[res, "Bit"], -1]
  ]


UTMorphologicalThinning3D[im_Image3D, args___] := UTMorphologicalThinning3D[Binarize[im], args]


(* ::Subsection::Closed:: *)
(*Compiled Utilities*)


With[{
  inds = {25,26,16,17,22,23,13,27,24,18,15,26,23,17,19,22,10,13,20,23,11,21,24,20,23,12,15,11,7,16,8,17,4,13,5,9,8,18,17,6,5,15,1,10,4,13,2,11,5,3,2,12,11,6,5,15},
  \[Delta]G6 = Riffle[{1,-1,-1,1,-3,-1,-1,1,-1,1,1,-1,3,1,1,-1,-3,-1,3,1,1,-1,3,1,-1,1,1,-1,3,1,1,-1,-3,3,-1,1,1,3,-1,1,-1,1,1,-1,3,1,1,-1,1,3,3,1,5,3,3,1,-1,1,1,-1,3,1,1,-1,-7,-1,-1,1,-3,-1,-1,1,-1,1,1,-1,3,1,1,-1,-3,-1,3,1,1,-1,3,1,-1,1,1,-1,3,1,1,-1,-3,3,-1,1,1,3,-1,1,-1,1,1,-1,3,1,1,-1,1,3,3,1,5,3,3,1,-1,1,1,-1,3,1,1,-1},ConstantArray[0,128]]
},

UTCompile[preservesEulerCharacteristicQ, {{nb, _Integer, 1}},
  Module[{n},
    n = Partition[nb[[inds]], 7] . {128,64,32,16,8,4,2};
    Total[\[Delta]G6[[n + 1]]] == 0
  ], 
  Parallelization -> False
];

];


With[{a27 = ConstantArray[0, {27, 27}], tups = Tuples[{-1,0,1}, 3], dig = IntegerDigits[Range[0, 26], 3, 3] + 1, rng = Range[27]},
With[{c1 = Select[Transpose[Transpose[tups] + #], 0 < Min[#] <= Max[#] < 4&] . {9,3,1}-12& /@ dig},
With[{c = Table[Select[c1[[i]], i <= # <= 27 && i != 14&], {i, 27}]},
With[{conn1=c[[1]],conn2=c[[2]],conn3=c[[3]],conn4=c[[4]],conn5=c[[5]],conn6=c[[6]],conn7=c[[7]],conn8=c[[8]],conn9=c[[9]],conn10=c[[10]],conn11=c[[11]],conn12=c[[12]],conn13=c[[13]],conn15=c[[15]],conn16=c[[16]],conn17=c[[17]],conn18=c[[18]],conn19=c[[19]],conn20=c[[20]],conn21=c[[21]],conn22=c[[22]],conn23=c[[23]],conn24=c[[24]],conn25=c[[25]],conn26=c[[26]],conn27=c[[27]]},

UTCompile[simplePointQ, {{nb, _Integer, 1}},
  Module[{cb = nb, adj, pp},
    cb[[14]] = 0;
    adj = a27;

    If[Total[cb] > 23, Return[True]];

    If[Compile`GetElement[cb,1] == 1, Do[If[Compile`GetElement[cb,j] == 1, adj[[1, j]] = 1], {j, conn1}]];
    If[Compile`GetElement[cb,2] == 1, Do[If[Compile`GetElement[cb,j] == 1, adj[[2, j]] = 1], {j, conn2}]];
    If[Compile`GetElement[cb,3] == 1, Do[If[Compile`GetElement[cb,j] == 1, adj[[3, j]] = 1], {j, conn3}]];
    If[Compile`GetElement[cb,4] == 1, Do[If[Compile`GetElement[cb,j] == 1, adj[[4, j]] = 1], {j, conn4}]];
    If[Compile`GetElement[cb,5] == 1, Do[If[Compile`GetElement[cb,j] == 1, adj[[5, j]] = 1], {j, conn5}]];
    If[Compile`GetElement[cb,6] == 1, Do[If[Compile`GetElement[cb,j] == 1, adj[[6, j]] = 1], {j, conn6}]];
    If[Compile`GetElement[cb,7] == 1, Do[If[Compile`GetElement[cb,j] == 1, adj[[7, j]] = 1], {j, conn7}]];
    If[Compile`GetElement[cb,8] == 1, Do[If[Compile`GetElement[cb,j] == 1, adj[[8, j]] = 1], {j, conn8}]];
    If[Compile`GetElement[cb,9] == 1, Do[If[Compile`GetElement[cb,j] == 1, adj[[9, j]] = 1], {j, conn9}]];
    If[Compile`GetElement[cb,10] == 1, Do[If[Compile`GetElement[cb,j] == 1, adj[[10, j]] = 1], {j, conn10}]];
    If[Compile`GetElement[cb,11] == 1, Do[If[Compile`GetElement[cb,j] == 1, adj[[11, j]] = 1], {j, conn11}]];
    If[Compile`GetElement[cb,12] == 1, Do[If[Compile`GetElement[cb,j] == 1, adj[[12, j]] = 1], {j, conn12}]];
    If[Compile`GetElement[cb,13] == 1, Do[If[Compile`GetElement[cb,j] == 1, adj[[13, j]] = 1], {j, conn13}]];
    If[Compile`GetElement[cb,15] == 1, Do[If[Compile`GetElement[cb,j] == 1, adj[[15, j]] = 1], {j, conn15}]];
    If[Compile`GetElement[cb,16] == 1, Do[If[Compile`GetElement[cb,j] == 1, adj[[16, j]] = 1], {j, conn16}]];
    If[Compile`GetElement[cb,17] == 1, Do[If[Compile`GetElement[cb,j] == 1, adj[[17, j]] = 1], {j, conn17}]];
    If[Compile`GetElement[cb,18] == 1, Do[If[Compile`GetElement[cb,j] == 1, adj[[18, j]] = 1], {j, conn18}]];
    If[Compile`GetElement[cb,19] == 1, Do[If[Compile`GetElement[cb,j] == 1, adj[[19, j]] = 1], {j, conn19}]];
    If[Compile`GetElement[cb,20] == 1, Do[If[Compile`GetElement[cb,j] == 1, adj[[20, j]] = 1], {j, conn20}]];
    If[Compile`GetElement[cb,21] == 1, Do[If[Compile`GetElement[cb,j] == 1, adj[[21, j]] = 1], {j, conn21}]];
    If[Compile`GetElement[cb,22] == 1, Do[If[Compile`GetElement[cb,j] == 1, adj[[22, j]] = 1], {j, conn22}]];
    If[Compile`GetElement[cb,23] == 1, Do[If[Compile`GetElement[cb,j] == 1, adj[[23, j]] = 1], {j, conn23}]];
    If[Compile`GetElement[cb,24] == 1, Do[If[Compile`GetElement[cb,j] == 1, adj[[24, j]] = 1], {j, conn24}]];
    If[Compile`GetElement[cb,25] == 1, Do[If[Compile`GetElement[cb,j] == 1, adj[[25, j]] = 1], {j, conn25}]];
    If[Compile`GetElement[cb,26] == 1, Do[If[Compile`GetElement[cb,j] == 1, adj[[26, j]] = 1], {j, conn26}]];
    If[Compile`GetElement[cb,27] == 1, Do[If[Compile`GetElement[cb,j] == 1, adj[[27, j]] = 1], {j, conn27}]];

    pp = Complement[rng cb, {0}];
    If[Length[pp] == 0, Return[False]];
    adj = adj[[pp, pp]];
    adj = adj + Transpose[adj];

    adj = adj . adj;
    If[Min[adj] > 0,
      True,
      adj = adj . adj;
      Min[adj] > 0
    ]
  ], 
  Parallelization -> False
];

]]]];


UTCompile[morphologicalThinning3DC, {{oim, _Integer, 3}, {iter, _Integer}},
  Block[{cnt = 0, X, Y, Z, im = oim, im2 = oim, samefaces = 0, nb, bag, nface, yo, sbp, il, jl, kl},
    {X, Y, Z} = Dimensions[im];
    While[cnt++ < iter && samefaces < 6,
      samefaces = 0;
      Do[
        nface = 1;
        Do[
          If[Compile`GetElement[im,i,j,k] == 0, Continue[]];

          If[!Or[
            f == 1 && Compile`GetElement[im,i,j-1,k] == 0,
            f == 2 && Compile`GetElement[im,i,j+1,k] == 0,
            f == 3 && Compile`GetElement[im,i+1,j,k] == 0,
            f == 4 && Compile`GetElement[im,i-1,j,k] == 0,
            f == 5 && Compile`GetElement[im,i,j,k+1] == 0,
            f == 6 && Compile`GetElement[im,i,j,k-1] == 0
          ], Continue[]];

          nb = Flatten[im[[i-1;;i+1, j-1;;j+1, k-1;;k+1]]];

          If[Total[nb] != 2 && preservesEulerCharacteristicQ[nb] && simplePointQ[nb], 
            If[!simplePointQ[Flatten[im2[[i-1;;i+1, j-1;;j+1, k-1;;k+1]]]],
              im2[[i, j, k]] = 1,
              im2[[i, j, k]] = 0;
              nface = 0;
            ];
          ],

          {k, 2, Z-1},
          {j, 2, Y-1},
          {i, 2, X-1}
        ];
        im = im2;
        samefaces += nface,
        {f, 1, 6}
      ];
    ];

    im
  ]
];


(* ::Section:: *)
(*UTMedialAxisMesh*)


(* ::Text:: *)
(*Not exposed! Not robust enough!*)


(* ::Subsection::Closed:: *)
(*Main*)


Options[UTMedialAxisMesh] = {Method -> Automatic};


UTMedialAxisMesh[mr_?UTTriangleMesh3DQ, opts:OptionsPattern[]] :=
	Block[{methoddata, res},
		methoddata = parseMedialAxisMethod[OptionValue[Method]];
		(
			res = #1[mr, ##2]& @@ methoddata;
			
			res /; RegionQ[res]
		
		) /; ListQ[methoddata]
	]

UTMedialAxisMesh[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Option parsing*)


Options[parseMedialAxisMethod] = {RasterSize -> 90, MaxRecursion -> 3};


parseMedialAxisMethod[Automatic] := parseMedialAxisMethod["Rasterization"]
parseMedialAxisMethod[{method_String, subopts___}] := parseMedialAxisMethod[method, subopts]

parseMedialAxisMethod["Rasterization", subopts:OptionsPattern[]] := 
	Block[{rsz, mx},
		rsz = OptionValue[RasterSize];
		mx = OptionValue[MaxRecursion];
		(
			{rasterMedialAxis, rsz, mx}
		
		) /; IntegerQ[rsz] && rsz > 0 && IntegerQ[mx] && rsz >= 0
	]

parseMedialAxisMethod[___] = $Failed;


(* ::Subsection::Closed:: *)
(*rasterMedialAxis*)


rasterMedialAxis[mr_?MeshRegionQ, rsz_, mx_] :=
	With[{bmr = UTToBMR[mr]},
		rasterMedialAxis[bmr, rsz, mx] /; BoundaryMeshRegionQ[bmr]
	]


rasterMedialAxis[bmr_?BoundaryMeshRegionQ, rsz_, mx_] /; loadIGraphMQ :=
	Block[{bds, im3d, skel, arr, lmr, mrg, remove, mat, comps, sv, subs, subpaths, submeshes},
		bds = CoordinateBounds[MeshCoordinates[bmr], Scaled[0.05]];
		im3d = RegionImage[bmr, bds, RasterSize -> rsz, Method -> {MaxRecursion -> mx}];
		
		skel = UTMorphologicalThinning3D[im3d];
		
		arr = ArrayMesh[ImageData[ImageReflect[skel, Top]], DataRange -> bds[[{3, 1, 2}]]];
		lmr = MeshRegion[
			PropertyValue[{arr, 3}, MeshCellCentroid][[All, {2, 3, 1}]],
			Line[UpperTriangularize[Unitize[# . Transpose[#]&[arr["ConnectivityMatrix"[3, 0]]]], 1]["NonzeroPositions"]]
		];
		
		mrg = FixedPoint[
			(
				remove = UndirectedEdge @@@ (First[MaximalBy[Partition[#, 2, 1, 1], EuclideanDistance @@ PropertyValue[{arr, {3, #}}, MeshCellCentroid]&]]& /@ IGraphM`IGTriangles[#]);
				EdgeDelete[#, Union[Sort /@ remove]]
			)&,
			IGraphM`IGMeshGraph[lmr]
		];
		
		mat = IncidenceMatrix[mrg];
		mat = mat*UnitStep[2 - Differences[mat["RowPointers"]]];
		
		comps = SimpleGraph /@ ConnectedGraphComponents[AdjacencyGraph[VertexList[mrg], mat . Transpose[mat]]];
		sv = Union @@ VertexList /@ Select[comps, VertexCount[#] == 1&];
		subs = Union @ Flatten[ConnectedGraphComponents[Subgraph[mrg, Join[VertexList[#], sv], EdgeWeight -> None]]& /@ Select[comps, VertexCount[#] > 1&]];
		subs = VertexDelete[#, Pick[VertexList[#], VertexDegree[#], 0]]& /@ subs;
		
		subpaths = Function[g, PropertyValue[{g, #}, VertexCoordinates]& /@ (FindShortestPath[g, ##]& @@ GraphPeriphery[g])] /@ subs;
		submeshes = MeshRegion[
			UTPathPoints[#, PlotPoints -> 2Length[#], SplineDegree -> 100], 
			Line[Range[2Length[#]]]
		]& /@ Cases[subpaths, _List];
		
		Region`Mesh`MeshRegionJoin @@ submeshes
	]


rasterMedialAxis[___] = $Failed;
