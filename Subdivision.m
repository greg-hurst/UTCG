(* ::Package:: *)

(* ::Title:: *)
(*Subdivision*)


(* ::Section:: *)
(*Begin*)


Package["UTCG`"]


PackageExport["UTLoopSubdivide"]
PackageExport["UTSubdivide"]


UTLoopSubdivide::usage = "Perform loop subdivision on a triangulated mesh.";
UTSubdivide::usage = "Perform uniform subdivision on a triangulated mesh.";


(* ::Section:: *)
(*UTLoopSubdivide*)


(* ::Text:: *)
(*Code originally by Henrik Schumacher: https://mathematica.stackexchange.com/a/161332/4346*)


(* ::Subsection::Closed:: *)
(*Main*)


ClearAll[UTLoopSubdivide];
Options[UTLoopSubdivide] = {
   "VertexWeightFunction" -> (0.625` - (0.375` + 0.25` Cos[Divide[6.283185307179586`, #]])^2&),
   "EdgeWeight" -> 0.375`,
   "AverageBoundary" -> True,
   "VertexTransform" -> Identity
   };

UTLoopSubdivide[R_, n_Integer?NonNegative, opts:OptionsPattern[]] :=
	Nest[UTLoopSubdivide[#, opts]&, R, n]

UTLoopSubdivide[R_?UTSurfaceMesh3DQ, opts : OptionsPattern[]] := 
  UTLoopSubdivide[{R, {{0}}}, opts][[1]];

UTLoopSubdivide[{R_?UTSurfaceMesh3DQ, A_?MatrixQ}, OptionsPattern[]] := 
 Module[{A00, A10, A12, A20, B00, B10, n, n0, n1, n2, \[Beta]n, pts, 
   newpts, edges, faces, edgelookuptable, triangleneighedges, 
   newfaces, subdivisionmatrix, bndedgelist, bndedges, bndvertices, 
   bndedgeQ, intedgeQ, bndvertexQ, 
   intvertexQ, \[Beta], \[Beta]bnd, \[Eta]},
  pts = MeshCoordinates[R];
  A10 = CellAdjacencyMatrix[R, 1, 0];
  A20 = CellAdjacencyMatrix[R, 2, 0];
  A12 = CellAdjacencyMatrix[R, 1, 2];
  edges = Join @@ MeshCells[R, 1, "Multicells" -> True][[All, 1]];
  faces = Join @@ MeshCells[R, 2, "Multicells" -> True][[All, 1]];
  n0 = Length[pts];
  n1 = Length[edges];
  n2 = Length[faces];
  edgelookuptable = SparseArray[
    Rule[
     Join[edges, Transpose[Transpose[edges][[{2, 1}]]]], 
     Join[Range[1, Length[edges]], Range[1, Length[edges]]]
     ],
    {n0, n0}];
  (*A00=CellAdjacencyMatrix[R,0,0];*)
  A00 = Unitize[edgelookuptable];
  bndedgelist = Flatten[Position[Total[A12, {2}], 1]];
  If[Length[bndedgelist] > 0, bndedges = edges[[bndedgelist]];
   bndvertices = Sort[DeleteDuplicates[Flatten[bndedges]]];
   bndedgeQ = SparseArray[Partition[bndedgelist, 1] -> 1, {n1}];
   bndvertexQ = SparseArray[Partition[bndvertices, 1] -> 1, {n0}];
   B00 = SparseArray[Join[bndedges, Reverse /@ bndedges] -> 1, {n0, n0}];
   B10 = SparseArray[Transpose[{Join[bndedgelist, bndedgelist], Join @@ Transpose[bndedges]}] -> 1, {n1, n0}];, 
   bndedgeQ = SparseArray[{}, {Length[edges]}];
   bndvertexQ = SparseArray[{}, {n0}];
   B00 = SparseArray[{}, {n0, n0}];
   B10 = SparseArray[{}, {n1, n0}];
  ];
  intedgeQ = SparseArray[Subtract[1, Normal[bndedgeQ]]];
  intvertexQ = SparseArray[Subtract[1, Normal[bndvertexQ]]];
  n = Total[A10];
  \[Beta] = OptionValue["VertexWeightFunction"];
  \[Eta] = OptionValue["EdgeWeight"];
  \[Beta]n = \[Beta] /@ n;
  \[Beta]bnd = If[TrueQ[OptionValue["AverageBoundary"]], 1./8., 0.];
  subdivisionmatrix = Join[
    Plus[
     DiagonalMatrix[SparseArray[1. - \[Beta]n] intvertexQ + (1. - 2. \[Beta]bnd) bndvertexQ],
     SparseArray[(\[Beta]n/n intvertexQ)] A00, 
     \[Beta]bnd B00
     ], 
    Plus @@ {
      ((3. \[Eta] - 1.) intedgeQ) (A10), 
      If[Abs[\[Eta] - 0.5] < Sqrt[$MachineEpsilon], Nothing, ((0.5 - \[Eta]) intedgeQ) (A12 . A20)], 0.5 B10}
    ];
  newpts = subdivisionmatrix . pts;
  triangleneighedges = Module[{f1, f2, f3},
    {f1, f2, f3} = Transpose[faces];
    Partition[
     Extract[
      edgelookuptable,
      Transpose[{Flatten[Transpose[{f2, f3, f1}]], Flatten[Transpose[{f3, f1, f2}]]}]],
     3]
    ];
  newfaces = 
   Flatten[getSubdividedTriangles[faces, triangleneighedges + n0], 
    1];

  {
   UTLaxBlock @ Head[R][OptionValue["VertexTransform"] @ newpts, Polygon[newfaces]],
   subdivisionmatrix
   }
  ]

UTLoopSubdivide[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Argument conform & completion*)


registerForDiscretization[UTLoopSubdivide, 1];


(* ::Subsection::Closed:: *)
(*Utilities*)


CellAdjacencyMatrix[R_?UTMeshQ, d_, 0] := If[MeshCellCount[R, d] > 0,
   Unitize[R["ConnectivityMatrix"[d, 0]]],
   {}
   ];

CellAdjacencyMatrix[R_?UTMeshQ, 0, d_] := If[MeshCellCount[R, d] > 0,
   Unitize[R["ConnectivityMatrix"[0, d]]],
   {}
   ];

CellAdjacencyMatrix[R_?UTMeshQ, 0, 0] := 
  If[MeshCellCount[R, 1] > 0,
   With[{A = CellAdjacencyMatrix[R, 0, 1]},
    With[{B = A . Transpose[A]},
     SparseArray[B - DiagonalMatrix[Diagonal[B]]]
     ]
    ],
   {}
   ];

CellAdjacencyMatrix[R_?UTMeshQ, d1_, d2_] := 
  If[(MeshCellCount[R, d1] > 0) && (MeshCellCount[R, d2] > 0), 
   With[{B = CellAdjacencyMatrix[R, d1, 0] . CellAdjacencyMatrix[R, 0, d2]},
    SparseArray[
     If[d1 == d2,
      UnitStep[B - DiagonalMatrix[Diagonal[B]] - d1],
      UnitStep[B - (Min[d1, d2] + 1)]
      ]
     ]
    ],
   {}
   ];


UTCompile[getSubdividedTriangles, {{ff, _Integer, 1}, {ee, _Integer, 1}},
   {
    {Compile`GetElement[ff, 1],Compile`GetElement[ee, 3],Compile`GetElement[ee, 2]},
    {Compile`GetElement[ff, 2],Compile`GetElement[ee, 1],Compile`GetElement[ee, 3]},
    {Compile`GetElement[ff, 3],Compile`GetElement[ee, 2],Compile`GetElement[ee, 1]},
    ee
    },
   RuntimeAttributes -> {Listable}
];


(* ::Section:: *)
(*UTSubdivide*)


(* ::Subsection::Closed:: *)
(*Main*)


UTSubdivide[mesh_?UTTriangleMesh3DQ, n_:1] /; IntegerQ[n] && n >= 0 := Nest[iSubdivideMesh, mesh, n]


UTSubdivide[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Argument conform & completion*)


registerForDiscretization[UTSubdivide, 1];


(* ::Subsection::Closed:: *)
(*Utilities*)


iSubdivideMesh[mesh_] := 
	Block[{coords, cells},
		coords = Join[
			MeshCoordinates[mesh], 
			PropertyValue[{mesh, 1}, MeshCellCentroid]
		];
		cells = Polygon[Flatten[Map[subdivideSimplex[mesh], Join @@ MeshCells[mesh, 2, "Multicells" -> True][[All, 1]]], 1]];
		
		UTLaxBlock[Head[mesh][coords, cells]]
	];


subdivideSimplex[mesh_] := 
	With[{n = MeshCellCount[mesh, 0], edges = {{1,2},{2,3},{3,1}}, sub = {{1,4,6},{4,2,5},{6,5,3},{4,5,6}}},
		Block[{em, sv},
			em = Part[MeshCellIndex[mesh, Line[listablePartC[#, edges]]], All, 2];
			sv = Join[#, em + n];
			listablePartC[sv, sub]
		]&
	];


UTCompile[listablePartC, {{lis, _Integer, 1}, {pos, _Integer, 1}}, lis[[pos]], RuntimeAttributes -> Listable, Parallelization -> False];
