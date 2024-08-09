(* ::Package:: *)

(* ::Title:: *)
(*Normals*)


(* ::Section:: *)
(*Begin*)


Package["UTCG`"]


PackageExport["UTInvertNormals"]
PackageExport["UTMeshCellNormals"]


UTInvertNormals::usage = "Inverts the orientation of surface mesh faces.";
UTMeshCellNormals::usage = "Returns normal vectors of specified mesh cells.";


(* ::Section:: *)
(*UTInvertNormals*)


(* ::Subsection::Closed:: *)
(*Main*)


UTInvertNormals[mr_?UTSurfaceMesh3DQ] := FastMeshRegion[MeshCoordinates[mr], Reverse[MeshCells[mr, 2, "Multicells" -> True], {4}], Options[mr]]


UTInvertNormals[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Argument conform & completion*)


registerForDiscretization[UTInvertNormals, 1];


(* ::Section:: *)
(*UTMeshCellNormals*)


(* ::Subsection::Closed:: *)
(*PackageScopes*)


PackageScope["faceUnitNormals"]


(* ::Subsection::Closed:: *)
(*Main*)


Options[UTMeshCellNormals] = {"Normalize" -> Automatic};


UTMeshCellNormals[mr_?UTMeshQ, cellspec_, OptionsPattern[]] /; validCellSpecQ[mr, cellspec] := 
	Block[{normalize, res},
		normalize = OptionValue["Normalize"];
		If[faceCellSpecQ[mr, cellspec],
			res = faceUnitNormals[mr, faceIndices[cellspec], normalize =!= False];,
			res = Region`Mesh`MeshCellNormals[mr, appendDimension[mr, cellspec]];
			If[ArrayQ[res] && normalize === True,
				res = Normalize /@ res;
			]
		];
		
		res /; ArrayQ[res]
	]


UTMeshCellNormals[___] = $Failed;


(* ::Subsection::Closed:: *)
(*faceUnitNormals*)


faceUnitNormals[mr_] := faceUnitNormals[mr, All, True]


faceUnitNormals[mr_, inds_] := faceUnitNormals[mr, inds, True]


faceUnitNormals[mr_, inds_, _] :=
	With[{n = Region`Mesh`MeshCellNormals[mr, If[ListQ[inds], {2, inds}, 2]]},
		n /; ArrayQ[n]
	]


(* ::Text:: *)
(*In the case where Region`Mesh`MeshCellNormals encountered degenerate faces*)


faceUnitNormals[mr_, inds_, normalizeQ_] := Join @@ faceUnitNormalsC[MeshPrimitives[mr, If[ListQ[inds], {2, inds}, 2], "Multicells" -> True][[All, 1]], TrueQ[normalizeQ]]


UTCompile[faceUnitNormalsC, {{faces, _Real, 2}, {normalizeQ, True|False}},
	Module[{cross, norm},
		cross = Cross[Subtract[Compile`GetElement[faces, 2], Compile`GetElement[faces, 1]], Subtract[Compile`GetElement[faces, 3], Compile`GetElement[faces, 1]]];
		If[normalizeQ,
			norm = Total[cross^2];
			If[norm > 0, Divide[cross, Sqrt[norm]], {1.0, 0.0, 0.0}],
			cross
		]
	],
	RuntimeAttributes -> {Listable},
	Parallelization -> False
];


(* ::Subsection::Closed:: *)
(*Utilities*)


validCellSpecQ[mr_, dim_Integer] := 0 <= dim <= Min[2, RegionDimension[mr]]


validCellSpecQ[mr_, {dim_Integer, indices_List}] :=
	And[
		0 <= dim <= Min[2, Subtract[RegionDimension[mr], Boole[BoundaryMeshRegionQ[mr]]]],
		VectorQ[indices, IntegerQ],
		1 <= #1 <= #2 <= MeshCellCount[mr, dim]& @@ MinMax[indices]
	]


validCellSpecQ[mr_, indices_List] :=
	And[
		RegionEmbeddingDimension[mr]-1 <= Subtract[RegionDimension[mr], Boole[BoundaryMeshRegionQ[mr]]] < 3,
		VectorQ[indices, IntegerQ],
		1 <= #1 <= #2 <= MeshCellCount[mr, Subtract[RegionDimension[mr], Boole[BoundaryMeshRegionQ[mr]]]]& @@ MinMax[indices]
	]


validCellSpecQ[___] = False;


faceCellSpecQ[_, 2] = True;
faceCellSpecQ[_, {2, _}] = True;
faceCellSpecQ[mr_, indices_List] /; VectorQ[indices, IntegerQ] := RegionEmbeddingDimension[mr] === 3
faceCellSpecQ[___] = False;


faceIndices[indices_List] /; VectorQ[indices, IntegerQ] := indices
faceIndices[{_, indices_List}] := indices
faceIndices[_] = All;


appendDimension[mr_, indices_List] /; VectorQ[indices, IntegerQ] := {Subtract[RegionDimension[mr], Boole[BoundaryMeshRegionQ[mr]]], indices}
appendDimension[_, cellspec_] := cellspec
