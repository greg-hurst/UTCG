(* ::Package:: *)

(* ::Title:: *)
(*Transform*)


(* ::Section:: *)
(*Begin*)


Package["UTCG`"]


PackageExport["UTCoordinateApply"]
PackageExport["UTCoordinateMap"]
PackageExport["UTCoordinateMapApply"]
PackageExport["UTCoordinateTransposeApply"]


UTCoordinateApply::usage = "Modify coordinates of a mesh by applying a function to all coordinates simultaneously.";
UTCoordinateTransposeApply::usage = "Modify coordinates of a mesh by applying a function that takes all ordinates as separate arguments.";
UTCoordinateMap::usage = "Modify coordinates of a mesh by mapping a function to across coordinates separately.";
UTCoordinateMapApply::usage = "Modify coordinates of a mesh by applying a function at level 1 to all coordinates.";


(* ::Section:: *)
(*iCoordinateApply*)


(* ::Text:: *)
(*TODO add adaptive subdivision option.*)


(* ::Subsection::Closed:: *)
(*Main*)


Options[iCoordinateApply] = {"MergeDuplicateCoordinates" -> Automatic, "FixBoundaryEdges" -> False};


iCoordinateApply[mr_, {Identity..}, ___] := mr


iCoordinateApply[mr_, {fs__}, form_, OptionsPattern[]] :=
	Block[{f, coords, coordsnew, fixbdQ, dupQ, mopts, res},
		f = Composition[fs];
		coords = MeshCoordinates[mr];
		coordsnew = coordApply[f, coords, form];
		(
			fixbdQ = OptionValue["FixBoundaryEdges"];
			dupQ = OptionValue["MergeDuplicateCoordinates"];
			
			If[TrueQ[fixbdQ],
				coordsnew = snapBackBoundary[mr, coordsnew];
			];
			
			If[dupQ === False || (dupQ === Automatic && Head[f] === TransformationFunction),
				mopts = {"EliminateUnusedCoordinates" -> False, "DeleteDuplicateCoordinates" -> False, "DeleteDuplicateCells" -> False, "TJunction" -> False, "CheckIntersections" -> False},
				mopts = {"EliminateUnusedCoordinates" -> False, "TJunction" -> False, "CheckIntersections" -> False}
			];
			
			res = Head[mr][coordsnew, Region`Mesh`MeshInputCells[mr], Method -> mopts];
			
			res /; RegionQ[res]
			
		) /; MatrixQ[coordsnew, Internal`RealValuedNumericQ]
	]


iCoordinateApply[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Utilities*)


coordApply[f_, coords_, Automatic | "Coordinates"] := f[coords]


coordApply[f_, coords_, "Transpose"] := Transpose[f @@ Transpose[coords]]


coordApply[f_, coords_, "Map"] := f /@ coords


coordApply[f_, coords_, "MapApply"] := f @@@ coords


coordApply[___] = $Failed;


snapBackBoundary[mr_, cnew_] :=
	Block[{bep, bdp, c},
		bep = Random`Private`PositionsOf[Differences[mr["ConnectivityMatrix"[1, 2]]["RowPointers"]], 1];
		If[Length[bep] === 0, Return[cnew]];
		
		bdp = Union[mr["ConnectivityMatrix"[1, 0]][[bep]]["NonzeroPositions"][[All, 2]]];
		
		c = cnew;
		c[[bdp]] = MeshCoordinates[mr][[bdp]];
		
		c
	]


(* ::Section:: *)
(*UTCoordinateApply*)


(* ::Subsection::Closed:: *)
(*Main*)


Options[UTCoordinateApply] = Options[iCoordinateApply];


UTCoordinateApply[mr_?UTMeshQ, fs__, opts:Longest[OptionsPattern[]]] := 
	With[{res = iCoordinateApply[mr, svmToTransform[RegionEmbeddingDimension[mr]] /@ {fs}, Automatic, opts]},
		res /; RegionQ[res]
	]


UTCoordinateApply[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Argument conform & completion*)


registerForDiscretization[UTCoordinateApply, 1, False];


(* ::Subsection::Closed:: *)
(*Utilities*)


svmToTransform[d_][s_?NumericQ] := ScalingTransform[ConstantArray[s, d]]
svmToTransform[d_][v_List] /; Length[v] == d && VectorQ[v, Internal`RealValuedNumericQ] := TranslationTransform[v]
svmToTransform[d_][m_List] /; Length[m] == d && MatrixQ[m, Internal`RealValuedNumericQ] := AffineTransform[m]
svmToTransform[_][tfunc_] := tfunc


(* ::Section:: *)
(*UTCoordinateTransposeApply*)


(* ::Subsection::Closed:: *)
(*Main*)


Options[UTCoordinateTransposeApply] = Options[iCoordinateApply];


UTCoordinateTransposeApply[mr_?UTMeshQ, fs__, opts:Longest[OptionsPattern[]]] := 
	With[{res = iCoordinateApply[mr, {fs}, "Transpose", opts]},
		res /; RegionQ[res]
	]


UTCoordinateTransposeApply[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Argument conform & completion*)


registerForDiscretization[UTCoordinateTransposeApply, 1, False];


(* ::Section:: *)
(*UTCoordinateMap*)


(* ::Subsection::Closed:: *)
(*Main*)


Options[UTCoordinateMap] = Options[iCoordinateApply];


UTCoordinateMap[mr_?UTMeshQ, fs__, opts:Longest[OptionsPattern[]]] := 
	With[{res = iCoordinateApply[mr, {fs}, "Map", opts]},
		res /; RegionQ[res]
	]


UTCoordinateMap[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Argument conform & completion*)


registerForDiscretization[UTCoordinateMap, 1, False];


(* ::Section:: *)
(*UTCoordinateMapApply*)


(* ::Subsection::Closed:: *)
(*Main*)


Options[UTCoordinateMapApply] = Options[iCoordinateApply];


UTCoordinateMapApply[mr_?UTMeshQ, fs__, opts:Longest[OptionsPattern[]]] := 
	With[{res = iCoordinateApply[mr, {fs}, "MapApply", opts]},
		res /; RegionQ[res]
	]


UTCoordinateMapApply[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Argument conform & completion*)


registerForDiscretization[UTCoordinateMapApply, 1, False];
