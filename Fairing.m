(* ::Package:: *)

(* ::Title:: *)
(*Fairing*)


(* ::Section:: *)
(*Begin*)


Package["UTCG`"]


PackageExport["UTLaplacianMatrix"]
PackageExport["UTGlobalLaplacianSmooth"]
PackageExport["UTLaplacianSmooth"]
PackageExport["UTTaubinSmooth"]


UTLaplacianMatrix::usage = "Computes the cotangent or uniform weight Laplacian matrix of a triangulated mesh.";
UTGlobalLaplacianSmooth::usage = "Smooths a triangulated mesh in a global sense while optionally preserving features.";
UTLaplacianSmooth::usage = "Performs classical Laplacian smoothing on a triangulated mesh.";
UTTaubinSmooth::usage = "Performs classical Taubin smoothing on a triangulated mesh.";


(* ::Section:: *)
(*UTLaplacianMatrix*)


(* ::Subsection::Closed:: *)
(*Main*)


UTLaplacianMatrix[mr_?UTTriangleMesh3DQ, type_:Automatic] :=
	Block[{matfunc, res},
		matfunc = Switch[type,
			"UniformWeight"|"Uniform", uniformWeightLaplacianMatrix,
			"CotangentNormalized", normalizedCotangentLaplacianMatrix,
			Automatic | "Cotangent" | _, cotangentLaplacianMatrix
		];
		
		res = matfunc[mr];
		
		res /; MatrixQ[res, NumericQ]
	]


UTLaplacianMatrix[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Argument conform & completion*)


addCodeCompletion[UTLaplacianMatrix][None, {"Cotangent", "Uniform"}];


(* ::Subsection::Closed:: *)
(*cotangentLaplacianMatrix*)


cotangentLaplacianMatrix[mr_] := 
	Block[{cells, edges},
		cells = Join @@ MeshCells[mr, 2, "Multicells" -> True][[All, 1]];
		edges = Transpose[{Flatten[cells], Flatten[RotateLeft[cells, {0, 1}]]}];
		
		cotangentLaplacianMatrix[MeshCoordinates[mr], cells, edges]
	]


cotangentLaplacianMatrix[coords_, cells_, edges_] :=
	Block[{n, prims, p1, p2, cos, cots, spopt, L},
		n = Length[coords];
		prims = Region`Mesh`ToCoordinates[cells, coords];
		
		p1 = prims - RotateRight[prims, {0, 1}];
		p2 = RotateRight[p1, {0, 1}];
		cos = Total[p1 p2, {3}] Power[Total[p1^2, {3}]*Total[p2^2, {3}], -0.5];
		cots = .5Flatten[cos*Power[Clip[1 - cos^2, {1.`*^-9, \[Infinity]}, {1.0, 1.0}], -.5]];
		
		Internal`WithLocalSettings[
			spopt = SystemOptions["SparseArrayOptions"];
			SetSystemOptions["SparseArrayOptions" -> {"TreatRepeatedEntries" -> Total}],
			L = SparseArray[edges -> cots, {n, n}, 0.0],
			SetSystemOptions[spopt]
		];
		
		L = L + Transpose[L];
		L = L - SparseArray[{Band[{1, 1}] -> Total[L, {2}]}];
		
		L
	]


(* ::Subsection::Closed:: *)
(*normalizedCotangentLaplacianMatrix*)


(* ::Text:: *)
(*This function is used for smoothing. Note that it is different than the standard area normalization-type mass matrix. Smoothing should be scale invariant, and so that type of normalization should not be used?*)


normalizedCotangentLaplacianMatrix[mr_] := 
	Block[{cells, edges},
		cells = Join @@ MeshCells[mr, 2, "Multicells" -> True][[All, 1]];
		edges = Transpose[{Flatten[cells], Flatten[RotateLeft[cells, {0, 1}]]}];
		
		normalizedCotangentLaplacianMatrix[MeshCoordinates[mr], cells, edges]
	]


normalizedCotangentLaplacianMatrix[coords_, cells_, edges_] :=
	Block[{L, tots, inv},
		L = cotangentLaplacianMatrix[coords, cells, edges];
		
		tots = Total[Abs[L], {1}];
		inv = 2.0*Power[Clip[tots, {1.`*^-9, \[Infinity]}, {1.0, 1.0}], -1.0];
		
		inv*L
	]


(* ::Subsection::Closed:: *)
(*uniformWeightLaplacianMatrix*)


uniformWeightLaplacianMatrix[mr_] :=
	Block[{C00, W, II},
		C00 = Unitize[# . Transpose[#]]&[mr["ConnectivityMatrix"[0, 2]]];
		W = SparseArray[-Power[Map[Length, C00["MatrixColumns"]] - 1, -1.0]];
		II = IdentityMatrix[Length[C00], SparseArray];
		
		SparseArray[(C00 - II)W + II]
	]


(* ::Section:: *)
(*UTGlobalLaplacianSmooth*)


(* ::Subsection::Closed:: *)
(*About*)


(* ::Text:: *)
(*Implements "Non-iterative Global Mesh Smoothing with Feature Preservation", Zhongping Ji*)


(* ::Text:: *)
(*https://koreascience.kr/article/JAKO200634514931603.pdf*)


(* ::Subsection::Closed:: *)
(*Main*)


Options[UTGlobalLaplacianSmooth] = {"FeatureVertices" -> Automatic, "VertexPenalty" -> Automatic, "DampenBarycenters" -> True};


UTGlobalLaplacianSmooth[mr_, opts:OptionsPattern[]] := UTGlobalLaplacianSmooth[mr, "Uniform", opts]


With[{lsolvemethod = If[StringStartsQ[$ProcessorType, "x86"], "Pardiso", Automatic]},
UTGlobalLaplacianSmooth[mr_?UTTriangleMesh3DQ, itype_?validGlobalSmoothTypeQ, OptionsPattern[]] :=
	Block[{coords, cells, n, k, type, L, findices, \[Lambda], Fdiag, \[Mu], F, m, 
			dampenCentroidsQ, centroids, cellsinds, Zdiag, Zrules, Z, A, b, At, ncoords, final},
		
		(* ------------ mesh data ------------ *)
		
		coords = MeshCoordinates[mr];
		cells = MeshCells[mr, 2, "Multicells" -> True];
		n = MeshCellCount[mr, 0];
		k = MeshCellCount[mr, 2];
		dampenCentroidsQ = TrueQ[OptionValue["DampenBarycenters"]];
	
		(* ------------ Laplacian matrix ------------ *)
		
		type = Switch[itype,
			"Uniform", "Uniform",
			"Cotangent", "CotangentNormalized",
			"CotangentNonNormalized", "Cotangent"
		];
		L = UTLaplacianMatrix[mr, type];
		
		(* ------------ vertex penalty matrix ------------ *)
		
		{findices, \[Mu]} = featureVertices[mr, OptionValue["FeatureVertices"]];
		
		\[Lambda] = OptionValue["VertexPenalty"];
		Fdiag = ReplacePart[vertexPenalty[coords, \[Lambda]*Boole[!dampenCentroidsQ]], Thread[findices -> \[Mu]]];
		F = DiagonalMatrix[SparseArray[Fdiag]];
		m = Length[F];
		
		(* ------------ centroid penalty matrix ------------ *)
		
		If[dampenCentroidsQ,
			cellsinds = Join @@ cells[[All, 1]];
			centroids = AnnotationValue[{mr, 2}, MeshCellCentroid];
			Zdiag = vertexPenalty[centroids, \[Lambda]];
			Zrules = Transpose[{Quotient[Range[3, 3k+2], 3], Flatten[cellsinds]}] -> Flatten[Transpose[ConstantArray[Zdiag, 3]]];
			Z = SparseArray[Zrules, {k, n}];,
			Z = {};
		];
		
		(* ------------ global matrix ------------ *)
		
		A = Join[L, F, Z];
		
		(* ------------ right hand side ------------ *)
		
		b = ConstantArray[0., {Length[A], 3}];
		If[dampenCentroidsQ,
			b[[n + 1 ;; n + m]] = Fdiag * coords;
			b[[n + m + 1 ;; n + m + Length[Z]]] = 3Zdiag*centroids;,
			b[[n + 1 ;; n + m]] = Fdiag * coords;
		];
		
		(* ------------ solve the system ------------ *)
		
		At = Transpose[A];
		ncoords = Quiet[LinearSolve[At . A, At . b, Method -> lsolvemethod]];
		
		(* for large enough \[Mu], ensure the feature vertices are truly fixed *)
		If[TrueQ[\[Mu] >= $\[Mu]],
			ncoords = ReplacePart[ncoords, Thread[findices -> coords[[findices]]]]
		];
		
		(* ------------ construct mesh ------------ *)
		
		UTFastBlock[Head[mr][ncoords[[1 ;; n]], cells, Options[mr]]]
	]
]


UTGlobalLaplacianSmooth[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Argument conform & completion*)


registerForDiscretization[UTGlobalLaplacianSmooth, 1];


addCodeCompletion[UTGlobalLaplacianSmooth][None, {"Uniform", "Cotangent", "CotangentNonNormalized"}];


(* ::Subsection::Closed:: *)
(*Utilities*)


$\[Mu] = 5.0;
$\[Lambda] = 0.1;


featureVertices[mr_, fv:Except[{_, _?Positive}]] := featureVertices[mr, {fv, $\[Mu]}];
featureVertices[_, {None, \[Mu]_}] := {{}, \[Mu]};
featureVertices[mr_, {Automatic, \[Mu]_}] := {Union @@ Region`InternalBoundaryEdges[mr][[All, 1]], \[Mu]}
featureVertices[mr_, {vinds_, \[Mu]_}] /; VectorQ[vinds, IntegerQ] && 1 <= Min[vinds] && Max[vinds] <= MeshCellCount[mr, 0] := {vinds, \[Mu]}
featureVertices[_, {_, \[Mu]_}] := {{}, \[Mu]};


vertexPenalty[coords_, \[Lambda]:(Automatic|_?NumericQ)] := ConstantArray[If[NumericQ[\[Lambda]] || TrueQ[NonNegative[\[Lambda]]], \[Lambda], $\[Lambda]], Length[coords]]
vertexPenalty[coords_, vpfunc_] := vpfunc[coords]


validGlobalSmoothTypeQ["Uniform"] = True;
validGlobalSmoothTypeQ["Cotangent"] = True;
validGlobalSmoothTypeQ["CotangentNonNormalized"] = True;


(* ::Section:: *)
(*UTLaplacianSmooth*)


(* ::Subsection::Closed:: *)
(*Main*)


UTLaplacianSmooth[mr_] := UTLaplacianSmooth[mr, 3, "Uniform"]


UTLaplacianSmooth[mr_, n_] := UTLaplacianSmooth[mr, n, "Uniform"]


UTLaplacianSmooth[mr_?UTTriangleMesh3DQ, n_Integer?NonNegative, itype:("Uniform"|"Cotangent")] :=
	Block[{type, smoothmr},
		type = If[itype === "Uniform", "Uniform", "CotangentNormalized"];
		
		With[{M = UTLaplacianMatrix[mr, type]}, (* M doesn't change per iteration *)
			Quiet[smoothmr = UTCoordinateApply[mr, Nest[Subtract[#,  M . #]&, #, n]&];];
		];
		
		smoothmr /; RegionQ[smoothmr]
	]


UTLaplacianSmooth[mr_?UTTriangleMesh3DQ, n_Integer?NonNegative, "PerIterationCotangent"] :=
	With[{cells = Join @@ MeshCells[mr, 2, "Multicells" -> True][[All, 1]]},
		With[{edges = Transpose[{Flatten[cells], Flatten[RotateLeft[cells, {0, 1}]]}]},
			
			UTCoordinateApply[
				mr, 
				Nest[
					With[{M = normalizedCotangentLaplacianMatrix[#, cells, edges]},
						Subtract[#,  M . #]
					]&,
					#,
					n
				]&
			]
		]
	]


UTLaplacianSmooth[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Argument conform & completion*)


registerForDiscretization[UTLaplacianSmooth, 1];


addCodeCompletion[UTLaplacianSmooth][None, None, {"Uniform", "Cotangent"(*, "PerIterationCotangent"*)}];


(* ::Section:: *)
(*UTTaubinSmooth*)


(* ::Subsection::Closed:: *)
(*Main*)


Options[UTTaubinSmooth] = {"Weights" -> {0.5, -0.53}};


UTTaubinSmooth[mr_, opts:OptionsPattern[]] := UTTaubinSmooth[mr, 3, "Uniform", opts]


UTTaubinSmooth[mr_, n_Integer, opts:OptionsPattern[]] := UTTaubinSmooth[mr, n, "Uniform", opts]


UTTaubinSmooth[mr_?UTTriangleMesh3DQ, n_Integer?NonNegative, type_?validTaubinTypeQ, OptionsPattern[]] :=
	Block[{w, res},
		w = parseTaubinWeights[OptionValue["Weights"]];
		(
			res = iTaubinSmooth[mr, n, w[[1]], w[[2]], type];
			
			res /; RegionQ[res]
		
		) /; w =!= $Failed
	]


UTTaubinSmooth[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Argument conform & completion*)


registerForDiscretization[UTTaubinSmooth, 1];


addCodeCompletion[UTTaubinSmooth][None, None, {"Uniform", "Cotangent"(*, "PerIterationCotangent"*)}];


(* ::Subsection::Closed:: *)
(*iTaubinSmooth*)


iTaubinSmooth[mr_, n_, \[Lambda]_, \[Mu]_, itype:("Uniform"|"Cotangent")] :=
	Block[{type, smoothmr},
		type = If[itype === "Uniform", "Uniform", "CotangentNormalized"];
		
		With[{M = UTLaplacianMatrix[mr, type]}, (* M doesn't change per iteration *)
			Quiet[smoothmr = UTCoordinateApply[mr, Nest[With[{p = Subtract[#, \[Lambda]*M . #]}, Subtract[p, \[Mu]*M . p]]&, #, n]&];];
		];
		
		smoothmr /; RegionQ[smoothmr]
	]


iTaubinSmooth[mr_, n_, \[Lambda]_, \[Mu]_, "PerIterationCotangent"] :=
	With[{cells = Join @@ MeshCells[mr, 2, "Multicells" -> True][[All, 1]]},
		With[{edges = Transpose[{Flatten[cells], Flatten[RotateLeft[cells, {0, 1}]]}]},
			
			UTCoordinateApply[
				mr, 
				Nest[
					With[{M = normalizedCotangentLaplacianMatrix[#, cells, edges]},
						With[{p = Subtract[#, \[Lambda]*M . #]}, Subtract[p, \[Mu]*M . p]]
					]&,
					#,
					n
				]&
			]
		]
	]


iTaubinSmooth[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Option Parsing*)


parseTaubinWeights[Automatic] = {0.5, -0.53};


parseTaubinWeights[w_] := If[VectorQ[w, NumericQ] && Length[w] === 2 && w \[Element] Reals, w, $Failed]


parseTaubinWeights[___] = $Failed;


validTaubinTypeQ["Uniform"] = True;
validTaubinTypeQ["Cotangent"] = True;
validTaubinTypeQ["PerIterationCotangent"] = True;


(* ::Section:: *)
(*UTCG`Legacy`UTSmooth*)


(* ::Subsection::Closed:: *)
(*Main*)


Options[UTCG`Legacy`UTSmooth] = {"FeatureVertices" -> Automatic, "LaplacianMatrixMethod" -> Automatic, "VertexPenalty" -> Automatic};


With[{lsolvemethod = If[StringStartsQ[$ProcessorType, "x86"], "Pardiso", Automatic]},
UTCG`Legacy`UTSmooth[mr_?UTTriangleMesh3DQ, OptionsPattern[]] :=
	Block[{coords, cells, n, L, findices, Fdiag, \[Mu], F, m, A, b, At, ncoords, final},
		
		(* ------------ mesh data ------------ *)
		
		coords = MeshCoordinates[mr];
		cells = MeshCells[mr, 2, "Multicells" -> True];
		n = Length[coords];
	
		(* ------------ Laplacian matrix ------------ *)
		
		L = UTLaplacianMatrix[mr, OptionValue["LaplacianMatrixMethod"]];
		
		(* ------------ vertex penalty matrix ------------ *)
		
		{findices, \[Mu]} = legacyfeatureVertices[mr, OptionValue["FeatureVertices"]];
		
		Fdiag = legacyvertexPenalty[coords, OptionValue["VertexPenalty"]];
		Fdiag = ReplacePart[Fdiag, Thread[findices -> \[Mu]]];
		
		F = DiagonalMatrix[SparseArray[Fdiag]];
		m = Length[F];
		
		(* ------------ global matrix ------------ *)
		
		A = Join[L, F];
		
		(* ------------ right hand side ------------ *)
		
		b = ConstantArray[0., {Length[A], 3}];
		b[[n + 1 ;; n + m]] = Fdiag * coords;
		
		(* ------------ solve the system ------------ *)
		
		At = Transpose[A];
		ncoords = Quiet[LinearSolve[At . A, At . b, Method -> lsolvemethod]];
		
		(* for large enough \[Mu], ensure the feature vertices are truly fixed *)
		If[TrueQ[\[Mu] >= $legacy\[Mu]],
			ncoords = ReplacePart[ncoords, Thread[findices -> coords[[findices]]]]
		];
		
		(* ------------ construct mesh ------------ *)
		
		UTFastBlock[Head[mr][ncoords, cells, Options[mr]]]
	]
]


UTCG`Legacy`UTSmooth[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Argument conform & completion*)


registerForDiscretization[UTCG`Legacy`UTSmooth, 1];


(* ::Subsection::Closed:: *)
(*Utilities*)


$legacy\[Mu] = 5.0;
$legacy\[Lambda] = 0.1;


legacyfeatureVertices[mr_, fv:Except[{_, _?Positive}]] := legacyfeatureVertices[mr, {fv, $legacy\[Mu]}];
legacyfeatureVertices[_, {None, \[Mu]_}] := {{}, \[Mu]};
legacyfeatureVertices[mr_, {Automatic, \[Mu]_}] := {Union @@ Region`InternalBoundaryEdges[mr][[All, 1]], \[Mu]}
legacyfeatureVertices[mr_, {vinds_, \[Mu]_}] /; VectorQ[vinds, IntegerQ] && 1 <= Min[vinds] && Max[vinds] <= MeshCellCount[mr, 0] := {vinds, \[Mu]}
legacyfeatureVertices[_, {_, \[Mu]_}] := {{}, \[Mu]};


legacyvertexPenalty[coords_, \[Lambda]:(Automatic|_?NumericQ)] := ConstantArray[If[NumericQ[\[Lambda]] || TrueQ[NonNegative[\[Lambda]]], \[Lambda], $legacy\[Lambda]], Length[coords]]
legacyvertexPenalty[coords_, vpfunc_] := vpfunc[coords]
