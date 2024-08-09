(* ::Package:: *)

(* ::Title:: *)
(*Simplify*)


(* ::Section:: *)
(*Begin*)


Package["UTCG`"]


PackageExport["UTSimplifyMesh"]


UTSimplifyMesh::usage = "Simplify a mesh through iterative quadric based edge collapsing.";


(* ::Section:: *)
(*UTSimplifyMesh*)


(* ::Subsection::Closed:: *)
(*Main*)


UTSimplifyMesh[mr_] := UTSimplifyMesh[mr, Scaled[1/2]]


UTSimplifyMesh[mr_?UTTriangleMesh3DQ, itargetspec_] :=
	Block[{iters, res},
		iters = parseSimplifyMeshTargets[mr, itargetspec];
		(
			res = simplifyMesh[mr, iters];
			(
				res = If[Length[res[[2]]] === 0,
					EmptyRegion[3],
					UTLaxBlock[Head[mr][#1, Polygon[#2]]& @@ res]
				];
				
				res /; RegionQ[res]
				
			) /; res =!= $Failed
			
		) /; targetspec =!= $Failed
	]


UTSimplifyMesh[EmptyRegion[3], ___] = EmptyRegion[3];


UTSimplifyMesh[___] = $Failed;


(* ::Subsection::Closed:: *)
(*simplifyMesh*)


simplifyMesh[mr_, goals:{pgoal_, fgoal_}] := 
	Block[{cnts, flush},
		cnts = MeshCellCount[mr];
		
		If[pgoal === 0 && fgoal === 0, 
			Return[{{}, {}}]
		];
		
		If[pgoal >= cnts[[1]] && fgoal >= cnts[[3]],
			Return[{MeshCoordinates[mr], Join @@ MeshCells[mr, 2, "Multicells" -> True][[All, 1]]}];
		];
		
		flush = Max[Min[Quotient[cnts[[1]] - pgoal, 5], Quotient[cnts[[3]] - fgoal, 10]], 100];
		PQSimplifyMesh[mr, goals, flush]
	]


(* ::Subsection::Closed:: *)
(*PQSimplifyMesh*)


PQSimplifyMesh[mr_, {pgoal_, fgoal_}, flush_] :=
	Block[{
		coords, edges, faces, C01, C02, fQuads, vQuads, eQuads, vbar, keep, notdegenerate, degeneratep, cremaining, fremaining,
		decimated, fcnt = 0, v, e, vnew, v1, v2, v3, fmod, normals, ee, emod, enum, doneenum, heap, newE, newQ, newV},
		
		coords = MeshCoordinates[mr];
		edges = Developer`ToPackedArray[Sort /@ MeshCells[mr, 1, "Multicells" -> True][[1, 1]]];
		faces = Join @@ MeshCells[mr, 2, "Multicells" -> True][[All, 1]];
		decimated = ConstantArray[0, Length[faces]];
		
		cremaining = Length[coords];
		fremaining = Length[faces];
		
		C01 = mr["ConnectivityMatrix"[0, 1]]["MatrixColumns"];
		C02 = mr["ConnectivityMatrix"[0, 2]]["MatrixColumns"];
		
		fQuads = fundamentalErrorQuadrics[mr];
		vQuads = Developer`ToPackedArray @ cTotal[C02, fQuads];
		eQuads = Total[Partition[vQuads[[Flatten[edges]]], 2], {2}];
		
		vbar = inverseMaskP[coords, edges, eQuads];
		
		enum = ConstantArray[1, Length[vbar]];
		doneenum = 10*cremaining;
		
		heap = CreateDataStructure["PriorityQueue"];
		Scan[heap["Push", #]&, Transpose[{Minus[dCostP[vbar, eQuads]], Range[1, Length[vbar]], enum}]];
		
		While[cremaining > pgoal && fremaining > fgoal,
			
			If[fcnt++ === flush,
				If[cremaining > 500 || fremaining > 1000,
					fcnt = 0;
					heap = flushHeap[heap, enum];
				]
			];
			
			e = heap["Pop"];
			While[enum[[e[[2]]]] != Last[e],
				e = heap["Pop"];
			];
			e = e[[2]];
			
			vnew = vbar[[e]];
			v1 = edges[[e, 1]];
			v2 = edges[[e, 2]];
			
			(* a previous edge was singular *)
			If[v1 == v2,
				enum[[e]] = doneenum;
				Continue[];
			];
			
			coords[[v1]] = vnew;
			coords[[v2]] = vnew;
			cremaining--;
			
			fmod = DeleteDuplicates[Join[C02[[v1]], C02[[v2]]]];
			If[Length[fmod] == 0, Continue[]];
			normals = faceNormal[coords, faces[[fmod]]];
			
			notdegenerate = Unitize[Total[Abs[normals], {2}]];
			degeneratep = Pick[fmod, notdegenerate, 0];
			decimated[[degeneratep]] = 1;
			fremaining -= Length[degeneratep];
			faces[[fmod]] = replaceValue[faces[[fmod]], v2, v1];
			
			If[Length[degeneratep] > 0, 
				v3 = Total[replaceValue[faces[[degeneratep]], v1, 0], {2}];
				Do[
					v = v3[[i]];
					C02[[v]] = Complement[C02[[v]], {degeneratep[[i]]}];
					
					ee = Intersection[C01[[v2]], C01[[v]]];
					C01[[v2]] = Complement[C01[[v2]], ee];
					C01[[v]] = Complement[C01[[v]], ee];
					enum[[ee]] = doneenum,
					{i, Length[v3]}
				];
			];
			
			normals = Pick[normals, notdegenerate, 1];
			fmod = Pick[fmod, notdegenerate, 1];
			
			C02[[v1]] = fmod;
			C01[[v1]] = emod = Rest[DeleteDuplicates[Join[{e}, C01[[v1]], C01[[v2]]]]];
			
			If[Length[normals] > 0,
				fQuads[[fmod]] = feqC[normals, coords[[faces[[fmod]][[All, 1]]]]];
				Do[vQuads[[f]] = Total[fQuads[[C02[[f]]]]], {f, DeleteDuplicates[Flatten[faces[[fmod]]]]}];
			];
			
			newE = edges[[emod]] = replaceValue[edges[[emod]], v2, v1];
			newQ = eQuads[[emod]] = vQuads[[newE[[All, 1]]]] + vQuads[[newE[[All, 2]]]];
			
			newV = vbar[[emod]] = inverseMask[coords, newE, newQ];
			Scan[heap["Push", #]&, Transpose[{Minus[dCost[newV, newQ]], emod, ++enum[[emod]]}]];
			enum[[e]] = doneenum;
			
		];
		
		{coords, Pick[faces, decimated, 0]}
	]


(* ::Subsection::Closed:: *)
(*Utilities*)


(* ::Subsubsection::Closed:: *)
(*parseSimplifyMeshTargets*)


parseSimplifyMeshTargets[args___] := Replace[iParseSimplifyMeshTargets[args], \[Infinity] -> 0, {1}]


iParseSimplifyMeshTargets[mr_, spec:(_Integer | _Scaled)] := iParseSimplifyMeshTargets[mr, 2 -> spec]


iParseSimplifyMeshTargets[mr_, d_Integer -> spec:(_Integer | Scaled[_?(Between[{0, 1}])])] /; 0 <= d <= 2 :=
	Block[{cnt, goal},
		cnt = MeshCellCount[mr, d];
		
		goal = If[IntegerQ[spec], spec, Round[cnt * spec[[1]]]];
		
		Which[
			d === 0, {goal, \[Infinity]},
			d === 1, {Quotient[goal, 3], \[Infinity]},
			d === 2, {\[Infinity], goal}
		]
	]


iParseSimplifyMeshTargets[mr_, specs_List] := 
	Block[{iters},
		iters = iParseSimplifyMeshTargets[mr, #]& /@ specs;
		If[MatrixQ[iters],
			Transpose[Min /@ Transpose[iters]],
			$Failed
		]
	]


iParseSimplifyMeshTargets[___] = $Failed;


(* ::Subsubsection::Closed:: *)
(*fundamentalErrorQuadrics*)


fundamentalErrorQuadrics[mr_] := feqC[UTMeshCellNormals[mr, 2, "Normalize" -> True], MeshPrimitives[mr, 2, "Multicells" -> True][[1, 1, All, 1]]]


UTCompile[feqC, {{normals, _Real, 2}, {fp, _Real, 2}},
	Module[{tr},
		
		tr = Transpose[normals];
		AppendTo[tr, Minus[Total[tr Transpose[fp]]]];
		
		Transpose[Outer[Times, tr, tr, 1], {3, 2, 1}]
	]
];


(* ::Subsubsection::Closed:: *)
(*Compiled Utilities*)


UTCompile[cTotal, {{p, _Integer, 1}, {M, _Real, 3}},
	Total[M[[p]]],
	RuntimeAttributes -> {Listable}
];


UTCompile[faceNormal, {{coords, _Real, 2}, {inds, _Integer, 1}},
	Module[{cross, norm},
		cross = Cross[
			Subtract[Compile`GetElement[coords, Compile`GetElement[inds, 2]], Compile`GetElement[coords, Compile`GetElement[inds, 1]]], 
			Subtract[Compile`GetElement[coords, Compile`GetElement[inds, 3]], Compile`GetElement[coords, Compile`GetElement[inds, 1]]]
		];
		norm = cross . cross;
		Power[norm + Subtract[1.0, Unitize[norm]], -0.5]*cross
	],
	RuntimeAttributes -> {Listable},
	Parallelization -> False
];


UTCompile[replaceValue, {{i, _Integer}, {s, _Integer}, {f, _Integer}},
	If[i == s, f, i],
	RuntimeAttributes -> {Listable},
	Parallelization -> False
];


getTerms[expr_Plus, c_] := Total[Pick[List @@ expr, List @@ expr /. _Compile`GetElement -> 1, c] /. -1 -> 1]


Clear[inverseMask, inverseMaskP];
Quiet @ With[
	{
		code1 = Map[getTerms[#, 1]&, Det[Array[m, {4, 4}]] * Inverse[Array[m, {4, 4}]][[1;;3, -1]] /. {m[4, 4] -> 1, m[4, _] -> 0} /. {m[a__] :> Compile`GetElement[m, a]}],
		code2 = Map[getTerms[#, -1]&, Det[Array[m, {4, 4}]] * Inverse[Array[m, {4, 4}]][[1;;3, -1]] /. {m[4, 4] -> 1, m[4, _] -> 0} /. {m[a__] :> Compile`GetElement[m, a]}],
		det1 = getTerms[Det[Array[m, {4, 4}]] /. {m[4, 4] -> 1, m[4, _] -> 0} /. {m[a__] :> Compile`GetElement[m, a]}, 1],
		det2 = getTerms[Det[Array[m, {4, 4}]] /. {m[4, 4] -> 1, m[4, _] -> 0} /. {m[a__] :> Compile`GetElement[m, a]}, -1]
	},
	MapThread[
		(UTCompile[#1, {{coords, _Real, 2}, {i, _Integer, 1}, {m, _Real, 2}},
			Module[{d = Subtract[det1, det2]},
				If[Abs[d] > 10^-12.,
					Power[d, -1.0] Subtract[code1, code2],
					Mean[coords[[i]]]
				]],
			RuntimeAttributes -> {Listable},
			Parallelization -> #2
		])&,
		{{inverseMask, inverseMaskP}, {False, True}}
	]
];


Clear[dCost, dCostP];
MapThread[
	(UTCompile[#1, {{v, _Real, 1}, {m, _Real, 2}},
		Append[v, 1] . m . Append[v, 1],
		RuntimeAttributes -> {Listable},
		Parallelization -> #2
	])&,
	{{dCost, dCostP}, {False, True}}
];


(* ::Subsubsection::Closed:: *)
(*PriorityQueue*)


flushHeap[heap_, enum_] :=
	With[{hdata = Normal[heap], newheap = CreateDataStructure["PriorityQueue"]},
		Scan[
			newheap["Push", #]&, 
			Pick[hdata, Subtract[hdata[[All, 3]], enum[[hdata[[All, 2]]]]], 0]
		];
		
		newheap
	]
