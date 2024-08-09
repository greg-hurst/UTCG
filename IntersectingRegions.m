(* ::Package:: *)

(* ::Title:: *)
(*IntersectingRegions*)


(* ::Section:: *)
(*Begin*)


Package["UTCG`"]


PackageExport["UTIntersectingRegions"]
PackageExport["UTIntersectingRegionsFunction"]


UTIntersectingRegions::usage = "UTIntersectingRegions[{reg1, reg2, ...}, reg] gives the list of regi that intersect with reg";
UTIntersectingRegionsFunction::usage = "UTIntersectingRegionsFunction[regs] represents a function whose values give the regions that intersect with a region that is supplied.";


(* ::Section:: *)
(*Intersection detection*)


(* ::Subsection::Closed:: *)
(*Main*)


intersectingRegions[regs_, bboxTree_, reg_] :=
	Block[{cands, inds},
		cands = intersectingBBoxes[bboxTree, reg];
		inds = Select[cands, pairwiseRegionsIntersectQ[regs[[#]], reg]&];
		
		Sort[inds]
	]


intersectingBBoxes[bboxTree_, reg_] := OverlappingBBoxes[bboxTree, iRegionBounds[reg]]


pairwiseRegionsIntersectQ[r1_, r2_] := 
	With[{intQ = intersectionDetector[Head[r1], Head[r2]]},
		intQ[r1, r2]
	]


(*SetAttributes[intersectionDetector, Orderless];*)
intersectionDetector[CapsuleShape, CapsuleShape] = capsulesIntersectQ[#1[[1]], #1[[2]], #2[[1]], #2[[2]]]&;
intersectionDetector[__] = regionsIntersectQ;


(* ::Subsection::Closed:: *)
(*Capsule - capsule intersection*)


(* ::Text:: *)
(*Based off the naive approach at https://www.geometrictools.com/Documentation/DistanceLine3Line3.pdf*)


UTCompile[capsulesIntersectQ, {{seg1, _Real, 2}, {r1, _Real}, {seg2, _Real, 2}, {r2, _Real}},
	Module[{v1, v2, v3, n, nrm2, a, b, c, d, e, f, D2, \[CapitalDelta]},
		v1 = Subtract[Compile`GetElement[seg1, 2], Compile`GetElement[seg1, 1]];
		v2 = Subtract[Compile`GetElement[seg2, 2], Compile`GetElement[seg2, 1]];
		v3 = Subtract[Compile`GetElement[seg1, 1], Compile`GetElement[seg2, 1]];
		D2 = (r1+r2)^2;
		
		n = Cross[v1, v2];
		nrm2 = Total[n^2];
		
		If[nrm2 > 0 && (n . v3)^2 > nrm2*D2, Return[False]];
		
		a = v1 . v1; b = v1 . v2; c = v2 . v2;
		d = v1 . v3; e = v2 . v3; f = v3 . v3;
		
		\[CapitalDelta] = Subtract[a*c, b^2];
		
		Or[
			f <= D2, (* R[0, 0] \[LessEqual] d^2 *)
			a+2.0d+f <= D2, (* R[1, 0] \[LessEqual] d^2 *)
			Subtract[c+f, 2.0e] <= D2, (* R[0, 1] \[LessEqual] d^2 *)
			Subtract[a+c+2.0d+f, 2.0(b+e)] <= D2, (* R[1, 1] \[LessEqual] d^2 *)
			0 <= e <= c && Subtract[f, Divide[e^2, c]] <= D2, (* R[0, t0hat] \[LessEqual] d^2 *)
			0 <= Minus[d] <= a && Subtract[f, Divide[d^2, a]] <= D2, (* R[s0hat, 0] \[LessEqual] d^2 *)
			0 <= b+e <= c && Subtract[a+2.0d+f, Divide[(b+e)^2, c]] <= D2, (* R[1, t1hat] \[LessEqual] d^2 *)
			0 <= Subtract[b, d] <= a && Subtract[c+f, Divide[Subtract[b, d]^2, a] + 2.0e] <= D2, (* R[s1hat, 1] \[LessEqual] d^2 *)
			And[
				\[CapitalDelta] > 1.0*^-7, 
				0 <= b*e - c*d <= \[CapitalDelta],
				0 <= a*e - b*d <= \[CapitalDelta],
				Subtract[f, Divide[Subtract[c*d^2+a*e^2, 2.0b*d*e], \[CapitalDelta]]] <= D2
			] (* R[sbar, tbar] \[LessEqual] d^2 *)
		] 
	]
];


(* ::Subsection::Closed:: *)
(*General region intersection*)


regionsIntersectQ[r1_, r2_] := !TrueQ[RegionDisjoint[r1, r2]]


(* ::Section:: *)
(*Utilities*)


(* ::Subsection::Closed:: *)
(*constant3DRegionsQ*)


constant3DRegionsQ = VectorQ[#, RegionQ] && VectorQ[#, RegionEmbeddingDimension[#] === 3&]&;


(* ::Subsection::Closed:: *)
(*iRegionBounds*)


iRegionBounds[CapsuleShape[ps_, r_]] := CoordinateBounds[ps] + r{{-1,1},{-1,1},{-1,1}}
iRegionBounds[reg_] := RegionBounds[reg]


(* ::Subsection::Closed:: *)
(*validReturnTypeQ*)


validReturnTypeQ["Regions"] = True;
validReturnTypeQ["Indices"] = True;
validReturnTypeQ["Count"] = True;
validReturnTypeQ[Automatic] = True;
validReturnTypeQ[All] = True;
validReturnTypeQ[types_List] := Depth[types] === 2 && VectorQ[types, validReturnTypeQ];
validReturnTypeQ[___] = False;


(* ::Subsection::Closed:: *)
(*parseReturnTypes*)


parseReturnTypes[rets_] := Replace[Replace[rets, {Automatic -> "Regions", All -> {"Regions", "Indices", "Count"}}, {0, 1}], l_List :> Flatten[l], {0}]


(* ::Subsection::Closed:: *)
(*gatherRegionData*)


gatherRegionData[regs_, rets_, inds_] := rets /. Dispatch[{"Regions" -> regs[[inds]], "Indices" -> inds, "Count" -> Length[inds]}]


(* ::Subsection::Closed:: *)
(*swapableIntersectorsQ*)


swapableIntersectorsQ[regs1_, regs2_] := 2Length[regs1] < Length[regs2]


(* ::Subsection::Closed:: *)
(*invertIncidence*)


invertIncidence[invinds_, n_, m_] :=
	Block[{adjl, adj},
		adjl = Join @@ MapIndexed[Thread[{#1, First[#2]}]&, invinds];		
		adj = SparseArray[adjl -> ConstantArray[1, Length[adjl]], {n, m}];
		
		adj["MatrixColumns"]
	]


(* ::Section:: *)
(*UTIntersectingRegions*)


(* ::Subsection::Closed:: *)
(*Notes & examples*)


(* ::Text:: *)
(*Find capsules that intersect with another one:*)


(* ::Input:: *)
(*SeedRandom[1];*)
(*caps=Table[CapsuleShape[RandomReal[{-1,1},{2,3}],RandomReal[{0.1,0.3}]],10];*)


(* ::Input:: *)
(*c=CapsuleShape[{{0,0,0},{1,0,0}},0.1];*)


(* ::Input:: *)
(*UTIntersectingRegions[caps,c]*)


(* ::Text:: *)
(*Number of intersecting capsules:*)


(* ::Input:: *)
(*UTIntersectingRegions[caps->"Count",c]*)


(* ::Text:: *)
(*Indices of intersecting capsules:*)


(* ::Input:: *)
(*inds=UTIntersectingRegions[caps->"Indices",c]*)


(* ::Text:: *)
(*Visualize:*)


(* ::Input:: *)
(*Graphics3D[{*)
(*{Blue,c},*)
(*{Opacity[0.7],Red,caps[[inds]]},*)
(*{Green,Delete[caps,List/@inds]}*)
(*}]*)


(* ::Text:: *)
(*Query multiple capsules:*)


(* ::Input:: *)
(*c2=CapsuleShape[{{-0.5,0,0},{-1,0,0}},0.2];*)
(*c3=CapsuleShape[{{-0.5,1,0},{-1,0,0}},0.2];*)


(* ::Input:: *)
(*inds2=UTIntersectingRegions[caps->"Indices",{c,c2,c3}];*)


(* ::Text:: *)
(*Capsules that intersect with c2:*)


(* ::Input:: *)
(*Graphics3D[{*)
(*{Blue,c2},*)
(*{Opacity[0.7],Red,caps[[inds2[[2]]]]},*)
(*{Green,Delete[caps,List/@inds2[[2]]]}*)
(*}]*)


(* ::Text:: *)
(*Request all properties ("Regions", "Indices", "Count"):*)


(* ::Input:: *)
(*UTIntersectingRegions[caps->All,{c,c2,c3}]/._CapsuleShape->CapsuleShape[\[Ellipsis],\[Ellipsis]]//Column*)


(* ::Text:: *)
(*Generate a UTIntersectingRegionsFunction:*)


(* ::Input:: *)
(*irf=UTIntersectingRegions[caps->"Indices"]*)


(* ::Input:: *)
(*irf[{c,c2,c3}]*)


(* ::Subsection::Closed:: *)
(*Main*)


UTIntersectingRegions[regs:Except[_Rule], args___] := UTIntersectingRegions[regs -> "Regions", args]


UTIntersectingRegions[regs1_ -> rets_, regs2_] /; swapableIntersectorsQ[regs1, regs2] :=
	Block[{irets, invinds, inds},
		irets = "Indices";
		invinds = UTIntersectingRegions[regs2 -> irets, regs1];
		(
			inds = invertIncidence[invinds, Length[regs2], Length[regs1]];
			
			gatherRegionData[regs1, rets, #]& /@ inds
			
		) /; invinds =!= $Failed
	]


UTIntersectingRegions[regs1_, regs2_] :=
	Block[{irf, res},
		irf = UTIntersectingRegions[regs1];
		(
			res = irf[regs2];
			
			res /; res =!= $Failed
			
		) /; irf =!= $Failed
	]


UTIntersectingRegions[Rule[regs_?constant3DRegionsQ, rets_?validReturnTypeQ]] :=
	Block[{irets, allbds, bboxTree, bds},
		irets = parseReturnTypes[rets];
		(
			allbds = Developer`ToPackedArray[iRegionBounds /@ regs, Real];
			
			bboxTree = AABBTree[allbds];
			bds = Transpose[MapThread[#1 /@ #2&, {{Min, Max}, Transpose[allbds, {3, 2, 1}]}]];
			
			UTIntersectingRegionsFunction[regs, bboxTree, 3, bds, irets] /; Head[bboxTree] === DataStructure
			
		) /; irets =!= $Failed
	]


UTIntersectingRegions[___] = $Failed;


(* ::Section:: *)
(*UTIntersectingRegionsFunction*)


(* ::Subsection::Closed:: *)
(*Main*)


UTIntersectingRegionsFunction[regs1_, bboxTree_, bds_, dim_, rets_][regs2_?constant3DRegionsQ] :=
	Block[{res, inds},
		res = Catch @ Table[
			inds = intersectingRegions[regs1, bboxTree, reg];
			If[inds === $Failed, Throw[$Failed]];
			gatherRegionData[regs1, rets, inds],
			{reg, regs2}
		];
		
		res /; res =!= $Failed
	]


(irf_UTIntersectingRegionsFunction)[reg:Except[_List]] :=
	Block[{res},
		res = irf[{reg}];
		
		First[res] /; res =!= $Failed
	]


(* ::Subsection::Closed:: *)
(*Formatting*)


UTIntersectingRegionsFunction /: MakeBoxes[expr:UTIntersectingRegionsFunction[regs1_, bboxTree_, dim_, bds_, rets_], fmt_] :=
	Block[{infos},
		infos = {
			{BoxForm`SummaryItem[{"Regions: ", Length[regs1]}]},
			{BoxForm`SummaryItem[{"Dimension: ", dim}]},
			{BoxForm`SummaryItem[{"Bounds: ", Style[bds, Small]}]},
			{BoxForm`SummaryItem[{If[ListQ[rets], "Properties: ", "Property: "], rets}]}
		};
		
		BoxForm`ArrangeSummaryBox[
			UTIntersectingRegionsFunction,
			expr,
			$irficon,
			infos[[1 ;; 2]],
			infos[[3 ;; -1]],
			fmt,
			"Interpretable" -> Automatic
        ]
	]


$irficon = Graphics[
	{
		EdgeForm[Directive[AbsoluteThickness[0.5], Black]], 
		Opacity[0.5],
		{
			Green,
			Triangle[{{0, 0}, {1, 0}, {0, 1}}],
			StadiumShape[{{0.25, 1.1}, {0.35, 1.3}}, 0.1],
			Disk[{1.65, 1.3}, 0.1]
		},
		{
			Red,
			Disk[{1, 1}, 0.5],
			Rectangle[{1.1, 0.1}, {1.7, 0.4}]
		},
		{
			Lighter[Blue, 0.4],
			RegularPolygon[{1.2, 0.7}, 0.5, 5]
		}
	},
	ImageSize -> Dynamic[{Automatic, 3.5Divide[CurrentValue["FontCapHeight"], AbsoluteCurrentValue[Magnification]]}],
	PlotRange -> {{0, 1.75}, {0, 1.5}},
	PlotRangePadding -> None
];
