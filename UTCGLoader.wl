(* ::Package:: *)

(* ::Title:: *)
(*UTCGPrintLibrary*)


(* ::Subtitle:: *)
(*Greg Hurst, United Therapeutics*)


(* ::Subsubtitle:: *)
(*ghurst@unither.com*)


(* ::Subsubtitle:: *)
(*2018\[ThinSpace]-\[ThinSpace]2024*)


(* ::Section:: *)
(*Load*)


(* ::Subsection:: *)
(*Clear*)


ClearAll @@ Complement[Join[Names["UTCG`*"], Names["UTCG`*`*"]], Names["UTCG`Developer`*"]];


(* ::Subsection:: *)
(*Get*)


Get["CommonFunctions.m"];


(* ::Subsection:: *)
(*Sort Options*)


With[{iValueQ = If[$VersionNumber < 12.2, ValueQ, ValueQ[#, Method -> "Legacy"]&]},
	Quiet @ ToExpression[
		Names["UTCG`*"], 
		InputForm, 
		Function[s, If[!iValueQ[s], Options[s] = SortBy[Options[s], First];], HoldFirst]
	];
]
