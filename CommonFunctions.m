(* ::Package:: *)

(* ::Title:: *)
(*CommonFunctions*)


(* ::Section:: *)
(*Begin*)


Package["UTCG`"]


PackageImport["CCompilerDriver`"]


(* ::Section:: *)
(*Compilation Utilities*)


(* ::Subsection::Closed:: *)
(*PackageScopes*)


PackageScope["UTCompile"]
PackageScope["UTFunctionCompile"]


(* ::Subsection::Closed:: *)
(*UTCompile*)


Clear[UTCompile]; 


SetAttributes[UTCompile, HoldAllComplete]; 


UTCompile[sym_, args__] /; !$CCompileQ :=
    (
        sym = Compile[args, Evaluate[$UTCompileOptions]];
        sym = compiledFunctionToLibraryFunction[sym];
        sym
    )


UTCompile[sym_, args__] /; $reCompileQ || TrueQ[UTCG`Developer`$forceCompile] :=
    Block[{name, path},
        name = SymbolName[Unevaluated[sym]];
        path = FileNameJoin[{$CompilePath, name}];
        Quiet[CreateDirectory[path]];
        sym = relocateLibrary[name, path, Compile[args, Evaluate[$UTCompileOptions]]];
        DumpSave[FileNameJoin[{path, name <> ".mx"}], sym];
        sym
    ]


UTCompile[sym_, args___] :=
    Block[{name, UTCG`Developer`$forceCompile},
        name = SymbolName[Unevaluated[sym]];
        UTCG`Developer`$forceCompile =
            Quiet[
                Check[
                    Get[FileNameJoin[{$CompilePath, name, name <> ".mx"}]];
                    False,
                    True
                ]
            ];
        If[TrueQ[UTCG`Developer`$forceCompile],
            Return @ UTCompile[sym, args],
            sym = compiledFunctionToLibraryFunction[sym];
            sym
        ]
    ]


relocateLibrary[name_, path_, cfunc_] :=
    Quiet @
        Block[{ext, newfile},
            cfunc /.
                HoldPattern[LibraryFunction][location_, n_, rest__] :>         
                    With[{lf = (
                                    ext = FileExtension[location];
                                    newfile = FileNameJoin[{path, name <> "." <> ext}];
                                    CopyFile[location, newfile, OverwriteTarget -> True];
                                    LibraryFunction[newfile, n, rest]
                                )
                        },
                        lf /; True
                    ]
        ]


(* ::Text:: *)
(*Convert a CompiledFunction to a LibraryFunction when it's not Listable.*)


compiledFunctionToLibraryFunction[HoldPattern[CompiledFunction][_,_,_,_,_,_, f_Function, ___, lf_LibraryFunction]] /; !listablePureFunctionQ[f] := lf
compiledFunctionToLibraryFunction[cf_] := cf


listablePureFunctionQ[HoldPattern[Function][_, _, atts_]] := !FreeQ[atts, Listable]
listablePureFunctionQ[___] = False;


(* ::Subsection::Closed:: *)
(*Initialization*)


$CCompileQ = Length[CCompilerDriver`CCompilers[]] > 0; 


$CompilePath = FileNameJoin[{$UserBaseDirectory, "ApplicationData", "Compiled Libraries"}]; 


Quiet @
    If[$CCompileQ,
        CreateDirectory[$CompilePath];
        $compileHash = FileNameJoin[{$CompilePath, "hash"}];
        If[!FileExistsQ[$compileHash],
            CreateFile[$compileHash];
        ]
    ]; 


$UTCompileOptions =
    Sequence[
        If[$CCompileQ,
           CompilationTarget -> "C",
            Sequence @@ {}
        ],
        Parallelization -> True,
        RuntimeOptions -> "Speed",
        CompilationOptions -> {"InlineCompiledFunctions" -> True, "InlineExternalDefinitions" -> True}
    ]; 


compileInformationHash[] :=
    Quiet @
        With[{comp = CCompilerDriver`DefaultCCompiler[]},
            Hash[{SubValues[comp], 
                {
                    "AbsoluteFileName", 
                    "CreationDate",
                    "LastAccessDate", 
                    "LastModificationDate"
                } /. FileInformation[
                        FileNameJoin[{
                            comp["Installation"][], 
                            comp["ResolveCompilerName"][Automatic]
                        }]
                    ]}, 
                "SHA256", 
                "ByteArray"
            ]
        ]


Clear[testCompile];


$reCompileQ =
    Quiet @
        Or[
            !$CCompileQ,
            !DirectoryQ[$CompilePath],
            StringQ[$compileHash] && compileInformationHash[] =!= ReadByteArray[$compileHash],
            Get[FileNameJoin[{$CompilePath, "testCompile", "testCompile.mx"}]];
            testCompile[0] =!= 1
        ];


If[$reCompileQ,
    DeleteDirectory[$CompilePath, DeleteContents -> True];
    CreateDirectory[$CompilePath];
    If[StringQ[$compileHash],
        OpenWrite[$compileHash, BinaryFormat -> True];
        BinaryWrite[$compileHash, compileInformationHash[]];
        Close[$compileHash];
    ];
]


If[$CCompileQ,
    UTCompile[testCompile, {{n, _Integer}}, n + 1]
]; 


(* ::Subsection::Closed:: *)
(*UTFunctionCompile*)


$funcCompiledDirectory = FileNameJoin[{DirectoryName[$InputFileName], "Support Files", "Compiled Functions"}];


$funcCompileOpts = Sequence @@ {
	CompilerOptions -> {
		"AbortHandling" -> False, 
		"InlinePolicy" -> "MaximumCallCount" -> 1,
		"LLVMOptimization" -> "ClangOptimization"[3], 
		"OptimizationLevel" -> 3
	},
	TargetSystem -> All,
	UseEmbeddedLibrary -> True
};


SetAttributes[UTFunctionCompile, HoldAllComplete];


Options[UTFunctionCompile] = Options[FunctionCompile];


UTFunctionCompile[sym_, ___] :=
	Block[{file},
		file = FileNameJoin[{$funcCompiledDirectory, SymbolName[Unevaluated[sym]] <> ".wl"}];
		(
			Quiet[sym = Get[file];];
			Quiet[sym = sym;];
			
			sym /; Head[sym] === CompiledCodeFunction
			
		) /; FileExistsQ[file]
	]


UTFunctionCompile[sym_, func_, opts:Longest[OptionsPattern[]]] := UTFunctionCompile[sym, {}, func, opts]


UTFunctionCompile[sym_, decl_, func_, opts:Longest[OptionsPattern[]]] :=
	Block[{file},
		Quiet[sym = FunctionCompile[decl, func, opts, $funcCompileOpts]];
		(
			If[!DirectoryQ[$funcCompiledDirectory],
				CreateDirectory[$funcCompiledDirectory]
			];
			
			file = FileNameJoin[{$funcCompiledDirectory, SymbolName[Unevaluated[sym]] <> ".wl"}];
			Put[sym, file];
			
			sym
			
		) /; Head[sym] === CompiledCodeFunction
	]


UTFunctionCompile[___] = $Failed;


(* ::Section:: *)
(*Automatic argument conforming*)


(* ::Subsection::Closed:: *)
(*PackageScopes*)


PackageScope["registerForDiscretization"]
PackageScope["registerForSolid"]
PackageScope["importRegion"]


(* ::Subsection::Closed:: *)
(*registerForDiscretization*)


(* ::Subsubsection::Closed:: *)
(*Main*)


registerForDiscretization[func_Symbol, pos_:{{}}, triangulateQ_:True] :=
	Block[{},
		func[args__] /; TrueQ[UTCG`Developer`$UTTriangulateArguments] := 
			With[{res = tryTriangulation[pos, triangulateQ, func, args]},
				res /; res =!= $Failed
			]
	]


(* ::Subsubsection::Closed:: *)
(*Triangulation dispatch*)


$triangulationFailure = "triangulationFailure";


SetAttributes[{tryTriangulation, iTryTriangulation, triangulateFullDepth, heldTriangulatedArgument}, HoldAllComplete];


tryTriangulation[pos_, triangulateQ_, func_, args__] := 
	Catch[
		iTryTriangulation[pos, triangulateQ, func, args], 
		$triangulationFailure
	]


iTryTriangulation[pos_, triangulateQ_, func_, args__] := 
	Block[{targs, changeQ, reps, $UTMessageHead = func, $TriangulateQ = triangulateQ},
		{targs, changeQ} = Reap[Extract[HoldComplete[args], pos, triangulateFullDepth]];
		(
			reps = Which[
				pos === {{}}, pos -> First[targs],
				ListQ[targs], Thread[pos -> targs],
				True, pos -> targs
			];
			
			func @@ (ReplacePart[HoldComplete[args], reps] //. heldTriangulatedArgument[e_] :> e)
		
		) /; changeQ =!= {}
	]

iTryTriangulation[___] := Throw[$Failed, $triangulationFailure]


triangulateFullDepth[HoldPattern[HoldComplete][expr_]] := HoldComplete @@ {triangulateFullDepth[expr]}

triangulateFullDepth[expr_] :=
	With[{validQ = If[TrueQ[!$TriangulateQ], UTSurfaceMesh3DQ, UTTriangleMesh3DQ]},
	
		heldTriangulatedArgument[expr] /. {
			mr_MeshRegion?validQ :> mr,
			bmr_BoundaryMeshRegion?validQ :> bmr,
			r_ /; Quiet[ConstantRegionQ[Unevaluated[r]] && RegionEmbeddingDimension[r] === 3] :> With[{res = triangulateRegion[r]}, res /; Sow[True]],
			file:(_String|_File) /; FileExistsQ[FindFile[file]] :> With[{res = importTriangulatedRegion[file]}, res /; Sow[True]]
		}
		
	]


triangulateRegion[reg_] /; RegionEmbeddingDimension[reg] =!= 3 := reg

triangulateRegion[reg_?UTTriangleMesh3DQ] := reg

triangulateRegion[reg_] := 
	Block[{},
		triangulationMessage[reg];
		UTLaxBlock @ iTriangulateRegion[reg, Quiet @ RegionDimension[reg]]
	]

iTriangulateRegion[mr_?UTSurfaceMesh3DQ, _] := If[TrueQ[!$TriangulateQ], mr, toTriangleMesh[mr]]

iTriangulateRegion[mr_?UTMesh3DQ, _] := toTriangleMesh[mr]

iTriangulateRegion[reg_, 2] := iTriangulateRegion[discretizeSurface[reg], 2]

iTriangulateRegion[reg_, 3] := iTriangulateRegion[discretizeVolume[reg], 3]

iTriangulateRegion[reg_, ___] := Throw[$Failed, $triangulationFailure]


(* ::Subsubsection::Closed:: *)
(*Triangulation*)


toTriangleMesh[mr_?UTTriangleMesh3DQ] := mr


toTriangleMesh[mr_?UTSurfaceMesh3DQ] :=
	With[{res = Quiet @ Region`Mesh`TriangulateMeshCells[mr, triangulationOptions]},
		res /; UTTriangleMesh3DQ[res]
	]


toTriangleMesh[mr_?FullDimensionalMesh3DQ] :=
	With[{res = Quiet @ UTToBMR[RegionBoundary[mr]]},
		toTriangleMesh[res] /; UTSurfaceMesh3DQ[res]
	]


toTriangleMesh[___] := Throw[$Failed, $triangulationFailure]


FullDimensionalMesh3DQ[mr_] := MeshRegionQ[mr] && RegionDimension[mr] === 3 && FreeQ[mr["MeshCellTypes"], Point | Line | Polygon]


(* ::Subsubsection::Closed:: *)
(*Discretization*)


discretizeSurface[reg_] :=
	With[{res = Quiet @ DiscretizeRegion[reg, surfaceDiscretizationOptions]},
		res /; MeshRegionQ[res]
	]


discretizeSurface[___] := Throw[$Failed, $triangulationFailure]


discretizeVolume[reg_] :=
	Block[{res = Quiet @ BoundaryDiscretizeRegion[reg, volumeDiscretizationOptions]},
		res /; BoundaryMeshRegionQ[res]
	]


discretizeVolume[___] := Throw[$Failed, $triangulationFailure]


(* ::Subsubsection::Closed:: *)
(*File import*)


importTriangulatedRegion[file_] := 
	Block[{reg},
		reg = importRegion[file];
		(
			If[!UTMesh3DQ[reg], 
				reg = Quiet @ DiscretizeRegion[reg, surfaceDiscretizationOptions]
			];
			
			reg /; UTMeshQ[reg]
			
		) /; RegionQ[reg]
	]


importTriangulatedRegion[___] := Throw[$Failed, $triangulationFailure]


importRegion[File[file_]] := importRegion[file]


importRegion[file_String?FileExistsQ] := importRegion[file, ToLowerCase[FileExtension[file]]]


importRegion[file_, "stl"] := 
	With[{mr = UTCG`Legacy`UTImportSTL[file]},
		mr /; MeshRegionQ[mr]
	]


importRegion[file_, "ply"] :=
	With[{mr = UTCG`Legacy`UTImportPLY[file]},
		mr /; MeshRegionQ[mr]
	]


importRegion[file_, Except["stl"|"ply"]] := 
	Block[{elements, res},
		elements = Import[file, "Elements"];
		If[!FreeQ[elements, "MeshRegion", {1}],
			res = Import[file, "MeshRegion"];,
			res = Import[file];
		];
		(
			If[Head[res] === Graphics3D,
				res = Quiet @ DiscretizeGraphics[res, surfaceDiscretizationOptions]
			];
			
			res /; RegionQ[res]
			
		) /; RegionQ[res] || Head[res] === Graphics3D
	]


importRegion[___] = $Failed;


(* ::Subsubsection::Closed:: *)
(*Options extractors*)


triangulationOptions /; OptionQ[{UTCG`Developer`$UTTriangulationOptions}] := FilterRules[{UTCG`Developer`$UTTriangulationOptions}, Options[Region`Mesh`TriangulateMeshCells]]
triangulationOptions /; !OptionQ[{UTCG`Developer`$UTTriangulationOptions}] = Sequence[];

surfaceDiscretizationOptions /; OptionQ[{UTCG`Developer`$UTTriangulationOptions}] := FilterRules[{UTCG`Developer`$UTTriangulationOptions}, Options[DiscretizeRegion]]
surfaceDiscretizationOptions /; !OptionQ[{UTCG`Developer`$UTTriangulationOptions}] = Sequence[];

volumeDiscretizationOptions /; OptionQ[{UTCG`Developer`$UTTriangulationOptions}] := FilterRules[{UTCG`Developer`$UTTriangulationOptions}, Options[BoundaryDiscretizeRegion]]
volumeDiscretizationOptions /; !OptionQ[{UTCG`Developer`$UTTriangulationOptions}] = Sequence[];


(* ::Subsubsection::Closed:: *)
(*User flags*)


UTCG`Developer`$UTTriangulateArgumentsMessageQ = False;
UTCG`Developer`$UTTriangulateArguments = True;
UTCG`Developer`$UTTriangulationOptions = {MeshQualityGoal -> "Maximal"};


(* ::Subsubsection::Closed:: *)
(*Triangulation messages*)


SetAttributes[triangulationMessage, HoldFirst];

triangulationMessage[reg_] /; UTCG`Developer`$UTTriangulateArgumentsMessageQ := 
	With[{sym = $UTMessageHead}, Message[sym::"uttri", reg]]

$UTMessageHead = General;
General::"uttri" = "Converting `1` to a triangulated surface mesh representation.";


(* ::Subsection::Closed:: *)
(*registerForSolid*)


(* ::Subsubsection::Closed:: *)
(*Main*)


PackageScope["registerForSolid"]


registerForSolid[func_Symbol, pos_:{{}}] :=
	Block[{},
		func[args__] := 
			With[{res = tryToBMR[pos, func, args]},
				res /; res =!= $Failed
			]
	]


(* ::Subsubsection::Closed:: *)
(*ToBMR*)


$toBMRFailure = "toBMRFailure";


SetAttributes[{tryToBMR, iTryToBMR, toBMRFullDepth, heldToBMRArgument}, HoldAllComplete];


tryToBMR[pos__, func_, args__] := 
	Catch[
		iTryToBMR[pos, func, args], 
		$toBMRFailure
	]


iTryToBMR[pos_, func_, args__] := 
	Block[{targs, changeQ, reps},
		{targs, changeQ} = Reap[Extract[HoldComplete[args], pos, toBMRFullDepth]];
		(
			reps = Which[
				pos === {{}}, pos -> First[targs],
				ListQ[targs], Thread[pos -> targs],
				True, pos -> targs
			];
			
			func @@ (ReplacePart[HoldComplete[args], reps] //. heldToBMRArgument[e_] :> e)
		
		) /; changeQ =!= {}
	]

iTryToBMR[___] := Throw[$Failed, $toBMRFailure]


toBMRFullDepth[HoldPattern[HoldComplete][expr_]] := HoldComplete @@ {toBMRFullDepth[expr]}

toBMRFullDepth[expr_] :=
	Block[{},
		heldToBMRArgument[expr] /. {
			mr_MeshRegion?UTSurfaceMesh3DQ :> With[{res = makeSolid[mr]}, res /; Sow[True]]
		}
	]


makeSolid[mr_] := 
	With[{bmr = UTToBMR[mr]},
		bmr /; BoundaryMeshRegionQ[bmr]
	]

makeSolid[___] := Throw[$Failed, $toBMRFailure]


(* ::Section:: *)
(*External Package Loaders*)


(* ::Subsection::Closed:: *)
(*PackageScopes*)


PackageScope["loadIGraphMQ"]
PackageScope["loadOpenVDBLinkQ"]
PackageScope["loadTetGenQ"]


(* ::Subsection::Closed:: *)
(*IGraphM*)


loadIGraphMQ :=  
	Block[{loadQ},
		loadQ = Quiet[Check[Block[{Print}, <<IGraphM`];True, False]];
		If[loadQ, Clear[loadIGraphMQ]; loadIGraphMQ = True];
		loadQ
	]


(* ::Subsection::Closed:: *)
(*VDB*)


loadOpenVDBLinkQ :=  
	Block[{loadQ},
		loadQ = If[FileExistsQ[OpenVDBLink`$OpenVDBLibrary],
			True,
			$Path = DeleteDuplicates[Append[$Path, FileNameJoin[{FileNameDrop[$InputFileName, -2], "OpenVDBLink"}]]];
			Quiet[Check[Block[{Print}, <<OpenVDBLink`];True, False]]
		];
		If[loadQ, Clear[loadOpenVDBLinkQ]; loadOpenVDBLinkQ = True];
		loadQ
	]


(* ::Subsection::Closed:: *)
(*TetGen*)


loadTetGenQ := Quiet[Get["TetGenLink`"]; Clear[loadTetGenQ]; loadTetGenQ = True]


(* ::Section:: *)
(*Code Completion*)


(* ::Subsection::Closed:: *)
(*PackageScopes*)


PackageScope["addCodeCompletion"]


(* ::Subsection::Closed:: *)
(*addCodeCompletion*)


(* ::Text:: *)
(*https://resources.wolframcloud.com/FunctionRepository/resources/AddCodeCompletion*)


addCodeCompletion[sym_Symbol][args___] := addCodeCompletion[SymbolName[sym]][args]


addCodeCompletion[function_String][args___] := 
	With[{processed = {args} /. {
			None -> 0, "AbsoluteFileName" -> 2, "RelativeFileName" -> 3, 
			"Color" -> 4, "PackageName" -> 7, "DirectoryName" -> 8, 
			"InterpreterType" -> 9}
		}, 
		FE`Evaluate[FEPrivate`AddSpecialArgCompletion[function -> processed]];
	]


(* ::Subsection::Closed:: *)
(*System symbols*)


(* ::Subsubsection::Closed:: *)
(*RepairMesh*)


addCodeCompletion["RepairMesh"][
	None, 
	{"HoleEdges", "FlippedFaces", "SingularEdges", "SingularVertices", "TinyComponents", "TinyFaces", "OverlappingFaces", "TJunctionEdges", "DanglingEdges", "IsolatedVertices"}
];


(* ::Subsubsection::Closed:: *)
(*FindMeshDefects*)


addCodeCompletion["FindMeshDefects"][
	None, 
	{"HoleEdges", "FlippedFaces", "SingularEdges", "SingularVertices", "TinyComponents", "TinyFaces", "OverlappingFaces", "TJunctionEdges", "DanglingEdges", "IsolatedVertices"},
	{"Cell", "CellIndex"}
];


(* ::Section:: *)
(*Utilities*)


(* ::Subsection::Closed:: *)
(*PackageScopes*)


PackageScope["UTMeshQ"]
PackageScope["UTMesh3DQ"]
PackageScope["UTTriangleMesh3DQ"]
PackageScope["UTSurfaceMesh3DQ"]
PackageScope["UTLineMesh3DQ"]
PackageScope["UTLineMesh2DQ"]
PackageScope["UTMeshCollectionQ"]
PackageScope["UTTriangleMesh3DCollectionQ"]
PackageScope["FastMeshRegion"]
PackageScope["FastBoundaryMeshRegion"]
PackageScope["iTriangulateMesh"]


(* ::Subsection::Closed:: *)
(*UTMeshQ*)


UTMeshQ[expr_] := MeshRegionQ[expr] || BoundaryMeshRegionQ[expr]


(* ::Subsection::Closed:: *)
(*UTMesh3DQ*)


UTMesh3DQ[expr_] := (MeshRegionQ[expr] || BoundaryMeshRegionQ[expr]) && RegionEmbeddingDimension[expr] === 3


(* ::Subsection::Closed:: *)
(*UTTriangleMesh3DQ*)


UTTriangleMesh3DQ[mr_] := UTMesh3DQ[mr] && MatchQ[mr["MeshCellTypes"], {{{Polygon, {_, 3}}..}..}]


(* ::Subsection::Closed:: *)
(*UTSurfaceMesh3DQ*)


UTSurfaceMesh3DQ[mr_] := UTMesh3DQ[mr] && MatchQ[mr["MeshCellTypes"], {{{Polygon, _}..}..}]


(* ::Subsection::Closed:: *)
(*UTLineMesh3DQ*)


UTLineMesh3DQ[mr_] := UTMesh3DQ[mr] && MatchQ[mr["MeshCellTypes"], {{{Line, _}..}..}]


(* ::Subsection::Closed:: *)
(*UTLineMesh2DQ*)


UTLineMesh2DQ[mr_] := UTMeshQ[mr] && RegionEmbeddingDimension[mr] === 2 && MatchQ[mr["MeshCellTypes"], {{{Line, _}..}..}]


(* ::Subsection::Closed:: *)
(*UTMeshCollectionQ*)


UTMeshCollectionQ[_?UTMeshQ] = True;
UTMeshCollectionQ[l_List] := VectorQ[l, UTMeshCollectionQ]
UTMeshCollectionQ[___] = False;


(* ::Subsection::Closed:: *)
(*UTTriangleMesh3DCollectionQ*)


UTTriangleMesh3DCollectionQ[_?UTTriangleMesh3DQ] = True;
UTTriangleMesh3DCollectionQ[l_List] := VectorQ[l, UTTriangleMesh3DCollectionQ]
UTTriangleMesh3DCollectionQ[___] = False;


(* ::Subsection::Closed:: *)
(*FastMeshRegion*)


FastMeshRegion[args__] := 
	UTFastBlock @ Block[{res},
		res = MeshRegion[args];
		If[!MeshRegionQ[res],
			$Failed,
			res
		]
	]


(* ::Subsection::Closed:: *)
(*FastBoundaryMeshRegion*)


FastBoundaryMeshRegion[args__] := 
	UTFastBlock @ Block[{res},
		res = BoundaryMeshRegion[args];
		If[!BoundaryMeshRegionQ[res],
			$Failed,
			res
		]
	]


(* ::Subsection::Closed:: *)
(*iTriangulateMesh*)


Options[iTriangulateMesh] = Options[TriangulateMesh];


iTriangulateMesh[mr_, opts:OptionsPattern[]] :=
	With[{tri = triangulateMesh[mr, opts]},
		tri /; MeshRegionQ[tri]
	]


iTriangulateMesh[___] = $Failed;


If[$VersionNumber < 12.2,
	triangulateMesh[mr_, opts___] := TriangulateMesh[mr, opts, "SteinerPoints" -> None],
	triangulateMesh[mr_, opts___] := TriangulateMesh[mr, opts, Method -> "ConstrainedDelaunay"]
]


(* ::Section:: *)
(*General Utilities*)


(* ::Subsection::Closed:: *)
(*PackageScopes*)


PackageScope["GatherByList"]
PackageScope["GatherByLength"]


(* ::Subsection::Closed:: *)
(*GatherBy Utilities*)


(* ::Text:: *)
(*https://mathematica.stackexchange.com/a/158637/4346*)


GatherByList[list_, representatives_] := Module[{func},
    func /: Map[func, _] := representatives;
    GatherBy[list, func]
]


GatherByLength[list_] := Developer`ToPackedArray /@ GatherByList[list, Length /@ list]


(* ::Section:: *)
(*AABB Tree*)


(* ::Subsection::Closed:: *)
(*PackageScopes*)


PackageScope["AABBTree"]
PackageScope["OverlappingBBoxes"]
PackageScope["$AABBbinarytreetype"]


(* ::Subsection::Closed:: *)
(*Notes*)


(* ::Text:: *)
(*AABBTree[{{{x11,x12},{y11,y12},{z11,z12}}, {{x21,x22},{y21,y22},{z21,z22}}, ...}] gives an AABB tree for {Cuboid[{x11,y11,z11},{x12,y12,z12}], Cuboid[{x21,y21,z21},{x22,y22,z22}], ...}.*)


(* ::Text:: *)
(*OverlappingBBoxes[tree, {{x1, x2}, {y1, y2}, {z1, z2}}] returns the indices of all cuboids in tree that intersect the given bounding box.*)


(* ::Input:: *)
(*tree=AABBTree[{{{0,1},{0,1},{0,1}},{{0.5,1.5},{0.25,1.5},{0.3,0.7}},{{0.7,0.8},{0.8,2},{0.2,0.8}}}];*)
(*OverlappingBBoxes[tree,{{0.6,0.9},{1.1,1.4},{0.6,0.9}}]*)


(* ::Text:: *)
(*This returns {2, 3} since the bounding box intersects the 2nd and 3rd cuboid, but not the first.*)


(* ::Input:: *)
(*Graphics3D[{*)
(*{Red,Cuboid[{0,0,0},{1,1,1}]},*)
(*{Green,Cuboid[{.5,.25,.3},{1.5,1.5,.7}]},*)
(*{Blue,Cuboid[{.7,.8,.2},{.8,2,.8}]},*)
(*{Yellow,Cuboid[{.6,1.1,.6},{.9,1.4,.9}]}*)
(*}]*)


(* ::Subsection::Closed:: *)
(*Utilities*)


$AABBbinarytreetype = "BinaryTree"::["PackedArray"::["Real64", 1]];


(* ::Subsection::Closed:: *)
(*AABBTree*)


declnode = FunctionDeclaration[
	centerNode3D,
	Typed[{"MachineInteger", "PackedArray"::["Real64", 2], "Real64", "Real64"} -> $AABBbinarytreetype] @ Function[{d, bboxestagged, xl, xr},
		Native`UncheckedBlock@Module[{splitdata},
			splitdata = ConstantArray[0., 7];
			splitdata[[1]] = Cast[d, "Real64"];
			splitdata[[2]] = xl;
			splitdata[[3]] = xr;
			splitdata[[4]] = Min[bboxestagged[[All, 2d-1]]];
			splitdata[[5]] = Max[bboxestagged[[All, 2d]]];
			CreateDataStructure["BinaryTree", Flatten[Append[bboxestagged[[Ordering[bboxestagged[[All, 2Mod[d+1, 3, 1]-1]]]]], splitdata]]]
		]
	]
];


declnodeempty = FunctionDeclaration[
	centerNodeEmpty3D,
	Typed[{"MachineInteger", "Real64", "Real64"} -> $AABBbinarytreetype] @ Function[{d, xl, xr},
		Native`UncheckedBlock@Module[{splitdata},
			splitdata = ConstantArray[0., 7];
			splitdata[[1]] = Cast[d, "Real64"];
			splitdata[[2]] = xl;
			splitdata[[3]] = xr;
			CreateDataStructure["BinaryTree", splitdata]
		]
	]
];


pureAABBTree = Function[{Typed[bboxes, "PackedArray"::["Real64", 3]]},
	Native`UncheckedBlock@Module[{iAABBTree, bboxestagged},
		iAABBTree = Function[{Typed[d, "MachineInteger"], Typed[bboxestagged, "PackedArray"::["Real64", 2]]},
			Native`UncheckedBlock@Module[{len, n, mnp, mxp, loc, r = 0, l = 0, Sleft, Scent, Sright, node, x, xl, xr},
				len = Length[bboxestagged];
				
				mxp = 2d;
				mnp = mxp-1;
				
				n = Ordering[bboxestagged[[All, mnp]]][[Quotient[len+1, 2]]];
				x = bboxestagged[[n, mnp]];
				
				If[len <= 50, Return[centerNode3D[d, bboxestagged, x, x]]];
				
				loc = ConstantArray[0, Length[bboxestagged]];
				xl = xr = x;
				Do[
					If[bboxestagged[[i, mxp]] < x,
						l++;
						loc[[i]] = 1,
						If[bboxestagged[[i, mnp]] <= x,
							l++;
							xl = Max[xl, bboxestagged[[i, mxp]]];
							loc[[i]] = 2;,
							r++;
						]
					],
					{i, Length[bboxestagged]}
				];
				
				Sleft = ConstantArray[0., {l, 7}];
				Sright = ConstantArray[0., {r, 7}];
				
				r = l = 1;
				Do[
					If[loc[[i]] === 0,
						Sright[[r++]] = bboxestagged[[i]];,
						Sleft[[l++]] = bboxestagged[[i]];
					],
					{i, Length[bboxestagged]}
				];
				
				node = centerNodeEmpty3D[d, xl, xr];
				
				If[l > 1, BinaryTree`SetLeft[node, iAABBTree[Mod[d+1, 3, 1], Sleft]]];
				If[r > 1, BinaryTree`SetRight[node, iAABBTree[Mod[d+1, 3, 1], Sright]]];
				
				node
			]
		];
		
		bboxestagged = Table[Append[Flatten[bboxes[[i]]], Cast[i, "Real64"]], {i, Length[bboxes]}][[Ordering[bboxes[[All, 1, 1]]]]];
		
		iAABBTree[1, bboxestagged]
	]
];


UTFunctionCompile[AABBTree, {declnode, declnodeempty}, pureAABBTree];


(* ::Subsection::Closed:: *)
(*OverlappingBBoxes*)


With[{$btype = $AABBbinarytreetype},
pureOverlappingBBoxes = Function[
    {
       Typed[tree, $btype], 
       Typed[bbox, "PackedArray"::["Real64", 2]]
    },
    Native`UncheckedBlock@Block[{ilen, icap, inds, stack, x1, x2, y1, y2, z1, z2, chcnt, node, data, d, a, b, len7, len, j, d1, b1, lo, hi, rl, rr},
       ilen = 0; icap = 16;
       inds = ConstantArray[0, icap];
       stack = TypeHint[CreateDataStructure["LinkedList"], "LinkedList"::[$btype]];

       x1 = bbox[[1, 1]]; x2 = bbox[[1, 2]];
       y1 = bbox[[2, 1]]; y2 = bbox[[2, 2]];
       z1 = bbox[[3, 1]]; z2 = bbox[[3, 2]];
       
       chcnt = 1;
       node = tree;
       While[chcnt === 1 || !stack["EmptyQ"],
         If[chcnt =!= 1, 
           node = stack["DropFirst"];
         ];
         data = node["Data"];
         len7 = Length[data];
         len = Quotient[len7, 7];
         
         d = Cast[data[[len7-6]], "MachineInteger", "CCast"];
         a = bbox[[d, 1]];
         b = bbox[[d, 2]];
         
         chcnt = 0;
         If[len === 1,
           (* len === 1 means we traverse the tree *)
           rl = a < data[[len7-5]] && !node["LeftNullQ"];
           rr = data[[len7-4]] < b && !node["RightNullQ"];
           
           (* only insert children into the stack when both children need to be traversed *)
           If[rl, chcnt++];
           If[rr, chcnt++];
           
           If[chcnt === 2, 
             stack["Prepend", node["Left"]];
             stack["Prepend", node["Right"]],
             If[rl, node = node["Left"]];
             If[rr, node = node["Right"]];
           ];,
           
           (* len =!= 1 means we test the bboxes in the node *)
           If[a <= data[[len7-2]] && data[[len7-3]] <= b,
           
            If[len <= 9,
              j = len-1,
              d1 = Mod[d+1, 3, 1];
              b1 = bbox[[d1, 2]];
              lo = 1; hi = len-1;
              j = Quotient[len, 2];
              While[lo =!= j && j =!= hi,
                If[b1 < data[[7j+2d1-8]], 
                  hi = j;
                  j = Quotient[lo+j, 2];,
                  lo = j;
                  j = Quotient[hi+j, 2];
                ];
              ];
              j = Min[j+1, len-1];
            ];
           
            Do[
              If[data[[i-6]] <= x2 && x1 <= data[[i-5]] && data[[i-4]] <= y2 && y1 <= data[[i-3]] && data[[i-2]] <= z2 && z1 <= data[[i-1]],
                 ilen++;
                 If[ilen > icap,
                   icap *= 2;
                   inds = Join[inds, inds];
                 ];
                 Native`SetArrayElementUnary[inds, ilen-1, Cast[data[[i]], "MachineInteger", "CCast"]];
              ];,
              {i, 7, 7j, 7}
            ];
           ];
         ];
       ];
       
       If[ilen > 0,
         inds[[1 ;; ilen]],
         TypeHint[Most[{0}], "PackedArray"::["MachineInteger", 1]]
       ]
    ]
]] /. HoldPattern[Part][a_, b_, c_] :> Native`GetPartBinary[a, b, c];


UTFunctionCompile[OverlappingBBoxes, pureOverlappingBBoxes];


(* ::Section:: *)
(*System*)


(* ::Subsection::Closed:: *)
(*Degenerate cell messages*)


Off[MeshRegion::dgcell, BoundaryMeshRegion::dgcell, MeshRegion::dgcellr, BoundaryMeshRegion::dgcellr];


(* ::Subsection::Closed:: *)
(*MeshRegion / BoundaryMeshRegion head interchange*)


(* ::Text:: *)
(*Sometimes when adding stylistic options such as MeshRegion[mr, ImageSize -> 512, PlotTheme -> "SmoothShading"] one might forget if mr is a MeshRegion or BoundaryMeshRegion.*)


Internal`WithLocalSettings[
	Unprotect[BoundaryMeshRegion, MeshRegion],
	BoundaryMeshRegion[mr_MeshRegion?MeshRegionQ, opts:OptionsPattern[]] := MeshRegion[mr, opts];
	MeshRegion[bmr_BoundaryMeshRegion?BoundaryMeshRegionQ, opts:OptionsPattern[]] := BoundaryMeshRegion[bmr, opts];,
	Protect[BoundaryMeshRegion, MeshRegion]
]


(* ::Subsection::Closed:: *)
(*EmptyRegion*)


Internal`WithLocalSettings[
	Unprotect[EmptyRegion],
	EmptyRegion /: MeshCellCount[_EmptyRegion, _] = 0;
	EmptyRegion /: MeshCellCount[_EmptyRegion] = {},
	Protect[EmptyRegion]
];


(* ::Subsection::Closed:: *)
(*Overload Mesh Property symbols*)


With[{mcp = {MeshCellCentroid, MeshCellMeasure, MeshCellQuality}},
	Internal`WithLocalSettings[
		Unprotect[mcp],
		Scan[(#[args__] := PropertyValue[{args}, #])&, mcp];
		Scan[(#[mr_?UTTriangleMesh3DQ] := PropertyValue[{mr, 2}, #])&, mcp];,
		Protect[mcp]
	]
]


(* ::Subsection::Closed:: *)
(*Overload MeshCoordinates*)


Internal`WithLocalSettings[
	Unprotect[MeshCoordinates],
	SyntaxInformation[MeshCoordinates] = {"ArgumentsPattern" -> {_, _.}};
	MeshCoordinates[mr_, spec_] := With[{pts = Quiet[MeshCoordinates[mr][[spec]]]}, pts /; ListQ[pts]],
	Protect[MeshCoordinates]
];


(* ::Subsection::Closed:: *)
(*BoundaryMeshRegion construction hang V12.2+*)


Internal`WithLocalSettings[
	Unprotect[BoundaryMeshRegion],
	BoundaryMeshRegion[coords_List?MatrixQ, rest___] /; Length[coords[[1]]] == 3 && !TrueQ[inBMRQ] :=
		Internal`InheritedBlock[{inBMRQ = True, Region`Mesh`CrossingCount},
			Unprotect[Region`Mesh`CrossingCount];
			Region`Mesh`CrossingCount[poly_, pts:{pt_List, ___}] := ConstantArray[Region`Mesh`CrossingCount[poly, pt], Length[pts]];
			BoundaryMeshRegion[coords, rest]
		],
	Protect[BoundaryMeshRegion]
]
