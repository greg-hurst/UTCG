(* ::Package:: *)

(* ::Title:: *)
(*Display*)


(* ::Section:: *)
(*Begin*)


Package["UTCG`"]


PackageExport["UTDisplay"]
PackageExport["UTFixAspectRatio"]


UTDisplay::usage = "Display a 3D object with custom lighting and face effects.";
UTFixAspectRatio::usage = "Display a mesh with the smallest possible padding to accomidate all orientations during rotation.";


(* ::Section:: *)
(*Display*)


(* ::Subsection::Closed:: *)
(*Main*)


UTDisplay[expr_] := UTDisplay[expr, Automatic, Automatic]
UTDisplay[expr_, type_] := UTDisplay[expr, type, Automatic]


UTDisplay[expr_?validDisplayQ, icolor_, itype_] :=
	Block[{type, color, res},
		type = display3DType[itype, expr];
		(
			color = display3DColor[icolor];
			(
				res = iDisplay[expr, color, type];
				
				res /; res =!= $Failed
				
			) /; ColorQ[color]
			
		) /; StringQ[type]
	]


UTDisplay[expr_, ___] := expr


(* ::Subsection::Closed:: *)
(*Argument conform & completion*)


addCodeCompletion[UTDisplay][None, None, {"Solid", "Transparent", "Peel"}];


(* ::Subsection::Closed:: *)
(*iDisplay*)


iDisplay[{coords_List, cells_List}, args__] /; MatrixQ[coords, Internal`RealValuedNumericQ] && 2 <= Length[coords] <= 3 && cellSpecQ[cells, Length[coords]] := 
	displayComplex[{coords, cells}, args]


iDisplay[reg_?RegionQ, args__] := displayRegion[reg, args]


iDisplay[im3d_Image3D, args__] := displayImage3D[im3d, args]


iDisplay[file:(_File|_String)?FileExistsQ, args__] := 
	With[{reg = importRegion[file]},
		iDisplay[reg, args] /; RegionQ[reg]
	]


iDisplay[loc:(_File|_String) /; FileExistsQ[loc] || DirectoryQ[loc], args__] := 
	With[{res = displayPNGStack[loc, args, Automatic]},
		res /; res =!= $Failed
	]


iDisplay[{loc:(_File|_String) /; FileExistsQ[loc] || DirectoryQ[loc], {pxl_, z_}}, args__] := 
	With[{res = displayPNGStack[loc, args, {pxl, z}]},
		res /; res =!= $Failed
	]


iDisplay[lis_List, args__] /; !MatchQ[lis, {_, {__?Positive}}] :=
	Block[{res},
		res = iDisplay[#, args]& /@ lis;
		
		Show[res] /; FreeQ[res, $Failed, {1}]
	]


iDisplay[___] = $Failed;


(* ::Subsection::Closed:: *)
(*displayComplex*)


cellSpecQ[n_, cells_List] := VectorQ[cells, cellSpecQ[n, #]&]


cellSpecQ[n_, (Polygon|Triangle|Line|Point|Tetrahedron|Hexahedron|Pyramid|Prism|Simplex)[cell_]] := 
	(VectorQ[cell, IntegerQ] || MatrixQ[cell, IntegerQ] || VectorQ[cell, VectorQ[#, IntegerQ]&]) && Min[cell] > 0 && Max[cell] <= n


cellSpecQ[___] = False;


displayComplex[{coords_, cells_}, color_, type_] :=
	Block[{base},
		
		base = Switch[type,
			"Transparent", Directive[Opacity[0.3], color],
			"Peel", FaceForm[Opacity[0], Directive[Opacity[1], color]],
			_, color
		];
		
		Graphics3D[
			GraphicsComplex[coords, {AbsoluteThickness[0.75], EdgeForm[], base, cells}],
			Lighting -> Prepend[{"Directional", GrayLevel[0.3], ImageScaled[#]}& /@ {{2,0,2},{2,2,2},{0,2,2}}, {"Ambient", GrayLevel[0.45]}]
		]
	]


(* ::Subsection::Closed:: *)
(*displayRegion*)


displayRegion[reg_ /; RegionEmbeddingDimension[reg] == 3, color_, type_] :=
	Block[{meshQ, head, base},
		meshQ = UTMesh3DQ[reg];
		head = If[meshQ, Head[reg], Region];
		
		base = Switch[type,
			"Transparent", Directive[Opacity[0.3], color],
			"Peel", FaceForm[Opacity[0], Directive[Opacity[1], color]],
			_, color
		];
		
		Show[
			head[reg, BaseStyle -> {EdgeForm[], base}, PlotTheme -> "Minimal"],
			Lighting -> Prepend[{"Directional", GrayLevel[0.3], ImageScaled[#]}& /@ {{2,0,2},{2,2,2},{0,2,2}}, {"Ambient", GrayLevel[0.45]}],
			Sequence @@ If[meshQ, {Method -> {}, SphericalRegion -> (Sphere[#1, 1.03#2]& @@ BoundingRegion[reg, "MinBall"])}, {}]
		]
	]


displayRegion[bmr_BoundaryMeshRegion /; RegionDimension[bmr] == 2, color_, type_] :=
	Block[{\[Alpha]},
		\[Alpha] = Switch[type,
			"Transparent", 0.3,
			_, 1
		];
		
		Show[BoundaryMeshRegion[bmr, MeshCellStyle -> {2 -> Directive[EdgeForm[Directive[AbsoluteThickness[0.75], Opacity[1], Black]], Opacity[\[Alpha]], color]}]]
	]


displayRegion[mr_MeshRegion /; RegionDimension[mr] == 2, color_, type_] :=
	Block[{\[Alpha], bd},
		\[Alpha] = Switch[type,
			"Transparent", 0.3,
			_, 1
		];
		bd = RegionBoundary[mr];
		
		Show[
			Region[Style[mr, Directive[Opacity[\[Alpha]], color]]],
			Graphics[GraphicsComplex[
				MeshCoordinates[bd],
				{AbsoluteThickness[0.75], Opacity[1], Black, MeshCells[bd, 1, "Multicells" -> True]}
			]]
		]
	]


displayRegion[reg_?RegionQ /; RegionDimension[reg] == 2, color_, type_] :=
	Block[{\[Alpha]},
		\[Alpha] = Switch[type,
			"Transparent", 0.3,
			_, 1
		];
		
		Show[Region[Style[reg, Directive[EdgeForm[Directive[AbsoluteThickness[0.75], Opacity[1], Black]], Opacity[\[Alpha]], color]]]]
	]


displayRegion[___] = $Failed


(* ::Subsection::Closed:: *)
(*displayImage3D*)


displayImage3D[im3d_, color_, type_] /; Times @@ ImageDimensions[im3d] > 2^31-1 :=
	Block[{scale, downsampled},
		scale = .98*2^10CubeRoot[2/(Times @@ ImageDimensions[im3d])];
		downsampled = ImageResize[im3d, Scaled[scale], Resampling -> "Nearest"];
		iDisplay[downsampled, color, type]
	]


displayImage3D[im3d_, color_, type_] :=
	Block[{rgb, cf},
		rgb = ColorConvert[color, "RGB"];
		
		cf = Switch[type,
			"Transparent", raster3DOpacityCF[rgb],
			_, raster3DCF[rgb]
		];
		
		Show[
			Image3D[ImagePad[im3d, 1], "Byte", ColorFunction -> cf, Method -> {"VolumeLighting" -> True, "InterpolateValues" -> True}],
			Boxed -> False,
			Lighting -> Prepend[{"Directional", GrayLevel[0.3], ImageScaled[#]}& /@ {{2,0,2},{2,2,2},{0,2,2}}, {"Ambient", GrayLevel[1]}]
		]
	]


displayImage3D[___] = $Failed;


raster3DCF[RGBColor[r_, g_, b_, ___]] := RGBColor[r #, g #, b #, #]&;


raster3DOpacityCF[RGBColor[r_, g_, b_, ___]] := With[{c1 = 0.65, c2 = 0.9}, RGBColor[r #/c1, g #/c1, b #/c1, If[# > c1, 0, c2 (#/c2)^4]]&]


(* ::Subsection::Closed:: *)
(*displayPNGStack*)


displayPNGStack[loc_, color_, type_, pxlspec_] := 
	Block[{im3d, res, dims, bxrats},
		im3d = importPNGStackDisplay[loc];
		(
			res = iDisplay[im3d, color, type];
			(
				If[MatchQ[pxlspec, {_?Positive, _?Positive}],
					dims = ImageDimensions[im3d];
					bxrats = dims*{1., 1., Divide[pxlspec[[2]], pxlspec[[1]]]};
					bxrats /= Max[bxrats];
					res = Show[res, BoxRatios -> bxrats]
				];
				
				res
				
			) /; res =!= $Failed
			
		) /; Head[im3d] === Image3D && ImageQ[im3d]
	]


displayPNGStack[___] = $Failed;


(* ::Subsection::Closed:: *)
(*importPNGStackDisplay*)


(* ::Subsubsection::Closed:: *)
(*importPNGStackDisplay*)


importPNGStackDisplay[dir_?DirectoryQ] :=
	Block[{pngs, method, res},
		pngs = pngStackNumericSort[FileNames[FileNameJoin[{dir, "*.png"}]]];
		(
			method = determinePNGStackImportMethod[pngs];
			(
				res = Switch[method,
					"Parallel",
						parallelPNGStackImport[pngs],
					"Sequential",
						sequentialPNGStackImport[pngs],
					_,
						$Failed
				];
				
				ImageReflect[res, Front -> Back] /; Head[res] === Image3D && ImageQ[res]
				
			) /; method =!= $Failed
			
		) /; Length[pngs] > 0
	]


importPNGStackDisplay[file_?FileExistsQ] := importPNGStackDisplay[DirectoryName[file]]


importPNGStackDisplay[___] = $Failed;


(* ::Subsubsection::Closed:: *)
(*determinePNGStackImportMethod*)


$maxVoxelCount = 2^31-1;


determinePNGStackImportMethod[files_] := 
	Block[{im},
		im = importGrayscalePNG[files[[1]]];
		Which[
			!ImageQ[im], 
				$Failed,
			Times[Times @@ ImageDimensions[im], Length[files]] > 100000000, 
				"Parallel",
			True, 
				"Sequential"
		]
	]


(* ::Subsubsection::Closed:: *)
(*sequentialPNGStackImport*)


sequentialPNGStackImport[files_] :=
	Block[{dim, bd, voxcnt, \[Delta], ifiles, frames, i, j},
		dim = ImageDimensions[importGrayscalePNG[files[[1]], 1]];
		bd = getStackBorderDimension[files, True];
		(
			voxcnt = (dim[[1]] - Total[bd[[1]]] + 2)*(dim[[2]] - Total[bd[[2]]] + 2)*(Length[files] + 2);
			\[Delta] = Max[Ceiling[CubeRoot[Divide[voxcnt, $maxVoxelCount]]], 1];
			
			ifiles = Reverse[files[[1 ;; -1 ;; \[Delta]]]];
			frames = importPNGFrames[ifiles, bd, dim, \[Delta], True];
			(
				i = 1;
				j = Length[frames];
				While[i < j && ImageMeasurements[frames[[i]], "Total"] == 0, i++];
				While[i < j && ImageMeasurements[frames[[j]], "Total"] == 0, j--];
				
				Image3D[frames[[i ;; j]]]
				
			) /; VectorQ[frames, ImageQ] && SameQ @@ ImageDimensions /@ frames
			
		) /; MatrixQ[bd]
	]


sequentialPNGStackImport[___] = $Failed;


(* ::Subsubsection::Closed:: *)
(*parallelPNGStackImport*)


parallelPNGStackImport[files_] /; !initializeParallelPNGStackImport[] := sequentialPNGStackImport[files]


parallelPNGStackImport[files_] :=
	Block[{dim, bd, voxcnt, \[Delta], ifiles, chunksize, pfiles, objs, frames, i, j},
		dim = ImageDimensions[importGrayscalePNG[files[[1]], 1]];
		bd = parallelGetStackBorderDimension[files];
		(
			voxcnt = (dim[[1]] - Total[bd[[1]]] + 2)*(dim[[2]] - Total[bd[[2]]] + 2)*(Length[files] + 2);
			\[Delta] = Max[Ceiling[CubeRoot[Divide[voxcnt, $maxVoxelCount]]], 1];
			
			ifiles = Reverse[files[[1 ;; -1 ;; \[Delta]]]];
			chunksize = Ceiling[Length[ifiles]/$KernelCount];
			pfiles = Partition[ifiles, chunksize, chunksize, 1, Nothing];
			
			objs = Table[ParallelSubmit[{f, bd, dim, \[Delta]}, importPNGFrames[f, bd, dim, \[Delta]]], {f, pfiles}];
			frames = Flatten[WaitAll[objs]];
			(
				i = 1;
				j = Length[frames];
				While[i < j && ImageMeasurements[frames[[i]], "Total"] == 0, i++];
				While[i < j && ImageMeasurements[frames[[j]], "Total"] == 0, j--];
				
				Image3D[frames[[i ;; j]]]
				
			) /; VectorQ[frames, ImageQ] && SameQ @@ ImageDimensions /@ frames
			
		) /; MatrixQ[bd]
	]


parallelPNGStackImport[___] = $Failed;


importPNGFrames[files_, bdim_, {x_, y_}, \[Delta]_, monQ_:False] :=
	Block[{},
		If[TrueQ[monQ], GeneralUtilities`MonitoredMap, Map][imageScaledResize[ImagePad[importGrayscalePNG[#, 1, bdim[[2, 2]]+1, y-bdim[[2, 1]]], {-1,0}bdim], 1/\[Delta]]&, files]
	]


initializeParallelPNGStackImport[] :=
	Block[{},
		If[$KernelCount === 0, Quiet[LaunchKernels[]]];
		
		If[$KernelCount === 0, 
			False,
			DistributeDefinitions[imageScaledResize];
			DistributeDefinitions[importGrayscalePNG];
			DistributeDefinitions[getStackBorderDimension];
			DistributeDefinitions[importPNGFrames];
			True
		]
	]


(* ::Subsubsection::Closed:: *)
(*BorderDimensions*)


getStackBorderDimension[files_, monQ_:False] :=
	Block[{im, dims, temp},
		im = importGrayscalePNG[files[[1]], 1];
		If[!ImageQ[im], Return[$Failed]];
		dims = ImageDimensions[im];
		
		If[TrueQ[monQ], GeneralUtilities`MonitoredScan, Scan][
			(
				temp = importGrayscalePNG[#, 1];
				If[!ImageQ[temp] || ImageDimensions[temp] =!= dims, Return[$Failed]];
				im = ImageAdd[im, temp];
			)&,
			Rest[files]
		];
		
		BorderDimensions[im, 0]
	]


parallelGetStackBorderDimension[files_] :=
	Block[{chunksize, pfiles, objs, res},
		chunksize = Ceiling[Length[files]/$KernelCount];
		pfiles = Partition[files, chunksize, chunksize, 1, Nothing];
		
		objs = Table[ParallelSubmit[{f}, getStackBorderDimension[f]], {f, pfiles}];
		res = WaitAll[objs];
		
		If[!AllTrue[res, MatrixQ], Return[$Failed]];
		Partition[Min /@ Transpose[Flatten /@ res], 2]
	]


(* ::Subsubsection::Closed:: *)
(*Utilities*)


importGrayscalePNG[args__] :=
	Block[{img},
		img = Image`ImportExportDump`ImageReadPNG[args][[1]];
		Image`ImportExportDump`DeleteCachePNG[];
		
		If[!ImageQ[img], Return[$Failed]];
		
		If[ImageType[img] =!= "Byte", 
			img = Image[img, "Byte"];
		];
		
		If[TrueQ[ImageMeasurements[img, "Transparency"]],
			img = RemoveAlphaChannel[img];
		];
		
		If[ImageChannels[img] =!= 1,
			img = ColorConvert[img, "Grayscale"];
		];
		
		img
	]


imageScaledResize[im_, 1] := im;
imageScaledResize[im_, s_] := ImageResize[im, Scaled[s]]


pngStackNumericSort[{}] = {};


pngStackNumericSort[files_] :=
	Module[{digits, lens, len, cands, ord},
		digits = StringCases[FileBaseName /@ files, x:Longest[DigitCharacter..] :> ToExpression[x]];
		lens = Length /@ digits;
		
		If[Max[lens] == 0, Return[{}]];
		len = Commonest[Select[lens, Positive]][[1]];
		
		cands = Pick[files, lens, len];
		digits = Pick[digits, lens, len];
		
		Do[
			ord = Ordering[digits[[All, j]]];
			If[MinMax[Differences[digits[[ord, j]]]] === {1, 1},
				Return[cands[[ord]], Module]
			],
			{j, len}
		];
		
		{}
	]


(* ::Subsection::Closed:: *)
(*Utilities*)


validDisplayQ[_Image3D?ImageQ] = True;
validDisplayQ[bmr_BoundaryMeshRegion?BoundaryMeshRegionQ] := RegionEmbeddingDimension[bmr] > 1;
validDisplayQ[mr_MeshRegion?MeshRegionQ] := RegionEmbeddingDimension[mr] > 1;
validDisplayQ[reg_?ConstantRegionQ] := TrueQ[1 < RegionEmbeddingDimension[reg] < 4];
validDisplayQ[File[f_]] := validDisplayQ[f]
validDisplayQ[file_String] := DirectoryQ[file] || FileExistsQ[file]
validDisplayQ[{file:_File|_String, {_?Positive, _?Positive}}] := validDisplayQ[file]
validDisplayQ[lis_List] := VectorQ[lis, validDisplayQ]
validDisplayQ[_] = False;


display3DType[Automatic, _] = "Solid";
display3DType[type:("Solid"|"Transparent"), _] := type;
display3DType[type:"Peel", reg_?ConstantRegionQ /; RegionEmbeddingDimension[reg] == 3] := type;
display3DType[___] = $Failed;


display3DColor[Automatic] = ColorData[110, 1];
display3DColor[c_?ColorQ] := c;
display3DColor[___] = $Failed;


(* ::Section:: *)
(*UTFixAspectRatio*)


(* ::Subsection::Closed:: *)
(*Main*)


Options[UTFixAspectRatio] = Options[MeshRegion];


UTFixAspectRatio[mr_?UTMesh3DQ, opts:OptionsPattern[]] := UTFixAspectRatio[mr, Automatic, opts]


UTFixAspectRatio[mr_?UTMesh3DQ, padding:Except[_?OptionQ], opts:OptionsPattern[]] := 
	Block[{center, radius},
		{center, radius} = List @@ BoundingRegion[mr, "MinBall"];
		radius = resolvePadding[padding, radius];
		
		Head[mr][mr, opts, SphericalRegion -> Sphere[center, radius]]
	]


(* ::Subsection::Closed:: *)
(*Utilities*)


resolvePadding[Automatic, r_] := resolvePadding[Scaled[0.03], r]
resolvePadding[p_, r_] /; Quiet[r+p > 0] := r+p
resolvePadding[Scaled[p_], r_] := resolvePadding[r*p, r]
resolvePadding[_, r_] := r


(* ::Section:: *)
(*PlotThemes*)


(* ::Subsection::Closed:: *)
(*Autoload*)


Region`MeshThemeDump`resolveMeshTheme;


(* ::Subsection::Closed:: *)
(*BackFaceCull*)


Region`MeshThemeDump`resolveMeshTheme["BackFaceCull", "MeshRegion"|"BoundaryMeshRegion", _, 3] :=
	Themes`SetWeight[MeshCellStyle -> {2 -> Directive[EdgeForm[], FaceForm[Specularity[GrayLevel[0.2], 30], None]]}, Themes`$DesignWeight]


(* ::Subsection::Closed:: *)
(*FrontFaceCull*)


Region`MeshThemeDump`resolveMeshTheme["FrontFaceCull", "MeshRegion"|"BoundaryMeshRegion", _, 3] :=
	Themes`SetWeight[MeshCellStyle -> {2 -> Directive[EdgeForm[], FaceForm[None, Specularity[GrayLevel[0.2], 30]]]}, Themes`$DesignWeight]
