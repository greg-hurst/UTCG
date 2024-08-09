(* ::Package:: *)

(* ::Title:: *)
(*ImportExport*)


(* ::Section:: *)
(*Begin*)


Package["UTCG`"]


PackageExport["UTExportMesh"]
PackageExport["UTImportMesh"]
PackageExport["UTRoundTripMesh"]
PackageExport["UTImportBMP"]
PackageExport["$UTImportExportPath"]


UTImportMesh::usage = "Import a 3D mesh or its data. A faster way to import a binary STL or PLY file. Supported formats are PLY, STL, OBJ, OFF, DAE, DXF.";
UTExportMesh::usage = "Export a mesh to a 3D file format. A faster way to export binary STL or PLY. Normals are exported for STL. Supported formats are PLY, STL, OBJ, OFF, DAE, DXF.";
UTRoundTripMesh::usage = "Imports an 3D mesh file, applies a function to the object, then exports the result to the same file.";
UTImportBMP::usage = "A faster way to import a BMP file.";
$UTImportExportPath::usage = "Default file path for import and export of geometries.";


(* ::Section:: *)
(*Mesh IO*)


(* ::Subsection::Closed:: *)
(*UTImportMesh*)


(* ::Subsubsection::Closed:: *)
(*Main*)


Options[UTImportMesh] = Options[MeshRegion];


UTImportMesh[oargs__] := 
	Block[{args, res},
		args = processImportMeshArgs[oargs];
		(
			res = iImportMesh @@ args;
			
			res /; res =!= $Failed
			
		) /; ListQ[args]
	]


UTImportMesh[___] = $Failed;


(* ::Subsubsection::Closed:: *)
(*Argument conform & completion*)


addCodeCompletion[UTImportMesh]["AbsoluteFileName", {"MeshRegion", "BoundaryMeshRegion", "ComplexData", "FaceData"}];


(* ::Subsubsection::Closed:: *)
(*iImportMesh*)


iImportMesh[ext_, file_, type_, tfunc_, opts___] :=
	Block[{importer, res},
		importer = Switch[ext,
			"ply", iImportPLY,
			"stl", iImportSTL,
			_?meshFormatQ, defaultImportMesh,
			_, $Failed
		];
		(
			res = importer[file, type, tfunc, opts];
			
			res /; res =!= $Failed
			
		) /; importer =!= $Failed
	]


(* ::Subsubsection::Closed:: *)
(*defaultImportMesh*)


defaultImportMesh[file_, type_, tfunc_, opts___] :=
	Block[{mr, res},
		mr = Quiet[Import[file, "MeshRegion"]];
		(
			res = constructDefaultImportMesh[mr, type, tfunc, opts];
			
			res /; res =!= $Failed
			
		) /; MeshRegionQ[mr]
	]


defaultImportMesh[___] = $Failed;


constructDefaultImportMesh[mr_, Automatic | MeshRegion | "MeshRegion", tfunc_, opts___] := MeshRegion[UTCoordinateApply[mr, tfunc], FilterRules[{opts}, Options[MeshRegion]]]


constructDefaultImportMesh[mr_, Automatic | MeshRegion | "MeshRegion", tfunc_, opts___] := 
	Block[{bmr = UTToBMR[UTCoordinateApply[mr, tfunc]]},
		BoundaryMeshRegion[bmr, FilterRules[{opts}, Options[BoundaryMeshRegion]]] /; BoundaryMeshRegionQ[bmr]
	]


constructDefaultImportMesh[mr_, "ComplexData", tfunc_, ___] := {tfunc[MeshCoordinates[mr]], Join @@ (MeshCells[mr, #, "Multicells" -> True][[All, 1]]& /@ mr["ComponentDimensions"])}


constructDefaultImportMesh[mr_, "FaceData", tfunc_, ___] := tfunc /@ MeshPrimitives[mr, 2, "Multicells" -> True][[All, 1]]


constructDefaultImportMesh[___] = $Failed;


(* ::Subsubsection::Closed:: *)
(*Utilities*)


processImportMeshArgs[file_String, opts___?OptionQ] := processImportMeshArgs[file, Automatic, Identity, opts]


processImportMeshArgs[file_String, h_?validImportSTLType, opts___?OptionQ] := processImportMeshArgs[file, h, Identity, opts]


processImportMeshArgs[file_String?FileExistsQ, h_?validImportMeshType, tf_, opts___?OptionQ] := 
	With[{ext = getMeshFileExtension[file]},
		{ext, file, h, tf, opts} /; ext =!= $Failed
	]


processImportMeshArgs[File[file_], args___] := processImportMeshArgs[file, args]


processImportMeshArgs[___] = $Failed;


validImportMeshType[Automatic] = True;
validImportMeshType[BoundaryMeshRegion] = True;
validImportMeshType["BoundaryMeshRegion"] = True;
validImportMeshType[MeshRegion] = True;
validImportMeshType["MeshRegion"] = True;
validImportMeshType["ComplexData"] = True;
validImportMeshType["FaceData"] = True;


(* ::Subsection::Closed:: *)
(*UTExportMesh*)


(* ::Subsubsection::Closed:: *)
(*Main*)


UTExportMesh[mr_, args___] /; !StringQ[mr] && Head[mr] =!= File := UTExportMesh[canonicalFileName["ply", uniqueModelFileName[]], mr, args]


UTExportMesh[file:(_String | _File), mr_, args___] := 
	Block[{ext, filename, strm, path},
		ext = getMeshFileExtension[file];
		(
			filename = canonicalFileName[ext, file];
			(
				path = iExportMesh[ext, file, mr, args];
				
				path /; StringQ[path]
				
			) /; StringQ[filename]
			
		) /; StringQ[ext]
	]


UTExportMesh[___] = $Failed;


(* ::Subsubsection::Closed:: *)
(*iExportMesh*)


iExportMesh["ply", file_, mr_, ___] := UTCG`Legacy`UTExportPLY[file, mr]


iExportMesh["stl", file_, mr_, color_:None] := UTCG`Legacy`UTExportSTL[file, mr, color]


iExportMesh[_?meshFormatQ, file_, mr_, ___] := defaultExportMesh[file, mr]


iExportMesh[___] = $Failed;


(* ::Subsubsection::Closed:: *)
(*defaultExportMesh*)


defaultExportMesh[file_, mr_?UTMesh3DQ] := 
	Block[{res},
		res = Quiet[Export[file, mr]];
		
		res /; StringQ[res]
	]


defaultExportMesh[file_, mrs_List] /; VectorQ[Flatten[mrs], UTMesh3DQ] := defaultExportMesh[file, UTJoin[mrs]]


defaultExportMesh[___] = $Failed;


(* ::Subsection::Closed:: *)
(*UTRoundTripMesh*)


(* ::Subsubsection::Closed:: *)
(*Main*)


UTRoundTripMesh[f_, file_] :=
	Block[{ext, filename, mesh, res, path},
		ext = getMeshFileExtension[file];
		(
			filename = canonicalFileName[ext, file];
			(
				mesh = UTImportMesh[filename];
				(
					res = f[mesh];
					(
						path = UTExportMesh[file, res];
						
						path /; StringQ[path]
						
					) /; RegionQ[res]
					
				) /; UTMesh3DQ[mesh]
				
			) /; FileExistsQ[filename]
		
		) /; StringQ[ext]
	]


UTRoundTripMesh[___] = $Failed;


(* ::Subsubsection::Closed:: *)
(*Argument conform & completion*)


addCodeCompletion[UTRoundTripMesh][None, "AbsoluteFileName"];


(* ::Section:: *)
(*BMP*)


(* ::Subsection::Closed:: *)
(*UTImportBMP*)


(* Autoload *)
Import[FileNameJoin[{$InstallationDirectory,"Documentation","English","System","ExampleData","spikey3.bmp"}]];


UTImportBMP[file_] := 
	Block[{strm, img},
		Quiet @ Internal`WithLocalSettings[
			strm = OpenRead[file, BinaryFormat -> True],
			img = Image["RawData" /. System`Convert`BitmapDump`ImportBMP[strm], "Byte"];
			If[ImageQ[img], img, $Failed],
			Close[strm]
		]
	]


addCodeCompletion[UTImportBMP]["AbsoluteFileName"];


(* ::Section:: *)
(*PLY*)


(* ::Subsection::Closed:: *)
(*UTCG`Legacy`UTImportPLY*)


(* ::Text:: *)
(*Barebones code to import binary little endian PLY that has only one cell type, e.g. only triangles, or only quads, etc.*)


(* ::Text:: *)
(*Ignores everything except vertex data and cell indices. Not tested super extensively.*)


(* ::Subsubsection::Closed:: *)
(*Main*)


Options[UTCG`Legacy`UTImportPLY] = Options[UTImportMesh];


UTCG`Legacy`UTImportPLY[oargs___] :=
	Block[{args, res},
		args = processImportPLYArgs[oargs];
		(
			res = iImportPLY @@ args;
			
			res /; res =!= $Failed
			
		) /; ListQ[args]
	]


UTCG`Legacy`UTImportPLY[___] = $Failed;


(* ::Subsubsection::Closed:: *)
(*Argument conform & completion*)


addCodeCompletion[UTCG`Legacy`UTImportPLY]["AbsoluteFileName", {"MeshRegion", "BoundaryMeshRegion", "ComplexData", "FaceData"}];


(* ::Subsubsection::Closed:: *)
(*iImportPLY*)


iImportPLY[file_, ret_, tfunc_, opts:OptionsPattern[]] :=
	Block[{strm, res},
		Internal`WithLocalSettings[
			strm = OpenRead[file, BinaryFormat -> True];,
			res = importPLYStream[strm, ret, tfunc, opts],
			Close[strm];
		];
		
		res /; res =!= $notBinaryLittleEndian
	]


iImportPLY[file_, ret_, tfunc_, opts:OptionsPattern[]] := 
	With[{mr = Quiet[Import[file, {"PLY", "MeshRegion"}]]},
		applyPLYImportType[mr, ret, tfunc, opts] /; MeshRegionQ[mr]
	]


iImportPLY[___] = $Failed;


(* ::Subsubsection::Closed:: *)
(*importPLYStream*)


(* Autoload System`Convert`PLYDump`PLYReadHeader *)
Import["ExampleData/seashell.ply", "BinaryFormat"];


importPLYStream[strm_InputStream, ret_, tfunc_, opts:OptionsPattern[]] := 
	Block[{header, plyraw, plydata, head, res},
		header = Quiet@System`Convert`PLYDump`PLYReadHeader[strm];
		If[!MatchQ[header, {"binary_little_endian", {__}, __}], 
			Return[$notBinaryLittleEndian]
		];
		
		plyraw = ReadByteArray[strm][[2 ;; -1]];
		plydata = Catch[parseRawBinaryPLY[plyraw, header[[2]]]];
		If[plydata === $Failed,
			Return[$Failed]
		];
		
		Quiet[constructPLYImport[##, ret, tfunc, opts]& @@ plydata]
	]


$notBinaryLittleEndian = "NotBinaryLittleEndian";


(* ::Subsubsection::Closed:: *)
(*parseRawBinaryPLY*)


parseRawBinaryPLY[plyraw_, dspec_] :=
	Block[{i = 1, type, len, spec, size, coords, cells},
		Do[
			{type, len, spec} = d;
			Switch[type,
				"vertex",
					size = len*elementSize[spec];
					coords = parsePLYVertex[plyraw[[i ;; i+size-1]], len, spec];
					i += size;
					,
				"face",
					cells = parsePLYCells[plyraw[[i ;; -1]], len, spec];
					i += Times @@ Dimensions[cells],
				_,
					i += len*elementSize[spec];
			],
			{d, dspec}
		];
		
		{coords, cells} /; MatrixQ[coords] && MatrixQ[cells]
	]

parseRawBinaryPLY[___] = $Failed;


(* ::Subsubsection::Closed:: *)
(*parsePLYVertex*)


parsePLYVertex[vdata_, len_, {System`Convert`PLYDump`PLYType["Real32", "x"], System`Convert`PLYDump`PLYType["Real32", "y"], System`Convert`PLYDump`PLYType["Real32", "z"]}] := plyMatrix[vdata, 3, "Real32"]
parsePLYVertex[vdata_, len_, spec_] :=
	Block[{px, py, pz, sizes, sz, vertexbitpos, na},
		px = Position[spec, System`Convert`PLYDump`PLYType["Real32", "x"], {1}];
		py = Position[spec, System`Convert`PLYDump`PLYType["Real32", "y"], {1}];
		pz = Position[spec, System`Convert`PLYDump`PLYType["Real32", "z"], {1}];
		If[MatchQ[{px, py, pz}, {{{_}}, {{_}}, {{_}}}], 
			{px, py, pz} = {px, py, pz}[[All, 1, 1]],
			Throw[$Failed]
		];
		
		sizes = elementSize /@ spec;
		sz = Total[sizes];
		vertexbitpos = Flatten[Internal`PartitionRagged[Range[sz], sizes][[{px, py, pz}]]];
		
		na = ByteArrayPartition[vdata, sz];
		na = na[[All, vertexbitpos]];
		
		plyMatrix[ByteArray[Flatten[na]], 3, "Real32"]
	]


(* ::Subsubsection::Closed:: *)
(*parsePLYCells*)


parsePLYCells[plyraw_, len_, spec_] :=
	Block[{cpos, sizes, offset, fcnt, sz, fcnts, celltype, spec2, cellbitpos, na, cells, head},
		cpos = Position[spec, System`Convert`PLYDump`PLYListType[_, _, "vertex_indices"], {1}];
		If[MatchQ[cpos, {{_}}],
			cpos = cpos[[1, 1]], 
			Throw[$Failed]
		];
		
		sizes = elementSize /@ Delete[spec, {cpos}];
		offset = Total[sizes[[1 ;; cpos-1]]];
		fcnt = plyraw[[1+offset]];
		sz = Total[sizes] + elementSize[spec[[cpos, 1]]] + fcnt*elementSize[spec[[cpos, 2]]];
		If[Length[plyraw] < len*sz, Throw[$Failed]];
		
		fcnts = plyraw[[1+offset ;; len*sz + offset ;; sz]];
		If[Length[DeleteDuplicates[Normal[fcnts]]] =!= 1, Throw[$Failed]];
		
		celltype = spec[[cpos, 2]];
		spec2 = Join[spec[[1 ;; cpos-1]], {spec[[cpos, 1]]}, ConstantArray[celltype, fcnt], spec[[cpos+1 ;; -1]]];
		sizes = elementSize /@ spec2;
		sz = Total[sizes];
		cellbitpos = Flatten[Internal`PartitionRagged[Range[sz], sizes][[Range[cpos+1, cpos+fcnt]]]];
		
		na = ByteArrayPartition[plyraw[[1 ;; len*sz]], sz];
		na = na[[All, cellbitpos]];
		
		plyMatrix[ByteArray[Flatten[na]], fcnt, celltype] + 1
	]


(* ::Subsubsection::Closed:: *)
(*Construct object*)


constructPLYImport[coords_, cells_, Automatic | "MeshRegion", tfunc_, opts___] := 
	With[{res = UTFastBlock[MeshRegion[tfunc[coords], cellHead[cells][cells], FilterRules[{opts}, MeshRegion]]]},
		If[RegionQ[res], res, $Failed]
	]


constructPLYImport[coords_, cells_, "BoundaryMeshRegion", tfunc_, opts___] := 
	With[{res = UTFastBlock[BoundaryMeshRegion[tfunc[coords], cellHead[cells][cells], FilterRules[{opts}, BoundaryMeshRegion]]]},
		If[RegionQ[res], res, $Failed]
	]


constructPLYImport[coords_, cells_, "ComplexData", tfunc_, ___] := {tfunc[coords], {cellHead[cells][cells]}}


constructPLYImport[coords_, cells_, "FaceData", tfunc_, ___] := Partition[tfunc[coords][[Flatten[cells]]], Length[First[cells]]]


constructPLYImport[___] = $Failed;


cellHead[cells_] := Switch[Last[Dimensions[cells]], 1, Point, 2, Line, _, Polygon]


(* ::Subsubsection::Closed:: *)
(*Utilities*)


processImportPLYArgs[file_String, opts___?OptionQ] := processImportPLYArgs[file, Automatic, Identity, opts]


processImportPLYArgs[file_String, h_?validImportSTLType, opts___?OptionQ] := processImportPLYArgs[file, h, Identity, opts]


processImportPLYArgs[file_String?FileExistsQ, h_?validImportPLYType, tf_, opts___?OptionQ] := {canonicalFileName["ply", file], h, tf, opts}


processImportPLYArgs[File[file_], args___] := processImportPLYArgs[file, args]


processImportPLYArgs[___] = $Failed;


validImportPLYType[Automatic] = True;
validImportPLYType[BoundaryMeshRegion] = True;
validImportPLYType["BoundaryMeshRegion"] = True;
validImportPLYType[MeshRegion] = True;
validImportPLYType["MeshRegion"] = True;
validImportPLYType["ComplexData"] = True;
validImportPLYType["FaceData"] = True;


plyMatrix[bdata_, n_, type_] := Normal[ByteArrayPartition[bdata, n, type]]


applyPLYImportType[mr_, ___] /; !RegionQ[mr] = $Failed;


applyPLYImportType[mr_, Automatic | "MeshRegion", tfunc_, opts:OptionsPattern[]] := 
	With[{reg = UTCoordinateApply[mr, tfunc]},
		MeshRegion[reg, FilterRules[{opts}, MeshRegion]] /; MeshRegionQ[reg]
	]


applyPLYImportType[mr_, "BoundaryMeshRegion", tfunc_, opts:OptionsPattern[]] := 
	With[{reg = UTToBMR[UTCoordinateApply[mr, tfunc]]},
		BoundaryMeshRegion[reg, FilterRules[{opts}, BoundaryMeshRegion]] /; BoundaryMeshRegionQ[reg]
	]


applyPLYImportType[mr_, "ComplexData", tfunc_, ___] := {tfunc[MeshCoordinates[mr]], Join @@ MeshCells[mr, 2, "Multicells" -> True][[All, 1]]}


applyPLYImportType[mr_, "FaceData", tfunc_, ___] := tfunc[Join @@ MeshPrimitives[mr, 2, "Multicells" -> True][[All, 1]]]


applyPLYImportType[___] = $Failed;


(* ::Subsection::Closed:: *)
(*UTCG`Legacy`UTExportPLY*)


(* ::Subsubsection::Closed:: *)
(*UTCG`Legacy`UTExportPLY*)


UTCG`Legacy`UTExportPLY[mr_] /; !StringQ[mr] := UTCG`Legacy`UTExportPLY[uniqueModelFileName[], mr]


UTCG`Legacy`UTExportPLY[file_, mrs_List?validExportPLYQ] := UTCG`Legacy`UTExportPLY[file, UTJoin[mrs]]


UTCG`Legacy`UTExportPLY[file_, mr_?validExportPLYQ] := 
	Block[{filename, strm},
		filename = canonicalFileName["ply", file];
		Internal`WithLocalSettings[
			strm = OpenWrite[filename, BinaryFormat -> True];,
			iExportPLYHeader[strm, mr];
			iExportPLYMesh[strm, mr];
			filename,
			Close[strm]
		]
	]


UTCG`Legacy`UTExportPLY[___] = $Failed;


(* ::Subsubsection::Closed:: *)
(*UTCG`Legacy`UTExportStringPLY*)


UTCG`Legacy`UTExportStringPLY[mr_] /; validExportPLYQ[mr] := 
	Block[{strm},
		Internal`WithLocalSettings[
			strm = Developer`StreamToString[BinaryFormat -> True];,
			iExportPLYHeader[strm, mr];
			iExportPLYMesh[strm, mr];
			Developer`StringFromStream[strm],
			Close[strm]
		]
	]


UTCG`Legacy`UTExportStringSTL[___] = $Failed;


(* ::Subsubsection::Closed:: *)
(*Argument conform & completion*)


addCodeCompletion[UTCG`Legacy`UTExportPLY]["AbsoluteFileName"];


registerForDiscretization[UTCG`Legacy`UTExportPLY, 2];


registerForDiscretization[UTCG`Legacy`UTExportStringPLY, 1];


(* ::Subsubsection::Closed:: *)
(*iExportPLYHeader*)


iExportPLYHeader[strm_, mr_] := BinaryWrite[strm, $plyHeaderTemplate[MeshCellCount[mr, 0], MeshCellCount[mr, 2]]]


$plyHeaderTemplate = "ply
format binary_little_endian 1.0
comment Created_in_the_Wolfram_Language__with_UTMeshLibrary
element vertex " <> ToString[#1] <> "
property float x
property float y
property float z
element face " <> ToString[#2] <> "
property list uchar int vertex_indices
end_header
"&;


(* ::Subsubsection::Closed:: *)
(*iExportPLYMesh*)


iExportPLYMesh[strm_, mr_] := 
	Block[{chunksize = 2^31-1, cnt0, cnt2, coorddata, celldata},
		cnt0 = MeshCellCount[mr, 0];
		cnt2 = MeshCellCount[mr, 2];
		
		coorddata = BinarySerialize[MeshCoordinates[mr], Method -> {"PackedArrayRealType" -> "Real32"}][[-12cnt0 ;; -1]];
		
		writePLYData[strm, coorddata, chunksize];
		coorddata=.;
		
		celldata = BinarySerialize[
			Subtract[Join @@ MeshCells[mr, 2, "Multicells" -> True][[All, 1]], 1], 
			Method -> {"PackedArrayIntegerType" -> "Integer32"}
		][[-12cnt2 ;; -1]];
		
		celldata = ByteArray[Flatten[Transpose[Join[NumericArray[ConstantArray[3, {1, cnt2}], "UnsignedInteger8"], Transpose[ByteArrayPartition[celldata, 12]]]]]];
		
		writePLYData[strm, celldata, chunksize];
		celldata=.;
	]


writePLYData[strm_, data_, sz_] /; Length[data] <= sz := BinaryWrite[strm, data]


writePLYData[strm_, data_, sz_] := 
	Do[
		BinaryWrite[strm, data[[1 + sz(i-1) ;; Min[sz*i, Length[data]]]]],
		{i, Ceiling[Length[data]/sz]}
	]


(* ::Subsubsection::Closed:: *)
(*Utilities*)


validExportPLYQ[mr_List] := Length[mr] > 0 && VectorQ[mr, validExportPLYQ]
validExportPLYQ[mr_] := UTTriangleMesh3DQ[mr]


(* ::Section:: *)
(*STL*)


(* ::Subsection::Closed:: *)
(*UTCG`Legacy`UTImportSTL*)


(* ::Subsubsection::Closed:: *)
(*Main*)


Options[UTCG`Legacy`UTImportSTL] = Options[UTImportMesh];


UTCG`Legacy`UTImportSTL[oargs___] :=
	Block[{args, res},
		args = processImportSTLArgs[oargs];
		(
			res = iImportSTL @@ args;
			
			res /; res =!= $Failed
			
		) /; ListQ[args]
	]


UTCG`Legacy`UTImportSTL[___] = $Failed;


(* ::Subsubsection::Closed:: *)
(*Argument conform & completion*)


addCodeCompletion[UTCG`Legacy`UTImportSTL]["AbsoluteFileName", {"MeshRegion", "BoundaryMeshRegion", "ComplexData", "FaceData"}];


(* ::Subsubsection::Closed:: *)
(*iImportSTL*)


iImportSTL[filename_, type_, tf_, opts:OptionsPattern[]] := iImportSTL[filename, type, tf, STLEncoding[filename], opts]


iImportSTL[filename_, type_, tf_, "binary", opts:OptionsPattern[]] :=
	Block[{bdata, cnt, preamble, pts, tfunc, res},
		bdata = ReadByteArray[File[filename]];
		cnt = Divide[Length[bdata] - 84, 50];
		(
			preamble = Developer`ToPackedArray @ Join[
				{56, 58, 193, 34, 2},
				Append[BitOr[Most[#], 128], Last[#]]&[Reverse[IntegerDigits[3cnt, 128]]],
				{3}
			];
			
			bdata = BinarySerialize[Flatten[ByteArrayPartition[bdata[[85 ;; -1]], 50][[All, 13 ;; 48]]]][[-36cnt ;; -1]];
			
			tfunc = If[tf =!= Automatic, tf, Identity];
			pts = tfunc @ BinaryDeserialize[Join[ByteArray[preamble], bdata]];
			
			res = constructSTLImport[type, pts, opts];
			
			res /; res =!= $Failed
			
		) /; Length[bdata] >= 84 && cnt === {1, 256, 65536, 16777216} . Normal[bdata[[81 ;; 84]]]
	]


iImportSTL[filename_, type_, tf_, "ascii", opts:OptionsPattern[]] :=
	Block[{res},
		res = defaultImportMesh[filename, type, tf, opts];
		
		res /; res =!= $Failed
	]


iImportSTL[___] = $Failed;


(* ::Subsubsection::Closed:: *)
(*Construct object*)


constructSTLImport[type:(Automatic | MeshRegion | BoundaryMeshRegion | "MeshRegion" | "BoundaryMeshRegion"), pts_, opts___] :=
	Block[{head, res},
		head = If[MatchQ[type, BoundaryMeshRegion|"BoundaryMeshRegion"], BoundaryMeshRegion, MeshRegion];
		
		res = Quiet @ head[
			pts, 
			Polygon[Partition[Range[Length[pts]], 3]],
			FilterRules[Flatten[{opts}], Options[MeshRegion]],
			Method -> {"EliminateUnusedCoordinates" -> False, "CheckIntersections" -> False}
		];
		
		res /; RegionQ[res]
	]


constructSTLImport["ComplexData", pts_, ___] :=
	Block[{coords, cells},
		{coords, cells} = Region`Mesh`DeleteDuplicateCoordinates[pts];
		If[!ListQ[cells], cells = Range[Length[coords]]];
		
		{coords, {Polygon[Partition[cells, 3]]}}
	]


constructSTLImport["FaceData", pts_, ___] := Partition[pts, 3]


constructSTLImport[___] = $Failed;


(* ::Subsubsection::Closed:: *)
(*Utilities*)


processImportSTLArgs[file_String, opts___?OptionQ] := processImportSTLArgs[file, Automatic, Identity, opts]


processImportSTLArgs[file_String, h_?validImportSTLType, opts___?OptionQ] := processImportSTLArgs[file, h, Identity, opts]


processImportSTLArgs[file_String?FileExistsQ, h_?validImportSTLType, tf_, opts___?OptionQ] := {canonicalFileName["stl", file], h, tf, opts}


processImportSTLArgs[File[file_], args___] := processImportSTLArgs[file, args]


processImportSTLArgs[___] = $Failed;


validImportSTLType[Automatic] = True;
validImportSTLType[BoundaryMeshRegion] = True;
validImportSTLType["BoundaryMeshRegion"] = True;
validImportSTLType[MeshRegion] = True;
validImportSTLType["MeshRegion"] = True;
validImportSTLType["ComplexData"] = True;
validImportSTLType["FaceData"] = True;


STLEncoding[file_] :=
	Quiet @ Block[{strm, spec},
		Internal`WithLocalSettings[
			strm = OpenRead[file, BinaryFormat -> True];
			If[Head[strm] =!= InputStream, 
				Return[$Failed]
			];,
			
			spec = BinaryRead[strm, {"Byte", "Byte", "Byte", "Byte", "Byte", "Byte"}];
			Which[
				spec === {115, 111, 108, 105, 100, 32}, "ascii",
				ListQ[spec], "binary",
				True, $Failed
			],
			
			Close[strm]
		]
	]


(* ::Subsection::Closed:: *)
(*UTCG`Legacy`UTExportSTL*)


(* ::Subsubsection::Closed:: *)
(*UTCG`Legacy`UTExportSTL*)


UTCG`Legacy`UTExportSTL[mr_, color_:None] /; !StringQ[mr] && Head[mr] =!= File := UTCG`Legacy`UTExportSTL[uniqueModelFileName[], mr, color]


UTCG`Legacy`UTExportSTL[file:(_String | _File), mr_, color_:None] /; validExportSTLQ[mr, color] := 
	Block[{filename, strm},
		filename = canonicalFileName["stl", file];
		Internal`WithLocalSettings[
			strm = OpenWrite[filename, BinaryFormat -> True];,
			iExportSTLHeader[strm, mr];
			iExportSTLMesh[strm, mr, color];
			filename,
			Close[strm]
		]
	]


UTCG`Legacy`UTExportSTL[___] = $Failed;


(* ::Subsubsection::Closed:: *)
(*UTCG`Legacy`UTExportStringSTL*)


UTCG`Legacy`UTExportStringSTL[mr_, color_:None] /; validExportSTLQ[mr, color] := 
	Block[{strm},
		Internal`WithLocalSettings[
			strm = Developer`StreamToString[BinaryFormat -> True];,
			iExportSTLHeader[strm, mr];
			iExportSTLMesh[strm, mr, color];
			Developer`StringFromStream[strm],
			Close[strm]
		]
	]


UTCG`Legacy`UTExportStringSTL[___] = $Failed;


(* ::Subsubsection::Closed:: *)
(*Argument conform & completion*)


addCodeCompletion[UTCG`Legacy`UTExportSTL]["AbsoluteFileName"];


registerForDiscretization[UTCG`Legacy`UTExportSTL, 2];


registerForDiscretization[UTCG`Legacy`UTExportStringSTL, 1];


(* ::Subsubsection::Closed:: *)
(*iExportSTLHeader*)


iExportSTLHeader[strm_, mr_] :=
	Block[{header, cnt},
		header = StringPadRight["Created_in_the_Wolfram_Language__with_UTMeshLibrary", 80, FromCharacterCode[0]];
		If[ListQ[mr],
			cnt = Total[MeshCellCount[#, 2]& /@ mr],
			cnt = MeshCellCount[mr, 2]
		];
		
		BinaryWrite[strm, header];
		BinaryWrite[strm, cnt, "Integer32", ByteOrdering -> -1];
	]


(* ::Subsubsection::Closed:: *)
(*iExportSTLMesh*)


SetAttributes[iExportSTLMesh, Listable];


iExportSTLMesh[strm_, mr_, color_] := 
	Block[{cnt, tdata, start, attr, chunksize = 2^31-1},
		cnt = MeshCellCount[mr, 2];
		
		tdata = ConstantArray[0., {cnt, 4, 3}];
		tdata[[All, 1]] = faceUnitNormals[mr];
		tdata[[All, 2;;4]] = Join @@ MeshPrimitives[mr, 2, "Multicells" -> True][[All, 1]];
		
		start = 4Minus[Times @@ Dimensions[tdata]];
		If[ColorQ[color],
			attr = ByteArray[Partition[Reverse@Prepend[Flatten[IntegerDigits[Floor[255/8(List @@ ColorConvert[color, "RGB"])[[1 ;; 3]]], 2, 5]], 1], 8] . (2^Range[0,7])],
			attr = ByteArray[{0, 0}]
		];
		
		tdata = BinarySerialize[tdata, Method -> {"PackedArrayRealType" -> "Real32"}][[start ;; -1]];
		tdata = Join[Join @@ Riffle[Table[Take[tdata, {k, k + 47}], {k, 1, Length[tdata], 48}], attr], attr];
		
		If[Length[tdata] <= chunksize,
			BinaryWrite[strm, tdata],
			Do[
				BinaryWrite[strm, tdata[[1 + chunksize(i-1) ;; Min[chunksize*i, Length[tdata]]]]],
				{i, Ceiling[Length[tdata]/chunksize]}
			]
		]
	]


(* ::Subsubsection::Closed:: *)
(*Utilities*)


validExportSTLQ[mr_, color_] := validExportSTLMeshesQ[mr] && validExportSTLShapeQ[mr, color]
validExportSTLQ[___] = False;


validExportSTLMeshesQ[mr_List] := Length[mr] > 0 && VectorQ[mr, validExportSTLMeshesQ]
validExportSTLMeshesQ[mr_] := UTTriangleMesh3DQ[mr]


validExportSTLShapeQ[mr_List, color_List] := Length[mr] == Length[color]
validExportSTLShapeQ[___] = True;


(* ::Subsection::Closed:: *)
(*UTCG`Legacy`UTRoundTripSTL*)


(* ::Subsubsection::Closed:: *)
(*Main*)


UTCG`Legacy`UTRoundTripSTL[f_, file_] :=
	Block[{filename, mesh, res},
		filename = canonicalFileName["stl", file];
		(
			mesh = UTCG`Legacy`UTImportSTL[filename];
			(
				res = f[mesh];
				
				UTCG`Legacy`UTExportSTL[file, res] /; validExportSTLMeshesQ[res]
				
			) /; UTMesh3DQ[mesh]
			
		) /; FileExistsQ[filename]
	]


UTCG`Legacy`UTRoundTripSTL[___] = $Failed;


(* ::Subsubsection::Closed:: *)
(*Argument conform & completion*)


addCodeCompletion[UTCG`Legacy`UTRoundTripSTL][None, "AbsoluteFileName"];


(* ::Section:: *)
(*Paths*)


(* ::Subsection::Closed:: *)
(*$UTImportExportPath*)


$UTImportExportPath /: SetDelayed[$UTImportExportPath, expr_] := 
	(
		OwnValues[$UTImportExportPath] = {HoldPattern[$UTImportExportPath] :> With[{path = expr}, If[DirectoryQ[path], path, $HomeDirectory]]};
	)


$UTImportExportPath /: Set[$UTImportExportPath, path_String?DirectoryQ] := 
	(
		OwnValues[$UTImportExportPath] = {HoldPattern[$UTImportExportPath] :> path};
		path
	)


$UTImportExportPath /: Set[$UTImportExportPath, _] = $Failed;


$UTImportExportPath = $HomeDirectory;


If[DirectoryQ[FileNameJoin[{$HomeDirectory, "Desktop"}]], $UTImportExportPath = FileNameJoin[{$HomeDirectory, "Desktop"}]];


(* ::Section:: *)
(*Utilities*)


(* ::Subsection::Closed:: *)
(*Mesh file types*)


getMeshFileExtension[file_] :=
	With[{ext = ToLowerCase[FileExtension[file]]},
		ext /; meshFormatQ[ext]
	]


getMeshFileExtension[___] = $Failed;


iMeshFormatQ["ply"] = True;
iMeshFormatQ["stl"] = True;
iMeshFormatQ["obj"] = True;
iMeshFormatQ["dae"] = True;
iMeshFormatQ["off"] = True;
iMeshFormatQ["dxf"] = True;


meshFormatQ[ext_] := StringQ[ext] && TrueQ[iMeshFormatQ[ToLowerCase[ext]]];


(* ::Subsection::Closed:: *)
(*File names*)


uniqueModelFileName[] := DateString[{"model_", "Month", "Day", "YearShort", "_", "Hour12", "Minute", "Second", "AMPM"}]


canonicalFileName[_, file_String?FileExistsQ] := file


canonicalFileName[ext_, file_String] :=
	With[{filename = file <> If[StringFreeQ[file, "."], "." <> ext, ""]},
		If[!FileExistsQ[filename] && Length[FileNameSplit[filename]] == 1,
			FileNameJoin[{$UTImportExportPath, filename}],
			filename
		]
	]


canonicalFileName[ext_, File[file_]] := canonicalFileName[ext, file]


canonicalFileName[___] = $Failed;


(* ::Subsection::Closed:: *)
(*Byte code*)


elementSize[l_List] := Total[elementSize /@ l]
elementSize[System`Convert`PLYDump`PLYType[s_, _]] := elementSize[s]
elementSize["Real32"] = 4;
elementSize["Integer32"] = 4;
elementSize["Integer16"] = 2;
elementSize["Integer8"] = 1;
elementSize["UnsignedInteger32"] = 4;
elementSize["UnsignedInteger16"] = 2;
elementSize["UnsignedInteger8"] = 1;
elementSize[___] := Throw[$Failed]


numericArrayOpCode["Real32"] = 34;
numericArrayOpCode["Integer8"] = 0;
numericArrayOpCode["Integer16"] = 1;
numericArrayOpCode["Integer32"] = 2;
numericArrayOpCode["UnsignedInteger8"] = 16;
numericArrayOpCode["UnsignedInteger16"] = 17;
numericArrayOpCode["UnsignedInteger32"] = 18;
numericArrayOpCode[___] := Throw[$Failed]


ByteArrayPartition[bdata_, n_Integer?Positive, type_:"UnsignedInteger8"] :=
	Block[{d, preamble},
		d = Quotient[Length[bdata], n*elementSize[type]];
		
		preamble = ByteArray @ Join[
			{56, 58, 194, numericArrayOpCode[type], 2},
			Append[BitOr[Most[#], 128], Last[#]]&[Reverse[IntegerDigits[d, 128]]],
			Append[BitOr[Most[#], 128], Last[#]]&[Reverse[IntegerDigits[n, 128]]]
		];
		
		BinaryDeserialize[Join[preamble, bdata]]
	]
