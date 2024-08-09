(* ::Package:: *)

(* ::Title:: *)
(*TPMS*)


Package["UTCG`"]


PackageExport["UTTriplyPeriodicMinimalSurface"]
PackageExport["UTGyroid"]
PackageExport["UTFischerS"]
PackageExport["UTTPMSInfill"]
PackageExport["UTTPMSInfillComplement"]


UTTriplyPeriodicMinimalSurface::usage = "Creates a grid containing copies of a triply periodic minimal surface.";
UTGyroid::usage = "Creates a grid containing copies of a gyroid.";
UTFischerS::usage = "Creates a grid containing copies of a Fischer S surface.";
UTTPMSInfill::usage = "Infill a solid with a triply periodic minimal surface.";
UTTPMSInfillComplement::usage = "Creates the void space of a solid infilled with a triply periodic minimal surface.";


(* ::Section:: *)
(*TPMS Template Data*)


(* ::Subsection::Closed:: *)
(*TPMS Data*)


(* ::Subsubsection::Closed:: *)
(*Initialize*)


registerTPMS[surf_, data_] := AppendTo[$tpmsTemplates, surf -> data]


$tpmsTemplates = <||>;


(* ::Subsubsection::Closed:: *)
(*G*)


registerTPMS["G",
	<|
		"MeshDataSymbolName" -> "$gyroid",
		"Sources" -> {"https://mathworld.wolfram.com/notebooks/Surfaces/Gyroid.nb",
					"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/gyroid/gyroid.html",
					"https://minimalsurfaces.blog/home/repository/triply-periodic/gyroid/"},
		"Comments" -> {},
		"Aliases" -> {"Gyroid", "SchoenG"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*P*)


registerTPMS["P",
	<|
		"MeshDataSymbolName" -> "$schwarzP",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/periodic.html#psurface",
					"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/periodic.html#fishercs",
					"https://minimalsurfaces.blog/home/repository/triply-periodic/schwarz-p-surface/"},
		"Comments" -> {"Shifted by 0.25 in each cardinal direction to avoid non-manifold vertices on the boundary."},
		"Aliases" -> {"SchwarzP", "CS", "FischerCS", "FischerKochCS"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*D*)


registerTPMS["D",
	<|
		"MeshDataSymbolName" -> "$schwarzD",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/periodic.html#dsurface",
					"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/disphenoids.html",
					"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/periodic.html#fishery",
					"https://minimalsurfaces.blog/home/repository/triply-periodic/schwarz-g-surface/",
					"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/periodic.html#triplane"},
		"Comments" -> {},
		"Aliases" -> {"SchwarzD", "Disphenoid3", "Y", "FischerY", "FischerKochY", "Triplane0"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*Magic D*)


registerTPMS[{"Magic", "D"},
	<|
		"MeshDataSymbolName" -> "$schwarzDmagic",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/periodic.html#dsurface",
					"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/disphenoids.html",
					"https://en.wikipedia.org/wiki/Magic_angle"},
		"Comments" -> {"Rotated version of \"D\" with period {1/Sqrt[3], 1, 1/Sqrt[2]}.", "The angle ArcSec[Sqrt[3]] is referred to as the magic angle.", 
					"Rotation transform: TransformedRegion[UTTriplyPeriodicMinimalSurface[\"D\"],RotationTransform[-\[Pi]/12,{0,0,1}]@*RotationTransform[-ArcSec[Sqrt[3]],{-1,1,0}]]"},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*Batwing*)


registerTPMS["Batwing",
	<|
		"MeshDataSymbolName" -> "$batwing",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/batwing.html"},
		"Comments" -> {},
		"Aliases" -> {"Batwing25"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*BatwingPseudo*)


registerTPMS["BatwingPseudo",
	<|
		"MeshDataSymbolName" -> "$pbatwing",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/batwing.html"},
		"Comments" -> {},
		"Aliases" -> {"PseudoBatwing"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*Batwing41*)


registerTPMS["Batwing41",
	<|
		"MeshDataSymbolName" -> "$batwing41",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/batwing.html"},
		"Comments" -> {},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*Batwing57*)


registerTPMS["Batwing57",
	<|
		"MeshDataSymbolName" -> "$batwing57",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/batwing.html"},
		"Comments" -> {},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*N*)


registerTPMS["N",
	<|
		"MeshDataSymbolName" -> "$neovius",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/periodic.html#neovius"},
		"Comments" -> {},
		"Aliases" -> {"Neovius"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*N14*)


registerTPMS["N14",
	<|
		"MeshDataSymbolName" -> "$n14",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/periodic.html#n14"},
		"Comments" -> {},
		"Aliases" -> {"Neovius14"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*N26*)


registerTPMS["N26",
	<|
		"MeshDataSymbolName" -> "$n26",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/periodic.html#n26"},
		"Comments" -> {},
		"Aliases" -> {"Neovius26"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*N38*)


registerTPMS["N38",
	<|
		"MeshDataSymbolName" -> "$n38",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/periodic.html#n38"},
		"Comments" -> {},
		"Aliases" -> {"Neovius38"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*CP*)


registerTPMS["CP",
	<|
		"MeshDataSymbolName" -> "$cp15",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/cpfamily.html"},
		"Comments" -> {},
		"Aliases" -> {"CP15", "SchoenCP15"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*CP21*)


registerTPMS["CP21",
	<|
		"MeshDataSymbolName" -> "$cp21",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/cpfamily.html"},
		"Comments" -> {},
		"Aliases" -> {"SchoenCP21"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*CP27*)


registerTPMS["CP27",
	<|
		"MeshDataSymbolName" -> "$cp27",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/cpfamily.html"},
		"Comments" -> {},
		"Aliases" -> {"SchoenCP27"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*CP33*)


registerTPMS["CP33",
	<|
		"MeshDataSymbolName" -> "$cp33",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/cpfamily.html"},
		"Comments" -> {},
		"Aliases" -> {"SchoenCP33"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*CP39*)


registerTPMS["CP39",
	<|
		"MeshDataSymbolName" -> "$cp39",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/cpfamily.html"},
		"Comments" -> {},
		"Aliases" -> {"SchoenCP39"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*CP45*)


registerTPMS["CP45",
	<|
		"MeshDataSymbolName" -> "$cp45",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/cpfamily.html"},
		"Comments" -> {},
		"Aliases" -> {"SchoenCP45"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*Starfish*)


registerTPMS["Starfish",
	<|
		"MeshDataSymbolName" -> "$star31",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/starfish/starfish.html"},
		"Comments" -> {"Rhombic configuration clipped with centered cube with side length 4 offset 0.375 in each direction.",
					"This clipping region gives a connected mesh and avoids non-manifold vertices on the boundary."},
		"Aliases" -> {"Starfish31"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*Starfish43*)


registerTPMS["Starfish43",
	<|
		"MeshDataSymbolName" -> "$star43",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/starfish/starfish.html"},
		"Comments" -> {"Rhombic configuration clipped with centered cube with side length 4 offset 0.375 in each direction.",
					"This clipping region gives a connected mesh and avoids non-manifold vertices on the boundary."},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*Starfish47*)


registerTPMS["Starfish47",
	<|
		"MeshDataSymbolName" -> "$star47",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/starfish/starfish.html"},
		"Comments" -> {"Rhombic configuration clipped with centered cube with side length 4 offset 0.375 in each direction.",
					"This clipping region gives a connected mesh and avoids non-manifold vertices on the boundary."},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*Starfish55*)


registerTPMS["Starfish55",
	<|
		"MeshDataSymbolName" -> "$star55",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/starfish/starfish.html"},
		"Comments" -> {"Rhombic configuration clipped with centered cube with side length 4 offset 0.375 in each direction.",
					"This clipping region gives a connected mesh and avoids non-manifold vertices on the boundary."},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*Starfish59*)


registerTPMS["Starfish59",
	<|
		"MeshDataSymbolName" -> "$star59",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/starfish/starfish.html"},
		"Comments" -> {"Rhombic configuration clipped with centered cube with side length 4 offset 0.375 in each direction.",
					"This clipping region gives a connected mesh and avoids non-manifold vertices on the boundary."},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*Starfish63*)


registerTPMS["Starfish63",
	<|
		"MeshDataSymbolName" -> "$star63",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/starfish/starfish.html"},
		"Comments" -> {"Rhombic configuration clipped with centered cube with side length 4 offset 0.375 in each direction.",
					"This clipping region gives a connected mesh and avoids non-manifold vertices on the boundary."},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*Starfish75*)


registerTPMS["Starfish75",
	<|
		"MeshDataSymbolName" -> "$star75",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/starfish/starfish.html"},
		"Comments" -> {"Rhombic configuration clipped with centered cube with side length 4 offset 0.375 in each direction.",
					"This clipping region gives a connected mesh and avoids non-manifold vertices on the boundary."},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*Starfish87*)


registerTPMS["Starfish87",
	<|
		"MeshDataSymbolName" -> "$star87",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/starfish/starfish.html"},
		"Comments" -> {"Rhombic configuration clipped with centered cube with side length 4 offset 0.375 in each direction.",
					"This clipping region gives a connected mesh and avoids non-manifold vertices on the boundary."},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*CD*)


registerTPMS["CD",
	<|
		"MeshDataSymbolName" -> "$schoenCD",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/periodic.html#complementaryd",
					"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/disphenoids.html"},
		"Comments" -> {"Rhombic configuration clipped with centered cube with side length 4 offset 0.125 in each direction.",
					"This clipping region avoids non-manifold vertices on the boundary."},
		"Aliases" -> {"SchoenCD", "Disphenoid", "Disphenoid19"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*Magic CD*)


registerTPMS[{"Magic", "CD"},
	<|
		"MeshDataSymbolName" -> "$schoenCDmagic",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/periodic.html#complementaryd",
					"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/disphenoids.html",
					"https://en.wikipedia.org/wiki/Magic_angle"},
		"Comments" -> {"Rotated version of \"CD\" with period {1/Sqrt[3], 1, 1/Sqrt[2]}.", "The angle ArcSec[Sqrt[3]] is referred to as the magic angle.", 
					"Rotation transform: TransformedRegion[UTTriplyPeriodicMinimalSurface[\"CD\"],RotationTransform[-\[Pi]/12,{0,0,1}]@*RotationTransform[-ArcSec[Sqrt[3]],{-1,1,0}]]"},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*Disphenoid*)


registerTPMS["Disphenoid",
	<|
		"MeshDataSymbolName" -> "$ds31",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/disphenoids.html"},
		"Comments" -> {"Rhombic configuration clipped with centered cube with side length 4 offset 0.5686132007374 in each direction.",
					"This clipping region gives a connected mesh and avoids non-manifold vertices on the boundary."},
		"Aliases" -> {"Disphenoid31"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*Magic Disphenoid*)


registerTPMS[{"Magic", "Disphenoid"},
	<|
		"MeshDataSymbolName" -> "$ds31magic",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/disphenoids.html",
					"https://en.wikipedia.org/wiki/Magic_angle"},
		"Comments" -> {"Rotated version of \"Disphenoid31\" with period {1/Sqrt[3], 1, 1/Sqrt[2]}.", "The angle ArcSec[Sqrt[3]] is referred to as the magic angle.", 
					"Rotation transform: TransformedRegion[UTTriplyPeriodicMinimalSurface[\"Disphenoid31\"],RotationTransform[-\[Pi]/12,{0,0,1}]@*RotationTransform[-ArcSec[Sqrt[3]],{-1,1,0}]]"},
		"Aliases" -> {{"Magic", "Disphenoid31"}}
	|>
];


(* ::Subsubsection::Closed:: *)
(*Disphenoid35*)


registerTPMS["Disphenoid35",
	<|
		"MeshDataSymbolName" -> "$ds35",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/disphenoids.html"},
		"Comments" -> {"Rhombic configuration clipped with centered cube with side length 4 offset 0.125 in each direction.",
					"This clipping region avoids non-manifold vertices on the boundary."},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*Magic Disphenoid35*)


registerTPMS[{"Magic", "Disphenoid35"},
	<|
		"MeshDataSymbolName" -> "$ds35magic",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/disphenoids.html",
					"https://en.wikipedia.org/wiki/Magic_angle"},
		"Comments" -> {"Rotated version of \"Disphenoid35\" with period {1/Sqrt[3], 1, 1/Sqrt[2]}.", "The angle ArcSec[Sqrt[3]] is referred to as the magic angle.", 
					"Rotation transform: TransformedRegion[UTTriplyPeriodicMinimalSurface[\"Disphenoid35\"],RotationTransform[-\[Pi]/12,{0,0,1}]@*RotationTransform[-ArcSec[Sqrt[3]],{-1,1,0}]]"},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*Disphenoid43*)


registerTPMS["Disphenoid43",
	<|
		"MeshDataSymbolName" -> "$ds43",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/disphenoids.html"},
		"Comments" -> {"Rhombic configuration clipped with centered cube with side length 4 offset 0.5686132007374 in each direction.",
					"This clipping region gives a connected mesh and avoids non-manifold vertices on the boundary."},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*Disphenoid51*)


registerTPMS["Disphenoid51",
	<|
		"MeshDataSymbolName" -> "$ds51",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/disphenoids.html"},
		"Comments" -> {"Rhombic configuration clipped with centered cube with side length 4 offset 0.0625 in each direction.",
					"This clipping region avoids non-manifold vertices on the boundary."},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*Disphenoid55*)


registerTPMS["Disphenoid55",
	<|
		"MeshDataSymbolName" -> "$ds55",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/disphenoids.html"},
		"Comments" -> {"Rhombic configuration clipped with centered cube with side length 4 offset 0.5686132007374 in each direction.",
					"This clipping region gives a connected mesh and avoids non-manifold vertices on the boundary."},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*Disphenoid67*)


registerTPMS["Disphenoid67",
	<|
		"MeshDataSymbolName" -> "$ds67",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/disphenoids.html"},
		"Comments" -> {"Rhombic configuration clipped with centered cube with side length 4 offset 0.125 in each direction.",
					"This clipping region avoids non-manifold vertices on the boundary."},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*P3a*)


registerTPMS["P3a",
	<|
		"MeshDataSymbolName" -> "$p3a",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/periodic.html#P3a",
					"https://eprints.bbk.ac.uk/id/eprint/282/1/mackay1.pdf"},
		"Comments" -> {"Rhombic configuration clipped with the middle 1/3 cube.",
					"This clipping region gives a connected mesh and avoids non-manifold vertices on the boundary."},
		"Aliases" -> {"LordP3a", "LordMackayP3a"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*SS*)


registerTPMS["SS",
	<|
		"MeshDataSymbolName" -> "$ss",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/hybrids.html",
					"https://minimalsurfaces.blog/home/repository/triply-periodic/schoen-s-s/"},
		"Comments" -> {"This surface has period {1/Sqrt[3], 1, 0.7827728986740112`}."},
		"Aliases" -> {"S'S''"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*SSP*)


registerTPMS["SSP",
	<|
		"MeshDataSymbolName" -> "$ssp",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/hybrids.html"},
		"Comments" -> {"This surface has period {1/Sqrt[3], 1, 0.6883723735809326`}."},
		"Aliases" -> {"S'S''|P"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*HT*)


registerTPMS["HT",
	<|
		"MeshDataSymbolName" -> "$ht",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/hybrids.html",
					"https://minimalsurfaces.blog/home/repository/triply-periodic/schoen-h-t/"},
		"Comments" -> {"Clipped hexagonal tiling with period {1/Sqrt[3], 1, 0.24411456286907196`}."},
		"Aliases" -> {"H'T"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*HR*)


registerTPMS["HR",
	<|
		"MeshDataSymbolName" -> "$hr",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/hybrids.html",
					"https://minimalsurfaces.blog/home/repository/triply-periodic/schoen-h-r/"},
		"Comments" -> {"Clipped hexagonal tiling with period {1/Sqrt[3], 1, 0.22812364995479584`}."},
		"Aliases" -> {"H''R"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*TR*)


registerTPMS["TR",
	<|
		"MeshDataSymbolName" -> "$tr",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/hybrids.html",
					"https://minimalsurfaces.blog/home/repository/triply-periodic/schoen-t-r/"},
		"Comments" -> {"Clipped hexagonal tiling with period {1/Sqrt[3], 1, 0.17231673002243042`}."},
		"Aliases" -> {"T'R'"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*HTHR*)


registerTPMS["HTHR",
	<|
		"MeshDataSymbolName" -> "$hthr",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/hybrids.html"},
		"Comments" -> {"Clipped hexagonal tiling with period {1/Sqrt[3], 1, 0.23699600994586945`}."},
		"Aliases" -> {"H'T|H''R"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*TRHT*)


registerTPMS["TRHT",
	<|
		"MeshDataSymbolName" -> "$trht",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/hybrids.html"},
		"Comments" -> {"Clipped hexagonal tiling with period {1/Sqrt[3], 1, 0.17861740291118622`}."},
		"Aliases" -> {"T'R'|H'T"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*HRTR*)


registerTPMS["HRTR",
	<|
		"MeshDataSymbolName" -> "$hrtr",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/hybrids.html"},
		"Comments" -> {"Clipped hexagonal tiling with period {1/Sqrt[3], 1, 0.16838109493255615`}."},
		"Aliases" -> {"H''R'|T'R'"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*H*)


registerTPMS["H",
	<|
		"MeshDataSymbolName" -> "$schwarzH",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/periodic.html#hsurface",
					"https://minimalsurfaces.blog/home/repository/triply-periodic/schwarz-h-surfaces/"},
		"Comments" -> {"Clipped hexagonal tiling with period {1/Sqrt[3], 1, 1/3}."},
		"Aliases" -> {"SchwarzH"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*R2*)


registerTPMS["R2",
	<|
		"MeshDataSymbolName" -> "$rii",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/periodic.html#rII",
					"https://minimalsurfaces.blog/home/repository/triply-periodic/schoen-r2/"},
		"Comments" -> {"Clipped tiling with period {1/Sqrt[3], 1, 1/5}."},
		"Aliases" -> {"SchoenR2", "RII", "SchoenRII"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*R3*)


registerTPMS["R3",
	<|
		"MeshDataSymbolName" -> "$riii",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/periodic.html#rIII",
					"https://minimalsurfaces.blog/home/repository/triply-periodic/schoen-r3/"},
		"Comments" -> {"Clipped hexagonal tiling with period {1/Sqrt[3], 1, 1/(6 Sqrt[3])}."},
		"Aliases" -> {"SchoenR3", "RIII", "SchoenRIII"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*I6*)


registerTPMS["I6",
	<|
		"MeshDataSymbolName" -> "$i6",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/periodic.html#I6",
					"https://minimalsurfaces.blog/home/repository/triply-periodic/schoen-i6/"},
		"Comments" -> {"This surface has period {1, 1, 4/5}.",
					"Clipped tiling inside the bounds {{0.25,1.25},{0.25,1.25},{0.2,1.0}}.",
					"This clipping region avoids non-manifold vertices on the boundary."},
		"Aliases" -> {"SchoenI6"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*I8*)


registerTPMS["I8",
	<|
		"MeshDataSymbolName" -> "$i8",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/periodic.html#I8"},
		"Comments" -> {"This surface has period {1, 1, Sqrt[2]/4}.",
					"Clipped tiling off center in the z direction by a factor of 1/8.",
					"This clipping region avoids non-manifold vertices on the boundary."},
		"Aliases" -> {"SchoenI8"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*I9*)


registerTPMS["I9",
	<|
		"MeshDataSymbolName" -> "$i9",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/periodic.html#I9"},
		"Comments" -> {"This surface has period {1, 1, 1/2}.",
					"Clipped tiling inside the bounds {{0.0,1.0},{0.0,1.0},{0.0,0.5}}.",
					"This clipping region avoids non-manifold vertices on the boundary."},
		"Aliases" -> {"SchoenI9"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*CLP*)


registerTPMS["CLP",
	<|
		"MeshDataSymbolName" -> "$clp",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/periodic.html#clp",
					"https://minimalsurfaces.blog/home/repository/triply-periodic/schwarz-clp-surfaces/"},
		"Comments" -> {"Clipped tiling inside the bounds {{0.25,1.25},{0.0,1.0},{0.0,1.0}}.",
					"This clipping region avoids non-manifold vertices on the boundary."},
		"Aliases" -> {"SchwarzCLP"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*FRD*)


registerTPMS["FRD",
	<|
		"MeshDataSymbolName" -> "$frd",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/periodic.html#frd",
					"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/periodic.html#hexplane",
					"https://minimalsurfaces.blog/home/repository/triply-periodic/schoen-f-rd/"},
		"Comments" -> {},
		"Aliases" -> {"F-RD", "SchoenFRD", "Hexplane1"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*PFRD*)


registerTPMS["PFRD",
	<|
		"MeshDataSymbolName" -> "$pfrd",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/periodic.html#pfrd"},
		"Comments" -> {},
		"Aliases" -> {"P,F-RD", "SchoenPFRD"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*GW*)


registerTPMS["GW",
	<|
		"MeshDataSymbolName" -> "$gw",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/periodic.html#gw"},
		"Comments" -> {"Clipped hexagonal tiling with period {1, Sqrt[3]/2, Sqrt[3]/2}."},
		"Aliases" -> {"SchoenGW"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*IWP*)


registerTPMS["IWP",
	<|
		"MeshDataSymbolName" -> "$iwp",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/periodic.html#iwp",
					"https://minimalsurfaces.blog/home/repository/triply-periodic/schoen-i-wp/"},
		"Comments" -> {},
		"Aliases" -> {"I-WP", "SchoenIWP"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*OCTO*)


registerTPMS["OCTO",
	<|
		"MeshDataSymbolName" -> "$octo",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/periodic.html#octo"},
		"Comments" -> {},
		"Aliases" -> {"O,C-TO", "SchoenOCTO"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*FRDr*)


registerTPMS["FRDr",
	<|
		"MeshDataSymbolName" -> "$frdr",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/periodic.html#frdr",
					"https://minimalsurfaces.blog/home/repository/triply-periodic/schoens-unnamed-surface-12-f-rd-r/"},
		"Comments" -> {"This surface has period {0.4544171094894409`, 0.4620257616043091`, 1}"},
		"Aliases" -> {"F-RDr", "SchoenFRDr", "Box++|-"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*IWPr*)


registerTPMS["IWPr",
	<|
		"MeshDataSymbolName" -> "$iwpr",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/periodic.html#iwpr"},
		"Comments" -> {"This surface has period {0.4544171094894409`, 0.4620257616043091`, 1}"},
		"Aliases" -> {"I-WPr", "SchoenIWPr"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*Manta*)


registerTPMS["Manta",
	<|
		"MeshDataSymbolName" -> "$manta",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/periodic.html#manta19"},
		"Comments" -> {},
		"Aliases" -> {"Manta19"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*Manta35*)


registerTPMS["Manta35",
	<|
		"MeshDataSymbolName" -> "$manta35",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/periodic.html#manta35"},
		"Comments" -> {},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*Manta51*)


registerTPMS["Manta51",
	<|
		"MeshDataSymbolName" -> "$manta51",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/periodic.html#manta51"},
		"Comments" -> {},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*Triplane1*)


registerTPMS["Triplane1",
	<|
		"MeshDataSymbolName" -> "$tp1",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/periodic.html#triplane"},
		"Comments" -> {"Clipped tiling inside the bounds {{0.25,1.25},{0.25,1.25},{0.25,1.25}}.",
					"This clipping region gives a connected mesh and avoids non-manifold vertices on the boundary."},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*Triplane2*)


registerTPMS["Triplane2",
	<|
		"MeshDataSymbolName" -> "$tp2",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/periodic.html#triplane"},
		"Comments" -> {"Clipped tiling inside the bounds {{0.125,1.125},{0.125,1.125},{0.125,1.125}}.",
					"This clipping region gives a connected mesh and avoids non-manifold vertices on the boundary."},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*Triplane3*)


registerTPMS["Triplane3",
	<|
		"MeshDataSymbolName" -> "$tp3",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/periodic.html#triplane"},
		"Comments" -> {"Clipped tiling inside the bounds {{0.25,1.25},{0.25,1.25},{0.25,1.25}}.",
					"This clipping region gives a connected mesh and avoids non-manifold vertices on the boundary."},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*Triplane4*)


registerTPMS["Triplane4",
	<|
		"MeshDataSymbolName" -> "$tp4",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/periodic.html#triplane"},
		"Comments" -> {"Clipped tiling inside the bounds {{1/6,7/6},{1/6,7/6},{1/6,7/6}}.",
					"This clipping region gives a connected mesh and avoids non-manifold vertices on the boundary."},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*Triplane5*)


registerTPMS["Triplane5",
	<|
		"MeshDataSymbolName" -> "$tp5",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/periodic.html#triplane"},
		"Comments" -> {"Clipped tiling inside the bounds {{0.125,1.125},{0.125,1.125},{0.125,1.125}}.",
					"This clipping region gives a connected mesh and avoids non-manifold vertices on the boundary."},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*Hexplane2*)


registerTPMS["Hexplane2",
	<|
		"MeshDataSymbolName" -> "$hp2",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/periodic.html#hexplane"},
		"Comments" -> {},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*Hexplane3*)


registerTPMS["Hexplane3",
	<|
		"MeshDataSymbolName" -> "$hp3",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/periodic.html#hexplane"},
		"Comments" -> {},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*Hexplane4*)


registerTPMS["Hexplane4",
	<|
		"MeshDataSymbolName" -> "$hp4",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/periodic.html#hexplane"},
		"Comments" -> {},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*Hexplane5*)


registerTPMS["Hexplane5",
	<|
		"MeshDataSymbolName" -> "$hp5",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/periodic.html#hexplane"},
		"Comments" -> {},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*S*)


registerTPMS["S",
	<|
		"MeshDataSymbolName" -> "$fischerS",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/periodic.html#fishers"},
		"Comments" -> {"Clipped hexagonal tiling inside the bounds {{1.125,2.125},{1.125,2.125},{1.375,2.375}}.",
					"This clipping region gives a connected mesh and avoids non-manifold vertices on the boundary."},
		"Aliases" -> {"FischerS", "FischerKochS"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*CY*)


registerTPMS["CY",
	<|
		"MeshDataSymbolName" -> "$fischerCY",
		"Sources" -> {"http://facstaff.susqu.edu/brakke/evolver/examples/periodic/periodic.html#fishercy"},
		"Comments" -> {"Clipped hexagonal tiling inside the bounds {{1.3125,2.3125},{0.8125,1.8125},{1.4375,2.4375}}.",
					"This clipping region gives a connected mesh and avoids non-manifold vertices on the boundary."},
		"Aliases" -> {"FischerCY", "FischerKochCY"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*CH*)


registerTPMS["CH",
	<|
		"MeshDataSymbolName" -> "$schoenCH",
		"Sources" -> {"https://minimalsurfaces.blog/home/repository/triply-periodic/schoen-ch/"},
		"Comments" -> {"Clipped hexagonal tiling with period {1/Sqrt[3], 1, 0.45379143953323364`}."},
		"Aliases" -> {"SchoenCH"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*Lidinoid*)


registerTPMS["Lidinoid",
	<|
		"MeshDataSymbolName" -> "$lidinoid",
		"Sources" -> {"https://minimalsurfaces.blog/home/repository/triply-periodic/lidinoid/"},
		"Comments" -> {"Clipped hexagonal tiling with period {1/Sqrt[3], 1, 0.4406758248806`}."},
		"Aliases" -> {"L"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*tD*)


registerTPMS["tD",
	<|
		"MeshDataSymbolName" -> "$td",
		"Sources" -> {"https://minimalsurfaces.blog/home/repository/triply-periodic/hao-chens-o\[Delta]-t\[Delta]-family/",
					"https://arxiv.org/abs/1804.01442"},
		"Comments" -> {"This surface has period {0.6032315492630005`, 0.6032315492630005`, 1}.",
					"This surface belongs to 4 different parametric families of minimal surfaces."},
		"Aliases" -> {"tD*", "t\[CapitalDelta]", "oD", "o\[CapitalDelta]", "ChenExoticD"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*Wei*)


registerTPMS["Wei",
	<|
		"MeshDataSymbolName" -> "$wei",
		"Sources" -> {"https://minimalsurfaces.blog/home/repository/triply-periodic/weis-triply-periodic-surface-of-genus-4/",
					"https://doi.org/10.1007/BF01232021"},
		"Comments" -> {"This surface has period {0.7777995472887127`, 0.8315314971829638`, 1}."},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*RTW*)


registerTPMS["RTW",
	<|
		"MeshDataSymbolName" -> "$rtw",
		"Sources" -> {"https://minimalsurfaces.blog/home/repository/triply-periodic/triply-periodic-rtw-surfaces/"},
		"Comments" -> {"This surface has period {0.6130569733306103`, 1, 0.4423513114452362`}."},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*Batista*)


registerTPMS["Batista",
	<|
		"MeshDataSymbolName" -> "$batista",
		"Sources" -> {"https://minimalsurfaces.blog/home/repository/triply-periodic/box-symmetry-type-4/"},
		"Comments" -> {"This surface has period {0.9775073054584638`, 1, 0.9403476298859109`}."},
		"Aliases" -> {"Box+++|-"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*CrPD*)


registerTPMS["CrPD",
	<|
		"MeshDataSymbolName" -> "$crpd",
		"Sources" -> {"https://minimalsurfaces.blog/home/repository/triply-periodic/complementary-crpd/"},
		"Comments" -> {"Clipped triangular tiling with period {0.44877946376800537`, 1, 0.5182058811187744`}."},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*CS4*)


registerTPMS["CS4",
	<|
		"MeshDataSymbolName" -> "$costaschrek",
		"Sources" -> {"https://minimalsurfaces.blog/home/repository/triply-periodic/triply-periodic-costa-scherk/",
					"https://arxiv.org/abs/1807.08661"},
		"Comments" -> {"Clipped parallelepiped tiling with period {0.47756860797201117`, 1, 0.47756860845069027`}."},
		"Aliases" -> {"CostaSchrek"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*TriCosta5*)


registerTPMS["TriCosta5",
	<|
		"MeshDataSymbolName" -> "$tcosta5",
		"Sources" -> {"https://minimalsurfaces.blog/home/repository/triply-periodic/catenoid-scherk/"},
		"Comments" -> {"Clipped parallelepiped tiling with period {0.5255458950996399`, 1, 0.5255458985900441}."},
		"Aliases" -> {"CatenoidSchrek", "Horgan"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*TriCosta7*)


registerTPMS["TriCosta7",
	<|
		"MeshDataSymbolName" -> "$tcosta7",
		"Sources" -> {"https://minimalsurfaces.blog/home/repository/triply-periodic/triangular-antiprism-g7/"},
		"Comments" -> {"Clipped parallelepiped tiling with period {0.5255458950996399`, 1, 0.5255458985900441}."},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*SS4*)


registerTPMS["SS4",
	<|
		"MeshDataSymbolName" -> "$clphandle",
		"Sources" -> {"https://minimalsurfaces.blog/home/repository/triply-periodic/clp-with-handle/",
					"https://arxiv.org/abs/1807.08661"},
		"Comments" -> {"Clipped parallelepiped tiling with period {0.9319925308227539`, 0.9319925308227539`, 1}."},
		"Aliases" -> {"CLPHandle"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*oSP*)


registerTPMS["oSP",
	<|
		"MeshDataSymbolName" -> "$osp",
		"Sources" -> {"https://minimalsurfaces.blog/home/repository/triply-periodic/os-deformation-of-p/"},
		"Comments" -> {},
		"Aliases" -> {"oS-P"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*Box+-|+*)


registerTPMS["Box+-|+",
	<|
		"MeshDataSymbolName" -> "$boxpnp",
		"Sources" -> {"https://minimalsurfaces.blog/home/repository/triply-periodic/box-symmetry-type/"},
		"Comments" -> {"This surface has period {0.43598705530166626` ,1, 0.373664425917983`}."},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*Box+-+|-*)


registerTPMS["Box+-+|-",
	<|
		"MeshDataSymbolName" -> "$boxpnpn",
		"Sources" -> {"https://minimalsurfaces.blog/home/repository/triply-periodic/box-symmetry-type-2/"},
		"Comments" -> {"This surface has period {0.49671179688239275` ,1, 0.49671179688239275`}."},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*Box+-+|+*)


registerTPMS["Box+-+|+",
	<|
		"MeshDataSymbolName" -> "$boxpnpp",
		"Sources" -> {"https://minimalsurfaces.blog/home/repository/triply-periodic/box-symmetry-type-3/"},
		"Comments" -> {"This surface has period {1, 1, 0.746703070899162`}."},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*Box++|+-a*)


registerTPMS["Box++|+-a",
	<|
		"MeshDataSymbolName" -> "$boxpppna",
		"Sources" -> {"https://minimalsurfaces.blog/home/repository/triply-periodic/box-type-a/"},
		"Comments" -> {"This surface has period {0.9830702543258667`, 1, 0.5640897154808044`}."},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*Box++|+-b*)


registerTPMS["Box++|+-b",
	<|
		"MeshDataSymbolName" -> "$boxpppnb",
		"Sources" -> {"https://minimalsurfaces.blog/home/repository/triply-periodic/box-symmetry-type-b/"},
		"Comments" -> {"This surface has period {0.7289177775382996`, 1, 0.5968437194824219`}."},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*BoxIA2*)


registerTPMS["BoxIA2",
	<|
		"MeshDataSymbolName" -> "$boxia2",
		"Sources" -> {"https://minimalsurfaces.blog/home/repository/triply-periodic/square-box-with-diagonals-g6/box-g6-type-ii-a-1/"},
		"Comments" -> {"This surface has period {0.9944413452125376`, 1, 0.9944413452125376`}."},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*BoxIA3*)


registerTPMS["BoxIA3",
	<|
		"MeshDataSymbolName" -> "$boxia3",
		"Sources" -> {"https://minimalsurfaces.blog/home/repository/triply-periodic/square-box-with-diagonals-g6/box-g6-type-i-a-3/"},
		"Comments" -> {"This surface has period {0.4140818466197099`, 1, 0.4140818466197099`}."},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*BoxIB3*)


registerTPMS["BoxIB3",
	<|
		"MeshDataSymbolName" -> "$boxib3",
		"Sources" -> {"https://minimalsurfaces.blog/home/repository/triply-periodic/square-box-with-diagonals-g6/box-g6-type-i-b-3/"},
		"Comments" -> {"This surface has period {0.11340793527567679`, 1, 0.11340793527567679`}."},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*BoxIIA1*)


registerTPMS["BoxIIA1",
	<|
		"MeshDataSymbolName" -> "$boxiia1",
		"Sources" -> {"https://minimalsurfaces.blog/home/repository/triply-periodic/square-box-with-diagonals-g6/box-g6-type-ii-a-1-2/"},
		"Comments" -> {"This surface has period {0.21205417484529798`, 1, 0.21205401420593262`}."},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*BoxIIA2*)


registerTPMS["BoxIIA2",
	<|
		"MeshDataSymbolName" -> "$boxiia2",
		"Sources" -> {"https://minimalsurfaces.blog/home/repository/triply-periodic/square-box-with-diagonals-g6/box-g6-type-ii-a-2/"},
		"Comments" -> {"This surface has period {0.12697772681713104`, 1, 0.12697772655221545`}."},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*BoxIIB1*)


registerTPMS["BoxIIB1",
	<|
		"MeshDataSymbolName" -> "$boxiib1",
		"Sources" -> {"https://minimalsurfaces.blog/home/repository/triply-periodic/square-box-with-diagonals-g6/box-g6-type-ii-b-1/"},
		"Comments" -> {"This surface has period {0.941871119800288`, 1, 0.9418711198002879`}."},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*BoxIIB2*)


registerTPMS["BoxIIB2",
	<|
		"MeshDataSymbolName" -> "$boxiib2",
		"Sources" -> {"https://minimalsurfaces.blog/home/repository/triply-periodic/square-box-with-diagonals-g6/box-g6-type-ii-b-2/"},
		"Comments" -> {"This surface has period {0.6239758970813533`, 1, 0.6239758729934692`}."},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*BoxIIB3*)


registerTPMS["BoxIIB3",
	<|
		"MeshDataSymbolName" -> "$boxiib3",
		"Sources" -> {"https://minimalsurfaces.blog/home/repository/triply-periodic/square-box-with-diagonals-g6/box-g6-type-ii-b-3/"},
		"Comments" -> {"This surface has period {0.7540267199982758`, 1, 0.7540267703955201`}."},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*oP*)


registerTPMS["oP",
	<|
		"MeshDataSymbolName" -> "$op",
		"Sources" -> {"https://minimalsurfaces.blog/home/repository/triply-periodic/op-deformation-of-p/"},
		"Comments" -> {"This surface has period {0.811840037941845`, 1, 0.9111242851652339`}."},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*oD*)


registerTPMS["oD",
	<|
		"MeshDataSymbolName" -> "$od",
		"Sources" -> {"https://minimalsurfaces.blog/home/repository/triply-periodic/od-orthorhombic-deformation-of-d-surface/"},
		"Comments" -> {"This surface has period {0.7695426412748032`, 1, 0.30812944934337855`}."},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*Magic rPD*)


registerTPMS[{"Magic", "rPD"},
	<|
		"MeshDataSymbolName" -> "$rpdmagic",
		"Sources" -> {"https://minimalsurfaces.blog/home/repository/triply-periodic/rpd-deformation/",
					"https://en.wikipedia.org/wiki/Magic_angle"},
		"Comments" -> {"Rotated version of \"rPD\" with period {1/Sqrt[3], 1, 1/Sqrt[2]}.", "The angle ArcSec[Sqrt[3]] is referred to as the magic angle.", 
					"Rotation transform: TransformedRegion[UTTriplyPeriodicMinimalSurface[\"rPD\"],RotationTransform[-\[Pi]/12,{0,0,1}]@*RotationTransform[-ArcSec[Sqrt[3]],{-1,1,0}]]"},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*rPD*)


registerTPMS["rPD",
	<|
		"MeshDataSymbolName" -> "$rpd",
		"Sources" -> {"https://minimalsurfaces.blog/home/repository/triply-periodic/rpd-deformation/"},
		"Comments" -> {"Rotated model from source with RotationTransform[ArcSec[Sqrt[3]],{-1,1,0}]@*RotationTransform[7\[Pi]/12,{0,0,1}] to give cubic periodicity."},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*oH*)


registerTPMS["oH",
	<|
		"MeshDataSymbolName" -> "$oh",
		"Sources" -> {"https://minimalsurfaces.blog/home/repository/triply-periodic/deformed-h/"},
		"Comments" -> {"This surface has period {0.610746024850757`, 1, 0.493712233545088`}."},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*DeformedFRD*)


registerTPMS["DeformedFRD",
	<|
		"MeshDataSymbolName" -> "$deformedfrd",
		"Sources" -> {"https://minimalsurfaces.blog/home/repository/triply-periodic/deformed-f-rd/"},
		"Comments" -> {"This surface has period {0.5320808637923237`, 1, 0.5315422103324833`}."},
		"Aliases" -> {}
	|>
];


(* ::Subsubsection::Closed:: *)
(*DeformedHT*)


registerTPMS["DeformedHT",
	<|
		"MeshDataSymbolName" -> "$deformedht",
		"Sources" -> {"https://minimalsurfaces.blog/home/repository/triply-periodic/deformed-h-t/"},
		"Comments" -> {"This surface has period {0.5104066133499146`, 1, 0.5104063337159426`}."},
		"Aliases" -> {"DeformedH'T"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*DeformedSS*)


registerTPMS["DeformedSS",
	<|
		"MeshDataSymbolName" -> "$deformedss",
		"Sources" -> {"https://minimalsurfaces.blog/home/repository/triply-periodic/deformed-s-s/"},
		"Comments" -> {"This surface has period {0.5104066133499146`, 1, 0.5104063337159426`}."},
		"Aliases" -> {"DeformedS'S''"}
	|>
];


(* ::Subsubsection::Closed:: *)
(*tDHandles*)


registerTPMS["tDHandles",
	<|
		"MeshDataSymbolName" -> "$tdhandles",
		"Sources" -> {"https://minimalsurfaces.blog/home/repository/triply-periodic/td-\[Delta]-with-handles/"},
		"Comments" -> {"This surface has period {0.2527958793506884`, 1, 0.2527958781007859`}."},
		"Aliases" -> {"t\[CapitalDelta]Handles"}
	|>
];


(* ::Subsection::Closed:: *)
(*Template Initialization*)


(* ::Subsubsection::Closed:: *)
(*Utilities*)


canonicalTPMSName[surf_] /; KeyExistsQ[$tpmsTemplates, surf] := surf


canonicalTPMSName[surf_] :=
	Block[{pos},
		pos = Position[$tpmsTemplates, surf, Heads -> False];
		
		pos[[1, 1, 1]] /; MatchQ[pos, {{Key[_], _, _}}]
	]


canonicalTPMSName[___] = $Failed;


lookupTPMSData[surf_] :=
	Block[{csurf},
		csurf = canonicalTPMSName[surf];
		
		$tpmsTemplates[csurf] /; csurf =!= $Failed
	]


lookupTPMSData[___] = $Failed;


tpmsKeySort[assoc_] := KeySortBy[assoc, If[ListQ[#], #, {#, -1}]&]


$tpmsMeshDataContext = "UTCG`TPMS`MeshData`";


initializeTPMSMeshData[assoc_] /; KeyExistsQ[assoc, "MeshDataSymbolName"] := 
	Block[{meshdata},
		meshdata = ToExpression[$tpmsMeshDataContext <> assoc["MeshDataSymbolName"]];
		
		Prepend[assoc, "MeshData" -> meshdata]
	]

initializeTPMSMeshData[assoc_] := assoc


initializeTPMSTemplates[] :=
	(
		$tpmsTemplates = initializeTPMSMeshData /@ tpmsKeySort[$tpmsTemplates];
	)


(* ::Subsubsection::Closed:: *)
(*Run*)


initializeTPMSTemplates[];


Scan[
	If[Length[DownValues[#]] == 0,
		# := (Clear[#]; Get[FileNameJoin[{$tpmsDirectory, SymbolName[#] <> ".mx"}]]; #)
	]&,
	Lookup[Values[$tpmsTemplates], "MeshData"]
];


(* ::Section:: *)
(*TPMS*)


(* ::Subsection::Closed:: *)
(*UTTriplyPeriodicMinimalSurface*)


(* ::Subsubsection::Closed:: *)
(*Main*)


UTTriplyPeriodicMinimalSurface[surf_, metadata_?tpmsMetaDataQ] :=
	With[{res = tpmsMetaData[surf, metadata]},
		res /; res =!= $Failed
	]


UTTriplyPeriodicMinimalSurface[surf_, args___] :=
	Block[{sdata, res},
		sdata = lookupTPMSData[surf];
		(
			res = tpmsMesh[sdata["MeshData"], args];
			
			res /; res =!= $Failed
			
		) /; sdata =!= $Failed
	]


UTTriplyPeriodicMinimalSurface[mr_MeshRegion, args__] :=
	Block[{$lookup, lookupTPMSData},
		$lookup[_] = {MeshCoordinates[mr], MeshCells[mr, 2, "Multicells" -> True][[1, 1]]};
		lookupTPMSData[_] = <|"MeshData" -> $lookup|>;
		
		UTTriplyPeriodicMinimalSurface["", args]
	]


UTTriplyPeriodicMinimalSurface[] := DeleteCases[Keys[$tpmsTemplates], {"Magic", ___}]


(* ::Subsubsection::Closed:: *)
(*tpmsMetaData*)


$validTPMSMetaProperties = {"Aliases", "Comments", "Sources"};


tpmsMetaDataQ["Information"] = True;
Scan[(tpmsMetaDataQ[#] = True)&, $validTPMSMetaProperties];
tpmsMetaDataQ[l:{__String}] := VectorQ[l, tpmsMetaDataQ]
tpmsMetaDataQ[___] = False;


tpmsMetaData[surf_, key_] :=
	Block[{sdata, csurf, res},
		sdata = lookupTPMSData[surf];
		(
			csurf = canonicalTPMSName[surf];
			
			res = iTpmsMetaData[csurf, sdata, key];
			
			res /; res =!= $Failed
			
		) /; sdata =!= $Failed
	]


tpmsMetaData[___] = $Failed;


iTpmsMetaData[surf_, data_, "Information"] := iTpmsMetaData[surf, data, $validTPMSMetaProperties]


iTpmsMetaData[surf_, data_, key:"Aliases"] := 
	With[{res = data[key]},
		Prepend[res, surf] /; !MissingQ[res]
	]


iTpmsMetaData[surf_, data_, key:("Sources" | "Comments")] :=
	With[{res = data[key]},
		res /; !MissingQ[res]
	]


iTpmsMetaData[surf_, data_, keys_List] :=
	Block[{res},
		res = iTpmsMetaData[surf, data, #]& /@ keys;
		(
			AssociationThread[keys, res]
			
		) /; FreeQ[res, $Failed, {1}]
	]


iTpmsMetaData[___] = $Failed;


(* ::Subsubsection::Closed:: *)
(*tpmsMesh*)


tpmsMesh[surffunc_, Automatic, args___] := tpmsMesh[surffunc, $tpmscorner, args]
tpmsMesh[surffunc_, corner_, Automatic, args___] := tpmsMesh[surffunc, corner, $tpmsscale, args]
tpmsMesh[surffunc_, corner_, scale_, Automatic, args___] := tpmsMesh[surffunc, corner, scale, $tpmscnts, args]
tpmsMesh[surffunc_, corner_, scale_, cnts_, Automatic, args___] := tpmsMesh[surffunc, corner, scale, cnts, $tpmsthickness, args]
tpmsMesh[surffunc_, corner_, scale_, cnts_, thickness_, Automatic, args___] := tpmsMesh[surffunc, corner, scale, cnts, thickness, $tpmssize, args]
tpmsMesh[surffunc_, corner_, scale_, cnts_, thickness_, coarseness_, Automatic] := tpmsMesh[surffunc, corner, scale, cnts, thickness, coarseness, $tpmsrettype]


tpmsMesh[surffunc_, assoc_?AssociationQ] :=
	With[{inputdata = tpmsAssociationData[surffunc, assoc]},
		(
			tpmsMesh @@ inputdata
			
		) /; ListQ[inputdata]
	]


tpmsMesh[surffunc_, All] := tpmsMesh[surffunc, #]& /@ $tpmssizes
tpmsMesh[surffunc_, corner_, scale_, cnts_, thickness_, All] := tpmsMesh[surffunc, corner, scale, cnts, thickness, #]& /@ $tpmssizes


tpmsMesh[surffunc_, size_String] := tpmsMesh[surffunc, $tpmscorner, $tpmsscale, $tpmscnts, $tpmsthickness, size]


tpmsMesh[surffunc_, cnt_Integer?Positive] := tpmsMesh[surffunc, Automatic, Automatic, cnt]


tpmsMesh[surffunc_, corner_:$tpmscorner, scale_:$tpmsscale, cnts_:$tpmscnts, thickness_:$tpmsthickness, size_:$tpmssize, ret_:$tpmsrettype] /; validTPMSInputQ[corner, scale, cnts, thickness, size] :=
	Block[{meshdata, scale2, basecoords, basecells, normals, translate, flip, coords, cells, len, cnt, mesh, res},
		meshdata = surffunc[size];
		(
			{basecoords, basecells} = meshdata;
			
			scale2 = scale*Abs[Subtract @@@ CoordinateBounds[basecoords]];
			basecoords = (TranslationTransform[corner] @* ScalingTransform[scale{1, 1, 1}])[basecoords];
			
			translate = surffunc["TranslationFunction"];
			If[Head[translate] =!= Function,
				translate = Transpose[#2{#3, #4, #5} + Transpose[#1]]&;
			];
			
			flip = surffunc["Flip"];
			If[Head[flip] =!= Function,
				flip = #1&;
			];
			
			{coords, cells, normals} = tpmsMeshData[basecoords, basecells, cnts, scale2, thickness, translate, flip];
			
			res = extrudeMesh[coords, cells, normals, thickness, ret];
			
			res /; res =!= $Failed
			
		) /; ListQ[meshdata] && Length[meshdata] == 2 && VectorQ[meshdata, MatrixQ]
	]


tpmsMesh[___] = $Failed;


(* ::Subsubsection::Closed:: *)
(*Lookup and argument utilities*)


$tpmsDirectory = FileNameJoin[{DirectoryName[$InputFileName], "Support Files", "TPMS Data"}];


makeTPMSConstructor = Function[{F, template}, Clear[F];F[args___] := With[{res = tpmsMesh[template, args]}, res /; res =!= $Failed];, HoldAll];


$tpmssizes = {"Coarse", "MediumCoarse", "Medium", "MediumFine", "Fine"};


$tpmscorner = {0, 0, 0};
$tpmsscale = {1, 1, 1};
$tpmscnts = {1, 1, 1};
$tpmsthickness = 0;
$tpmssize = $tpmssizes[[3]];
$tpmsrettype = "MeshRegion";


validTPMSInputQ[corner_List, scale_, cnts_, thickness_, size_String] :=
	And[
		Length[corner] === 3 && ConstantRegionQ[Point[corner]],
		TrueQ[scale > 0] || (Length[scale] === 3 && VectorQ[scale, Positive]),
		TrueQ[IntegerQ[cnts] && cnts > 0] || (Length[cnts] === 3 && VectorQ[cnts, IntegerQ[#] && # > 0&]),
		TrueQ[thickness >= 0] || !NumericQ[thickness],
		MemberQ[$tpmssizes, size]
	]

validTPMSInputQ[___] = False;


tpmsAssociationData[surffunc_, assoc_] :=
	{
		surffunc,
		Lookup[assoc, "Corner", $tpmscorner],
		Lookup[assoc, "Scale", $tpmsscale],
		Lookup[assoc, "Counts", $tpmscnts],
		Lookup[assoc, "Thickness", $tpmsthickness],
		Lookup[assoc, "Coarseness", $tpmssize],
		Lookup[assoc, "ReturnType", $tpmsrettype]
	}


(* ::Subsubsection::Closed:: *)
(*Mesh creation*)


(* ::Text:: *)
(*Broken:*)
(*{MediumFine,BoxIB3}*)
(*{Fine,BoxIB3}*)
(*{Coarse,CrPD}*)
(*{MediumCoarse,CrPD}*)
(*{Medium,CrPD}*)
(*{MediumFine,CrPD}*)
(*{Fine,CrPD}*)
(*{Coarse,GW}*)
(*{MediumCoarse,GW}*)
(*{Medium,GW}*)
(*{MediumFine,GW}*)
(*{Fine,GW}*)


tpmsMeshData[basecoords_, basecells_, corner_, scale_] := tpmsMeshData[basecoords, basecells, corner, scale, 0]

tpmsMeshData[basecoords_, basecells_, corner_, scale_, thickness_] := tpmsMeshData[basecoords, basecells, corner, scale, thickness, Transpose[(#2*{#3,#4,#5})+Transpose[#]]&]

tpmsMeshData[basecoords_, basecells_, corner_, scale_, thickness_, translate_] := tpmsMeshData[basecoords, basecells, corner, scale, thickness, translate, #&]

tpmsMeshData[basecoords_, basecells_, x_Integer, scale_, thickness_, translate_, flip_] := tpmsMeshData[basecoords, basecells, {x, x, x}, scale, thickness, translate, flip]

tpmsMeshData[basecoords_, basecells_, {x_, y_, z_}, scale_, thickness_, translate_, flip_] := 
	Block[{len, cnt, coords, cells, normals},
		len = Length[basecoords];
		cnt = 0;
		
		{coords, cells} = Join @@@ Reap[
			Do[
				Sow[translate[basecoords, scale, i, j, k], "coords"];
				Sow[flip[basecells + len*(cnt++), i, j, k], "cells"];,
				{i, 0, x-1}, {j, 0, y-1}, {k, 0, z-1}
			],
			{"coords", "cells"}
		][[-1, All, 1]];
		
		If[TrueQ[Chop[thickness] == 0],
			normals = None,
			normals = tpmsCellSurfaceNormals[basecoords, basecells, {x, y, z}];
		];
		
		deleteDuplicateTPMSCoordinates[coords, cells, normals]
	]


extrudeMesh[mesh_, thickness_] := extrudeMesh[mesh, thickness, Automatic, Automatic]


extrudeMesh[mesh_, thickness_, nn_] := extrudeMesh[mesh, thickness, nn, Automatic]


extrudeMesh[scoords_, scells_, normals_, thickness_, ret_] /; !ListQ[normals] || Chop[thickness] == 0 := tpmsReturn[scoords, scells, ret]

extrudeMesh[scoords_, scells_, unitnormals_, thickness_, ret_] :=
	Block[{normals, t, cnt, bdedges, coords, cells, res},
		t = If[NumericQ[thickness], thickness, thickness /@ scoords];
		(
			normals = .5t * unitnormals;
			
			cnt = Length[scoords];
			bdedges = boundaryEdges[scells];
			
			coords = Join[
				Subtract[scoords, normals],
				Plus[scoords, normals]
			];
			
			cells = Join[
				Reverse[scells, {2}],
				scells + cnt
			];
			If[Length[bdedges] > 0,
				cells = Join[
					cells,
					Transpose[{#1, #2, #1+cnt}& @@ Transpose[bdedges]],
					Transpose[{#2, #2+cnt, #1+cnt}& @@ Transpose[bdedges]]
				]
			];
			
			tpmsReturn[coords, cells, ret]
			
		) /; NumericQ[t] || (VectorQ[t, Positive] && Length[t] == Length[scoords])
	]

extrudeMesh[___] = $Failed;


tpmsReturn[coords_, cells_, "BoundaryMeshRegion"] := 
	Block[{res},
		res = Quiet @ BoundaryMeshRegion[
			coords,
			Polygon[cells],
			Method -> {"EliminateUnusedCoordinates" -> False, "DeleteDuplicateCoordinates" -> False, "DeleteDuplicateCells" -> False, "TJunction" -> False, "CheckIntersections" -> False}
		];
		
		If[!BoundaryMeshRegionQ[res], $Failed, res]
	]


tpmsReturn[coords_, cells_, "ComplexData"] := {coords, cells}


tpmsReturn[coords_, cells_, "FaceData"] := Partition[coords[[Flatten[cells]]], 3]


tpmsReturn[coords_, cells_, _] := 
	MeshRegion[
		coords,
		Polygon[cells],
		Method -> {"EliminateUnusedCoordinates" -> False, "DeleteDuplicateCoordinates" -> False, "DeleteDuplicateCells" -> False, "TJunction" -> False}
	]


tpmsReturn[mesh_, "BoundaryMeshRegion"] := UTToBMR[mesh]


tpmsReturn[mesh_, "ComplexData"] := {MeshCoordinates[mesh], Join @@ MeshCells[mesh, 2, "Multicells" -> True][[All, 1]]}


tpmsReturn[mesh_, "FaceData"] := Join @@ MeshPrimitives[mesh, 2, "Multicells" -> True][[All, 1]]


tpmsReturn[mesh_, _] := mesh


tpmsReturn[___] = $Failed;


(* ::Subsubsection::Closed:: *)
(*Mesh complex utilities*)


(* ::Text:: *)
(*Is there a faster way to do this without the unpacked "MatrixColumns" property?*)


tpmsCellSurfaceNormals[coords_, cells_, {x_, y_, z_}] :=
	Block[{facenormals, adj, columns, vnormals},
		facenormals = faceNormals[coords, cells];
		
		adj = tpmsVertexFaceAdjacency[coords, cells];
		columns = adj["MatrixColumns"];
		
		vnormals = Developer`ToPackedArray[Mean[facenormals[[#]]]& /@ columns];
		vnormals /= Sqrt[Total[vnormals^2, {2}]];
		
		Flatten[ConstantArray[vnormals, x*y*z], 1]
	]


faceNormals[coords_, cells_] :=
	With[{tcoords = Transpose[coords], t = Transpose[cells]},
		With[{a1 = tcoords[[1, t[[1]]]], a2 = tcoords[[2, t[[1]]]], a3 = tcoords[[3, t[[1]]]]},
			With[{c1 = Subtract[tcoords[[1, t[[2]]]], a1], c2 = Subtract[tcoords[[2, t[[2]]]], a2], c3 = Subtract[tcoords[[3, t[[2]]]], a3],
				c4 = Subtract[tcoords[[1, t[[3]]]], a1], c5 = Subtract[tcoords[[2, t[[3]]]], a2], c6 = Subtract[tcoords[[3, t[[3]]]], a3]},
				With[{n = Transpose[Developer`ToPackedArray[{Subtract[c2*c6, c3*c5], Subtract[c3*c4, c1*c6], Subtract[c1*c5, c2*c4]}]]},
					Divide[n, Sqrt[Total[n^2, {2}]]]
				]
			]
		]
	]


tpmsVertexFaceAdjacency[coords_, cells_] :=
	Block[{ncoords, ncells, posreal, adjreal, \[CurlyEpsilon] = 10^-7., bds, crng, bdmnx, bdmxx, bdmny, bdmxy, bdmnz, bdmxz, posvirtual, adjvirtual},
		ncoords = Length[coords];
		ncells = Length[cells];
		
		posreal = Partition[Riffle[Flatten[cells], Quotient[Range[3, 3ncells+2], 3]], 2];
		adjreal = SparseArray[posreal -> ConstantArray[1, Length[posreal]], {ncoords, ncells}];
		
		bds = CoordinateBounds[coords];
		
		crng = Range[ncoords];
		bdmnx = Pick[crng, UnitStep[Subtract[bds[[1, 1]] + \[CurlyEpsilon], coords[[All, 1]]]], 1];
		bdmxx = Pick[crng, UnitStep[Subtract[coords[[All, 1]], bds[[1, 2]] - \[CurlyEpsilon]]], 1];
		bdmny = Pick[crng, UnitStep[Subtract[bds[[2, 1]] + \[CurlyEpsilon], coords[[All, 2]]]], 1];
		bdmxy = Pick[crng, UnitStep[Subtract[coords[[All, 2]], bds[[2, 2]] - \[CurlyEpsilon]]], 1];
		bdmnz = Pick[crng, UnitStep[Subtract[bds[[3, 1]] + \[CurlyEpsilon], coords[[All, 3]]]], 1];
		bdmxz = Pick[crng, UnitStep[Subtract[coords[[All, 3]], bds[[3, 2]] - \[CurlyEpsilon]]], 1];
		
		posvirtual = Union @ Flatten[MapThread[If[Length[#1] == 1,
				Thread[{#2, adjreal[[#1[[1]]]]["NonzeroPositions"][[All, 1]]}],
				Nothing
			]&, 
			{
				Join[
					Nearest[coords[[bdmnx, 2;;3]] -> bdmnx, coords[[bdmxx, 2;;3]], {All, \[CurlyEpsilon]}], 
					Nearest[coords[[bdmny, {1,3}]] -> bdmny, coords[[bdmxy, {1,3}]], {All, \[CurlyEpsilon]}], 
					Nearest[coords[[bdmnz, 1;;2]] -> bdmnz, coords[[bdmxz, 1;;2]], {All, \[CurlyEpsilon]}],
					Nearest[coords[[bdmxx, 2;;3]] -> bdmxx, coords[[bdmnx, 2;;3]], {All, \[CurlyEpsilon]}], 
					Nearest[coords[[bdmxy, {1,3}]] -> bdmxy, coords[[bdmny, {1,3}]], {All, \[CurlyEpsilon]}], 
					Nearest[coords[[bdmxz, 1;;2]] -> bdmxz, coords[[bdmnz, 1;;2]], {All, \[CurlyEpsilon]}]
				], 
				Join[bdmxx, bdmxy, bdmxz, bdmnx, bdmny, bdmnz]
			}
		], 1];
		
		adjvirtual = SparseArray[posvirtual -> ConstantArray[1, Length[posvirtual]], {ncoords, ncells}];
		
		Unitize[adjreal + adjvirtual]
	]


boundaryEdges[cells_] :=
	Block[{scells, n, spopt, arr, arr2, nzp2},
		scells = Join[cells[[All, 1 ;; 2]], cells[[All, 2 ;; 3]], cells[[All, {3, 1}]]];
		n = Max[cells];
		
		Internal`WithLocalSettings[
			spopt = SystemOptions["SparseArrayOptions"];
			SetSystemOptions["SparseArrayOptions" -> {"TreatRepeatedEntries" -> Total}],
			arr = SparseArray[scells -> ConstantArray[1, Length[scells]], {n, n}];,
			SetSystemOptions[spopt]
		];
		
		arr2 = arr + Transpose[arr];
		nzp2 = Pick[arr2["NonzeroPositions"], arr2["NonzeroValues"], 1];
		
		Pick[nzp2, Developer`ToPackedArray[Extract[arr, nzp2]], 1]
	]


deleteDuplicateTPMSCoordinates[coords_, cells_, normals_] :=
	Block[{coordsnew, rep, cellsnew, normalsnew},
		{coordsnew, rep} = Region`Mesh`DeleteDuplicateCoordinates[coords];
		
		If[!ListQ[rep], 
			Return[{coords, cells, normals}]
		];
		
		cellsnew = Region`Mesh`ReplaceIncidents[cells, rep];
		normalsnew = If[!ListQ[normals], 
			normals, 
			(* Can we do this while always having a packed array? *)
			Developer`ToPackedArray[Nearest[coords -> normals, coordsnew][[All, 1]]]
		];
		
		{coordsnew, cellsnew, normalsnew}
	]


(* ::Subsubsection::Closed:: *)
(*Argument completion*)


$validTPMSFirstArguments = Join[{"Coarse", "MediumCoarse", "Medium", "MediumFine", "Fine"}, {"Information"}, $validTPMSMetaProperties];


addCodeCompletion[UTTriplyPeriodicMinimalSurface][
	UTTriplyPeriodicMinimalSurface[], 
	$validTPMSFirstArguments, 
	None, 
	None, 
	None, 
	{"Coarse", "MediumCoarse", "Medium", "MediumFine", "Fine"}, 
	{"MeshRegion", "BoundaryMeshRegion", "ComplexData", "FaceData"}
];


(* ::Subsection::Closed:: *)
(*UTGyroid*)


UTGyroid[args___] := With[{res = UTTriplyPeriodicMinimalSurface["G", args]}, res /; res =!= $Failed]


(* ::Subsubsection::Closed:: *)
(*Argument completion*)


addCodeCompletion[UTGyroid][$validTPMSFirstArguments];


(* ::Subsection::Closed:: *)
(*UTFischerS*)


UTFischerS[args___] := With[{res = UTTriplyPeriodicMinimalSurface["S", args]}, res /; res =!= $Failed]


(* ::Subsubsection::Closed:: *)
(*Argument completion*)


addCodeCompletion[UTFischerS][$validTPMSFirstArguments];


(* ::Section:: *)
(*TPMS Infill*)


(* ::Subsection::Closed:: *)
(*iUTTPMSInfill*)


(* ::Subsubsection::Closed:: *)
(*Main*)


Options[iUTTPMSInfill] = DeleteDuplicatesBy[Join[{"Rotation" -> Automatic}, Options[UTIntersection], Options[UTDifference]], First];


iUTTPMSInfill[bhead_, bmr_, surf_, scale_, thickness_, opts:OptionsPattern[]] :=
	iUTTPMSInfill[bhead, bmr, surf, scale, thickness, "Medium", opts]


iUTTPMSInfill[bhead_, bmr_, isurf_, scale_, thickness_, coarseness_, opts:OptionsPattern[]] :=
	Block[{surf, t, tfunc, tbmr, corner, num, tpms, res},
		surf = parseTPMSName[isurf];
		t = If[NumericQ[thickness], thickness, thickness@(RegionBounds[bmr][[All, 1]])];
		tfunc = infillRotation[OptionValue["Rotation"], isurf];
		tbmr = UTCoordinateApply[bmr, InverseFunction[tfunc]];
		
		corner = RegionBounds[tbmr][[All, 1]] - 2t;
		num = Ceiling[Divide[UTDimensions[tbmr] + 4t, scale]];
		
		tpms = UTTriplyPeriodicMinimalSurface[surf, corner, scale, num, thickness, coarseness];
		
		res = bhead[tbmr, tpms, FilterRules[{opts}, Options[bhead]]];
		(
			res = UTCoordinateApply[res, tfunc];
			
			res /; RegionQ[res]
			
		) /; RegionQ[res]
	]


UTTPMSInfill[___] = $Failed;


(* ::Subsubsection::Closed:: *)
(*Argument conform & completion*)


registerForDiscretization[iUTTPMSInfill, 2];
registerForSolid[iUTTPMSInfill, 2];


(* ::Subsubsection::Closed:: *)
(*Utilities*)


parseTPMSName["S15"] = "S";
parseTPMSName[expr_] := expr


tpmsRotation["S15"] = RotationTransform[15.0\[Degree], {0, 1, 0}];
tpmsRotation[_] = Identity;


infillRotation[expr_, surf_] := infillRotation[expr] @* tpmsRotation[surf]


infillRotation["Random"] := infillRotation[RandomReal[{0, 2\[Pi]}, 3]]
infillRotation[tfunc_TransformationFunction] := tfunc
infillRotation[angles_List] /; Length[angles] == 3 && VectorQ[angles, Internal`RealValuedNumericQ] := AffineTransform[EulerMatrix[angles]]
infillRotation[_] = Identity;


(* ::Subsection::Closed:: *)
(*UTTPMSInfill*)


(* ::Subsubsection::Closed:: *)
(*Main*)


Options[UTTPMSInfill] = Options[iUTTPMSInfill];


UTTPMSInfill[args__] :=
	With[{res = iUTTPMSInfill[UTIntersection, args]},
		res /; RegionQ[res]
	]


UTTPMSInfill[___] = $Failed;


(* ::Subsubsection::Closed:: *)
(*Argument completion*)


addCodeCompletion[UTTPMSInfill][
	None,
	Prepend[UTTriplyPeriodicMinimalSurface[], "S15"], 
	None, 
	None, 
	None, 
	None, 
	{"Coarse", "MediumCoarse", "Medium", "MediumFine", "Fine"}
];


(* ::Subsection::Closed:: *)
(*UTTPMSInfillComplement*)


(* ::Subsubsection::Closed:: *)
(*Main*)


Options[UTTPMSInfillComplement] = Options[iUTTPMSInfill];


UTTPMSInfillComplement[args__] :=
	With[{res = iUTTPMSInfill[UTDifference, args]},
		res /; RegionQ[res]
	]


UTTPMSInfillComplement[___] = $Failed;


(* ::Subsubsection::Closed:: *)
(*Argument completion*)


addCodeCompletion[UTTPMSInfillComplement][
	None,
	Prepend[UTTriplyPeriodicMinimalSurface[], "S15"], 
	None, 
	None, 
	None, 
	None, 
	{"Coarse", "MediumCoarse", "Medium", "MediumFine", "Fine"}
];
