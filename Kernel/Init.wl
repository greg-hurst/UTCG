BeginPackage["UTCG`"]
Begin["`Private`"]

root = $InputFileName // DirectoryName // ParentDirectory;

$Path = DeleteDuplicates @ Join[$Path, {root}];

Get["UTCGLoader`"];

End[]
EndPackage[]