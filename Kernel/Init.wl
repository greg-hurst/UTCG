BeginPackage["UTCG`"]
Begin["`Private`"]

root = $InputFileName // DirectoryName // ParentDirectory;

$Path = DeleteDuplicates @ Join[$Path, {root}];
Echo["UTCG loader might take 10-20 seconds due to initial compilations"];

Get["UTCGLoader`"];

End[]
EndPackage[]