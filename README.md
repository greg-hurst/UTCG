# UTCGLibrary

### Utility for Triangulated Computational Geometry

To load the package in Mathematica, evaluate

```mathematica
$ContextPath = DeleteDuplicates @ Join[$ContextPath, {"UTCG`"}];
$Path = DeleteDuplicates @ Join[$Path, {FileNameJoin[{"path", "to", "UTCG"}]}];
Get["UTCGLoader`"];
```

The first time running the loader will take 10-20 seconds due to initial compilations.

To see a list of available functions, run ``??UTCG`*``.

Check out _some_ examples in `MiscExamples.nb`.
