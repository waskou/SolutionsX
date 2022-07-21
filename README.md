# SolutionsX

## Installation instructions

1. Determine your installation directory
   * In a new Mathematica notebook run `FileNameJoin[{$UserBaseDirectory, "Applications"}]`. The result is the installation directory where you will be placing files
2. Install xAct
   * Download the `xAct_version.tgz` (Linux/Mac) or the `xAct_version.zip` (Windows) archive from http://www.xact.es/
   * Unzip it in the installation directory you determined above. The result should be something like:
```
Applications
└───xAct
|   └───xTensor
|   └───xCoba
|   └───xCore
|   └───...
```
3. Install FieldsX
   * Download the `Multisets.m` file from https://library.wolfram.com/infocenter/MathSource/8115/ and place it inside the `Applications` folder (not inside the `xAct` folder)
   * Download the archive of the code of FieldsX by clicking on the green button `Code` and then `DownloadZIP` from https://github.com/mfroeb/FieldsX
   * Unzip the archive and place the file `FieldsX.m` inside the xAct folder. The result should be something like:
```
Applications
└───xAct
|   └───xTensor
|   └───xCoba
|   └───xCore
|   └───FieldsX.m
|   └───...
└───Multisets.m
```
4. Install SolutionsX
   * Download the archive of the code of SolutionsX by clicking on the green button `Code` and then `DownloadZIP` from https://github.com/waskou/SolutionsX
   * Unzip it inside the installation directory `Applications` and rename the folder to `SolutionsX` (instead of `SolutionsX-main`). The end result should be something like:
```
Applications
└───xAct
|   └───xTensor
|   └───xCoba
|   └───xCore
|   └───FieldsX.m
|   └───...
└───Multisets.m
└───SolutionsX
|   └───Data
|   └───Documentation
|   └───Generator
|   └───Kernel
|   └───...
```
5. Enjoy
   * In a new Mathematica notebook run ``<<SolutionsX` ``. That loads the package and you are set to use its functions and database