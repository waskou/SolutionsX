# SolutionsX

This is a Mathematica package based on the xAct suite http://www.xact.es/ and the FieldsX package. It provides the following functionality:
* Fill in the details of a particular field configuration of a particular supergravity theory: (tensors, forms, vielbein, spinors,...)
* Batch functions to calculate the components of all tensors (with curved, flat and/or spin indices) using incredibly fast [^fast] parallel computations, based on the `TensorValues` framework of xAct [^xAct_differences]
* Check equations of motion and BPS explicitly in coordinates
* Store many solutions in a database, such that they can easily be retrieved and used later on
* Ability to add tensors and other data on top of an existing and verified solution
* Ability to quickly change coordinates from one solution to another (only on the level of differential forms, currently)

If you are interested in contributing a solution to the database, email me at vasildimi@gmail.com : )

## 1. Installation instructions

### 1.1. One-off installation

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
   * Download the `Multisets.m` file from https://library.wolfram.com/infocenter/MathSource/8115/ and place it inside the `Applications` folder (not inside the `xAct` folder) [^common_problem]
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

### 1.2. Continuous updates installation

1. Follow steps 1-3 above
2. From the terminal go to the `Applications` directory and type `git clone https://github.com/waskou/SolutionsX.git`. That clones the repository in your `Applications` directory and should create the folder structure outlined above, with the difference that in the `SolutionsX` folder there is now also a hidden `.git` folder.
3. Go to `https://github.com/waskou/SolutionsX.git` and click on `Watch` with the setting `All Activity` (or choose your own notifications settings) to be instantly notified when there has been an update in the code or in the database.
4. Everytime you see an update that you want to pull open a terminal, navigate to the `SolutionsX` folder and type `git pull https://github.com/waskou/SolutionsX.git main`. That pull the most recent version of the code and the database to your local `SolutionsX` folder

## 2. Getting started

TBA

## 3. Contributing a solution

1. Email me on vasildimi@gmail.com, and I can give you rights to push your own solutions to the database or modifying the code in a different branch. Depending on interest, I might setup some quality control before your contributed solutions get uploaded.

[^fast]: On the CCLP black hole example that I have most thoroughly tested this code the parallel computation of the tensors associated to the metric takes less than 2 hours on my laptop to `FullSimplify`. I have not been able to achieve that using other tools.

[^xAct_differences]: Nowadays xAct provides similar functionality based on th `CTensor` framework, when it comes to the metric and its derived tensors (Christoffels, Riemann, Ricci,...). However, I personally found it difficult to extend the parallel code that the developers of xAct have provided to the case of tensors with indices on other vector bundles, such as the spin or the flat bundle. I will be very interested if anyone can show me a way to do that.

[^common_problem]: For some browsers and/or operating systems when you click on the download button for `Multisets.m` it actually saves the file with a `.nb` extension. If that is the case for you, simply rename it to have a `.m` extension before placing it in `Applications`