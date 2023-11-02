# SolutionsX
Storing, verifying and using solutions to Supergravity theories

## Requirements
* [Mathematica](https://www.wolfram.com/mathematica/) >= 12.3
* [xAct](http://xact.es/)
* [FieldsX](https://arxiv.org/abs/2008.12422)

## Installation from the [Wolfram Language Paclet Repository](https://resources.wolframcloud.com/PacletRepository/)

From a mathematica notebook run:
```mathematica
PacletInstall["VasilDimitrov/SolutionsX"]
```
That's it!

## Installation from the `build` files in this repositry

* From a terminal, navigate to a directory of your choice, for example `/User/<your-username>/Downloads` and clone this repository there.
* From a mathematica notebook run:
```mathematica
PacletInstall["/Users/<your-username>/Downloads/SolutionsX/build/VasilDimitrov__\
SolutionsX-<desired-version-of-paclet>.paclet", ForceVersionInstall -> True]
```
replacing `<desired-version-of-paclet>` with the version you want, currently the latest one is `1.0.1`.

## Get the pre-prepared solutions from this repository (optional)

* From a terminal, navigate to a directory of your choice, for example `/User/<your-username>/Downloads` and clone this repository there.
* From a mathematica notebook run:
```mathematica
ExpandFileName[URL[$LocalBase]]
```
That will determine the Local Base directory on your machine where SolutionsX will look for solutions data. If not already created, make a directory called `SolutionsX` in your Local Base directory.
* Copy the contents of `/User/<your-username>/Downloads/SolutionsX/SolutionsX` to the `SolutionsX` directory in you Local Base.
