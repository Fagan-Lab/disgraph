---
title: 'Inferring, comparing and exploring ecological networks from time-series data through R packages constructnet, disgraph and dynet'
tags:
  - Time-series networks
  - network construction
  - graph distance
  - network dynamics
  - ecological networks
authors:
  - name: Anshuman Swain
    orcid: 0000-0002-9180-2222
    affiliation: '1, 2'
  - name: Travis Byrum
    affiliation: 2
  - name: Zhaoyi Zhuang
    affiliation: 3
  - name: Luke Perry
    affiliation: 3
  - name: Michael Lin
    affiliation: 3
  - name: William F Fagan
    affiliation: 3
affiliations:
  - name: Department of Biology, University of Maryland, College Park
    index: 1
date: 03 May 2021
bibliography: paper.bib
---

# Summary

Network inference is a major field of interest for the ecological community, especially in light of the high cost and difficulty of manual observation, and easy availability of remote, long term monitoring data. In addition, comparing across similar network structures, especially with spatial, environmental, or temporal variability and, simulating processes on networks to create toy models and hypotheses – are topics of considerable interest to the researchers.
A large number of methods are being developed in the network science community to achieve these objectives but either don’t have their code available or an implementation in R, the language preferred by ecologists and other biologists.
We provide a suite of three packages which will provide a central suite of standardized network inference methods from time-series data (constructnet), distance metrics (disgraph) and (process) simulation models (dynet) to the growing R network analysis environment and would help ecologists and biologists to perform and compare methods under one roof.
These packages are implemented in a coherent, consistent framework – making comparisons across methods and metrics easier. We hope that these tools in R will help increase the accessibility of network tools to ecologists and other biologists, who the language for most of their analysis.

# Statement of need

`Gala` is an Astropy-affiliated Python package for galactic dynamics. Python
enables wrapping low-level languages (e.g., C) for speed without losing
flexibility or ease-of-use in the user-interface. The API for `Gala` was
designed to provide a class-based and user-friendly interface to fast (C or
Cython-optimized) implementations of common operations such as gravitational
potential and force evaluation, orbit integration, dynamical transformations,
and chaos indicators for nonlinear dynamics. `Gala` also relies heavily on and
interfaces well with the implementations of physical units and astronomical
coordinate systems in the `Astropy` package [@astropy] (`astropy.units` and
`astropy.coordinates`).

`Gala` was designed to be used by both astronomical researchers and by
students in courses on gravitational dynamics or astronomy. It has already been
used in a number of scientific publications [@Pearson:2017] and has also been
used in graduate courses on Galactic dynamics to, e.g., provide interactive
visualizations of textbook material [@Binney:2008]. The combination of speed,
design, and support for Astropy functionality in `Gala` will enable exciting
scientific explorations of forthcoming data releases from the _Gaia_ mission
[@gaia] by students and experts alike.

# Acknowledgements

We would like to thank Morelle Tchuindjo and Nathan Stiff for their contributions to the project. We
acknowledge the role of netrd package in Python in providing a baseline for our R development at all
times. AS thanks National Science Foundation award DGE-1632976 for training and support.

# References
