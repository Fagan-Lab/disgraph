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

# Acknowledgements

We would like to thank Morelle Tchuindjo and Nathan Stiff for their contributions to the project. We
acknowledge the role of netrd package in Python in providing a baseline for our R development at all
times. AS thanks National Science Foundation award DGE-1632976 for training and support.

# References
