

# README

<br>

This repository contains data, code, and output for a manuscript
entitled “Does computational modeling yield invariant measures of
executive functioning?”.

Do you want to download or clone the materials for this project? Go to
<https://github.com/stefanvermeent/DDM_DIF>. Please note that this is
still a work in progress. As a result, some of the files and links
mentioned below may not be available just yet.

## Directory Structure

The directory structure follows the [TIER
Protocol](https://www.projecttier.org/tier-protocol/protocol-4-0/). The
names of each folder are intended to be self-explanatory. Each main
folder contains its own README file to explain its content.

    .
    ├── 0_preregistration
    │   ├── DDM_DIF_preregistration.docx
    │   ├── preregistration.md
    │   └── preregistration.qmd
    ├── 1_data
    │   ├── 1_InputData
    │   │   └── MetaData
    │   ├── 2_IntermediateData
    │   └── 3_AnalysisData
    ├── 2_scripts
    │   ├── 1_ProcessingScripts
    │   ├── 2_AnalysisScripts
    │   └── 3_SupplementScripts
    ├── 3_output
    │   ├── Results
    │   └── SupplementResults
    ├── 4_manuscript
    ├── bibfiles
    │   ├── apa.csl
    │   ├── reference-doc.docx
    │   └── references.bib
    ├── DDM_DIF.Rproj
    ├── README.md
    ├── README.qmd
    └── README.rmarkdown

## Reproducing our results

### 1. Accessing LISS data

This GitHub repository does not contain raw or intermediate data files,
as access to LISS data requires a signed data use agreement. If you want
to fully reproduce our analysis pipeline, you should first sign a [Data
use agreement with LISS](https://liss.statements.centerdata.nl/). At the
time of writing this README, accessing LISS data is free of charge.

Upon receiving access, you can download the data at
<https::/lissdata.nl>. See [the Data Sources
Guide](https://github.com/stefanvermeent/tree/main/1_InputData/DataSources.txt)
for an overview of all the required files and their exact source.

*Note*: It is possible to **partially** reproduce our findings with the
output files that are present on this repository. See step 2 below for
more information.

### 2. Reproduce results

There are two ways to reproduce our results:

1.  **Full reproduction.** If you have followed the instructions under
    step 1, full reproduction (i.e., from preprocessing to building the
    manuscript file) is possible. The easiest way to achieve this is by
    running
    [2_scripts/MasterScript.R](https://github.com/StefanVermeent/tree/main/2_scripts/MasterScript.R),
    which executes all preprocessing and analysis scripts in the correct
    order. Please be aware that running the entire pipeline is very
    computationally costly—mostly due to DDM and MNLFA estimation. We
    strongly advice doing this in a high-performance cloud-computing
    environment that contains a large number of cores.

2.  **Partial reproduction.** Without access to the raw data, you can
    still render the manuscript and supplemental materials using the
    output files that are available on the public repository.

## Contact

For questions or comments, please feel free to contact me at
p.c.s.vermeent@gmail.com.
