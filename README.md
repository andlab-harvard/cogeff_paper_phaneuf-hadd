# cogeff_paper_phaneuf-hadd

Data and Code

*to accompany*

Characterizing Age-Related Change in Learning the Value of Cognitive Effort

Phaneuf-Hadd, C.V., Jacques, I.M., Insel, C., Otto, A.R., & Somerville, L.H. (2024)

## Developer Contact Information

Github profile: https://github.com/cphaneuf

Email: (current) cphaneuf@g.harvard.edu, (permanent) cphaneuf@umich.edu

## Contents

### data/ directory

Contains demographic, learning task, practice phase, post-task rating, self-report questionnaire, and paradigm validation data for study 1. Contains demographic and learning task data for study 2.

### analyses/ directory

*utilities.R* defines variables and functions to be shared across scripts.

*demog.R* takes demographic data inputs from data/study1/ and data/study2/ and writes outputs to results/study1/demog/ and results/study2/demog.

*study1_analyses.R* takes learning task, practice phase, post-task rating, self-report questionnaire, and paradigm validation data inputs from data/study1/ and writes outputs to results/study1/verify/ and results/study1/hyp/.

*study2_analyses.R* takes learning task data inputs from data/study2/ and writes outputs to results/study2/hyp/.

### results/ directory

Contains text and png file outputs from scripts in analyses/, sorted by study and analysis type.

### annotated_figs/ directory

Contains annotated_figs.pptx (which annotates several figures beyond the limits of R) and the png files it produces.
