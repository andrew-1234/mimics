# MIMiCS

_Multi-Index Motif Classification for Soundscapes in R._

## 🔊 Foreword

Welcome! This package is under active development and is not complete. Check
back soon for updates; a working alpha is planned to release by the end of 2022
😀. This package is an implementation of the methods provided by [Scarpelli
et al. 2021](https://doi.org/10.3389/fevo.2021.738537), which is published in
Frontiers in Ecology and Evolution. In the mean time, you can find the original
analysis scripts [here](http://doi.org/10.5281/zenodo.4784758).

## ✨ Features

This package is designed to provide a complete analysis pipeline to get you
from sound files, all the way to classified acoustic motifs. Alongside the core
functionality of the package, there will be several
helper functions to streamline your analysis process:
- Calling [AnalysisPrograms](https://github.com/QutEcoacoustics/audio-analysis) from R to
generate acoustic indices
- Helper functions to integrate with [A2O](https://acousticobservatory.org/)
  - Download audio and format metadata for use in the MIMiCS pipeline
- Helper functions to use [EMU](https://github.com/QutEcoacoustics/emu) from R
  - EMU can identify issues in audio files such as corrupt metadata,
    and repair those issues if necessary
- Generate helpful audio summary statistics and sensor location maps

## 🗯 Acknowledgements

- The original research article: Marina D. A. Scarpelli, Benoit Liquet, David Tucker, Susan Fuller and Paul Roe
- R scripts: Marina Scarpelli, Anthony Truskinger, Benoit Liquet
- MIMiCS pacakge: Andrew Schwenke
- HIME [(HierarchIcal based Motif Enumeration)](https://github.com/flash121123/HIME)

## 📧 Correspondence 

If you have questions, issues, feature requests, or simply want to learn more,
please don't hesitate to get in contact. You can raise a GitHub issue directly
on this repository, or get in touch with myself or Marina Scarpelli via email:

- Marina D. A. Scarpelli, ninascarpelli@gmail.com
- Andrew Schwenke, andrew.schwenke@gmail.com 
