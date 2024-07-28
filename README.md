# MetaGate

MetaGate is an R package for visualization and statistical analysis of large cytometry data sets through a web browser-based user interface.

MetaGate was [published in _Patterns_](https://doi.org/10.1016/j.patter.2024.100989) on May 13, 2024.

For more information about how to use MetaGate, please visit [metagate.malmberglab.com](https://metagate.malmberglab.com).


## Installation

The following instructions describe how to install MetaGate directly from this GitHub repository.

	if (!require("remotes")) install.packages("remotes")
	remotes::install_github("malmberglab/metagate")


## Launch MetaGate

Start MetaGate by entering the following code in the R console:

    library(metagate)
    run_metagate()

MetaGate will now launch in your web browser. If your web browser does not open automatically, open it manually and go to the address that was outputted in the R console.

**To exit MetaGate**, close the web browser, and hit the ESC key inside the R application. You can then quit R.


## How to use MetaGate

Please visit our website at [metagate.malmberglab.com](http://metagate.malmberglab.com) for information on how to use MetaGate.


## Citation

Please cite the following article:
> Ask EH, Tschan-Plessl A, Hoel HJ, Kolstad A, Holte H, Malmberg KJ. **MetaGate: Interactive analysis of high-dimensional cytometry data with metadata integration.** Patterns. 2024 May;100989. https://doi.org/10.1016/j.patter.2024.100989.