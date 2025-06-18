
# How to build this `targets` example pipeline

This folder contains a simple, example targets pipeline. To see a more complex
pipeline example, see the following options:

1. Galen's example in this repo: [CZ-Sync/code-sandbox/Gorski_newcode/targets_example](https://github.com/CZ-Sync/code-sandbox/tree/main/Gorski_newcode)
1. Lindsay's CZ-Sync data preprocessing: [CZ-Sync/data-preprocessing](https://github.com/CZ-Sync/data-preprocessing)
1. A water quality example from the USGS: [DOI-USGS/ds-pipelines-targets-example-wqp](https://github.com/DOI-USGS/ds-pipelines-targets-example-wqp)
1. A water quality example accompanying [an ESIP blog post by Lindsay's colleague Abner Bogan](https://www.esipfed.org/guest-blog-reproducible-data-pipelines-in-r-with-targets/): [CUAHSI/from-scripts-to-pipelines-workshop](https://github.com/CUAHSI/from-scripts-to-pipelines-workshop)

The pipeline downloads some USGS NWIS data, calculates some summaries, then creates 
a plot. It is separated into 3 different "phases" to try to group parts of the 
workflow. Follow the steps below to build it.

1. **Install dependencies.** First, you need to install the `targets` package and 
anything listed in the `_targets.R` file under the argument "packages" in `tar_option_set()`.
1. **Set the working directory to this folder.** To run the pipeline you need to be in 
the folder that the pipeline is in or it won't find anything to build. Run 
`setwd('LPlatt_Targets_Example')` or an equivalent command in R to make this 
your current working directory.
1. **Load the `targets` package.** Load the targets package into your current 
R session by running `library(targets)`. If it fails, it means your package 
install during step 1 was not successful. Try doing that again.
1. **Build the pipeline.** Build the full pipeline by running `tar_make()`.
1. **Inspect outputs.** You should now see 4 PNG files in the folder. The pipeline
created these! You can inspect the data downloaded by loading the target output into 
your environment with `tar_load(p1_flow_daily_tbl)`.
1. **Make a change and rebuild.** Try changing something about the pipeline and rebuild
to see how the pipeline handles this. You could edit some aesthetic about the plots,
you could add a new site number to `p1_sites` (e.g. '05587450' which is the Mississippi
River at Grafton Illinois), or you could change the plot filenames. Then, run `tar_make()` 
again and observe what rebuilt and what was skipped.

Hopefully this small example gives you a taste of how this approach can be powerful for
managing large, complex workflows or just helping you stay organized with your code.
