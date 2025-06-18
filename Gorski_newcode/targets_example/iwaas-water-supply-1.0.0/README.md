![USGS](USGS_ID_black.png)

# Analysis of monthly water supply for the United States for water years 2010-2020 using modeled states and fluxes

**Authors**: Anthony J. Martinez, Hayley R. Corson-Dosch, Julie A. Padilla, Galen A. Gorski.

**Point of contact**: Anthony J. Martinez ([ajmartinez\@usgs.gov](mailto:ajmartinez@usgs.gov))

**Repository Type**: *R* script supporting publication

**Year of Origin**: 2025 (original publication)

**Year of Version**: 2025

**Digital Object Identifier (DOI)**: <https://doi.org/10.5066/P1YTXWKY>

**USGS Information Product Data System (IPDS) no.**: IP-169961 (internal agency tracking)

------------------------------------------------------------------------

This pipeline analyzes estimates of streamflow, evapotranspiration, soil moisture, and snow water equivalent among other hydrologic states and fluxes from two national hydrologic models: the National Hydrologic Model application of the Precipitation-Runoff Modeling System (NHM-PRMS) and the Weather Research and Forecasting hydrologic modeling system (WRF-Hydro). Both of these models are driven by the newly developed WRF-based CONUS404 bias adjusted atmospheric forcing dataset. Spatial and temporal patterns and error distributions are compared between the two models and against external benchmarking datasets.

## Purpose

The code in this repository is used to complete the analysis for the publication Water supply in the conterminous United States, Alaska, Hawaii, and Puerto Rico, water years 2010–20, chap. B of U.S. Geological Survey integrated water availability assessment—2010–20: U.S. Geological Survey Professional Paper 1894–B (Gorski et al., 2025; https://doi.org/10.3133/pp1894B5).

## Repository organization

-   `01_configuration/` directory containing files related to configuration.
    -   `src/` directory containing source code to define functions.
        -   `sb_cache.R` R script defining functions used for caching credentials for <https://www.ScienceBase.gov>.
        -   `years_to_days` R script defining functions to convert a sequence of years to a sequence of dates by days.
        -   `years_to_months` R script defining functions to convert a sequence of years to a sequence of dates by months.
-   `02_fetch/` directory containing files related to fetching data.
    -   `src/` directory containing source code to define functions.
        -   `sb_login_token.R` R script defining functions used for logging in to and retrieving data from <https://www.ScienceBase.gov>.
        -   `download_data.R` R script defining functions related to downloading data files
    -   `out/` directory containing subdirectories that contain output files (once code is run).
-   `03_process/` directory containing files related to the initial processing of the data.
    -   `src/` directory containing source code to define functions.
        -   `data_mask.R` R script defining function to mask data based on presence of snow.
        -   `ensemble_models.R` R script defining functions that combine NHM and WRF-Hydro model into "model ensemble" data.
        -   `extract_ensemble_huc12.R` R script defining functions that extract and ensemble model data.
        -   `get_dates.R` R script defining functions for extracting dates out of filenames from certain datasets.
        -   ` model_nc_process_functions.R` R script defining functions for extracting data from NHM/WRF-Hydro NetCDF files.
        -   `prep_huc_sf.R` R script defining functions for preparing Watershed Boundary Dataset HUC12 data.
        -   `prep_hydrolakes.R` R script defining functions that prepare hydrolakes lake storage data.
        -   `prep_van_metre.R` R script defining functions to prep Hydrologic Region data.
        -   `process_functions.R` R script defining functions for accomplishing a variety of processing-related tasks.
        -   `uncertanty_metrics.R` R script containing functions to calculate uncertainty of modeled water supply data.
    -   `out/` directory containing output files (once code is run).
        -   `huc_summaries` directory containing summaries (by HUC8 or HUC12) of modeled or benchmark comparison data.
        -   `uncertainty_metrics` directory containing uncertainty metrics by variable.
-   `04_viz_prep/` directory containing files related to processing data in preparation for data visualization.
    -   `src/` directory containing source code to define functions.
        -   `build_breakdown_fig_data.R` R script defining functions for preparing data for figures that compare NHM and WRF-Hydro model results over time.
        -   `build_breakdown_var_tbl.R` R script defining functions to build a parameter table for generating figures that compare NHM and WRF-Hydro model results over time.
        -   `model_summary_fxns.R` R script defining functions to create summaries of water stores/flux data to be used to generate map matrix figures.
        -   `prep_viz_data` R script defining functions for accomplishing a variety of data visualization preparation-related tasks.
    -   `out/` directory containing output files (once code is run).
-   `05_visualize/` directory containing files related to data visualization.
    -   `in/` directory containing files directly input into the data pipeline.
        -   `van_metre_grid.csv` a CSV file used to define the geofacet shape of the conterminous United States by hydrologic region.
    -   `src/` directory containing source code to define functions.
        -   `build_map_matrix.R` R script defining functions used to generate the map matrix figures.
        -   `build_maps.R` R script defining functions used to generate constituents of the map matrix figures.
        -   `flux_geofacet_plot.R` R script defining functions used to generate geofacet figures of water fluxes.
        -   `format_uncertainty_tbl.R` R script defining functions used to create formatted table (as HTML) displaying model uncertainty metrics.
        -   `map_of_van_metre_regions.R` R script defining functions used to build a map of hydrologic regions within the conterminous United States for the inset map in the geofacet maps.
        -   `plot_breakdown_bars.R` R script defining functions to plot barplots for figures that compare NHM and WRF-Hydro model results over time.
        -   `plot_breakdown_figs.R` R script defining functions to plot figures that compare NHM and WRF-Hydro model results over time.
        -   `plot_breakdown_lines.R` R script defining functions to plot line graphs for figures that compare NHM and WRF-Hydro model results over time.
        -   `plot_maps.R` R script defining functions to plot individual maps for map matrix figures.
        -   `streamflow_baseflow_grid.R` R script defining functions to plot streamflow and baseflow proportional point size map matrix figure.
        -   `streamflow_percentiles_geofacet.R` R script defining functions to plot streamflow vs baseflow barplot geofacet figure.
        -   `viz_helpers.R` R script defining a variety of plotting utility functions.
    -   `out/` directory containing output files (once code is run).
-   `renv/` a directory of files used by the [renv](https://rstudio.github.io/renv/articles/renv.html) package to manage environments and package dependencies.
    -   `.gitignore` files for git to ignore within the `renv/` directory
    -   `activate.R` R script (created by the renv package) to activate renv environment and package management within the project
    -   `settings.json` metadata file containing project-specific configuration settings
-   `.gitignore` file specifying which files should not be tracked in git repository
-   `.Rprofile` file which helps initialize renv.
-   `.renvignore` file specifying which files renv should not be inspected for package dependencies.
-   `00_setup.R` R script used for logging in and caching credentials for <https://www.ScienceBase.gov>.
-   `01_configuration.R` R script used for defining the "configuration" phase of the data pipeline.
-   `02_fetch.R` R script defining the "fetch" phase of the data pipeline.
-   `03_process.R` R script defining the "process" phase of the pipeline
-   `04_viz_prep.R` R script defining the "visualization preparation" phase of the pipeline.
-   `05_visualize.R` R script defining the "visualization" phase of the pipeline.
-   `CODE_OF_CONDUCT.md` information on the contribution code of conduct.
-   `CONTRIBUTING.md` information on how to contribute to this repository
-   `DISCLAIMER.md` Provisional USGS disclaimer of code in repository. Should be replaced with approved version when approved.
-   `LICENSE.md` license information for code.
-   `README.md` general information about the project.
-   `USGS_ID_black.png` USGS logo used at the top of README.md.
-   `_targets.R` a [target script file](https://books.ropensci.org/targets/walkthrough.html#target-script-file) used to define a [targets pipeline](https://docs.ropensci.org/targets/index.html). See the [targets documentation](https://books.ropensci.org/targets/walkthrough.html) for how to run a targets pipeline.
-   `code.json` metadata file containing information for code contained in this repository. Compliant with [USGS template](https://www.usgs.gov/products/software/software-management/distribution-usgs-code).
-   `renv.lock` records the packages and package versions used within the code. Used automatically by [renv](https://rstudio.github.io/renv/reference/lockfiles.html).
-   `iwaas-water-supply.Rproj` an R project file. See [6.2 Projects \| R for data science](https://r4ds.hadley.nz/workflow-scripts#projects) for more information on RStudio projects.

## Setup

#### Package management with [renv](https://rstudio.github.io/renv)

This project uses [renv](https://rstudio.github.io/renv) to manage packages used by the pipeline. Renv works behind the scenes to ensure that the same package versions used by pipeline are used across contributors. It installs specific versions of packages to a `renv/` folder within the project directory that it loads when `library()` is invoked, even if the packages are installed elsewhere (e.g., in the `.libPaths()` path). When opening the project, renv should, behind the scenes, initiate itself and prompt the user for any additional actions needed. If this is the first time using renv, it may take a little while as specific package versions are downloaded and installed. See [Collaboration in renv](https://rstudio.github.io/renv/articles/renv.html#collaboration) for more information.

To initialize renv, use the command `renv::init()` from within the R Project (i.e., the repository directory). When asked to make a selection, select **1: Restore the project from the lockfile**.

Note to MacOS/Linux users: some of the packages rely on system dependencies that may not be installed when the R packages are installed. If you receive errors during package installation, these may be address by installing system dependencies from your terminal. For example: 
- `brew install openssl` 
- `brew install netcdf` 
- `brew install gdal`

Additionally, the default repository, Posit Package Manager (P3M), contains package binaries (pre-built packages) for Windows and MacOS. If you are using Linux, you must:

1. Identify the correct P3M URL by filling out the information about your specific setup at <https://packagemanager.posit.co/client/#/repos/cran/setup>.
2. Change the package manager in the lockfile using the following code (ensuring that you paste the URL in):

    ```r
    renv::lockfile_modify(repos = c(P3M = "<paste URL here>")) |> 
      renv::lockfile_write()
    ```

#### ScienceBase login

To access controlled ScienceBase items, an additional step must be completed to authenticate a ScienceBase session before the targets pipeline is run.

1.  Ensure `.gitignore` includes a line for ".Renviron" to prevent credentials from being committed the the repository by git.
2.  Run the entire contents of `00_setup.R` either (1) interactively (i.e., line by line) or (2) by running `source("00_setup.R")`.
3.  You will be prompted to enter the token text by R/RStudio. A browser window should pop-up directing you to <https://sciencebase.usgs.gov/manager/> to login.
4.  Once logged in, copy your API token: click the account drop down button (person shaped icon) and click **Copy API Token**.
5.  Paste the API token into the RStudio popup.

Once the above steps are complete, `initialize_and_cache_sb()` will cache your username and token into the .Renviron file. From here, the pipeline should be able to use those cached credentials to re-initialize and refresh ScienceBase sessions in different R sessions for up to 10 hours.

## Running the code

This code has been organized into a data pipeline using the [targets](https://books.ropensci.org/targets/) package. Targets is a pipeline building tool that automatically tracks the status (i.e., up-to-date vs. out-of-date) and builds intermediate objects only as needed. To run the entire pipeline, you can run the command [`targets::tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html), after the **Setup** steps (above) have been completed. For more information on targets, the package author has written [a walkthrough to get started](https://books.ropensci.org/targets/walkthrough.html) and there is an [Introduction to targets](https://carpentries-incubator.github.io/targets-workshop/) training that is currently in development but still may be useful in its current state.

## Code runtime

The entire pipeline runs in approximately 6 hours on a high-end laptop (e.g., 64 GB RAM, Intel i9 11th gen processor).

## Expertise required

No subject matter expertise is required to run the code, although knowledge of the [hydrologic cycle](https://en.wikipedia.org/wiki/Water_cycle) is required to understand the outputs.

## Built With

-   [R](https://www.r-project.org/about.html) - data processing, analysis, and visualization

## Contributing

Please read [CONTRIBUTING.md](CONTRIBUTING.md) for details on the process for submitting pull requests to us. Please read [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md) for details on adhering by the [USGS Code of Scientific Conduct](https://www.doi.gov/sites/doi.gov/files/migrated/scientificintegrity/upload/DOI-Code-of-Scientific-and-Scholarly-Conduct-Poster-December-2014.pdf).

## Authors

-   [**Anthony Martinez**](https://www.usgs.gov/staff-profiles/anthony-martinez) - *Data scientist* - [USGS Water Mission Area](https://www.usgs.gov/mission-areas/water-resources)
-   [**Hayley Corson-Dosch**](https://www.usgs.gov/staff-profiles/hayley-corson-dosch) - *Hydrologist/Data scientist* - [USGS Water Mission Area](https://www.usgs.gov/mission-areas/water-resources)
-   [**Julie Padilla**](https://www.usgs.gov/staff-profiles/julie-padilla) - *Data scientist (former employee)* - [USGS Water Mission Area](https://www.usgs.gov/mission-areas/water-resources)
-   [**Galen Gorski**](https://www.usgs.gov/staff-profiles/galen-gorski) - *Machine Learning Specialist* - [USGS Water Mission Area](https://www.usgs.gov/mission-areas/water-resources)

## License

This project is licensed under the Creative Commons CC0 1.0 Universal License - see the [LICENSE.md](LICENSE.md) file for details.

## Suggested Citation

In the spirit of open source, please cite any re-use of the source code stored in this repository. Below is the suggested citation:

> Martinez AJ, Corson-Dosch HR, Padilla JA, Gorski GA. 2025. Analysis of monthly water supply for the United States for water years 2010-2020 using modeled states and fluxes. U.S. Geological Survey software release. Reston, Va. <https://doi.org/10.5066/P1YTXWKY>.

## Associated products

Martinez AJ, Padilla JA, Gorski GA. 2025. Monthly ensemble outputs from the National Hydrologic Model Precipitation-Runoff Modeling System and the Weather Research and Forecasting model hydrologic modeling system for the conterminous United States, Alaska, Hawaii, and Puerto Rico for water years 2010–2020. U.S. Geological Survey data release. Reston, Va. <https://doi.org/10.5066/P1RBMDUT>

Gorski GA, Stets E, Scholl MA, Degnan JR, Mullaney JR, Galanter AE, Martinez AJ, Padilla JA, LaFontaine JH, Corson-Dosch HR, Shapiro AM. 2025. Water supply in the conterminous United States, Alaska, Hawaii, and Puerto Rico, water years 2010–20, , chap. B of U.S. Geological Survey integrated water availability assessment—2010–20: U.S. Geological Survey Professional Paper 1894–B, https://doi.org/10.3133/pp1894B.

## Acknowledgments

-   Thank you to Richard E. Marinos(<https://github.com/marinosr/>) for the help that your [`SNODASR`](https://github.com/marinosr/SNODASR/blob/main/R/download.SNODAS.R) package gave in figuring out how to download SNODAS data programatically.
-   Thank you to the [Integrated Water Availability Assessments Trends and Drivers](https://code.usgs.gov/water/IWAAs-trends/water-quality-data) project team at the United States Geological Survey (USGS) for the use of their repository template.
