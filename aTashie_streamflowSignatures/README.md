# This script processes streamflow data from USGS and Canadian gages to calculate various hydrological signatures and trends. It analyzes flow volumes, flow duration curves, flashiness, and flow timing.


# Streamflow Signatures

A simple script for automating the ingestion and processing of publicly available streamflow data in the US and Canada.

Modular functions allow for the calculation of a variety of streamflow signatures, analysis of trends in these signatures, and saving a summary of the analysis locally.

Additionally, the script leverages the HydroSHEDS to identify all watersheds upstream each gage, which allows for a quick identification of various watershed hydro-, socio-, geophysical features.

Note that this workflow leverages several datasets that are not included in the github repo. All files (in the expected structure) are located here: https://drive.google.com/drive/folders/1N7wAXsQRexsNQi7h4tlwCf6um2VNUFLo.

Please contact Arik Tashie (arik@climate.ai) if you have any questions.

## Contributing

Pull requests are welcome. For major changes, please open an issue first
to discuss what you would like to change.

Please make sure to update tests as appropriate.

## License

[CC0-1.0 license](https://creativecommons.org/publicdomain/zero/1.0/deed.en)
