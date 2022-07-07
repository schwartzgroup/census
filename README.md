# Census variables

This repository will serve as a unified source for all code related to Census data extraction, transformation, interpolation, and imputation. The this project has three main goals:

1.  To bridge together and consolidate the functionalities of three distinct projects:

    -   [schwartzgroup/census-ses-covariates](https://github.com/schwartzgroup/census-ses-covariates), which uses the [GL-Li/totalcensus](https://github.com/GL-Li/totalcensus) library to perform bulk extracts of American Community Survey (ACS) data (a process which requires large amounts of disk space),

    -   [schwartzgroup/nhgis](https://github.com/schwartzgroup/nhgis), which uses in-house functions to parse decennial Census data retrieved from the [National Historical Geographic Information System (NHGIS)](https://www.nhgis.org/), and

    -   [schwartzgroup/ses-imputation](https://github.com/schwartzgroup/ses-imputation), which first uses a variant of target-density weighting (TDW) to interpolate data between disparate Census regionalizations and then imputes missing Census data from non-decennial years pre-ACS using the interpolated data;

2.  To base all retrieved and generated data directly on the Census API and file server and therefore be free of sharing restrictions; and

3.  To abstract away the complicated components of each process and provide straightforward and simple interfaces that future users / maintainers can easily modify and update.

### Stage 1: Retrieving and transforming Census data

The initial step of extracting specific Census variables and either applying labels to them or transforming them in some way (e.g. by combining them with other variables) is performed by [stage1_extract_census.R](stage1_extract_census.R). Most of the functionality from this script is located in [util.R](util.R), which abstracts away the fetching process. Specifically, this script makes use of two excellent Census data retrieval libraries:

-   [walkerke/tidycensus](https://github.com/walkerke/tidycensus), which queries the Census API, and
-   [GL-Li/totalcensus](https://github.com/GL-Li/totalcensus), which downloads Census data in bulk and parses out desired columns locally.

`totalcensus` uses up much more space than `tidycensus`, which only retrieves needed variables, but access to block groups in the 2009-2012 ACS 5-year estimate surveys is not currently available from the Census API; this necessitates the use of `totalcensus` to download the raw data directly.

Variable labels and transforms are expressed via lists or vectors of formulas operating directly on Census table names (e.g. `pct_female ~ P012026 / P001001`). As these can be somewhat abstract and hard to parse, an `explain(...)` function is provided to substitute variable definitions directly into the formulas:

    > explain(2010, "sf1", pct_female ~ P012026 / P001001, delimiter = " -> ")
    [1] "pct_female ~ [Sex by Age -> Total -> Female]/[Total Population -> Total]"

To facilitate fast iteration and re-exporting, all referenced tables are locally cached for future use. `totalcensus` includes its own caching system; for `tidycensus`, the cache file hierarchy is structured as follows:

    cache/
    └─ tidycensus/
       └─ <survey>/
          ├─ <year>/
          │  └─ <geography>/
          │     ├─ GEOID.csv.gz
          │     └─ <table>.csv.gz
          └─ variables.csv.gz

where:

-   `<survey>` is the abbreviation of the survey, e.g. `sf1`, `sf3`, `acs5`, `pl`, etc.;

-   `<year>` is the year that the survey was administered;

-   `<geography>` is the geographic level, e.g. `block`, `block group`, `tract`, etc.;

-   `GEOID.csv.gz` is a file containing all GEOIDs for the given geographic level;

-   `<table>.csv.gz` is a file containing the value of the given table for all geographies in the given level, e.g. `P001001.csv.gz`, `B01001_001`, etc.; and

-   `variables.csv.gz` is a list of all available variables in a given year for a given survey along with variable and topic descriptions.

### External data sources

A few files have been included in the [external/](external/) directory that were compiled from other sources. Those sources are as follows:

| Name                                   | Source                                                                                                                                                                                                                                                                                                                                     |
|----------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| [external/fips.csv](external/fips.csv) | National Bureau of Standards. (1987). Federal Information Processing Standards Publication: Codes for the identification of the States, the District of Columbia and the outlying areas of the United States, and associated areas (NIST FIPS 5-2; p. NIST FIPS 5-2). National Bureau of Standards. <https://doi.org/10.6028/NBS.FIPS.5-2> |
