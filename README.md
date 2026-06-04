# TxPathways

<!-- badges: start -->
<!-- badges: end -->

The goal of `TxPathways` is to provide a module to run drug utilization. This includes calculating:
- enumeration of treatment patterns (i.e. switch, single line, combination)
- time to pattern
- sankey diagram of pattern sequences
- calculation of daily dose and dose changes

More to come.....

## Installation

To install `TxPathways`, follow these steps:

1) clone the repository.
2) Open the `TxPathways.RProj` file in the repository
3) Navigate to the build tab in RStudio and select Install
4) Exit out of the `TxPathways.RProj` session
5) **Recommended** create a new `RProj` to test the package.


## Example

This is a basic example of the available functionality in `TxPathways`:

``` r
library(TxPathways)
library(picard)

# Set up connection details
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "snowflake",
  connectionString = "<connection_string>",
  user = "<user>",
  password = "<password>"
)

# Create execution settings with database and schema information
settings <- createExecutionSettings(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = "cdm_schema",
    workDatabaseSchema = "work_schema",
    cohortTable = "cohort",
    tempEmulationSchema = "temp_schema",
    databaseName = "database_name"
)

# Create treatment history settings
th <- createTreatmentHistorySettings(
    txCohorts = createTxCohorts(
        cohortIds = c(1, 2, 3),
        cohortLabels = c("treatment_a", "treatment_b", "target_condition"),
        cohortTypes = c("event", "event", "target")
    ),
    followUpWindow = createFollowUpWindow(
        startAnchor = "cohort_start_date",
        startDays = 0,
        endAnchor = "cohort_end_date",
        endDays = 0
    ),
    eraCollapseSettings = createEraCollapseSettings(
        minEraDuration = 0,
        eraCollapseSize = 30
    ),
    combinationTreatmentSettings = createCombinationTreatmentSettings(
        minPostCombinationDuration = 30,
        combinationWindow = 30
    ),
    pathwayOptions = createPathwayOptions(
        maxPathwayLength = 5,
        filterTreatments = "All"
    ),
    executionSettings = settings
)

# View the analysis configuration
th$printAnalysis()

# Generate and save the SQL query
th$saveQuery()

# Build the treatment history table
th$buildTreatmentHistory()


```

After the treatment history table is built in the dbms you can run various analyses to build sankey, sunburst, or other bits of information. 


``` r
txAnalysis <- createTxAnalysis(
  txHistory,
    analysisName,
    treatmentDuration         = createDurationSettings(),
    treatmentPathways          = createPathwaySettings(),
    treatmentAdherence         = createAdherenceSettings(),
    minCellCount               = 5L
)
jj <-txAnalysis$retrieve()

```
