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

This is a basic example of the availabe functionality in `TxPathways`:

``` r
library(TxPathways)


connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "redshift",
  connectionString = "<jdbc_string>",
  user = "<user>",
  password = "<password>"
)
connection <- DatabaseConnector::connect(connectionDetails)

# call atlas results 
workDatabaseSchema <- "<scratch_johndoe>" # your scratch schema, or the ATLAS results schema
cohortTable <- "cohort"
targetCohortId <- 1
eventCohortIds <- c(2, 3)

# Make the treatment history table
createTxHistoryTable(
  connection = connection,
  workDatabaseSchema = workDatabaseSchema,
  cohortTable = cohortTable,
  targetCohortId = targetCohortId,
  eventCohortIds = eventCohortIds
)

targetCohortKey <- tibble::tibble(
  target_cohort_id = 1,
  target_cohort_name = "cohort"
)

eventCohortKey <- tibble::tibble(
  event_cohort_id = c(2, 3),
  event_cohort_name = c("drugA", "drugB")
)


th <- getTxHistoryTable(connection = connection,
                        targetCohortKey = targetCohortKey,
                        eventCohortKey = eventCohortKey)


viewSankey(th, maxPathLength = 2)

# check how many of each event

howManyHaveSingleTreatment(th)
howManyHaveMultipleTreatments(th)
howManyHaveComboTreatments(th)

# check denominator - currently all
howManySwitchedFromAtoB(th, depth = 2)
# check denominator - currently all
howManyHadInterruption(th, seqGap = '1-2', interruption = "30-60")
howManyHadInterruption(th, seqGap = '1-2', interruption = "60-9999")
howManyStoppedBefore(th, days = 365)


# check how long til event
howLongUntilFirstTreatment(th)
howLongUntilNextTreatment(th, seqGap = '1-2')
howLongAreDrugEras(th)

```

