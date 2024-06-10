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

Users can install the current development version of TxPathways from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ohdsi/TxPathways")
```

## Treatment History Query Methods

Users can choose between 2 different methods for generating patients' treatment histories:

  * **ATLAS Pathways method**: The methodology used in [ATLAS](https://atlas.ohdsi.org/)'s [Cohort Pathways](https://ohdsi.github.io/TheBookOfOhdsi/Characterization.html#cohort-pathways-in-atlas) feature.  Importantly, the `gapDays` parameter in `createTxHistoryTable` will be used to collapse any dates found within the specified number of days into the earliest date, both within and across treatment eras.  Doing so will reduce noise in the results by inferring nearby and closely overlapping eras to be part of the same combination era.  *However*, this logic may result in the collapse of era end dates and as such, this method is *not* suitable for analyses requiring accurate treatment era end dates (duration of therapy, discontinuation, etc.)
  * **Default method**: Same as the ATLAS method, except that `gapDays` will only be used to collapse dates within a given therapy cohort and will *not* collapse end dates.  Use this method if your analysis requires accurate treatment era end dates; however, it might not be a good fit if significant use of combination therapy is expected in your study population


## Example

This is a basic example of the available functionality in `TxPathways`:

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

