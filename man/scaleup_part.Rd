% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dashboard.R
\name{scaleup_part}
\alias{scaleup_part}
\alias{scaleup_recruit}
\title{Scale segmented participation counts to total (if needed)}
\usage{
scaleup_part(part_segment, part_total, test_threshold = 10,
  show_test_stat = FALSE, outvar = "participants")

scaleup_recruit(part_segment, part_total, test_threshold = 10,
  show_test_stat = FALSE, outvar = "recruits")
}
\arguments{
\item{part_segment}{data frame: A segmented  participation table
produced by \code{\link{est_part}} (e.g., with segment argument set to "res")}

\item{part_total}{data frame: An overall participation table produced by
\code{\link{est_part}}}

\item{test_threshold}{numeric: threshold in whole number percentage points 
which defines the upper limit of acceptable proportion of missing values for 
the segment. The function will stop with an error if this threshold
is exceeded. Relaxing the threshold can allow the check to pass, but use this
with caution since a high percentage of missing values might suggests that 
the breakouts aren't representative (e.g., if not missing at random).}

\item{show_test_stat}{logical: If TRUE, the output table will include
a variable holding the test statistic for each row.}

\item{outvar}{character: name of variable that stores metric}
}
\description{
This scaling accounts for missing values in segments, scaling up all counts
to ensure the sum matches the total count. It expects 2 tables as input, both
produced by \code{\link{est_part}}. If no scaling is needed (i.e., sum(part_segment$part)
== sum(part_total$part)) the function will simply return the input df.
}
\examples{
library(dplyr)
data(history)
history <- label_categories(history)

# demonstrate the need for scaling
part_total <- est_part(history)
part_segment <- est_part(history, "sex", test_threshold = 40)
left_join(
    select(part_total, year, part_tot = participants),
    group_by(part_segment, year) \%>\% summarise(part_seg = sum(participants)),
)

# perform scaling
part_segment <- scaleup_part(part_segment, part_total)
left_join(
    select(part_total, year, part_tot = participants),
    group_by(part_segment, year) \%>\% summarise(part_seg = sum(participants)),
)

# new recruits - unscaled
history_new <- filter(history, R3 == "Recruit")
part_total <- est_recruit(history_new, "tot")
part_segment <- est_recruit(history_new, "sex")
part_segment

# new recruits - scaled
scaleup_recruit(part_segment, part_total)
}
\seealso{
Salic Function Reference: \code{\link{salic}}

Other dashboard functions: \code{\link{check_threshold}},
  \code{\link{est_churn}}, \code{\link{est_part}},
  \code{\link{format_result}}
}
\concept{dashboard functions}
