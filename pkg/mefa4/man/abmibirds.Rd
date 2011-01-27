\name{abmibirds}
\alias{abmibirds}
\docType{data}
\title{
Raw Dataset of Bird Point Counts
}
\description{
A data set of bird point counts collected by the Alberta
Biodiversity Monitoring Institute.
}
\usage{data(abmibirds)}
\format{
  A data frame with 59341 observations on the following 21 variables.
  \describe{
    \item{\code{Rotation}}{a factor. Reference describing when data was 
      collected at a broad level.
      Code definition: Prototype = 2003--2006, Rotation 1 = 2007--2012}
    \item{\code{ABMI.Site}}{a numeric vector. Reference number given to 
      each ABMI data collection site (1--1656).}
    \item{\code{Year}}{a numeric vector. Collection year.}
    \item{\code{Field.Date}}{a factor. Day, month, and year data was 
      collected.}
    \item{\code{Field.Crew.Members}}{a factor. Initials for the field 
      technicians collecting the field data.}
    \item{\code{Identification.Date}}{a factor. Day, month, and year 
      data was analyzed by specialist.}
    \item{\code{Identification.Analyst}}{a factor. Initials for the 
      technicians/specialists identifying the specimens.}
    \item{\code{Point.Count.Station}}{a numeric vector. Point count 
      station where recording was made: 9 stations were
      located around each ABMI site (1--9).}
    \item{\code{Wind.Conditions}}{a factor. Estimate of wind conditions 
      on a scale of 0--5. 
      0 = no wind, 1 = calm, 2 = leaves rustling, 3 small branches
      moving, 4 = large branches moving, 5 = large branches moving and
      the tree is swaying}
    \item{\code{Precipitation}}{a factor. Classification for precipitation 
      conditions in 5 categories.
    Input value: Drizzle, Fog, Rain, Sleet, Snow, None}
    \item{\code{Start.of.Point.Count}}{a factor. Time of day recording was 
      started.
    Input value: 24 hour clock (hh:mm).}
    \item{\code{End.of.Point.Count}}{a factor. Time of day recording was 
      finished. Input value: 24 hour clock (hh:mm).}
    \item{\code{Common.Name}}{a factor. Common name of bird species detected 
      during point counts.}
    \item{\code{Scientific.Name}}{a factor. Scientific name of bird species 
      detected during point count.}
    \item{\code{Unique.Taxonomic.Identification.Number}}{a factor. Globally 
      unique identifier of bird species detected during point
      count. Unique taxonomic identifiers are generally taken from the
      International Taxonomic Information System 
      (ITIS; \url{http://www.itis.gov/}).}
    \item{\code{Taxonomic.Resolution}}{a factor. Resolution to which bird 
      species was identified (e.g. Family, Genus, Species etc.).}
    \item{\code{Time.First.Detected}}{a factor. Approximate time the bird 
      analyst first detects a bird species from
the recording; listed in 10-second intervals.}
    \item{\code{Interval.1}}{a factor. First time interval of the 10-minute 
      point count (0--200 seconds)
      when bird species are detected and identified.}
    \item{\code{Interval.2}}{a factor. Middle time interval of the 10-minute 
      point count (201--400 seconds) when bird species are detected or 
      re-detected.}
    \item{\code{Interval.3}}{a factor. Last time interval of the 10-minute 
      point count (401--600 seconds) when bird species are detected or 
      re-detected.}
    \item{\code{Behaviour}}{a factor. Classification given to each species 
      detection (if possible).}
  }
}
\details{
Breeding birds were measured at nine point count stations. Point count stations
were in a grid pattern with point count station no. 1 located at site-centre and the
remaining stations located 300 m apart surrounding site centre.
We conducted breeding bird surveys from one half hour before sunrise to 10:00 hrs.

We recorded vocalizations of birds for 10 minutes at each point count station using
an omni-directional microphone (CZM microphone; River Forks Research Corp.)
mounted at ear level on a professional tripod and connected to a mini hard drive
recorder. We recorded birds on a Marantz PM D670 or PM D660 Solid State
recorder at 320 kbps in .mp3 format. We calibrated the recorder volume to be in
the mid ranges.

While conducting the 10 minute bird
recordings, we scanned the areas surrounding the point count station for all birds (even those
vocalizing), noting species, number of individuals (including flock sizes of birds flying overhead), and
distance from the point count station, for all bird observations. We also noted factors that potentially
bias bird recordings, such as wind speed and precipitation. Bird recordings were later analyzed by bird
identification specialists in a laboratory setting.

If a bird point fell in open water, we established a new point if we were able to get within 100 m of the
original point, recording distance and direction from that original point. If it was not possible to get
within 100 m of the original point (i.e., <200 m from the last point), we conducted a 10 minute visual
point count of the waterbody recording observations into the microphone. We may not have sampled
certain points because they were inaccessible (e.g., a stream made access hazardous or impossible).

We analyzed bird recordings in a laboratory setting. We identified the species, time of first detection
(within 10 second intervals), behaviour (e.g., singing, calling, or alarm-calling), and the time interval that
individual birds were detected. We recognized 3 time intervals: Interval 1 (0--200 seconds), Interval 2
(201--400 seconds), and Interval 3 (401--600 seconds). Individual birds were detected in 1, 2, or 3 of
the time intervals. We identified red squirrel (\emph{Tamiasciurus hudsonicus}) vocalizations in addition to bird
vocalizations. Bird recordings are randomly sampled and verified by other experts in bird identification
to ensure accuracy.

Throughout ABMI raw data files, the following codes and definitions are applied.

None or 0:
None or 0 is applied to any variable that was examined by field crews and found to be absent.
None is used for text entries and 0 is used for numerical entries. For example, when field crews
examine the canopy and find no Veteran trees in the canopy, this is recorded as None. When
there is no slope at the survey site, slope is recorded as 0. The numeral 0 can also be used as a
nominal code, for example, wind conditions can be recorded as 0.

VNA Variable Not Applicable:
Some ABMI data is collected in a nested manner. For example Tree Species is a parent variable. This
variable has a number of child variables that are used to describe the parent variable in more detail
(e.g., condition, DBH, decay stage). When the parent variable is recorded as None, child variables
are no longer applied and are recorded as VNA. VNA is also used when the protocol calls for a
modified sampling procedure based on site conditions (e.g., surface substrate protocol variant for
hydric site conditions). The use of VNA implies that users of the data should not expect that any
data could be present.

DNC, Did Not Collect:
DNC is used to describe variables that should have been collected but were not. There are a
number of reasons that data might not have been collected (e.g., staff oversight, equipment failure,
safety concerns, environmental conditions, or time constraints). Regardless of the reason data was not
collected, if under ideal conditions it should have been, the record in the data entry file reads DNC.
The use of DNC implies that users should expect the data to be present, though it is not.

PNA, Protocol Not Available:
The ABMI's protocols were, and continue to be, implemented in a staged manner. As a result, the
collection of many variables began in years subsequent to the start of the prototype or operational
phases or where discontinued after a few years of trial. When a variable was not collected because the
protocol had yet to be implemented by the ABMI (or was discontinued by the ABMI), the data entry
record reads PNA. This is a global constraint to the data (i.e., a protocol was not implemented until
2006, therefore, previous years cannot have this variable). PNA is to be used to describe the lack of
data collection for entire years.

SNI, Species Not Identified:
In various fields related to species identification, SNI is used to indicate that the organism was not
identified. Some possible reasons that identification was not possible include insufficient or deficient
sample collected and lack of field time.
}
\source{
RAW_T26BreedingBirds28621.csv, \url{http://www.abmi.ca}
}
\references{
Raw breeding bird data (2004--2006 inclusive) from the Alberta Biodiversity Monitoring
Institute was used, in whole or part, to create this product. More information on the Institute
can be found at: \url{http://www.abmi.ca}
}
\examples{
data(abmibirds)
str(abmibirds)
}
\keyword{datasets}
