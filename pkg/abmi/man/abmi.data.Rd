\encoding{UTF-8}
\name{abmi.data}
\alias{abmi.data}
\docType{data}
\title{ABMI Bird Counts and Site Capabilities from 2007}
\description{
Raw breeding bird and site capability data (2007) from the Alberta Biodiversity Monitoring Institute.
}
\usage{data(abmi.data)}
\format{
  The format of raw breeding bird data is (dots are due to spaces and special characters in the original *.csv left unchanged):

  \item{Rotation}{Reference describing when data was collected at a broad level (i.e. Prototype, Rotation 1).}
  \item{ABMI.Site}{Reference number given to each ABMI data collection site.}
  \item{Year}{Collection year.}
  \item{Field.Date}{Day, month, and year data was collected.}
  \item{Field.Crew.Member.s.}{Initials for the field technicians collecting the field data.}
  \item{Identification.Date}{Day, month, and year data was analyzed by specialist.}
  \item{Identification.Analyst}{Initials for the technicians/specialists identifying the specimens.}
  \item{Point.Count.Station}{Location where site capability information was recorded. There are
    9 point count stations established per ABMI site with No. 1 being site
    centre and the remainder of the stations arranged around site
    centre at 300 m intervals.}
  \item{Wind.Conditions}{Estimate of wind conditions on a scale of 0--5.}
  \item{Precipitation}{Classification for precipitation conditions in 5 categories.}
  \item{Start.of.Point.Count...24.hour.clock.}{Time of day recording was started.}
  \item{End.of.Point.Count..24.hr.clock.}{Time of day recording was finished.}
  \item{Common.Name}{Common name of bird species detected during point counts.}
  \item{Scientific.Name}{Scientific name of bird species detected during point counts.}
  \item{Taxonomic.Resolution}{Resolution to which bird species was identified (e.g. Family, Genus,
    Species etc.).}
  \item{Unique.Taxonomic.Identification.Number}{Globally unique identifier of bird species detected during point
    count. Unique taxonomic identifiers are generally taken from the
    International Taxonomic Information System (ITIS;
    http://www.itis.gov/).}
  \item{Time.First.Detected..seconds.}{Approximate time the bird analyst first detects a bird species from
    the recording; listed in 10-second intervals.}
  \item{Interval.1..0.200.seconds.}{First time interval of the 10-minute point count (0-200 seconds)
    when bird species are detected and identified.}
  \item{Interval.2..201.400.seconds.}{Middle time interval of the 10-minute point count (201-400 seconds)
    when bird species are detected or re-detected.}
  \item{Interval.3..401.600.seconds.}{Last time interval of the 10-minute point count (401-600 seconds)
    when bird species are detected or re-detected.}
  \item{Behaviour}{Classification given to each species detection (if possible).}
  \item{Tally}{Number of individuals from each bird species that were counted
    within a 150 m radius of the point count station. This variable only
    applies to point count stations that fall within waterbodies.}

  The format site capability data is (dots are due to spaces and special characters in the original *.csv left unchanged):

  \item{Rotation}{Reference describing when data was collected at a broad level (i.e. Prototype, Rotation 1).}
  \item{ABMI.Site}{Reference number given to each ABMI data collection site.}
  \item{Year}{Collection year.}
  \item{Field.Date}{Day, month, and year data was collected.}
  \item{Field.Crew.Member.s.}{Initials for the field technicians collecting the field data.}
  \item{Point.Count.Station}{Location where site capability information was recorded. There are
    9 point count stations established per ABMI site with No. 1 being site
    centre and the remainder of the stations arranged around site
    centre at 300 m intervals.}
  \item{Primary.Secondary.Ecological.Site.Classification}{Indicates whether the ecological 
    site classification corresponds to a
    current primary, current secondary, historical primary or historical
    secondary classification.}
  \item{Ecological.Site.Classification}{Ecological site classification.}
  \item{Ecosite...Nutrient.Moisture.Code}{Nutrient and moisture code for the ecological site classification of a
    vegetated site based on the dominant vegetation community that is present.}
  \item{Ecosite...Tree.Species.Modifier}{At every vegetation sampling location, the tree species modifier is
    used to classify the site if trees are present and is combined with the
    moisture/nutrient category to give an ecological site classification.}
  \item{Ecosite..Structural.Stage.Community}{The general structural stage community being described including:
    tree dominated ecosites; non-tree dominated ecosites; open water
    dominated ecosites.}
  \item{Ecosite...Structural.Stage}{Describes the structure stage of the ecosite; determined after the
    ecological-site type has been designated i.e. the nutrient/moisture
    code designation and tree species modifer.}
  \item{Percent.Area.of.Ecological.Site.Classification}{Percent of the area surrounding the ABMI site/point count station
    described by the primary and secondary ecological site classification
    (10 percent increments).}
}
\details{
Original source files were \code{RAW_T_BirdCounts63451.csv} and \code{RAW_T_SiteCapability63352.csv}.
The derived binary file contains only the data for the year 2007.

For more details (exact meaning of different entries) see terrestrial ABMI guide downloadable with the datasets from the website.
}
\source{
Raw breeding bird and site capability data (2007) from the Alberta Biodiversity Monitoring
Institute was used, in whole, to create this product. More information on the Institute
can be found at: \url{http://www.abmi.ca/}
}
\references{
References on methodology: \url{http://www.abmi.ca/}
}
\examples{
data(abmi.data)
str(abmi.data)
}
\keyword{datasets}
