#' The point locations of Australian capital cities.
#'
#' A dataset containing the longitude and latitude values of Australian
#' capital cities.
#'
#' @format A data frame with 8 rows and 3 variables:
#' \describe{
#'   \item{points}{name of cities}
#'   \item{longitude}{location of point in longitude degrees}
#'   \item{latitude}{location of point in latitude degrees}
#' }
"capital_cities"


#' The polygons of Tasmanian Statistical Areas in 2016.
#'
#' A simple features dataset containing the polygons for all Tasmanian SA2s in
#' 2016.
#'
#' @format A simple features data frame with 99 rows and 15 variables:
#' \describe{
#'   \item{sa2_main_2016}{complete code of the Statistical Area}
#'   \item{sa2_5dig_2016}{simple code for the Statistical Area}
#'   \item{sa2_name_2016}{name of the Statistical Area}
#'   \item{sa3_code_2016}{code for the SA3 containing the Statistical Area}
#'   \item{sa3_name_2016}{name of the SA3 containing the Statistical Area}
#'   \item{sa4_code_2016}{code for the SA4 containing the Statistical Area}
#'   \item{sa4_name_2016}{name of the SA4 containing the Statistical Area}
#'   \item{gcc_code_2016}{code for the Greater Capital City region containing
#'    the Statistical Area}
#'   \item{gcc_name_2016}{name of the Greater Capital City region containing
#'   the Statistical Area}
#'   \item{ste_code_2016}{code for the state containing the Statistical Area}
#'   \item{ste_name_2016}{name of the state containing the Statistical Area}
#'   \item{areasqkm_2016}{area contained in the polygon}
#'   \item{id}{distinguishes SA2 regions}
#'   \item{population}{amount of people living within the region}
#'   \item{sa2_code_2016}{code of the Statistical Area}
#' }
#'
#' @name tas_sa2
#' @usage tas_sa2
"tas_sa2"

#' The polygons of Tasmanian Local Government Areas in 2016.
#'
#' A simple features dataset containing the polygons for all Australian LGAs in
#' 2016.
#'
#' @format A simple features data frame with 39 rows and 6 variables:
#' \describe{
#'   \item{lga_code_2016}{code for the Local Government Area}
#'   \item{lga_name_2016}{name of the Local Government Area}
#'   \item{ste_code_2016}{code for the state containing the Local Government Area}
#'   \item{ste_name_2016}{name of the state containing the Local Government Area}
#'   \item{areasqkm_2016}{area contained in the polygon}
#'   \item{geometry}{describes where on Earth the polygon is located}
#' }
"tas_lga"

#' The hexagon centres for polygons of Tasmanian Local Government Areas in 2016.
#'
#' A tibble dataset containing the processed data for all Australian LGAs in
#' 2016. Each point corresponds to hexagon centre.
#'
#' @format A simple features data frame with 39 rows and 6 variables:
#' \describe{
#'   \item{lga_code_2016}{code for the Local Government Area}
#'   \item{longitude, latitude}{polygon centroid}
#'   \item{points, focal_longitude, focal_latitude, focal_dist, focal_angle}{Focal point (capital city) information used for each polygon/hexagon}
#'   \item{rownumber}{row number, in case it can be useful}
#'   \item{hex_long, hex_lat, hex_id}{hexagon centre and id}
#' }
"tas_lga_hexctr"

#' The amount of homeless people in each Statistical Area at Level 2 in 2016.
#'
#' A data frame of the Statistical Area at Level 2 names and amount of homeless
#'
#' @format A data frame with 545 rows and 2 variables:
#' \describe{
#'   \item{homeless}{amount of homeless people}
#'   \item{sa2_name_2016}{name of the Statistical Area at Level 2}
#' }
"homeless"

#' 2019 Australian Federal election data: First preference votes for candidates
#' (House of Representatives) in each electorate.
#' 
#' A dataset containing first preference vote counts, candidate names, and 
#' other results for the House of Representatives from the 2016 Australian federal election. 
#' The data were obtained from the Australian Electoral Commission, and downloaded 
#' from \url{https://results.aec.gov.au/24310/Website/Downloads/HouseFirstPrefsByPartyDownload-24310.csv}
#' 
#' @format A data frame with the following variables:
#' \describe{
#'     \item{StateAb}{Abbreviation for state name}
#'     \item{DivisionID}{numeric identifier that links the electoral division
#'     with Census and other election datasets.}
#'     \item{DivisionNm}{Electoral division name}
#'     \item{CandidateID}{Candidate ID}
#'     \item{Surname}{Candidate surname}
#'     \item{GivenNm}{Candidate given name}
#'     \item{BallotPosition}{Candidate's position on the ballot}
#'     \item{Elected}{Whether the candidate was elected (Y/N)}
#'     \item{HistoricElected}{Whether the candidate is the incumbent member}
#'     \item{PartyAb}{Abbreviation for political party name}
#'     \item{PartyNm}{Political party name}
#'     \item{OrdinaryVotes}{Number of ordinary votes cast at the electorate
#'      for the candidate}
#'     \item{AbsentVotes}{Number of absentee votes}
#'     \item{ProvisionalVotes}{Number of provisional votes}
#'     \item{PrePollVotes}{Number of pre-poll votes}
#'     \item{PostalVotes}{Number of postal votes}
#'     \item{TotalVotes}{Number of total votes}
#'     \item{Swing}{% swing for or away from party}
#'     }
"fp19"