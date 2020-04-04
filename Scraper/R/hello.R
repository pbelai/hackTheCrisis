# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


# Google maps recording live visitor traffic (potential data scrapping)
# Google maps feature for density indication (red, orange, green)
# Sygic – sample data for Bratislava (location data for march 2020)
# Waze data
# Sample data from telco providers
# City open data platform – density of population
# Open street maps – identification of popular places


# 48.1486° N, 17.1077° E

# wifi
# petrzalska hrada , zlelezna studenie , koliba, nabrezie dunaja
#library(magrittr)
#library(httr)

# # slavin
# url <- "https://www.google.com/maps/place/Slav%C3%ADn/@48.1536998,17.0997204,18z/data=!4m12!1m6!3m5!1s0x476c8957e6c49333:0xf45a2f4a89dd2dae!2zU2xhdsOtbg!8m2!3d48.1538784!4d17.0997196!3m4!1s0x476c8957e6c49333:0xf45a2f4a89dd2dae!8m2!3d48.1538784!4d17.0997196"
# # r <- httr::GET()
# # 200
# httr::status_code(r)
# # http status
# httr::http_status(r)
# # headers
# httr::headers(r)
# #
# content(r, "text")
# # content(r, "text", encoding = "ISO-8859-1") - if the encoding doesnt match (also see stringi::stri_enc_detect(content(r, "raw")).)
#
# xxx <- content(r, "text")
fakeDataPath <- "data/fakeData.json"

runner <- function(fakeData = TRUE) {

  if (fakeData) {
    # read using data <- jsonlite::fromJSON("data/fakeData.json") %>% jsonlite::parse_json()
    if (file.exists(fakeDataPath) == FALSE) {
      generateData() %>%
        jsonlite::write_json(fakeDataPath)
    }
    return(0)
  }

  # I HATE MY LIFE
  # https://stackoverflow.com/questions/45395849/cant-execute-rsdriver-connection-refused
  # docker run -d -p 4445:4444 --shm-size 2g selenium/standalone-firefox   (start with 2 gigs of ram)
  # docker ps -q    (get x)
  # docker stop x   (use x)
  #
  # need to have docker container already running
  remDr <- RSelenium::remoteDriver(
    remoteServerAddr = "192.168.99.100",
    port = 4445L,
    browserName = "firefox"
  )
  queryList <- list(
    "Vojenský pamätník Slavin"
  )

  for (query in queryList) {

    rawData <- collectData(query, remDr)


  }


}

# a better interface may be to send POI name and we extract the data
collectData <- function(query, remDr) {
  remDr$open()
  remDr$navigate("https://www.google.com/maps/")
  # correct our location
  searchField <- remDr$findElement(using = "id", "searchboxinput")
  searchField$clearElement()
  searchField$sendKeysToElement(list(query)) #
  #searchField$getElementAttribute("value")
  searchButton <- remDr$findElement(using = "id", "searchbox-searchbutton")
  searchButton$clickElement()

  # check if we have more than one result section-result
  searchResults <- remDr$findElements(using = "class", "section-result")
  if (length(searchResults) > 1) {
    stop(paste0("the query '", query, "' resulted in ", length(searchResults), " search results"))
    # try to fix it with address section-info-action-button
    # for (singleSearchResult in multipleSeachResults) {
    #   # section-result-title
    #   res1 <- multipleSeachResults[[1]]$findElement(using = "class", "section-result-title")
    #   res1 <- res1$findElement("xpath", "h3") # section-result-title
    #   res1$getElementAttribute("innerHTML")
    #   singleSearchResult$clickElement()
    #   #singleSearchResult$findElements(using = "class", "section-info-action-button")
    # }
    # multipleSeachResults[[1]]$findElement(using = "xpath", "//h3[@class = 'section-result-title']")
  }
  poiTitle <- remDr$getTitle() %>% unlist()
  poiUrl <- remDr$getCurrentUrl() %>% unlist()
  popularTimes <- remDr$findElement(using = "class", "section-popular-times-container") # section-popular-times-live-value-gradient
  popularTimesHTML <- popularTimes$getElementAttribute("innerHTML") %>% unlist()
  #
  #   fileConn <- file("times.html")
  #   writeLines(unlist(popularTimesHTML), fileConn)
  #   close(fileConn)
  remDr$close()
  rawData <- list(
    query = query,
    poiTitle = poiTitle,
    poiUrl = poiUrl,
    html = popularTimesHTML
  )
  return(jsonlite::toJSON(rawData))
  # saveRDS(
  #   list(
  #     title = title,
  #     location = location,
  #     density = popularTimes
  #   )
  # )
}

processData <- function() {

}

submitData <- function() {

}

generateData <- function() {
  # bratislava - center
  pBaCenter <- c(48.1356952, 16.9758324) # bratislava
  # bratislava upper left corner
  pBaUpperLeft  <- c(48.2818733, 16.8558591)
  # bratislava lower right corner
  pBaLowerRight <- c(48.0208401, 17.2861986)

  boundBoxBa <- abs(pBaUpperLeft - pBaLowerRight)

  nObservations <- 400

  long <- rnorm(nObservations, pBaCenter[1], boundBoxBa[1] / 2)
  lat  <- rnorm(nObservations, pBaCenter[2], boundBoxBa[2] / 2)
  traffic <- pmax(0, rnorm(nObservations, 0.2, 0.3))

  data <- data.frame(long = long, lat = lat, traffic = traffic) %>%
    apply(MARGIN = 1, function(row) as.list(row)) %>%
    jsonlite::toJSON(auto_unbox = TRUE)
  return(data)
}
