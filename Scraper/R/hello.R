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
realDataPath <- "data/real"

#' @importFrom magrittr %>%
runner <- function(fakeData = TRUE) {

  if (fakeData) {
    # read using data <- jsonlite::fromJSON("data/fakeData.json") %>% jsonlite::parse_json()
    if (file.exists(fakeDataPath) == FALSE) {
      generateData() %>%
        jsonlite::write_json(fakeDataPath)
    }
    return(0)
  }




  tryCatch(
    {
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
      queryList <- c(
        "Vojenský pamätník Slavin",
        "Železná studienka, Cesta mládeže, Nové Mesto",
        "Sad Janka Kráľa, Sad Janka Kráľa, Petržalka",
        "Aupark, Einsteinova, Petržalka"
      )
      remDr$open() # this operation may take a long time

      realData <- NULL
      for (query in unique(queryList)) {
        rawFile       <- file.path(realDataPath, "raw", paste0(query, ".json"))
        cat("query: ", query, "\n")

        if (TRUE) { #!file.exists(rawFile)
          rawData <- collectData(query, remDr)
          cat("  writing to file", rawFile, "\n")
          jsonlite::write_json(rawData, rawFile)
        } else {
          cat("  reading file", rawFile, "\n")
          rawData <- jsonlite::fromJSON(rawFile)
        }

        realData[query] <- processData(jsonlite::parse_json(rawData))


      }

      browser()
    },
    error=function(cond) {

    },
    warning=function(cond) {

    },
    finally={
      remDr$close()
    }
  )



}


#' @importFrom magrittr %>%
collectData <- function(query, remDr) {
  cat("  navigating\n")
  remDr$navigate("https://www.google.com/maps/")
  cat("  searching by query\n")
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
  sleepTime <- 1 # sec

  cat("  extracting info\n")
  startTime <- Sys.time()
  poiTitle <- remDr$getTitle() %>% unlist()
  poiUrl   <- remDr$getCurrentUrl() %>% unlist()
  while(grepl("@", poiUrl) == FALSE) {
    Sys.sleep(sleepTime)
    poiUrl <- remDr$getCurrentUrl() %>% unlist()
  }
  popularTimes <- NULL
  while(is.null(popularTimes)) {
    popularTimes <- tryCatch(
      {
        suppressMessages({
          remDr$findElement(using = "class", "section-popular-times-container") # section-popular-times-live-value-gradient
        })
      },
      error = function(cond) {
        Sys.sleep(sleepTime)
        return(NULL)
      }
    )
  }
  endtTime <- Sys.time()
  cat("  object read after", endtTime - startTime, "seconds\n")

  popularTimesHTML <- popularTimes$getElementAttribute("innerHTML") %>% unlist()

  #
  #   fileConn <- file("times.html")
  #   writeLines(unlist(popularTimesHTML), fileConn)
  #   close(fileConn)
  cat("  preparing raw object\n")
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

processData <- function(rawData) {
  # {\"long\":48.2182,\"lat\":17.0481,\"traffic\":0}
  # lat & long pattern in the url
  longLatPattern <-
    ".*@([[:digit:]]+[.][[:digit:]]+[,][[:digit:]]+[.][[:digit:]]+)[,].*"
  liveTrafficPattern <- ".*"


  longLat <- gsub(longLatPattern, "\\1", rawData$poiUrl) %>%
    strsplit(",") %>%
    unlist()
  xml <- xml2::read_html(unlist(rawData$html))
  trafficBars <- xml2::xml_find_all(xml, "//div[contains(@class, 'section-popular-times-bar')]") # section-popular-times-value section-popular-times-live-value
  # check which is the live bar
  trafficBarsLive <- sapply(trafficBars, function(bar) {
    0 < length(xml2::xml_find_all(bar, ".//div[contains(@class, 'live-value')]"))
  })
  if (length(which(trafficBarsLive)) != 1) {
    stop(" raw data for query", rawData$query, "has", length(which(trafficBarsLive)), "live traffic bars")
  }
  verboseTraffic <- xml2::xml_attr(trafficBars[trafficBarsLive][[1]], "aria-label")


  browser()
  processedData <- list(
    long = longLat[1],
    lat  = longLat[2],
    traffic = ""#xml2::read_xml()
  )


}

submitData <- function() {

}

#' @importFrom magrittr %>%
generateData <- function() {
  # old c(48.1356952, 16.9758324)
  # bratislava - center
  pBaCenter <- c(48.144851, 17.113107) # bratislava
  # bratislava upper left corner
  pBaUpperLeft  <- pBaCenter + c(0.05,-0.05) # higher lower
  # bratislava lower right corner
  pBaLowerRight <- pBaCenter + c(-0.05,0.05) # lower higher

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

sendData <- function(generate = TRUE) {
  body <- generateData()
  url <- "od misa" # TODO:
  httr::POST(url, body = body)
}
