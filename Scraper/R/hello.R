fakeDataPath <- "data/fakeData.json"
realDataPath <- "data/real"
completeFile <- file.path(realDataPath, "realData.json")

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
      # IF YOU WANT TO USE fakeData = FALSE, YOU NEED TO HAVE DOCKER INSTALLED.
      # SEE THE LINK BELOW @ TOP ANSWER
      #
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
      suppressMessages({
        remDr$open()
      })

      realData <- NULL
      for (query in unique(queryList)) {
        rawFile <- file.path(realDataPath, "raw", paste0(query, ".json"))
        cat("query: ", query, "\n")

        # change to TRUE if want to collectData always
        if (!file.exists(rawFile)) {
          rawData <- collectData(query, remDr)
          cat("  writing to file", rawFile, "\n")
          jsonlite::write_json(rawData, rawFile)
        } else {
          cat("  reading file", rawFile, "\n")
          rawData <- jsonlite::fromJSON(rawFile)
        }
        realData[[query]] <-
          processData(jsonlite::parse_json(rawData, simplifyVector = TRUE))
      }
      cat("*** Writing aggregated data to", completeFile, "***\n")
      jsonlite::toJSON(realData, auto_unbox = TRUE) %>%
        jsonlite::write_json(completeFile)
    },
    finally = {
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
  # find outs which day of traffic data to use (use the default selected)
  script <-
  "x = document.getElementsByClassName('section-popular-times-container');
   output = [];
   for (child of x[0].childNodes) {
     xx = child.childNodes[3];
     if (typeof xx !== 'undefined') {
       isHidden = xx.getAttribute('aria-hidden') != 'true';
       output.push(isHidden);
     }
   }
   return output
  "
  day <- remDr$executeScript(script) %>% unlist %>% which()

  endtTime <- Sys.time()
  cat("  object read after", endtTime - startTime, "seconds\n")

  popularTimesHTML <- popularTimes$getElementAttribute("innerHTML") %>% unlist()

  cat("  preparing raw object\n")
  rawData <- list(
    query = query,
    poiTitle = poiTitle,
    poiUrl = poiUrl,
    day = day,
    html = popularTimesHTML
  )
  return(jsonlite::toJSON(rawData))
}

processData <- function(rawData) {
  # {\"long\":48.2182,\"lat\":17.0481,\"traffic\":0}
  # lat & long pattern in the url
  longLatPattern <-
    ".*@([[:digit:]]+[.][[:digit:]]+[,][[:digit:]]+[.][[:digit:]]+)[,].*"
  longLat <- gsub(longLatPattern, "\\1", rawData$poiUrl) %>%
    strsplit(",") %>%
    unlist()
  xml <- xml2::read_html(unlist(rawData$html))
  trafficBars <- xml2::xml_find_all(xml, "//div[contains(@class, 'section-popular-times-bar')]") # section-popular-times-value section-popular-times-live-value
  # check which is the live bar
  trafficBarsLive <- sapply(trafficBars, function(bar) {
    0 < length(xml2::xml_find_all(bar, ".//div[contains(@class, 'live-value')]"))
  })
  if (length(which(trafficBarsLive)) != 0) {
    # stop(" raw data for query ", rawData$query, " has ", length(which(trafficBarsLive)), " live traffic bars")
    trafficBar <- trafficBars[trafficBarsLive][[1]]
  } else {
    #section-popular-times-current-value
    trafficBarsCurrentHour <- sapply(trafficBars, function(trafficBar) {
      (1 == (length(xml2::xml_find_all(trafficBar, "./div[contains(@class, 'section-popular-times-current-value')]"))))
    })
    if (length(which(trafficBarsCurrentHour)) != 7) {
      stop(" raw data for query", rawData$query, " contains traffic values for ", length(which(trafficBarsCurrentHour)), " days")
    }
    trafficBar <- trafficBars[trafficBarsCurrentHour][rawData$day]
  }
  verboseTraffic <- xml2::xml_attr(trafficBar, "aria-label")
  # gsub("[^[:alnum:] ]", "", verboseTraffic) %>% strsplit(" ") %>% .[[1]] %>%  as.numeric()
  x <- gsub("[^[:alnum:] ]", "", verboseTraffic) %>%
    strsplit(" ") %>%
    unlist()
  suppressWarnings({
    # pick the numeric values which are in percent format
    numericTraffic <- as.numeric(x[!is.na(as.numeric(x))]) / 100
  })
  processedData <- list(
    long = longLat[1],
    lat  = longLat[2],
    traffic     = ifelse(length(numericTraffic) > 1, numericTraffic[2], numericTraffic[1]),
    liveTraffic = ifelse(length(numericTraffic) > 1, numericTraffic[1], NA)
  )
  return(processedData)
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
