getGeolocation <- function() {
'
  $(document).ready(function () {
    navigator.geolocation.getCurrentPosition(onSuccess, onError);

    function onError (err) {
      Shiny.onInputChange("geolocation", false);
    }

    function onSuccess (position) {
      setTimeout(function () {
        var coords = position.coords;
        console.log(coords.latitude + ", " + coords.longitude);
        Shiny.onInputChange("geolocation", true);
        Shiny.onInputChange("lat", coords.latitude);
        Shiny.onInputChange("long", coords.longitude);
        Shiny.onInputChange("locationChange", [coords.latitude,coords.longitude])
      }, 1100)
    }
  });
'
}


addProperties <- function(data) {
  .getProperty <- function(people, area) paste0(',"properties": {"fill": "',getColor(people, area),'","fill-opacity": 0.5},')
  .toFeature <- function(x) paste0('{"type": "Feature", "geometry": ',x,"}")
  apply(data, 1, function(x) {


    x[["st_asgeojson"]] <- x[["st_asgeojson"]] %>%
      .toFeature() %>%
      sub(",", .getProperty(as.numeric(x[["numberOfPeople"]]),as.numeric(x[["area"]])), .)
    x
  }) %>% t() %>%  as.data.frame()
}


createFeatureCollection <- function(geojsonData) {
  paste('{"type": "FeatureCollection","features": [',paste0(as.character(geojsonData), collapse = ", "), ']}')

}
