## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=FALSE, message=FALSE------------------------------------------------
require(leaflet)
require(rnaturalearth)
included_countries <- dplyr::tribble(
  ~code, ~description, ~time, ~supported, ~license, ~links,
    "sk", "Data is publicly available", "Data seems to be available for about one month","Readable","CC BY 4.0<sup>    <a href='https://data.europa.eu/data/datasets/https-data-gov-sk-set-4432e760-cb76-411e-883a-4ed4a182dee7'>1</a>    <a href='https://data.europa.eu/data/datasets/https-data-gov-sk-set-841fe874-0ce2-4dc9-96f4-a058d078dac5'>2</a>    <a href='https://data.europa.eu/data/datasets/https-data-gov-sk-set-1367914a-2847-420e-906e-4bdece18888a'>3</a>    <a href='https://data.europa.eu/data/datasets/https-data-gov-sk-set-07de9eb1-d329-43fd-ab44-c00c6dc91207'>4</a>  </sup>",c("Data source"="https://opendata.shmu.sk/meteorology/weather/radar/volume/"),
      "ro", "Data is publicly available", "Data seems to be available for about the last 4 days","Readable","CC BY 4.0<sup><a href='https://data.gov.ro/en/dataset/date-radare-meteorologice'>1</a></sup>",c("Data source"="https://opendata.meteoromania.ro/radar/"),
  "se", "Data is publicly available", "Data is only available for 24 hours","Readable","CC BY 4.0 SE<sup><a href='https://www.smhi.se/data/om-smhis-data/villkor-for-anvandning'>1</a></sup>",c("API documentation"="https://opendata.smhi.se/radar/api","Documentation of radar products"="https://www.smhi.se/data/sok-oppna-data-i-utforskaren/meteorologiska-observationer-radar-sverigekomposit-och-enskilda-volymer"),
  "us", "Data is available in an aws S3 bucket", NA, "Readable", "NOAA data disseminated through NODD are open to the public and can be used as desired<sup> <a href='https://registry.opendata.aws/noaa-nexrad/'>1</a>", c("Bucket" = "https://noaa-nexrad-level2.s3.amazonaws.com/index.html", "More info" = "https://www.ncei.noaa.gov/products/radar/next-generation-weather-radar"),
  "be", "Data is suggested to be opened into the future, it is however not yet accessible.", NA, "Information", NA, c("More info" = "https://opendata.meteo.be/geonetwork/srv/eng/catalog.search#/metadata/RMI_DATASET_JABBEKE_VOLUME"),
  "pl", "Volume data should be open according to a presentation by <a href='https://poster.easyabstract.it/ERAD2024/abstract/15550/161/884'>Groenemeijer et al</a> at erad2024. However no link is known by the authors of the package up to now.", NA, "Information", NA, list(),
  "fr", "Currently we have not implemented reading the bufr format", NA, "Not implemented", NA, list(),
  "nl", "For reading a special piece of converter software needs to be available. Furthermore for large datasets it might be advantagious to create a personal API key.", NA, "Readable", "CC BY 4.0<sup> <a href='https://dataplatform.knmi.nl/dataset/radar-volume-full-herwijnen-1-0'>1</a>, <a href='https://dataplatform.knmi.nl/dataset/radar-volume-denhelder-2-0'>2</a></sup>", list(),
  "fi", "Data are downloaded from an open S3 bucket. Only a small correction to the `hdf5` file is needed to read the files.", NA, "Readable", "CC BY 4.0<sup> <a href='https://en.ilmatieteenlaitos.fi/radar-data-on-aws-s3'>1</a></sup>", c("Data Documentation" = "https://en.ilmatieteenlaitos.fi/radar-data-on-aws-s3"),
  "dk", "An API key is needed to download the data. Furthermore in some cases the RHOHV parameter does not seem to reach 1.", "Only the last 6 months", "Readable", "CC BY 4.0<sup><a href='https://opendatadocs.dmi.govcloud.dk/en/Terms_of_Use#license'>1</a></sup>", c("Data description" = "https://opendatadocs.dmi.govcloud.dk/Data/Radar_Data", "API description" = "https://opendatadocs.dmi.govcloud.dk/en/APIs/Radar_Data_API", "Getting a API key" = "https://opendatadocs.dmi.govcloud.dk/en/Authentication"),
  "de", "Data is downloaded from the unfiltered repository. Polar volumes are reconstructed from separate parameters", "Only the last three days", "Readable", NA, c("Data documentation" = "https://www.dwd.de/EN/ourservices/radar_products/radar_products.html", "Data availability"="https://www.dwd.de/DE/leistungen/radarniederschlag/rn_info/home_freie_radarstatus_kartendaten.html"),
  "cz", "Polar volumes are reconstructed from parameter specific polar volumes", "The last three days are available", "Readable", NA, c("Data description" = "https://opendata.chmi.cz/meteorology/weather/radar/radar_description_en.pdf"),
  "ee", "The repository seems to implement quite strict rate limiting therefore retries are implemented. However the http error 429 is still frequently returned.", NA, "Readable", NA, c("Data" = "https://avaandmed.keskkonnaportaal.ee/dhs/Active/documentList.aspx?ViewId=b92201f4-8b48-4d3a-b410-30c8ce4016d5")
) |>
  dplyr::left_join(
    rnaturalearth::ne_countries("medium") |>
      dplyr::mutate(key_lower = tolower(iso_a2_eh)),
    by = c(code = "key_lower")
  ) |>
  sf::st_as_sf() |>
  dplyr::rowwise() |>
  dplyr::mutate(
    supported = factor(supported),
    links_html = dplyr::if_else(
      length(links) == 0, "",
      paste(
        "<p><strong>Links: </strong><ul>",
        paste0("<li><a href='", links, "'>",
          names(links), "</a></li>",
          collapse = ""
        ), "</ul></p>"
      )
    )
  )

stopifnot(!any(is.na(included_countries$name)))
pal <- colorFactor("plasma", levels = unique(included_countries$supported))
labels <- glue::glue(
  "<h5>{included_countries$name}</h5><br/>
  <p>{included_countries$description}</p>
  {dplyr::if_else(!is.na(included_countries$time),
  paste('<p><strong>Temporal restrictions:</strong>',
  included_countries$time,'</p>'),'')}
  {dplyr::if_else(!is.na(included_countries$license),
  paste('<p><strong>License:</strong>',included_countries$license,'</p>'),'')}
  {included_countries$links_html}",
) %>% lapply(htmltools::HTML)
leaf <- leaflet(width = "100%") |>
  addTiles() |>
  addPolygons(
    opacity = 1,
    data = included_countries, color = "white", weight = 3,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      bringToFront = TRUE
    ), fillColor = ~ pal(supported),
    popup = labels
  ) |>
  setView(-30, 49, 2) |>
  addLegend("bottomright",
    pal = pal, values = unique(included_countries$supported),
    title = "Support level"
  )
library(htmltools)
browsable(
  tagList(list(
    tags$head(
      tags$style(
        ".leaflet-tooltip{ width: 150px; white-space: normal; }"
      )
    ),
    leaf
  ))
)

