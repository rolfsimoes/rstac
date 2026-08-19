mock_stac_response <- function(url, payload, status_code = 200L, headers = list(), simplify_vector = TRUE) {
  if (is.list(payload) || is.atomic(payload)) {
    payload <- jsonlite::toJSON(
      payload,
      auto_unbox = TRUE,
      null = "null",
      digits = NA
    )
  }

  structure(
    list(
      url = url,
      status_code = as.integer(status_code),
      headers = modifyList(
        list(`Content-Type` = "application/json"),
        headers
      ),
      content = charToRaw(payload)
    ),
    class = "response"
  )
}

mock_stac_service <- function(rate_limit_after = Inf) {
  landing_url <- "https://mock-stac.example/v1/"
  api_url <- "https://mock-stac.example/v1/api"
  search_url <- "https://mock-stac.example/v1/search"

  counts <- new.env(parent = emptyenv())
  counts$requests <- list()
  counts$post_bodies <- list()
  counts$total <- 0L

  count_request <- function(method, url, body = NULL) {
    path <- httr::parse_url(url)$path
    if (is.null(path) || identical(path, "")) {
      path <- "/"
    }
    if (!startsWith(path, "/")) {
      path <- paste0("/", path)
    }

    key <- paste(method, path)
    counts$total <- counts$total + 1L
    counts$requests[[key]] <- (counts$requests[[key]] %||% 0L) + 1L

    if (!is.null(body)) {
      counts$post_bodies[[length(counts$post_bodies) + 1L]] <- body
    }

    list(path = path, total = counts$total)
  }

  too_many_requests <- function(url) {
    mock_stac_response(
      url = url,
      status_code = 429L,
      headers = list(`Retry-After` = "2"),
      payload = list(description = "Too Many Requests")
    )
  }

  landing_page <- list(
    type = "Catalog",
    id = "mock-stac",
    stac_version = "1.0.0",
    conformsTo = list(
      "https://api.stacspec.org/v1.0.0/core",
      "https://api.stacspec.org/v1.0.0/item-search#filter",
      "http://www.opengis.net/spec/cql2/1.0/conf/cql2-json"
    ),
    links = list(
      list(
        rel = "service-desc",
        type = "application/vnd.oai.openapi+json;version=3.0",
        href = api_url
      ),
      list(
        rel = "search",
        href = search_url,
        method = "POST"
      )
    )
  )

  openapi_spec <- list(
    openapi = "3.0.3",
    info = list(title = "Mock STAC API", version = "1.0.0"),
    components = list(
      schemas = list(
        ItemProperties = list(
          properties = list(
            `product:type` = list(anyOf = list(list(type = "string"))),
            keywords = list(anyOf = list(list(type = "array")))
          )
        )
      )
    )
  )

  items_response <- list(
    type = "FeatureCollection",
    stac_version = "1.0.0",
    features = list(
      list(
        type = "Feature",
        stac_version = "1.0.0",
        id = "S2A_TEST_ITEM",
        geometry = NULL,
        properties = list(`product:type` = "S2MSI2A"),
        assets = list(),
        links = list()
      )
    ),
    links = list()
  )

  make_get_request <- function(url, ..., headers = NULL, error_msg = NULL) {
    req <- count_request("GET", url)

    if (req$total > rate_limit_after) {
      return(too_many_requests(url))
    }

    switch(req$path,
      "/v1/" = mock_stac_response(url, landing_page),
      "/v1/stac" = mock_stac_response(url, list(description = "Not Found"), status_code = 404L),
      mock_stac_response(url, list(description = "Not Found"), status_code = 404L)
    )
  }

  make_post_request <- function(url, ..., body, encode = c("json", "multipart", "form"), headers = NULL, error_msg = NULL) {
    req <- count_request("POST", url, body = body)

    if (req$total > rate_limit_after) {
      return(too_many_requests(url))
    }

    switch(req$path,
      "/v1/search" = mock_stac_response(url, items_response),
      mock_stac_response(url, list(description = "Not Found"), status_code = 404L)
    )
  }

  link_open <- function(link, base_url = NULL) {
    url <- link$href
    req <- count_request("GET", url)

    if (req$total > rate_limit_after) {
      stop("HTTP status '429'. Too Many Requests", call. = FALSE)
    }

    switch(req$path,
      "/v1/api" = rstac:::doc_openapi_specification(openapi_spec),
      stop("Not Found", call. = FALSE)
    )
  }

  list(
    url = landing_url,
    counts = function() {
      counts$requests
    },
    post_bodies = function() {
      counts$post_bodies
    },
    make_get_request = make_get_request,
    make_post_request = make_post_request,
    link_open = link_open
  )
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

test_that("get_request(stac()) reuses the version-detection landing page response", {
  mock <- mock_stac_service()

  testthat::local_mocked_bindings(
    make_get_request = mock$make_get_request,
    make_post_request = mock$make_post_request,
    link_open = mock$link_open,
    .package = "rstac"
  )

  res <- rstac::get_request(rstac::stac(mock$url))

  expect_s3_class(res, "doc_catalog")
  expect_equal(res$stac_version, "1.0.0")
  expect_equal(mock$counts()[["GET /v1/"]], 1L)
  expect_null(mock$counts()[["GET /v1/stac"]])
})

test_that("ext_filter retains discovered service metadata for post_request()", {
  mock <- mock_stac_service()

  testthat::local_mocked_bindings(
    make_get_request = mock$make_get_request,
    make_post_request = mock$make_post_request,
    link_open = mock$link_open,
    .package = "rstac"
  )

  query <- rstac::stac(mock$url) |>
    rstac::stac_search(
      collections = "sentinel-2-l2a",
      datetime = "2023-05-01/2023-09-01",
      limit = 1
    ) |>
    rstac::ext_filter(`product:type` == "S2MSI2A")

  expect_equal(query$version, "1.0.0")
  expect_equal(mock$counts()[["GET /v1/"]], 1L)
  expect_equal(mock$counts()[["GET /v1/api"]], 1L)

  res <- rstac::post_request(query)

  expect_s3_class(res, "doc_items")
  expect_equal(length(res$features), 1L)
  expect_equal(res$features[[1]]$properties[["product:type"]], "S2MSI2A")
  expect_equal(mock$counts()[["GET /v1/"]], 1L)
  expect_equal(mock$counts()[["GET /v1/api"]], 1L)
  expect_equal(mock$counts()[["POST /v1/search"]], 1L)
})

test_that("filtered STAC search avoids redundant requests and preserves POST body", {
  mock <- mock_stac_service()

  testthat::local_mocked_bindings(
    make_get_request = mock$make_get_request,
    make_post_request = mock$make_post_request,
    link_open = mock$link_open,
    .package = "rstac"
  )

  query <- rstac::stac(mock$url) |>
    rstac::stac_search(
      collections = "sentinel-2-l2a",
      datetime = "2023-05-01/2023-09-01",
      limit = 1
    ) |>
    rstac::ext_filter(`product:type` == "S2MSI2A")

  expected_query <- query
  expected_query$verb <- "POST"
  expected_query$encode <- "json"
  expected_body <- rstac:::before_request(expected_query)$params

  res <- rstac::post_request(query)

  expect_s3_class(res, "doc_items")
  expect_equal(length(res$features), 1L)
  expect_equal(res$features[[1]]$id, "S2A_TEST_ITEM")

  counts <- mock$counts()
  expect_equal(counts[["GET /v1/"]], 1L)
  expect_equal(counts[["GET /v1/api"]], 1L)
  expect_equal(counts[["POST /v1/search"]], 1L)
  expect_equal(length(counts), 3L)

  post_bodies <- mock$post_bodies()
  expect_length(post_bodies, 1L)
  expect_equal(post_bodies[[1]], expected_body)
})

test_that("filtered STAC search succeeds under a 3-request rate limit", {
  mock <- mock_stac_service(rate_limit_after = 3L)

  testthat::local_mocked_bindings(
    make_get_request = mock$make_get_request,
    make_post_request = mock$make_post_request,
    link_open = mock$link_open,
    .package = "rstac"
  )

  res <- rstac::stac(mock$url) |>
    rstac::stac_search(
      collections = "sentinel-2-l2a",
      datetime = "2023-05-01/2023-09-01",
      limit = 1
    ) |>
    rstac::ext_filter(`product:type` == "S2MSI2A") |>
    rstac::post_request()

  expect_s3_class(res, "doc_items")

  counts <- mock$counts()
  expect_equal(counts[["GET /v1/"]], 1L)
  expect_equal(counts[["GET /v1/api"]], 1L)
  expect_equal(counts[["POST /v1/search"]], 1L)
})
