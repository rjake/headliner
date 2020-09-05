build_phrase <- function(x, ...) UseMethod("build_phrase")


build_phrase.default <- function(compare,
                                 reference,
                                 calc = c("value", "prop"),
                                 phrasing = headliner::phrase_terms()
                                 ) {
  # calc = "value"; compare = b$cond_1_y_mean; reference = b$cond_2_y_mean; phrasing = terms()

  calc <- match.arg(calc)

  if (calc == "value") {
    res <- compare - reference
  } else {
    res <- (compare - reference) / reference
  }

  sign_res <- sign(res)

  phrase <-
    switch(
      as.character(sign_res), # must be a character
      "1" = phrasing$more,
      "-1" = phrasing$less,
      "0" = phrasing$same
    )


  list(
    delta = abs(res),
    phrase = phrase,
    comp_value = compare,
    ref_value = reference,
    raw_delta = res,
    sign = sign_res,
    calc = calc,
    expr_comp = deparse(match.call()[["compare"]]),
    expr_ref = deparse(match.call()[["reference"]])
  )
}


build_phrase.list <- function(x, compare, reference, ...) {
  comp <- x[[deparse(match.call()[["compare"]])]]
  ref <- x[[deparse(match.call()[["reference"]])]]

  build_phrase(comp, ref, ...)
}


view_components <- function(x) {
  data.frame(
    VALUES = unlist(x)
  )
}

phrase_terms <- function(more = "increase",
                         less = "decrease",
                         same = "difference") {
  list(
    more = more,
    less = less,
    same = same
  )
}

