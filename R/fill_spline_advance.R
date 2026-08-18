#' Advanced spline-based missing value filling
#'
#' Fills missing values in a numeric vector using local spline interpolation
#' and extrapolation with additional controls for trend, oscillation,
#' monotonicity, curvature, and rate of change.
#'
#' @param x A numeric vector containing missing (`NA`) values.
#' @param inside Logical. If `TRUE`, fill missing values between the first
#'   and last observed values.
#' @param outside Logical. If `TRUE`, fill missing values outside the range
#'   of observed values.
#' @param back Logical. If `TRUE`, fill leading missing values by backward
#'   extrapolation. This is used only when `outside = TRUE`.
#' @param method Character value specifying the spline method. Currently
#'   retained for compatibility and future extension.
#' @param n Number of local observations used for prediction. Use `"auto"`
#'   for adaptive selection, or an integer greater than or equal to 2.
#' @param k Numeric value between 0 and 1 controlling the distance weighting
#'   of local observations. Smaller values give more weight to nearby
#'   observations.
#' @param phi Numeric value between 0 and 1 controlling the damping of the
#'   extrapolated trend.
#' @param smooth Numeric value between 0 and 1 controlling the balance between
#'   the model prediction and the previous observed/fitted value.
#' @param monotonic Character value controlling monotonicity. Possible values
#'   are `"auto"`, `"increasing"`, `"decreasing"`, and `"none"`.
#'
#' @details
#' The function uses different strategies depending on the location and
#' structure of missing values.
#'
#' For internal missing values, local observations surrounding each missing
#' position are used for prediction. Linear regression is used when only two
#' observations are available, quadratic regression when three observations
#' are available, and `smooth.spline()` when four or more observations are
#' available.
#'
#' For extrapolation, the function combines local spline prediction with a
#' robust trend estimate. The trend uses the median of pairwise slopes,
#' providing greater resistance to individual unusual observations.
#'
#' Oscillating series are detected from turning points, direction changes,
#' and consistency of the distance between turning points. When an
#' oscillating pattern is detected, previous-cycle information is incorporated
#' into the prediction.
#'
#' Extrapolated predictions are additionally controlled using recent
#' percentage changes, curvature, and optional monotonicity constraints.
#' The extrapolated trend is damped using `phi` so that long-horizon
#' extrapolations do not increase indefinitely at the same rate.
#'
#' @return
#' A numeric vector of the same length as `x`, with missing values filled
#' according to the selected options.
#'
#' @examples
#' # Internal missing values
#' x <- c(10, 12, NA, NA, 20, 22, 24)
#' fill_spline_advance(x)
#'
#' # Trailing missing values
#' x <- c(100, 105, 112, 120, NA, NA, NA)
#' fill_spline_advance(x, outside = TRUE)
#'
#' # Leading and trailing missing values
#' x <- c(NA, NA, 100, 105, 112, 120, NA, NA)
#' fill_spline_advance(
#'   x,
#'   inside = TRUE,
#'   outside = TRUE,
#'   back = TRUE
#' )
#'
#' # Specify the number of local observations
#' x <- c(100, 110, 120, 115, 105, 95, 100,
#'        112, 125, 118, NA, NA, NA)
#' fill_spline_advance(x, n = 6)
#'
#' @seealso
#' [stats::smooth.spline()], [fill_spline1()]
#'
#' @export
fill_spline_advance <- function(
    x,
    inside = TRUE,
    outside = TRUE,
    back = FALSE,
    method = "monoH.FC",
    n = "auto",
    k = 0.6,
    phi = 0.8,
    smooth = 0.7,
    monotonic = "auto"
) {

  # ============================================================
  # BASIC CHECKS
  # ============================================================

  if (length(x) == 0) {
    return(x)
  }

  if (!is.numeric(x)) {
    x <- as.numeric(x)
  }

  len <- length(x)

  if (length(n) != 1 || is.na(n)) {
    stop("n must be 'auto' or a single integer >= 2.")
  }

  if (is.character(n)) {

    if (n != "auto") {
      stop("n must be 'auto' or a single integer >= 2.")
    }

  } else {

    if (!is.numeric(n) ||
        length(n) != 1 ||
        is.na(n) ||
        n < 2) {
      stop("n must be 'auto' or a single integer >= 2.")
    }

    n <- as.integer(n)
  }

  if (!is.numeric(k) ||
      length(k) != 1 ||
      is.na(k) ||
      k <= 0 ||
      k >= 1) {
    stop("k must be greater than 0 and less than 1.")
  }

  if (!is.numeric(phi) ||
      length(phi) != 1 ||
      is.na(phi) ||
      phi <= 0 ||
      phi >= 1) {
    stop("phi must be greater than 0 and less than 1.")
  }

  if (!is.numeric(smooth) ||
      length(smooth) != 1 ||
      is.na(smooth) ||
      smooth < 0 ||
      smooth > 1) {
    stop("smooth must be between 0 and 1.")
  }

  if (!is.character(monotonic) ||
      length(monotonic) != 1 ||
      !monotonic %in% c(
        "auto",
        "increasing",
        "decreasing",
        "none"
      )) {
    stop(
      "monotonic must be one of ",
      "'auto', 'increasing', 'decreasing', or 'none'."
    )
  }

  # ============================================================
  # ORIGINAL DATA
  # ============================================================

  original_x <- x
  y <- x

  original_idx <- which(!is.na(original_x))

  if (length(original_idx) < 2) {
    stop(
      "At least two non-NA values are required ",
      "for spline filling."
    )
  }

  # ============================================================
  # AUTOMATIC N
  # ============================================================

  get_n <- function(m) {

    if (m < 2) {
      return(m)
    }

    if (!identical(n, "auto")) {
      return(min(n, m))
    }

    value <- ceiling(sqrt(m))

    value <- max(3, value)
    value <- min(10, value)

    min(value, m)
  }

  # ============================================================
  # WEIGHTS
  # ============================================================

  get_weights <- function(distance) {

    weights <- (1 - k)^distance

    if (sum(weights) == 0 ||
        any(!is.finite(weights))) {
      weights <- rep(1, length(distance))
    }

    weights / sum(weights)
  }

  # ============================================================
  # ROBUST TREND
  # ============================================================

  robust_trend <- function(idx, values = original_x) {

    idx <- sort(unique(idx))

    if (length(idx) < 2) {
      return(
        list(
          slope = 0,
          intercept = values[idx[1]],
          direction = "flat"
        )
      )
    }

    xx <- idx
    yy <- values[idx]

    valid <- is.finite(xx) & is.finite(yy)

    xx <- xx[valid]
    yy <- yy[valid]

    if (length(xx) < 2) {
      return(
        list(
          slope = 0,
          intercept = mean(yy, na.rm = TRUE),
          direction = "flat"
        )
      )
    }

    slopes <- c()

    for (i in seq_len(length(xx) - 1)) {

      j <- (i + 1):length(xx)

      current_slopes <-
        (yy[j] - yy[i]) /
        (xx[j] - xx[i])

      slopes <- c(
        slopes,
        current_slopes
      )
    }

    slopes <- slopes[
      is.finite(slopes)
    ]

    if (length(slopes) == 0) {
      slope <- 0
    } else {
      slope <- median(slopes)
    }

    intercept <- median(
      yy - slope * xx,
      na.rm = TRUE
    )

    scale_value <- max(
      1,
      median(abs(yy), na.rm = TRUE)
    )

    tolerance <- .Machine$double.eps^0.5 *
      scale_value

    if (slope > tolerance) {
      direction <- "increasing"
    } else if (slope < -tolerance) {
      direction <- "decreasing"
    } else {
      direction <- "flat"
    }

    list(
      slope = slope,
      intercept = intercept,
      direction = direction
    )
  }

  # ============================================================
  # TURNING POINT DETECTION
  # ============================================================

  detect_turning_points <- function(idx) {

    idx <- sort(unique(idx))

    if (length(idx) < 5) {
      return(
        list(
          peaks = integer(0),
          troughs = integer(0)
        )
      )
    }

    values <- original_x[idx]

    d <- diff(values)

    if (length(d) < 3) {
      return(
        list(
          peaks = integer(0),
          troughs = integer(0)
        )
      )
    }

    s <- sign(d)

    for (i in seq_along(s)) {

      if (s[i] == 0) {

        if (i > 1) {
          s[i] <- s[i - 1]
        } else {
          next
        }
      }
    }

    peaks <- c()
    troughs <- c()

    for (i in 2:length(s)) {

      if (s[i - 1] > 0 &&
          s[i] < 0) {

        peaks <- c(
          peaks,
          idx[i]
        )
      }

      if (s[i - 1] < 0 &&
          s[i] > 0) {

        troughs <- c(
          troughs,
          idx[i]
        )
      }
    }

    list(
      peaks = peaks,
      troughs = troughs
    )
  }

  # ============================================================
  # OSCILLATION DETECTION
  # ============================================================

  detect_oscillation <- function(idx) {

    idx <- sort(unique(idx))

    if (length(idx) < 7) {
      return(
        list(
          oscillating = FALSE,
          period = NA_integer_,
          strength = 0,
          peaks = integer(0),
          troughs = integer(0)
        )
      )
    }

    tp <- detect_turning_points(idx)

    peaks <- tp$peaks
    troughs <- tp$troughs

    total_turns <-
      length(peaks) +
      length(troughs)

    if (total_turns < 3) {

      return(
        list(
          oscillating = FALSE,
          period = NA_integer_,
          strength = 0,
          peaks = peaks,
          troughs = troughs
        )
      )
    }

    peak_distances <- if (
      length(peaks) >= 2
    ) {
      diff(peaks)
    } else {
      numeric(0)
    }

    trough_distances <- if (
      length(troughs) >= 2
    ) {
      diff(troughs)
    } else {
      numeric(0)
    }

    distances <- c(
      peak_distances,
      trough_distances
    )

    distances <- distances[
      is.finite(distances) &
        distances > 0
    ]

    if (length(distances) == 0) {

      return(
        list(
          oscillating = FALSE,
          period = NA_integer_,
          strength = 0,
          peaks = peaks,
          troughs = troughs
        )
      )
    }

    period <- round(
      median(distances)
    )

    values <- original_x[idx]
    d <- diff(values)

    signs <- sign(d)
    signs <- signs[signs != 0]

    if (length(signs) < 4) {

      return(
        list(
          oscillating = FALSE,
          period = period,
          strength = 0,
          peaks = peaks,
          troughs = troughs
        )
      )
    }

    direction_changes <- sum(
      signs[-1] != signs[-length(signs)]
    )

    change_ratio <-
      direction_changes /
      max(1, length(signs) - 1)

    if (length(distances) >= 2) {

      period_mad <- mad(
        distances,
        center = median(distances),
        constant = 1
      )

      period_consistency <- 1 /
        (1 + period_mad /
           max(1, median(distances)))

    } else {

      period_consistency <- 0.5
    }

    strength <-
      0.5 * min(
        1,
        change_ratio / 0.35
      ) +
      0.5 * period_consistency

    oscillating <-
      total_turns >= 3 &&
      change_ratio >= 0.25 &&
      strength >= 0.55

    list(
      oscillating = oscillating,
      period = period,
      strength = strength,
      peaks = peaks,
      troughs = troughs
    )
  }

  # ============================================================
  # PATTERN FORECAST
  # ============================================================

  pattern_prediction <- function(
    target,
    idx
  ) {

    osc <- detect_oscillation(idx)

    if (!osc$oscillating ||
        is.na(osc$period) ||
        osc$period < 2) {

      return(NA_real_)
    }

    period <- osc$period

    previous_position <-
      target - period

    if (previous_position < min(idx)) {
      return(NA_real_)
    }

    previous_value <- NA_real_

    if (previous_position %in% idx) {

      previous_value <-
        original_x[
          which(idx == previous_position)
        ]
    }

    if (is.na(previous_value)) {

      available <- idx[
        is.finite(original_x[idx])
      ]

      if (length(available) < 2) {
        return(NA_real_)
      }

      previous_value <- approx(
        x = available,
        y = original_x[available],
        xout = previous_position,
        rule = 2
      )$y
    }

    compare_idx <- idx[
      idx >= min(idx) + period
    ]

    drift <- 0

    if (length(compare_idx) >= 3) {

      old_position <- compare_idx - period

      valid <- old_position %in% idx

      if (any(valid)) {

        new_values <- original_x[
          compare_idx[valid]
        ]

        old_values <- original_x[
          old_position[valid]
        ]

        cycle_drift <-
          new_values - old_values

        cycle_drift <-
          cycle_drift[
            is.finite(cycle_drift)
          ]

        if (length(cycle_drift) > 0) {

          drift <- median(
            cycle_drift
          )
        }
      }
    }

    previous_value + drift
  }

  # ============================================================
  # CHANGE LIMITS
  # ============================================================

  get_change_limits <- function(idx) {

    idx <- sort(unique(idx))

    window <- get_n(length(idx))

    idx <- tail(
      idx,
      window
    )

    values <- original_x[idx]

    if (length(values) < 3) {
      return(
        list(
          lower = -Inf,
          upper = Inf
        )
      )
    }

    previous <- head(
      values,
      -1
    )

    current <- tail(
      values,
      -1
    )

    denominator <- abs(previous)

    changes <- ifelse(
      denominator >
        .Machine$double.eps,
      (current - previous) /
        denominator,
      NA_real_
    )

    changes <- changes[
      is.finite(changes)
    ]

    if (length(changes) < 2) {

      absolute_changes <-
        diff(values)

      absolute_changes <-
        absolute_changes[
          is.finite(absolute_changes)
        ]

      if (length(absolute_changes) < 2) {

        return(
          list(
            lower = -Inf,
            upper = Inf
          )
        )
      }

      center <- median(
        absolute_changes
      )

      spread <- mad(
        absolute_changes,
        center = center,
        constant = 1
      )

      if (spread == 0) {
        spread <- max(
          abs(absolute_changes)
        )
      }

      return(
        list(
          lower = center - 2 * spread,
          upper = center + 2 * spread
        )
      )
    }

    center <- median(
      changes
    )

    spread <- mad(
      changes,
      center = center,
      constant = 1
    )

    if (!is.finite(spread) ||
        spread == 0) {

      lower <- min(changes)
      upper <- max(changes)

    } else {

      lower <- center -
        2 * spread

      upper <- center +
        2 * spread
    }

    list(
      lower = lower,
      upper = upper
    )
  }

  # ============================================================
  # CURVATURE LIMIT
  # ============================================================

  get_curvature_limit <- function(idx) {

    idx <- sort(unique(idx))

    window <- get_n(length(idx))

    idx <- tail(
      idx,
      window
    )

    values <- original_x[idx]

    if (length(values) < 3) {
      return(Inf)
    }

    second_diff <- diff(
      values,
      differences = 2
    )

    second_diff <- second_diff[
      is.finite(second_diff)
    ]

    if (length(second_diff) == 0) {
      return(Inf)
    }

    center <- median(
      second_diff
    )

    spread <- mad(
      second_diff,
      center = center,
      constant = 1
    )

    if (!is.finite(spread) ||
        spread == 0) {

      limit <- max(
        abs(second_diff)
      )

    } else {

      limit <-
        abs(center) +
        2 * spread
    }

    if (!is.finite(limit) ||
        limit <= 0) {

      return(Inf)
    }

    limit
  }

  # ============================================================
  # LOCAL SPLINE PREDICTION
  # ============================================================

  predict_local <- function(
    use_idx,
    target,
    source_y = y
  ) {

    use_idx <- sort(
      unique(use_idx)
    )

    m <- length(use_idx)

    if (m < 2) {
      return(NA_real_)
    }

    distance <- abs(
      use_idx - target
    )

    weights <- get_weights(
      distance
    )

    if (m == 2) {

      fit <- stats::lm(
        source_y[use_idx] ~ use_idx,
        weights = weights
      )

      return(
        as.numeric(
          stats::predict(
            fit,
            newdata = data.frame(
              use_idx = target
            )
          )
        )
      )
    }

    if (m == 3) {

      fit <- stats::lm(
        source_y[use_idx] ~
          use_idx +
          I(use_idx^2),
        weights = weights
      )

      return(
        as.numeric(
          stats::predict(
            fit,
            newdata = data.frame(
              use_idx = target
            )
          )
        )
      )
    }

    fit <- tryCatch(
      stats::smooth.spline(
        x = use_idx,
        y = source_y[use_idx],
        w = weights
      ),
      error = function(e) NULL
    )

    if (is.null(fit)) {

      fit <- stats::lm(
        source_y[use_idx] ~ use_idx,
        weights = weights
      )

      return(
        as.numeric(
          stats::predict(
            fit,
            newdata = data.frame(
              use_idx = target
            )
          )
        )
      )
    }

    as.numeric(
      stats::predict(
        fit,
        x = target
      )$y
    )
  }

  # ============================================================
  # MONOTONICITY
  # ============================================================

  apply_monotonic <- function(
    prediction,
    previous,
    direction
  ) {

    if (!is.finite(prediction) ||
        !is.finite(previous)) {

      return(prediction)
    }

    if (direction == "increasing") {

      prediction <- max(
        prediction,
        previous
      )

    } else if (direction == "decreasing") {

      prediction <- min(
        prediction,
        previous
      )
    }

    prediction
  }

  # ============================================================
  # CURVATURE CONTROL
  # ============================================================

  apply_curvature <- function(
    prediction,
    previous,
    previous2,
    limit
  ) {

    if (!is.finite(limit) ||
        !is.finite(previous) ||
        !is.finite(previous2) ||
        !is.finite(prediction)) {

      return(prediction)
    }

    curvature <-
      prediction -
      2 * previous +
      previous2

    curvature <- max(
      -limit,
      min(
        curvature,
        limit
      )
    )

    previous +
      (previous - previous2) +
      curvature
  }

  # ============================================================
  # CHANGE CONTROL
  # ============================================================

  apply_change_limit <- function(
    prediction,
    previous,
    limits
  ) {

    if (!is.finite(prediction) ||
        !is.finite(previous)) {

      return(prediction)
    }

    if (
      abs(previous) <=
      .Machine$double.eps
    ) {
      return(prediction)
    }

    change <-
      (prediction - previous) /
      abs(previous)

    if (!is.finite(change)) {
      return(prediction)
    }

    change <- max(
      limits$lower,
      min(
        change,
        limits$upper
      )
    )

    previous +
      abs(previous) * change
  }

  # ============================================================
  # CONTROL PREDICTION
  # ============================================================

  control_prediction <- function(
    prediction,
    previous,
    previous2,
    idx,
    use_monotonic = TRUE
  ) {

    if (!is.finite(prediction)) {
      return(previous)
    }

    limits <- get_change_limits(
      idx
    )

    prediction <- apply_change_limit(
      prediction,
      previous,
      limits
    )

    curvature_limit <-
      get_curvature_limit(idx)

    prediction <- apply_curvature(
      prediction,
      previous,
      previous2,
      curvature_limit
    )

    if (use_monotonic) {

      trend <- robust_trend(
        idx
      )

      prediction <- apply_monotonic(
        prediction,
        previous,
        trend$direction
      )
    }

    prediction <-
      smooth * prediction +
      (1 - smooth) * previous

    prediction
  }

  # ============================================================
  # INSIDE MISSING VALUES
  # ============================================================

  if (inside) {

    repeat {

      available <- which(
        !is.na(y)
      )

      if (length(available) < 2) {
        break
      }

      first_idx <- min(
        available
      )

      last_idx <- max(
        available
      )

      na_inside <- which(
        is.na(y) &
          seq_len(len) > first_idx &
          seq_len(len) < last_idx
      )

      if (length(na_inside) == 0) {
        break
      }

      for (i in na_inside) {

        available <- which(
          !is.na(y)
        )

        previous <- available[
          available < i
        ]

        future <- available[
          available > i
        ]

        candidates <- c(
          previous,
          future
        )

        if (length(candidates) < 2) {
          next
        }

        candidates <- candidates[
          order(
            abs(candidates - i)
          )
        ]

        k_points <- min(
          get_n(
            length(candidates)
          ),
          length(candidates)
        )

        use_idx <- candidates[
          seq_len(k_points)
        ]

        y[i] <- predict_local(
          use_idx,
          i,
          source_y = y
        )
      }
    }
  }

  # ============================================================
  # OUTSIDE MISSING VALUES
  # ============================================================

  if (outside) {

    # ----------------------------------------------------------
    # LEADING VALUES
    # ----------------------------------------------------------

    if (back) {

      first_idx <- which(
        !is.na(y)
      )[1]

      if (!is.na(first_idx) &&
          first_idx > 1) {

        for (
          i in seq(
            first_idx - 1,
            1,
            by = -1
          )
        ) {

          available <- which(
            !is.na(y)
          )

          future <- available[
            available > i
          ]

          if (length(future) < 2) {
            break
          }

          k_points <- min(
            get_n(
              length(future)
            ),
            length(future)
          )

          use_idx <- head(
            future,
            k_points
          )

          prediction <- predict_local(
            use_idx,
            i,
            source_y = y
          )

          previous <- y[i + 1]

          previous2 <- if (
            i + 2 <= len
          ) {
            y[i + 2]
          } else {
            NA_real_
          }

          prediction <- control_prediction(
            prediction,
            previous,
            previous2,
            use_idx,
            use_monotonic =
              monotonic != "none"
          )

          y[i] <- prediction
        }
      }
    }

    # ----------------------------------------------------------
    # TRAILING VALUES
    # ----------------------------------------------------------

    last_idx <- tail(
      which(!is.na(y)),
      1
    )

    if (
      length(last_idx) > 0 &&
      !is.na(last_idx) &&
      last_idx < len
    ) {

      for (
        i in seq(
          last_idx + 1,
          len,
          by = 1
        )
      ) {

        # Original observed values
        trend_idx <- original_idx[
          original_idx < i
        ]

        if (length(trend_idx) < 2) {
          break
        }

        trend_n <- get_n(
          length(trend_idx)
        )

        trend_idx <- tail(
          trend_idx,
          trend_n
        )

        # Oscillation detection
        oscillation <-
          detect_oscillation(
            original_idx[
              original_idx < i
            ]
          )

        # Robust trend
        trend <- robust_trend(
          trend_idx
        )

        # Local spline
        available <- which(
          !is.na(y)
        )

        previous_idx <- available[
          available < i
        ]

        if (length(previous_idx) < 2) {
          break
        }

        k_points <- min(
          get_n(
            length(previous_idx)
          ),
          length(previous_idx)
        )

        use_idx <- tail(
          previous_idx,
          k_points
        )

        spline_prediction <- predict_local(
          use_idx,
          i,
          source_y = y
        )

        # Damped trend
        last_original <-
          tail(
            trend_idx,
            1
          )

        last_value <-
          original_x[
            last_original
          ]

        h <-
          i - last_original

        damp_factor <-
          (1 - phi^h) /
          (1 - phi)

        trend_prediction <-
          last_value +
          trend$slope *
          damp_factor

        # Oscillating case
        if (oscillation$oscillating) {

          pattern_prediction_value <-
            pattern_prediction(
              target = i,
              idx = original_idx[
                original_idx < i
              ]
            )

          if (is.finite(
            pattern_prediction_value
          )) {

            prediction <-
              0.70 *
              pattern_prediction_value +
              0.30 *
              spline_prediction

          } else {

            prediction <-
              0.50 *
              trend_prediction +
              0.50 *
              spline_prediction
          }

          use_monotonic <- FALSE

        } else {

          prediction <-
            smooth *
            trend_prediction +
            (1 - smooth) *
            spline_prediction

          use_monotonic <-
            monotonic != "none"
        }

        previous <- y[i - 1]

        previous2 <- if (
          i >= 3
        ) {
          y[i - 2]
        } else {
          NA_real_
        }

        prediction <- control_prediction(
          prediction,
          previous,
          previous2,
          trend_idx,
          use_monotonic =
            use_monotonic
        )

        y[i] <- prediction
      }
    }
  }

  # ============================================================
  # RETURN
  # ============================================================

  y
}
