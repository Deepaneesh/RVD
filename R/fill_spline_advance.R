#' Advanced spline interpolation and extrapolation for missing values
#'
#' Fills missing values in a numeric vector using local spline interpolation
#' for internal missing values and robust extrapolation for leading and
#' trailing missing values. The function can optionally control the direction
#' of the filled values using monotonicity constraints and detect oscillating
#' patterns for forward extrapolation.
#'
#' @param x A numeric vector containing observed and missing (`NA`) values.
#' @param inside Logical. Should missing values occurring between observed
#'   values be interpolated? Default is `TRUE`.
#' @param outside Logical. Should missing values outside the observed range
#'   be extrapolated? Default is `TRUE`.
#' @param back Logical. Should leading missing values be filled using backward
#'   extrapolation? Default is `FALSE`.
#' @param method Character string specifying the spline method. Default is
#'   `"monoH.FC"`. Currently retained for compatibility with the function
#'   interface.
#' @param n Either `"auto"` or a single numeric value greater than or equal
#'   to 2. Controls the number of observations used for local calculations.
#'   When `"auto"`, the value is selected automatically based on the number
#'   of available observations.
#' @param k Numeric value between 0 and 1 controlling the distance-based
#'   weighting of observations. Larger values give greater weight to nearby
#'   observations.
#' @param phi Numeric value between 0 and 1 controlling the extrapolation
#'   behaviour. Default is `0.8`.
#' @param smooth Numeric value between 0 and 1 controlling the amount of
#'   smoothing applied to extrapolated predictions. A value of 1 uses the
#'   prediction completely, while lower values shrink the prediction toward
#'   the previous observed or filled value.
#' @param monotonicity_inside Character string specifying the monotonicity
#'   constraint for internal interpolation. Must be one of `"auto"`,
#'   `"increasing"`, `"decreasing"`, or `"none"`.
#' @param monotonicity_left Character string specifying the monotonicity
#'   constraint for leading-value extrapolation. Must be one of `"auto"`,
#'   `"increasing"`, `"decreasing"`, or `"none"`.
#' @param monotonicity_right Character string specifying the monotonicity
#'   constraint for trailing-value extrapolation. Must be one of `"auto"`,
#'   `"increasing"`, `"decreasing"`, or `"none"`.
#'
#' @return A numeric vector with missing values filled according to the
#'   selected interpolation and extrapolation options.
#'
#' @details
#' Internal missing values are filled using a local weighted regression or
#' smoothing spline depending on the number of available neighbouring
#' observations. Two observations use weighted linear regression, three
#' observations use weighted quadratic regression, and larger sets use
#' `stats::smooth.spline()` with a fallback to weighted linear regression.
#'
#' Leading and trailing missing values are handled using robust trend-based
#' extrapolation rather than extrapolating a smoothing spline outside its
#' observed range.
#'
#' The robust trend is estimated using the median of pairwise slopes. Change
#' limits and curvature limits are applied during extrapolation to reduce
#' unrealistic jumps and curvature.
#'
#' When the observed series exhibits an oscillating pattern, the function
#' attempts to use the detected period and cycle drift to predict future
#' values.
#'
#' Monotonicity can be automatically detected or explicitly specified using
#' the `monotonicity_inside`, `monotonicity_left`, and
#' `monotonicity_right` arguments.
#'
#' @examples
#' # Internal missing values
#' x <- c(10, 12, NA, NA, 20, 25, 28)
#' fill_spline_advance(x)
#'
#' # Leading missing values
#' x <- c(NA, NA, NA, 10, 15, 20, 25)
#' fill_spline_advance(
#'   x,
#'   back = TRUE,
#'   monotonicity_left = "increasing"
#' )
#'
#' # Trailing missing values
#' x <- c(10, 15, 20, 25, NA, NA)
#' fill_spline_advance(
#'   x,
#'   monotonicity_right = "increasing"
#' )
#'
#' # Fill only internal missing values
#' x <- c(NA, 10, 15, NA, 25, 30, NA)
#' fill_spline_advance(
#'   x,
#'   inside = TRUE,
#'   outside = FALSE
#' )
#'
#' @export
#'
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
    monotonicity_inside = "auto",
    monotonicity_left = "auto",
    monotonicity_right = "auto"
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

  # ============================================================
  # MONOTONICITY VALIDATION
  # ============================================================

  monotonicity_values <- c(
    "auto",
    "increasing",
    "decreasing",
    "none"
  )

  validate_monotonicity <- function(
    value,
    argument
  ) {

    if (!is.character(value) ||
        length(value) != 1 ||
        is.na(value) ||
        !value %in% monotonicity_values) {

      stop(
        argument,
        " must be one of ",
        "'auto', 'increasing', 'decreasing', or 'none'."
      )
    }
  }

  validate_monotonicity(
    monotonicity_inside,
    "monotonicity_inside"
  )

  validate_monotonicity(
    monotonicity_left,
    "monotonicity_left"
  )

  validate_monotonicity(
    monotonicity_right,
    "monotonicity_right"
  )

  # ============================================================
  # ORIGINAL DATA
  # ============================================================

  original_x <- x
  y <- x

  original_idx <- which(
    !is.na(original_x)
  )

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

    value <- ceiling(
      sqrt(m)
    )

    value <- max(
      3,
      value
    )

    value <- min(
      10,
      value
    )

    min(
      value,
      m
    )
  }

  # ============================================================
  # WEIGHTS
  # ============================================================

  get_weights <- function(distance) {

    weights <- (1 - k)^distance

    if (
      sum(weights) == 0 ||
      any(!is.finite(weights))
    ) {

      weights <- rep(
        1,
        length(distance)
      )
    }

    weights / sum(weights)
  }

  # ============================================================
  # ROBUST TREND
  # ============================================================

  robust_trend <- function(
    idx,
    values = original_x
  ) {

    idx <- sort(
      unique(idx)
    )

    idx <- idx[
      is.finite(values[idx])
    ]

    if (length(idx) < 2) {

      return(
        list(
          slope = 0,
          intercept = if (
            length(idx) == 1
          ) {
            values[idx]
          } else {
            NA_real_
          },
          direction = "flat"
        )
      )
    }

    xx <- idx
    yy <- values[idx]

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

      slope <- median(
        slopes
      )
    }

    intercept <- median(
      yy - slope * xx,
      na.rm = TRUE
    )

    scale_value <- max(
      1,
      median(
        abs(yy),
        na.rm = TRUE
      )
    )

    tolerance <-
      .Machine$double.eps^0.5 *
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
  # GET MONOTONICITY DIRECTION
  # ============================================================

  get_monotonic_direction <- function(
    idx,
    monotonicity,
    values = original_x
  ) {

    if (monotonicity == "none") {
      return("none")
    }

    if (monotonicity == "increasing") {
      return("increasing")
    }

    if (monotonicity == "decreasing") {
      return("decreasing")
    }

    if (monotonicity == "auto") {

      trend <- robust_trend(
        idx,
        values
      )

      return(
        trend$direction
      )
    }

    "none"
  }

  # ============================================================
  # LOCAL SPLINE / INTERPOLATION
  # ============================================================

  predict_local <- function(
    use_idx,
    target,
    source_y = y
  ) {

    use_idx <- sort(
      unique(use_idx)
    )

    use_idx <- use_idx[
      is.finite(source_y[use_idx])
    ]

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
  # ROBUST EXTRAPOLATION
  # ============================================================

  extrapolate_value <- function(
    idx,
    target,
    values = original_x
  ) {

    idx <- sort(
      unique(idx)
    )

    idx <- idx[
      is.finite(values[idx])
    ]

    if (length(idx) < 2) {
      return(NA_real_)
    }

    trend <- robust_trend(
      idx,
      values
    )

    trend$intercept +
      trend$slope * target
  }

  # ============================================================
  # MONOTONICITY CONTROL
  # ============================================================

  apply_monotonic <- function(
    prediction,
    boundary,
    direction,
    side = c(
      "left",
      "right"
    )
  ) {

    side <- match.arg(
      side
    )

    if (!is.finite(prediction) ||
        !is.finite(boundary)) {

      return(prediction)
    }

    if (side == "right") {

      if (direction == "increasing") {

        prediction <- max(
          prediction,
          boundary
        )

      } else if (
        direction == "decreasing"
      ) {

        prediction <- min(
          prediction,
          boundary
        )
      }
    }

    if (side == "left") {

      if (direction == "increasing") {

        prediction <- min(
          prediction,
          boundary
        )

      } else if (
        direction == "decreasing"
      ) {

        prediction <- max(
          prediction,
          boundary
        )
      }
    }

    prediction
  }

  # ============================================================
  # INSIDE MONOTONICITY
  # ============================================================

  apply_monotonic_inside <- function(
    prediction,
    left_value,
    right_value,
    direction
  ) {

    if (!is.finite(prediction)) {
      return(prediction)
    }

    if (!is.finite(left_value) ||
        !is.finite(right_value)) {

      return(prediction)
    }

    if (direction == "increasing") {

      lower <- min(
        left_value,
        right_value
      )

      upper <- max(
        left_value,
        right_value
      )

      prediction <- max(
        prediction,
        lower
      )

      prediction <- min(
        prediction,
        upper
      )

    } else if (
      direction == "decreasing"
    ) {

      upper <- max(
        left_value,
        right_value
      )

      lower <- min(
        left_value,
        right_value
      )

      prediction <- min(
        prediction,
        upper
      )

      prediction <- max(
        prediction,
        lower
      )
    }

    prediction
  }

  # ============================================================
  # CHANGE LIMITS
  # ============================================================

  get_change_limits <- function(idx) {

    idx <- sort(
      unique(idx)
    )

    idx <- idx[
      is.finite(original_x[idx])
    ]

    window <- get_n(
      length(idx)
    )

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

    denominator <- abs(
      previous
    )

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

      if (!is.finite(spread) ||
          spread == 0) {

        spread <- max(
          abs(absolute_changes)
        )
      }

      return(
        list(
          lower =
            center - 2 * spread,

          upper =
            center + 2 * spread
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

      lower <- min(
        changes
      )

      upper <- max(
        changes
      )

    } else {

      lower <-
        center - 2 * spread

      upper <-
        center + 2 * spread
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

    idx <- sort(
      unique(idx)
    )

    idx <- idx[
      is.finite(original_x[idx])
    ]

    window <- get_n(
      length(idx)
    )

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
  # TURNING POINT DETECTION
  # ============================================================

  detect_turning_points <- function(idx) {

    idx <- sort(
      unique(idx)
    )

    idx <- idx[
      is.finite(original_x[idx])
    ]

    if (length(idx) < 5) {

      return(
        list(
          peaks = integer(0),
          troughs = integer(0)
        )
      )
    }

    values <- original_x[idx]

    d <- diff(
      values
    )

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
        }
      }
    }

    peaks <- c()
    troughs <- c()

    for (i in 2:length(s)) {

      if (
        s[i - 1] > 0 &&
        s[i] < 0
      ) {

        peaks <- c(
          peaks,
          idx[i]
        )
      }

      if (
        s[i - 1] < 0 &&
        s[i] > 0
      ) {

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

    idx <- sort(
      unique(idx)
    )

    idx <- idx[
      is.finite(original_x[idx])
    ]

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

    tp <- detect_turning_points(
      idx
    )

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

    d <- diff(
      values
    )

    signs <- sign(d)

    signs <- signs[
      signs != 0
    ]

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
      signs[-1] !=
        signs[-length(signs)]
    )

    change_ratio <-
      direction_changes /
      max(
        1,
        length(signs) - 1
      )

    if (length(distances) >= 2) {

      period_mad <- mad(
        distances,
        center = median(distances),
        constant = 1
      )

      period_consistency <- 1 /
        (
          1 +
            period_mad /
            max(
              1,
              median(distances)
            )
        )

    } else {

      period_consistency <- 0.5
    }

    strength <-
      0.5 *
      min(
        1,
        change_ratio / 0.35
      ) +
      0.5 *
      period_consistency

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
  # PATTERN PREDICTION
  # ============================================================

  pattern_prediction <- function(
    target,
    idx
  ) {

    osc <- detect_oscillation(
      idx
    )

    if (
      !osc$oscillating ||
      is.na(osc$period) ||
      osc$period < 2
    ) {

      return(NA_real_)
    }

    period <- osc$period

    previous_position <-
      target - period

    if (
      previous_position <
      min(idx)
    ) {

      return(NA_real_)
    }

    previous_value <- NA_real_

    if (
      previous_position %in% idx
    ) {

      previous_value <-
        original_x[
          which(
            idx ==
              previous_position
          )
        ]
    }

    if (is.na(previous_value)) {

      available <- idx[
        is.finite(
          original_x[idx]
        )
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
      idx >=
        min(idx) + period
    ]

    drift <- 0

    if (length(compare_idx) >= 3) {

      old_position <-
        compare_idx - period

      valid <-
        old_position %in% idx

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

        if (
          length(cycle_drift) > 0
        ) {

          drift <- median(
            cycle_drift
          )
        }
      }
    }

    previous_value + drift
  }

  # ============================================================
  # CONTROL PREDICTION
  # ============================================================

  control_prediction <- function(
    prediction,
    previous,
    previous2,
    idx,
    monotonicity = "none",
    side = "right"
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
      get_curvature_limit(
        idx
      )

    prediction <- apply_curvature(
      prediction,
      previous,
      previous2,
      curvature_limit
    )

    if (
      monotonicity != "none"
    ) {

      direction <-
        get_monotonic_direction(
          idx,
          monotonicity
        )

      prediction <-
        apply_monotonic(
          prediction,
          previous,
          direction,
          side = side
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

      if (
        length(na_inside) == 0
      ) {

        break
      }

      for (i in na_inside) {

        available <- which(
          !is.na(y)
        )

        previous_idx <- available[
          available < i
        ]

        future_idx <- available[
          available > i
        ]

        if (
          length(previous_idx) == 0 ||
          length(future_idx) == 0
        ) {

          next
        }

        candidates <- c(
          previous_idx,
          future_idx
        )

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

        prediction <- predict_local(
          use_idx,
          i,
          source_y = y
        )

        if (
          monotonicity_inside !=
          "none"
        ) {

          left_idx <- tail(
            previous_idx,
            1
          )

          right_idx <- head(
            future_idx,
            1
          )

          direction <-
            get_monotonic_direction(
              c(
                left_idx,
                right_idx
              ),
              monotonicity_inside
            )

          prediction <-
            apply_monotonic_inside(
              prediction,
              y[left_idx],
              y[right_idx],
              direction
            )
        }

        y[i] <- prediction
      }
    }
  }

  # ============================================================
  # OUTSIDE MISSING VALUES
  # ============================================================

  if (outside) {

    # ==========================================================
    # LEFT / LEADING VALUES
    # ==========================================================

    if (back) {

      first_idx <- which(
        !is.na(y)
      )[1]

      if (
        !is.na(first_idx) &&
        first_idx > 1
      ) {

        first_observed <- original_idx[
          original_idx >= first_idx
        ]

        k_points <- min(
          get_n(
            length(first_observed)
          ),
          length(first_observed)
        )

        use_idx <- head(
          first_observed,
          k_points
        )

        direction <-
          get_monotonic_direction(
            use_idx,
            monotonicity_left
          )

        for (
          i in seq(
            first_idx - 1,
            1,
            by = -1
          )
        ) {

          prediction <-
            extrapolate_value(
              use_idx,
              i,
              original_x
            )

          boundary <- y[i + 1]

          if (
            monotonicity_left !=
            "none"
          ) {

            prediction <-
              apply_monotonic(
                prediction,
                boundary,
                direction,
                side = "left"
              )
          }

          y[i] <- prediction
        }
      }
    }

    # ==========================================================
    # RIGHT / TRAILING VALUES
    # ==========================================================

    last_idx <- tail(
      which(!is.na(y)),
      1
    )

    if (
      length(last_idx) > 0 &&
      !is.na(last_idx) &&
      last_idx < len
    ) {

      trend_idx <- original_idx[
        original_idx < len + 1
      ]

      trend_n <- get_n(
        length(trend_idx)
      )

      trend_idx <- tail(
        trend_idx,
        trend_n
      )

      direction <-
        get_monotonic_direction(
          trend_idx,
          monotonicity_right
        )

      for (
        i in seq(
          last_idx + 1,
          len,
          by = 1
        )
      ) {

        oscillation <-
          detect_oscillation(
            original_idx[
              original_idx < i
            ]
          )

        if (
          oscillation$oscillating
        ) {

          pattern_value <-
            pattern_prediction(
              target = i,
              idx = original_idx[
                original_idx < i
              ]
            )

          if (
            is.finite(
              pattern_value
            )
          ) {

            prediction <-
              pattern_value

          } else {

            prediction <-
              extrapolate_value(
                trend_idx,
                i,
                original_x
              )
          }

          monotonicity_to_use <-
            "none"

        } else {

          prediction <-
            extrapolate_value(
              trend_idx,
              i,
              original_x
            )

          monotonicity_to_use <-
            monotonicity_right
        }

        previous <- y[
          i - 1
        ]

        if (
          monotonicity_to_use !=
          "none"
        ) {

          prediction <-
            apply_monotonic(
              prediction,
              previous,
              direction,
              side = "right"
            )
        }

        y[i] <- prediction
      }
    }
  }

  y
}
