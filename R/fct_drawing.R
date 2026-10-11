#' Pitch drawings from coordinates
#'
#' A drawing (tegning) is a small JSON document with one or more sketches
#' side by side. Coordinates are in metres: x from 0 to `bredde` (left to
#' right), y from 0 to `lengde` (bottom to top). Everything outside the field
#' up to `marg` metres is grass, used for titles and captions.
#'
#' ```json
#' {"skisser": [{
#'   "tittel": "A: Alltid to alternativer",
#'   "bane": {"bredde": 12, "lengde": 12, "marg": 2.5, "midtlinje": false},
#'   "soner": [{"x": 0, "y": 0, "b": 4, "h": 22, "tekst": "Endesone 4 m"}],
#'   "linjer": [{"fra": [16, 0], "til": [16, 22]}],
#'   "hjelpelinjer": [{"fra": [19.7, 0], "til": [19.7, 22], "tekst": "ball-linjen"}],
#'   "objekter": [
#'     {"type": "spiller", "lag": 1, "x": 1.5, "y": 1.2, "tekst": "1"},
#'     {"type": "trener", "x": 30.9, "y": 11},
#'     {"type": "ball", "x": 2.5, "y": 1.9},
#'     {"type": "kjegle", "x": 0, "y": 0, "farge": "gul"},
#'     {"type": "maal", "x": 0, "y": 11, "bredde": 5, "side": "venstre"},
#'     {"type": "smaamaal", "x": 32, "y": 5, "bredde": 2, "side": "hoyre"}],
#'   "piler": [
#'     {"type": "pasning", "fra": [2.5, 1.9], "til": [9.7, 7.2]},
#'     {"type": "lop", "fra": [8.9, 1.1], "til": [10.6, 6.9], "bue": 0.25},
#'     {"type": "foring", "fra": [20, 11], "til": [14.6, 11.1]}],
#'   "etiketter": [{"x": 26.4, "y": 7.5, "tekst": "H sprinter forbi ballen og hjem"}],
#'   "tekster": [{"x": 6, "y": -1.3, "tekst": "Rute 12 × 12 m"}]
#' }]}
#' ```
#'
#' - Players (`lag` 1 = blue, 2 = red, "joker" = yellow) and trainers have up
#'   to 3 characters of text. Trainers default to "T".
#' - `maal` is a full goal, `smaamaal` a small goal; `side` says which side
#'   of the field the goal stands on (venstre, hoyre, topp, bunn).
#' - Arrows: `pasning` is a solid white arrow, `lop` a dashed black arrow,
#'   `foring` a wavy black arrow. `bue` (-1 to 1) bends the arrow; positive
#'   bends to the left of the direction of travel.
#' - `etiketter` are boxed labels on the field; `tekster` are plain white
#'   captions, usually in the margin (y < 0 or y > lengde).
#'
#' The app draws the PNG; a model never writes drawing code. Before drawing,
#' `drawing_check()` finds overlaps and `drawing_fix()` moves what can be
#' moved safely (players apart, labels off players).
#'
#' @name fct_drawing
#' @noRd
NULL

drawing_colours <- list(
  grass = c("#56a644", "#4c9a3a"), line = "#FFFFFF",
  team = c(`1` = "#1f5fbf", `2` = "#d63a2f", joker = "#f2b600"),
  team_text = c(`1` = "#FFFFFF", `2` = "#FFFFFF", joker = "#121212"),
  trainer = "#2b2b2b", cone = c(oransje = "#ff8c1a", gul = "#f5c518"),
  helper = "#f5d63d", label_fill = "#1d4a1d", shadow = "#00000055"
)

drawing_object_types <- c("spiller", "trener", "ball", "kjegle", "maal", "smaamaal")
drawing_arrow_types <- c("pasning", "lop", "foring")
drawing_goal_sides <- c("venstre", "hoyre", "topp", "bunn")

# Sizes of players, text and lines. As in the reference session, a player is
# about 0.85 m in radius, whatever the pitch size, but never smaller than
# 1.8 % or larger than 4 % of the image width (so they stay readable on a
# full pitch and do not swamp a small square). Everything else scales with
# the player. Line widths are in R's lwd units (1/96 inch); at 72 dpi one
# unit is 0.75 pixel, hence 4/3. Font sizes are in points (pixels at 72 dpi).
drawing_sizes <- function(width_px, scale = 0.024 * width_px / 0.85) {
  r <- min(max(0.85 * scale, 0.018 * width_px), 0.04 * width_px)
  u <- r / 0.024
  lw <- 4 / 3
  list(player_r = r, ball_r = 0.0085 * u, cone = 0.022 * u,
       arrow_lwd = 0.0042 * u * lw, head = 0.016 * u, line_lwd = 0.0022 * u * lw,
       font = 0.021 * u, gap = 0.004 * u)
}

# Drawing input ----------------------------------------------------------------------

#' Read a drawing from JSON text (or pass a list through)
#' @noRd
drawing_parse <- function(x) {
  if (is.list(x)) return(x)
  x <- txt1(x)
  if (!nzchar(x)) return(NULL)
  if (nchar(x) > drawing_max_chars) stop("Tegningen er for stor (maks ", drawing_max_chars, " tegn).", call. = FALSE)
  tryCatch(jsonlite::fromJSON(x, simplifyVector = FALSE),
            error = function(e) stop("Tegningen er ikke gyldig JSON. Sjekk komma, klammer og anf\u00f8rselstegn.", call. = FALSE))
}

drawing_num <- function(v, what, lo = -Inf, hi = Inf) {
  v <- suppressWarnings(as.numeric(unlist(v)))
  if (length(v) != 1 || is.na(v) || v < lo || v > hi) {
    stop(what, " må være et tall", if (is.finite(lo)) paste0(" fra ", lo, " til ", hi), ".", call. = FALSE)
  }
  v
}

drawing_point <- function(p, what) {
  v <- suppressWarnings(as.numeric(unlist(p)))
  if (length(v) != 2 || anyNA(v)) stop(what, " må være et punkt [x, y].", call. = FALSE)
  v
}

# Limits, so a drawing cannot keep the server busy (a few times what a rich
# sketch needs; the reference session uses at most 13 objects, 5 arrows,
# 2 zones, 1 label and 3 texts per sketch).
drawing_max_chars <- 20000
drawing_max_items <- c(objekter = 40, piler = 20, soner = 10, linjer = 20, hjelpelinjer = 10, etiketter = 15,
                       tekster = 15)

drawing_text <- function(t, what, max) {
  t <- txt1(t)
  if (nchar(t) > max) stop(what, " kan ha maks ", max, " tegn.", call. = FALSE)
  t
}

#' Check a drawing and return it in a clean, complete form
#'
#' Stops with a Norwegian message saying what is wrong. Coordinates must be
#' within the field plus its margin.
#' @noRd
drawing_validate <- function(d) {
  d <- drawing_parse(d)
  if (is.null(d)) return(NULL)
  sk <- d$skisser
  if (!is.list(sk) || length(sk) < 1 || length(sk) > 3) stop("En tegning må ha 1 til 3 skisser.", call. = FALSE)
  out <- lapply(seq_along(sk), function(i) {
    s <- sk[[i]]
    w <- paste0("Skisse ", i, ": ")
    # Count first, so a huge drawing is stopped before any work is done.
    for (k in names(drawing_max_items)) {
      if (length(s[[k]]) > drawing_max_items[[k]]) {
        stop(w, "Maks ", drawing_max_items[[k]], " ", k, " per skisse.", call. = FALSE)
      }
    }
    b <- s$bane %||% list()
    bane <- list(bredde = drawing_num(b$bredde, paste0(w, "Banens bredde"), 4, 120),
                 lengde = drawing_num(b$lengde, paste0(w, "Banens lengde"), 4, 120),
                 marg = if (is.null(b$marg)) 2.5 else drawing_num(b$marg, paste0(w, "Margen"), 0.5, 10),
                 midtlinje = isTRUE(b$midtlinje))
    lo_x <- -bane$marg; hi_x <- bane$bredde + bane$marg
    lo_y <- -bane$marg; hi_y <- bane$lengde + bane$marg
    inside <- function(x, y, what) {
      if (x < lo_x || x > hi_x || y < lo_y || y > hi_y) stop(w, what, " ligger utenfor tegningen.", call. = FALSE)
    }
    pt <- function(p, what) {
      v <- drawing_point(p, paste0(w, what))
      inside(v[1], v[2], what)
      v
    }
    objs <- lapply(s$objekter %||% list(), function(o) {
      type <- txt1(o$type)
      if (!type %in% drawing_object_types) stop(w, "Ukjent objekttype «", type, "».", call. = FALSE)
      x <- drawing_num(o$x, paste0(w, type, " x")); y <- drawing_num(o$y, paste0(w, type, " y"))
      inside(x, y, type)
      r <- list(type = type, x = x, y = y)
      if (type == "spiller") {
        lag <- txt1(o$lag %||% "1")
        if (!lag %in% names(drawing_colours$team)) stop(w, "Lag må være 1, 2 eller joker.", call. = FALSE)
        r$lag <- lag
      }
      if (type %in% c("spiller", "trener")) {
        r$tekst <- drawing_text(o$tekst %||% if (type == "trener") "T" else "", paste0(w, "Teksten på en spiller"), 3)
      }
      if (type == "kjegle") {
        r$farge <- txt1(o$farge %||% "oransje")
        if (!r$farge %in% names(drawing_colours$cone)) stop(w, "Kjeglefarge må være oransje eller gul.", call. = FALSE)
      }
      if (type %in% c("maal", "smaamaal")) {
        r$bredde <- drawing_num(o$bredde %||% if (type == "maal") 5 else 2, paste0(w, "Målets bredde"), 0.5, 8)
        r$side <- txt1(o$side %||% "venstre")
        if (!r$side %in% drawing_goal_sides) stop(w, "Målets side må være venstre, hoyre, topp eller bunn.", call. = FALSE)
      }
      r
    })
    arrows <- lapply(s$piler %||% list(), function(a) {
      type <- txt1(a$type)
      if (!type %in% drawing_arrow_types) stop(w, "Ukjent piltype «", type, "».", call. = FALSE)
      list(type = type, fra = pt(a$fra, "Pilens start"), til = pt(a$til, "Pilens slutt"),
           bue = if (is.null(a$bue)) 0 else drawing_num(a$bue, paste0(w, "Bue"), -1, 1))
    })
    zones <- lapply(s$soner %||% list(), function(z) {
      r <- list(x = drawing_num(z$x, paste0(w, "Sone x")), y = drawing_num(z$y, paste0(w, "Sone y")),
                b = drawing_num(z$b, paste0(w, "Sonens bredde"), 0.5, 120),
                h = drawing_num(z$h, paste0(w, "Sonens høyde"), 0.5, 120),
                tekst = drawing_text(z$tekst, paste0(w, "Sonetekst"), 40))
      inside(r$x, r$y, "Sonen"); inside(r$x + r$b, r$y + r$h, "Sonen")
      r
    })
    lines <- lapply(s$linjer %||% list(), function(l) list(fra = pt(l$fra, "Linjens start"), til = pt(l$til, "Linjens slutt")))
    helpers <- lapply(s$hjelpelinjer %||% list(), function(l) {
      list(fra = pt(l$fra, "Hjelpelinjens start"), til = pt(l$til, "Hjelpelinjens slutt"),
           tekst = drawing_text(l$tekst, paste0(w, "Hjelpelinjens tekst"), 40))
    })
    labels <- lapply(s$etiketter %||% list(), function(l) {
      x <- drawing_num(l$x, paste0(w, "Etikett x")); y <- drawing_num(l$y, paste0(w, "Etikett y"))
      inside(x, y, "Etiketten")
      list(x = x, y = y, tekst = drawing_text(l$tekst, paste0(w, "Etiketten"), 60))
    })
    texts <- lapply(s$tekster %||% list(), function(l) {
      x <- drawing_num(l$x, paste0(w, "Tekst x")); y <- drawing_num(l$y, paste0(w, "Tekst y"))
      inside(x, y, "Teksten")
      list(x = x, y = y, tekst = drawing_text(l$tekst, paste0(w, "Teksten"), 60))
    })
    list(tittel = drawing_text(s$tittel, paste0(w, "Tittelen"), 50), bane = bane, soner = zones, linjer = lines,
         hjelpelinjer = helpers, objekter = objs, piler = arrows, etiketter = labels, tekster = texts)
  })
  list(skisser = out)
}

# Layout -----------------------------------------------------------------------------

#' Where each sketch goes in the image, and the scale (pixels per metre)
#'
#' All sketches share one scale, so metres look the same side by side.
#' @return list(width, height, scale, sizes, panels = list of list(x0, y0)),
#'   where (x0, y0) is the pixel position of the sketch's world origin
#'   (x = -marg, y = -marg).
#' @noRd
drawing_layout <- function(d, width_px = 1800) {
  w_m <- vapply(d$skisser, function(s) s$bane$bredde + 2 * s$bane$marg, numeric(1))
  h_m <- vapply(d$skisser, function(s) s$bane$lengde + 2 * s$bane$marg, numeric(1))
  scale <- width_px / sum(w_m)
  height <- round(max(h_m) * scale)
  x0 <- cumsum(c(0, w_m[-length(w_m)])) * scale
  panels <- lapply(seq_along(w_m), function(i) list(x0 = x0[i], y0 = (height - h_m[i] * scale) / 2))
  list(width = width_px, height = height, scale = scale, sizes = drawing_sizes(width_px, scale), panels = panels)
}

# Pixel position of a world point in sketch i.
drawing_px <- function(lay, s, i, x, y) {
  p <- lay$panels[[i]]
  cbind(p$x0 + (x + s$bane$marg) * lay$scale, p$y0 + (y + s$bane$marg) * lay$scale)
}

# Round objects that must not overlap: players and trainers.
drawing_people <- function(s) {
  which(vapply(s$objekter, function(o) o$type %in% c("spiller", "trener"), logical(1)))
}

# "Blå spiller F" -> "blå spiller F" (only the first letter).
lower_first <- function(x) paste0(tolower(substr(x, 1, 1)), substring(x, 2))

drawing_person_name <- function(o) {
  t <- if (nzchar(o$tekst %||% "")) paste0(" ", o$tekst) else ""
  if (o$type == "trener") paste0("Trener", t)
  else paste0(c(`1` = "Blå", `2` = "Rød", joker = "Joker")[[o$lag]], " spiller", t)
}

# Points along an arrow (metres): a quadratic curve when `bue` is not 0.
drawing_arrow_path <- function(a, n = 40) {
  p0 <- a$fra; p2 <- a$til
  d <- p2 - p0
  len <- sqrt(sum(d^2))
  if (len == 0) return(rbind(p0, p2))
  normal <- c(-d[2], d[1]) / len
  p1 <- (p0 + p2) / 2 + normal * a$bue * len
  t <- seq(0, 1, length.out = n)
  cbind((1 - t)^2 * p0[1] + 2 * (1 - t) * t * p1[1] + t^2 * p2[1],
        (1 - t)^2 * p0[2] + 2 * (1 - t) * t * p1[2] + t^2 * p2[2])
}

# Shortest distance from point p to a polyline.
dist_to_path <- function(p, path) {
  best <- Inf
  for (k in seq_len(nrow(path) - 1)) {
    a <- path[k, ]; b <- path[k + 1, ]
    ab <- b - a
    l2 <- sum(ab^2)
    t <- if (l2 == 0) 0 else max(0, min(1, sum((p - a) * ab) / l2))
    best <- min(best, sqrt(sum((p - (a + t * ab))^2)))
  }
  best
}

# Size of a label box in metres (approximate text width).
drawing_label_box <- function(l, lay) {
  lines <- strsplit(drawing_wrap(l$tekst), "\n", fixed = TRUE)[[1]]
  f <- lay$sizes$font * 0.72
  list(w = (max(nchar(lines)) * 0.56 * f + 1.4 * f) / lay$scale, h = (length(lines) * 1.25 * f + 0.9 * f) / lay$scale)
}

# Labels wrap at about 18 characters per line.
drawing_wrap <- function(t, width = 18) {
  paste(strwrap(t, width = width), collapse = "\n")
}

#' Find overlaps in a drawing
#'
#' @return data.frame(skisse, type, melding): `type` is "spillere" (two
#'   players overlap), "pil" (an arrow passes through a player who neither
#'   starts nor receives it) or "etikett" (a label covers a player).
#' @noRd
drawing_check <- function(d, width_px = 1800) {
  lay <- drawing_layout(d, width_px)
  r <- lay$sizes$player_r / lay$scale
  rows <- list()
  add <- function(i, type, msg) rows[[length(rows) + 1]] <<- data.frame(skisse = i, type = type, melding = msg)
  for (i in seq_along(d$skisser)) {
    s <- d$skisser[[i]]
    ppl <- drawing_people(s)
    pos <- t(vapply(s$objekter[ppl], function(o) c(o$x, o$y), numeric(2)))
    if (length(ppl) > 1) {
      for (a in seq_along(ppl)[-length(ppl)]) for (b in (a + 1):length(ppl)) {
        if (sqrt(sum((pos[a, ] - pos[b, ])^2)) < 2 * r) {
          add(i, "spillere", paste(drawing_person_name(s$objekter[[ppl[a]]]), "og",
                                    lower_first(drawing_person_name(s$objekter[[ppl[b]]])), "overlapper."))
        }
      }
    }
    for (a in s$piler) {
      path <- drawing_arrow_path(a)
      for (k in seq_along(ppl)) {
        p <- pos[k, ]
        at_end <- sqrt(sum((p - a$fra)^2)) <= 1.6 * r || sqrt(sum((p - a$til)^2)) <= 1.6 * r
        if (!at_end && dist_to_path(p, path) < 0.9 * r) {
          add(i, "pil", paste0("En ", c(pasning = "pasning", lop = "løpsbane", foring = "føring")[[a$type]],
                               " går gjennom ", lower_first(drawing_person_name(s$objekter[[ppl[k]]])), "."))
        }
      }
    }
    for (l in s$etiketter) {
      box <- drawing_label_box(l, lay)
      for (k in seq_along(ppl)) {
        if (abs(pos[k, 1] - l$x) < box$w / 2 + r && abs(pos[k, 2] - l$y) < box$h / 2 + r) {
          add(i, "etikett", paste0("Etiketten «", l$tekst, "» dekker ",
                                   lower_first(drawing_person_name(s$objekter[[ppl[k]]])), "."))
        }
      }
    }
  }
  if (length(rows) == 0) return(data.frame(skisse = integer(), type = character(), melding = character()))
  do.call(rbind, rows)
}

#' Move what can be moved safely: players apart, labels off players
#'
#' Players that overlap are pushed apart along the line between them (a ball
#' at a player's feet moves with the player, and so do arrows that start or
#' end at the player). A label that covers a player is moved up or down to
#' the nearest free spot. Arrows through players are not changed; they are
#' left for `drawing_check()` to report.
#' @noRd
drawing_fix <- function(d, width_px = 1800) {
  lay <- drawing_layout(d, width_px)
  r <- lay$sizes$player_r / lay$scale
  for (i in seq_along(d$skisser)) {
    s <- d$skisser[[i]]
    ppl <- drawing_people(s)
    move <- function(k, dx, dy) {
      o <- s$objekter[[k]]
      old <- c(o$x, o$y)
      new <- c(min(max(o$x + dx, -s$bane$marg + r), s$bane$bredde + s$bane$marg - r),
               min(max(o$y + dy, -s$bane$marg + r), s$bane$lengde + s$bane$marg - r))
      s$objekter[[k]]$x <<- new[1]; s$objekter[[k]]$y <<- new[2]
      shift <- new - old
      for (j in seq_along(s$objekter)) {
        b <- s$objekter[[j]]
        if (b$type == "ball" && sqrt((b$x - old[1])^2 + (b$y - old[2])^2) <= 1.6 * r) {
          s$objekter[[j]]$x <<- b$x + shift[1]; s$objekter[[j]]$y <<- b$y + shift[2]
        }
      }
      for (j in seq_along(s$piler)) {
        if (sqrt(sum((s$piler[[j]]$fra - old)^2)) <= 1.6 * r) s$piler[[j]]$fra <<- s$piler[[j]]$fra + shift
        if (sqrt(sum((s$piler[[j]]$til - old)^2)) <= 1.6 * r) s$piler[[j]]$til <<- s$piler[[j]]$til + shift
      }
    }
    for (round in 1:5) {
      moved <- FALSE
      if (length(ppl) > 1) for (a in seq_along(ppl)[-length(ppl)]) for (b in (a + 1):length(ppl)) {
        pa <- c(s$objekter[[ppl[a]]]$x, s$objekter[[ppl[a]]]$y)
        pb <- c(s$objekter[[ppl[b]]]$x, s$objekter[[ppl[b]]]$y)
        dist <- sqrt(sum((pa - pb)^2))
        if (dist < 2 * r) {
          dir <- if (dist > 1e-6) (pb - pa) / dist else c(1, 0)
          push <- (2.15 * r - dist) / 2
          move(ppl[a], -dir[1] * push, -dir[2] * push)
          move(ppl[b], dir[1] * push, dir[2] * push)
          moved <- TRUE
        }
      }
      if (!moved) break
    }
    pos <- if (length(ppl)) t(vapply(s$objekter[ppl], function(o) c(o$x, o$y), numeric(2))) else matrix(numeric(), 0, 2)
    for (j in seq_along(s$etiketter)) {
      l <- s$etiketter[[j]]
      box <- drawing_label_box(l, lay)
      free <- function(y) !any(abs(pos[, 1] - l$x) < box$w / 2 + r & abs(pos[, 2] - y) < box$h / 2 + r)
      if (nrow(pos) == 0 || free(l$y)) next
      steps <- seq(0.25, s$bane$lengde, by = 0.25)
      cand <- c(rbind(l$y - steps, l$y + steps))
      cand <- cand[cand - box$h / 2 >= -s$bane$marg & cand + box$h / 2 <= s$bane$lengde + s$bane$marg]
      ok <- cand[vapply(cand, free, logical(1))]
      if (length(ok)) s$etiketter[[j]]$y <- ok[1]
    }
    d$skisser[[i]] <- s
  }
  d
}

# Drawing ----------------------------------------------------------------------------

# Arrow path in pixels, trimmed so it starts and ends at the edge of a player
# standing on its end points.
drawing_arrow_px <- function(a, s, i, lay) {
  path <- drawing_arrow_path(a)
  px <- drawing_px(lay, s, i, path[, 1], path[, 2])
  r_px <- lay$sizes$player_r
  ppl <- drawing_people(s)
  near <- function(pt) any(vapply(s$objekter[ppl], function(o) sqrt((o$x - pt[1])^2 + (o$y - pt[2])^2) * lay$scale <= r_px * 1.15,
                                   logical(1)))
  trim <- function(px, amount) {
    seglen <- sqrt(rowSums(diff(px)^2))
    cum <- c(0, cumsum(seglen))
    keep <- cum >= amount
    if (sum(keep) < 2) return(px)
    k <- which(keep)[1]
    t <- (amount - cum[k - 1]) / seglen[k - 1]
    rbind(px[k - 1, ] + t * (px[k, ] - px[k - 1, ]), px[keep, , drop = FALSE])
  }
  if (length(ppl) && near(a$fra)) px <- trim(px, r_px + lay$sizes$gap)
  flip <- function(m) m[nrow(m):1, , drop = FALSE]
  if (length(ppl) && near(a$til)) px <- flip(trim(flip(px), r_px + lay$sizes$gap * 2))
  px
}

drawing_head <- function(px, size, fill) {
  n <- nrow(px)
  end <- px[n, ]
  # Direction from a point a little way back, so curves get a straight head.
  back <- px[max(1, n - 3), ]
  d <- end - back
  d <- d / sqrt(sum(d^2))
  nrm <- c(-d[2], d[1])
  tip <- end
  base <- end - d * size
  grid::polygonGrob(x = c(tip[1], base[1] + nrm[1] * size * 0.45, base[1] - nrm[1] * size * 0.45),
                    y = c(tip[2], base[2] + nrm[2] * size * 0.45, base[2] - nrm[2] * size * 0.45),
                    default.units = "native", gp = grid::gpar(fill = fill, col = fill))
}

# Points along a line with a sine wave on it (for dribbling).
drawing_wave <- function(px, amp, wavelength) {
  seglen <- sqrt(rowSums(diff(px)^2))
  cum <- c(0, cumsum(seglen))
  total <- max(cum)
  stop_at <- total - wavelength * 0.6       # straight bit under the arrow head
  s <- seq(0, stop_at, length.out = max(20, round(stop_at / 2)))
  x <- stats::approx(cum, px[, 1], s)$y
  y <- stats::approx(cum, px[, 2], s)$y
  dx <- c(diff(x), diff(x)[length(diff(x))]); dy <- c(diff(y), diff(y)[length(diff(y))])
  l <- sqrt(dx^2 + dy^2); l[l == 0] <- 1
  off <- amp * sin(2 * pi * s / wavelength)
  rbind(cbind(x - dy / l * off, y + dx / l * off), px[nrow(px), ])
}

drawing_text_box <- function(x, y, label, lay, fill = drawing_colours$label_fill, alpha = 0.85, wrap = 18) {
  f <- lay$sizes$font * 0.72
  label <- drawing_wrap(label, wrap)
  n <- length(strsplit(label, "\n", fixed = TRUE)[[1]])
  w <- max(nchar(strsplit(label, "\n", fixed = TRUE)[[1]])) * 0.56 * f + 1.4 * f
  h <- n * 1.25 * f + 0.9 * f
  grid::gTree(children = grid::gList(
    grid::roundrectGrob(x, y, w, h, default.units = "native", r = grid::unit(0.15, "snpc"),
                        gp = grid::gpar(fill = grDevices::adjustcolor(fill, alpha.f = alpha), col = NA)),
    grid::textGrob(label, x, y, default.units = "native",
                   gp = grid::gpar(col = "white", fontface = "bold", fontsize = f, lineheight = 1.05))
  ))
}

drawing_goal <- function(o, s, i, lay) {
  sc <- lay$scale
  depth <- (if (o$type == "maal") 1.3 else 0.9) * sc
  half <- o$bredde / 2 * sc
  c0 <- drawing_px(lay, s, i, o$x, o$y)
  if (o$side %in% c("venstre", "hoyre")) {
    sign <- if (o$side == "venstre") -1 else 1
    x <- c0[1] + sign * depth / 2; y <- c0[2]; w <- depth; h <- 2 * half
  } else {
    sign <- if (o$side == "bunn") -1 else 1
    x <- c0[1]; y <- c0[2] + sign * depth / 2; w <- 2 * half; h <- depth
  }
  lwd <- lay$sizes$line_lwd
  net <- if (o$type == "maal") {
    step <- depth / 3.5
    xs <- seq(x - w / 2, x + w / 2, by = step); ys <- seq(y - h / 2, y + h / 2, by = step)
    grid::segmentsGrob(c(xs, rep(x - w / 2, length(ys))), c(rep(y - h / 2, length(xs)), ys),
                       c(xs, rep(x + w / 2, length(ys))), c(rep(y + h / 2, length(xs)), ys),
                       default.units = "native", gp = grid::gpar(col = "#d8d8d8", lwd = lwd * 0.5))
  } else {
    # Diagonal hatching, clipped to the goal.
    step <- min(w, h) / 2.2
    k <- seq(-max(w, h), max(w, h) * 2, by = step)
    grid::segmentsGrob(k, 0, k + max(w, h) * 2, max(w, h) * 2, default.units = "native",
                       gp = grid::gpar(col = "#555555", lwd = lwd * 0.6),
                       vp = grid::viewport(grid::unit(x, "native"), grid::unit(y, "native"), grid::unit(w, "native"),
                                           grid::unit(h, "native"), xscale = c(0, w), yscale = c(0, h), clip = "on"))
  }
  grid::gTree(children = grid::gList(
    grid::rectGrob(x, y, w, h, default.units = "native", gp = grid::gpar(fill = "white", col = NA)),
    net,
    grid::rectGrob(x, y, w, h, default.units = "native", gp = grid::gpar(fill = NA, col = "white", lwd = lwd * 1.4))
  ))
}

#' Draw one drawing as a grid gTree in a viewport of lay$width x lay$height pixels
#' @noRd
drawing_grob <- function(d, lay) {
  sz <- lay$sizes
  g <- list()
  add <- function(x) g[[length(g) + 1]] <<- x
  # Grass: six vertical stripes over the whole image.
  n <- 6
  add(grid::rectGrob((0:(n - 1) + 0.5) * lay$width / n, lay$height / 2, lay$width / n, lay$height, default.units = "native",
                     gp = grid::gpar(fill = rep(drawing_colours$grass, length.out = n), col = NA)))
  for (i in seq_along(d$skisser)) {
    s <- d$skisser[[i]]
    b <- s$bane
    corner <- drawing_px(lay, s, i, c(0, b$bredde), c(0, b$lengde))
    for (z in s$soner) {
      zp <- drawing_px(lay, s, i, c(z$x, z$x + z$b), c(z$y, z$y + z$h))
      add(grid::rectGrob(mean(zp[, 1]), mean(zp[, 2]), diff(zp[, 1]), diff(zp[, 2]), default.units = "native",
                         gp = grid::gpar(fill = "#FFFFFF2E", col = NA)))
      if (nzchar(z$tekst)) {
        tp <- drawing_px(lay, s, i, z$x + z$b / 2, b$lengde + b$marg / 2)
        add(grid::textGrob(z$tekst, tp[1], tp[2], default.units = "native",
                           gp = grid::gpar(col = "white", fontface = "bold", fontsize = sz$font * 0.68)))
      }
    }
    add(grid::rectGrob(mean(corner[, 1]), mean(corner[, 2]), diff(corner[, 1]), diff(corner[, 2]), default.units = "native",
                       gp = grid::gpar(fill = NA, col = drawing_colours$line, lwd = sz$line_lwd)))
    if (b$midtlinje) {
      m <- drawing_px(lay, s, i, c(b$bredde / 2, b$bredde / 2), c(0, b$lengde))
      add(grid::linesGrob(m[, 1], m[, 2], default.units = "native", gp = grid::gpar(col = drawing_colours$line, lwd = sz$line_lwd)))
    }
    for (l in s$linjer) {
      m <- drawing_px(lay, s, i, c(l$fra[1], l$til[1]), c(l$fra[2], l$til[2]))
      add(grid::linesGrob(m[, 1], m[, 2], default.units = "native", gp = grid::gpar(col = drawing_colours$line, lwd = sz$line_lwd)))
    }
    for (l in s$hjelpelinjer) {
      m <- drawing_px(lay, s, i, c(l$fra[1], l$til[1]), c(l$fra[2], l$til[2]))
      add(grid::linesGrob(m[, 1], m[, 2], default.units = "native",
                          gp = grid::gpar(col = drawing_colours$helper, lwd = sz$line_lwd * 1.1, lty = "22")))
      if (nzchar(l$tekst)) {
        top <- m[which.max(m[, 2]), ]
        add(grid::textGrob(l$tekst, top[1], top[2] + b$marg * lay$scale / 2, default.units = "native",
                           gp = grid::gpar(col = drawing_colours$helper, fontface = "bold", fontsize = sz$font * 0.68)))
      }
    }
    # Goals and cones under the arrows; players and the ball on top.
    for (o in s$objekter) {
      if (o$type %in% c("maal", "smaamaal")) add(drawing_goal(o, s, i, lay))
      if (o$type == "kjegle") {
        p <- drawing_px(lay, s, i, o$x, o$y)
        h <- sz$cone
        add(grid::polygonGrob(p[1] + c(-h / 2, h / 2, 0), p[2] + c(-h * 0.38, -h * 0.38, h * 0.5), default.units = "native",
                              gp = grid::gpar(fill = drawing_colours$cone[[o$farge]], col = "#c45c00", lwd = sz$line_lwd * 0.5)))
      }
    }
    for (a in s$piler) {
      px <- drawing_arrow_px(a, s, i, lay)
      if (nrow(px) < 2) next
      col <- if (a$type == "pasning") "white" else "#121212"
      line_px <- px
      if (a$type == "foring") {
        line_px <- drawing_wave(px, amp = sz$player_r * 0.28, wavelength = sz$player_r * 1.1)
      }
      # Stop the line where the head starts, so the dashes do not poke through.
      n <- nrow(line_px)
      add(grid::linesGrob(line_px[-n, 1], line_px[-n, 2], default.units = "native",
                          gp = grid::gpar(col = col, lwd = sz$arrow_lwd, lty = if (a$type == "lop") "33" else "solid",
                                          lineend = "butt", linejoin = "round")))
      add(drawing_head(px, sz$head, col))
    }
    for (o in s$objekter) {
      if (!o$type %in% c("spiller", "trener")) next
      p <- drawing_px(lay, s, i, o$x, o$y)
      fill <- if (o$type == "trener") drawing_colours$trainer else drawing_colours$team[[o$lag]]
      txtcol <- if (o$type == "trener") "white" else drawing_colours$team_text[[o$lag]]
      add(grid::circleGrob(p[1] + sz$player_r * 0.12, p[2] - sz$player_r * 0.14, sz$player_r, default.units = "native",
                           gp = grid::gpar(fill = drawing_colours$shadow, col = NA)))
      add(grid::circleGrob(p[1], p[2], sz$player_r, default.units = "native",
                           gp = grid::gpar(fill = fill, col = "white", lwd = sz$line_lwd * 1.1)))
      if (nzchar(o$tekst)) {
        add(grid::textGrob(o$tekst, p[1], p[2], default.units = "native",
                           gp = grid::gpar(col = txtcol, fontface = "bold", fontsize = sz$font * if (nchar(o$tekst) > 1) 0.6 else 0.72)))
      }
    }
    for (o in s$objekter) {
      if (o$type != "ball") next
      p <- drawing_px(lay, s, i, o$x, o$y)
      add(grid::circleGrob(p[1], p[2], sz$ball_r, default.units = "native",
                           gp = grid::gpar(fill = "white", col = "#121212", lwd = sz$line_lwd * 0.9)))
      add(grid::circleGrob(p[1], p[2], sz$ball_r * 0.38, default.units = "native", gp = grid::gpar(fill = "#121212", col = NA)))
    }
    for (l in s$etiketter) {
      p <- drawing_px(lay, s, i, l$x, l$y)
      add(drawing_text_box(p[1], p[2], l$tekst, lay))
    }
    for (l in s$tekster) {
      p <- drawing_px(lay, s, i, l$x, l$y)
      add(grid::textGrob(l$tekst, p[1], p[2], default.units = "native",
                         gp = grid::gpar(col = "white", fontface = "bold", fontsize = sz$font * 0.68)))
    }
    if (nzchar(s$tittel)) {
      p <- drawing_px(lay, s, i, b$bredde / 2, b$lengde + b$marg / 2)
      add(drawing_text_box(p[1], p[2], s$tittel, lay, alpha = 0.55, wrap = 40))
    }
  }
  grid::gTree(children = do.call(grid::gList, g),
              vp = grid::viewport(xscale = c(0, lay$width), yscale = c(0, lay$height)))
}

#' Draw a drawing to a PNG file
#'
#' @param d A drawing (list or JSON text); it is validated and, if `fix`,
#'   adjusted with `drawing_fix()` first.
#' @return The path, invisibly, with the attribute "problems" (from
#'   `drawing_check()` after fixing).
#' @noRd
drawing_png <- function(d, path, width_px = 1800, fix = TRUE) {
  d <- drawing_validate(d)
  if (is.null(d)) stop("Tegningen er tom.", call. = FALSE)
  if (fix) d <- drawing_fix(d, width_px)
  lay <- drawing_layout(d, width_px)
  # 72 dpi, so one point is one pixel and all sizes above are in pixels.
  args <- list(filename = path, width = lay$width, height = lay$height, res = 72, bg = drawing_colours$grass[1])
  if (isTRUE(capabilities("cairo"))) args$type <- "cairo"
  do.call(grDevices::png, args)
  ok <- FALSE
  on.exit(if (!ok) grDevices::dev.off(), add = TRUE)
  grid::grid.newpage()
  grid::grid.draw(drawing_grob(d, lay))
  grDevices::dev.off()
  ok <- TRUE
  structure(path, problems = drawing_check(d, width_px))
}

#' A validated drawing without empty parts and default values
#'
#' Keys come in a fixed, readable order (the order `drawing_validate()`
#' builds them in), so the JSON reads the same after a round trip through
#' Postgres' jsonb, which sorts keys.
#' @noRd
drawing_compact <- function(d) {
  drop <- function(x, defaults) {
    for (k in names(defaults)) if (identical(x[[k]], defaults[[k]])) x[[k]] <- NULL
    x[lengths(x) > 0]
  }
  d$skisser <- lapply(d$skisser, function(s) {
    s$bane <- drop(s$bane, list(marg = 2.5, midtlinje = FALSE))
    s$objekter <- lapply(s$objekter, function(o) {
      if (identical(o$type, "trener") && identical(o$tekst, "T")) o$tekst <- NULL
      drop(o, list(farge = "oransje", tekst = ""))
    })
    s$piler <- lapply(s$piler, drop, defaults = list(bue = 0))
    s$soner <- lapply(s$soner, drop, defaults = list(tekst = ""))
    s$hjelpelinjer <- lapply(s$hjelpelinjer, drop, defaults = list(tekst = ""))
    drop(s, list(tittel = ""))
  })
  d
}

#' A drawing as JSON text: compact for the database, indented for editing
#' @noRd
drawing_json <- function(d, pretty = FALSE) {
  d <- drawing_compact(d)
  js <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, digits = NA))
  if (!pretty) return(js(d))
  # One object per line, so a drawing is easy to edit by hand.
  sketch <- function(s) {
    parts <- vapply(names(s), function(k) {
      v <- s[[k]]
      if (is.list(v) && is.null(names(v))) {
        paste0('      "', k, '": [\n', paste0("        ", vapply(v, js, ""), collapse = ",\n"), "\n      ]")
      } else {
        paste0('      "', k, '": ', js(v))
      }
    }, "")
    paste0("    {\n", paste(parts, collapse = ",\n"), "\n    }")
  }
  paste0('{\n  "skisser": [\n', paste(vapply(d$skisser, sketch, ""), collapse = ",\n"), "\n  ]\n}")
}

#' A drawing as indented JSON for the edit field ("" for none)
#' @noRd
drawing_pretty <- function(x) {
  x <- if (is.null(x) || length(x) == 0 || is.na(x[1])) "" else as.character(x[1])
  if (!nzchar(x)) return("")
  tryCatch(drawing_json(drawing_validate(x), pretty = TRUE), error = function(e) x)
}

#' A small example drawing to start from (3 mot 1 in a 12 x 12 m square)
#' @noRd
drawing_example_json <- function() {
  ex <- list(skisser = list(list(
    tittel = "3 mot 1",
    bane = list(bredde = 12, lengde = 12, marg = 2.5),
    objekter = list(
      list(type = "kjegle", x = 0, y = 0), list(type = "kjegle", x = 12, y = 0),
      list(type = "kjegle", x = 0, y = 12), list(type = "kjegle", x = 12, y = 12),
      list(type = "spiller", lag = 1, x = 2, y = 2, tekst = "1"),
      list(type = "ball", x = 3, y = 2.6),
      list(type = "spiller", lag = 1, x = 2, y = 10, tekst = "2"),
      list(type = "spiller", lag = 1, x = 10, y = 3, tekst = "3"),
      list(type = "spiller", lag = 2, x = 6, y = 5.5, tekst = "F")),
    piler = list(
      list(type = "pasning", fra = c(3, 2.6), til = c(9.2, 3.3)),
      list(type = "lop", fra = c(2, 10), til = c(5, 10.5), bue = 0.2)),
    tekster = list(list(x = 6, y = -1.4, tekst = "Rute 12 × 12 m"))
  )))
  drawing_json(drawing_validate(ex), pretty = TRUE)
}

#' The drawing as a PNG data URI for showing in the app
#' @return list(src, problems) or stops with a Norwegian message.
#' @noRd
drawing_preview <- function(x, width_px = 1400) {
  path <- tempfile(fileext = ".png")
  on.exit(unlink(path), add = TRUE)
  res <- drawing_png(x, path, width_px = width_px)
  src <- paste0("data:image/png;base64,", jsonlite::base64_enc(readBin(path, "raw", file.size(path))))
  list(src = src, problems = attr(res, "problems"))
}
