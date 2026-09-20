# Price every Sportsbet Brownlow selection in results/markets/sportsbet_lines_latest.csv
# against the per-round 2026 simulation, and write the Kelly table the artifact reads.
#
# Replaces the hand-typed odds in the earlier kelly_markets_2026.R: run
# analysis/sportsbet_pull.R first (or analysis/reprice.R, which chains both).
#
# Sim cache: results/markets/votes_sims_2026_5000.rds (built here if missing,
# ~3 min). V is one row per (player x round) cell, one column per simulated
# season; every aggregate is a rowsum() so nothing copies a 3-D array.
#
# Run: powershell.exe -Command 'Rscript "analysis/price_markets_2026.R"'

suppressMessages({ library(data.table); devtools::load_all(quiet = TRUE) })
N_SIMS <- 5000
LINES  <- "results/markets/sportsbet_lines_latest.csv"

# ---- data --------------------------------------------------------------------
ep  <- fread("results/ensemble_probabilities.csv")[season == 2026]
raw <- fread("data/brownlow_datathon_dataset.csv",
             select = c("match_date","season","match_round","player_first_name",
                        "player_last_name","player_team","player_position"))[season == 2026]
raw[, `:=`(player_name = paste(player_first_name, player_last_name),
           date = as.IDate(match_date, format = "%d/%m/%Y"),
           team = sub(" Lions$", "", player_team))]
ep[, date := as.IDate(date)]
ep <- merge(ep, raw[, .(player_name, team, date, match_round, player_position)],
            by = c("player_name","team","date"), all.x = TRUE)
stopifnot(nrow(ep) == 9522, !anyNA(ep$match_round))

prep <- prepare_season_sim(ep)
keys <- unique(prep$player_key); key_id <- match(prep$player_key, keys)
rounds <- sort(unique(ep$match_round)); R <- length(rounds)
round_of_row <- match(ep$match_round, rounds); np <- length(keys)
cell <- (key_id - 1L) * R + round_of_row
cell_pid <- rep(seq_len(np), each = R); cell_rnd <- rep(rounds, times = np)
sim_file <- sprintf("results/markets/votes_sims_2026_%d.rds", N_SIMS)
dir.create("results/markets", showWarnings = FALSE)
if (file.exists(sim_file)) {
  V <- readRDS(sim_file); cat("loaded sims from", sim_file, "\n")
} else {
  set.seed(2026); t0 <- Sys.time()
  V <- matrix(0L, np * R, N_SIMS)
  for (s in seq_len(N_SIMS)) {
    v <- numeric(nrow(prep$P))
    for (ii in prep$match_idx) v[ii] <- simulate_match_votes_pl(prep$P[ii, , drop = FALSE])
    V[cell, s] <- as.integer(v)
  }
  saveRDS(V, sim_file); cat("simulated", N_SIMS, "seasons in", round(as.numeric(Sys.time() - t0, units = "mins"), 1), "min\n")
}
stopifnot(ncol(V) == N_SIMS, nrow(V) == np * R)
by_player <- function(M) { storage.mode(M) <- "integer"; rowsum(M, cell_pid, reorder = TRUE) }
totals <- by_player(V); rownames(totals) <- keys
pname <- sub("_[^_]+$", "", keys); pteam <- sub("^.*_", "", keys)

# ---- name resolution ---------------------------------------------------------
squash <- function(x) tolower(gsub("[^a-z]", "", tolower(x)))
pkey <- squash(pname)
# Sportsbet spelling -> (our name, team). Team disambiguates the two Bailey Williams
# (Sportsbet calls the West Coast one "Bailey J. Williams").
alias <- list(matthewcarroll = c("Matt Carroll", NA), harrisonpetty = c("Harry Petty", NA),
              baileyjwilliams = c("Bailey Williams", "West Coast"), baileywilliams = c("Bailey Williams", "Western Bulldogs"))
pid <- function(nm) {
  k <- squash(nm); team <- NA
  if (!is.null(alias[[k]])) { team <- alias[[k]][2]; k <- squash(alias[[k]][1]) }
  i <- which(pkey == k & (is.na(team) | pteam == team))
  if (length(i) == 1) return(i)
  if (length(i) > 1) { warning("ambiguous player: ", nm); return(NA_integer_) }
  NA_integer_
}
TEAM <- c("Adelaide Crows"="Adelaide","Brisbane Lions"="Brisbane","Carlton"="Carlton","Collingwood"="Collingwood",
          "Essendon"="Essendon","Fremantle"="Fremantle","Geelong Cats"="Geelong","Gold Coast SUNS"="Gold Coast",
          "GWS GIANTS"="Greater Western Sydney","Hawthorn"="Hawthorn","Melbourne"="Melbourne","North Melbourne"="North Melbourne",
          "Port Adelaide"="Port Adelaide","Richmond"="Richmond","St Kilda"="St Kilda","Sydney Swans"="Sydney",
          "West Coast Eagles"="West Coast","Western Bulldogs"="Western Bulldogs")
team_of <- function(x) { if (x %in% TEAM) return(x); t <- TEAM[x]; if (is.na(t)) stop("unknown team: ", x); unname(t) }

# ---- sim primitives ----------------------------------------------------------
rk   <- apply(-totals, 2, rank, ties.method = "min")
maxv <- apply(totals, 2, max)
uniq_max <- function(M) { mx <- apply(M, 2, max); one <- colSums(M == rep(mx, each = nrow(M))) == 1
  rowMeans(M == rep(mx, each = nrow(M)) & rep(one, each = nrow(M))) }
p_win <- uniq_max(totals)
p_topk <- function(k) rowMeans(rk <= k)
ord <- apply(totals, 2, order, decreasing = TRUE)
pos_name <- function(k) pname[ord[k, ]]
first <- pos_name(1); second <- pos_name(2); third <- pos_name(3); fourth <- pos_name(4); fifth <- pos_name(5)
srt2 <- apply(totals, 2, function(x) sort(x, decreasing = TRUE)[1:2]); marg <- srt2[1, ] - srt2[2, ]
cum <- function(r) { keep <- cell_rnd <= r; rowsum(V[keep, ], cell_pid[keep], reorder = TRUE) }
lead_cache <- list()
lead_at <- function(r) { k <- as.character(r); if (is.null(lead_cache[[k]])) lead_cache[[k]] <<- uniq_max(cum(r)); lead_cache[[k]] }
late <- cell_rnd >= 17
p_l8 <- uniq_max(rowsum(V[late, ], cell_pid[late], reorder = TRUE))
n3 <- by_player(V == 3L); ng <- by_player(V > 0L)
p_most3 <- uniq_max(n3); p_mostg <- uniq_max(ng)
tm_tot <- rowsum(totals, pteam); p_team <- uniq_max(tm_tot)
games_left <- sapply(rounds, function(r) tabulate(key_id[ep$match_round > r], np))
decl <- rep(NA_integer_, N_SIMS); w <- ord[1, ]; ws <- cbind(w, seq_len(N_SIMS)); cm <- matrix(0L, np, N_SIMS)
for (j in seq_len(R)) { cm <- cm + V[cell_rnd == rounds[j], ]; thr <- cm + 3L * games_left[, j]; wv <- cm[ws]; thr[ws] <- -1L
  hit <- is.na(decl) & wv > apply(thr, 2, max); decl[hit] <- rounds[j] }
d <- pid("Nick Daicos"); dv <- V[cell_pid == d, ]; dt_tot <- totals[d, ]
ld <- function(r) { cmr <- cum(r); mx <- apply(cmr, 2, max); cmr[d, ] == mx & colSums(cmr == rep(mx, each = np)) == 1 }

# ---- pricers: each takes (event, market, selection, handicap) -> p or NA ------
names_in <- function(sel) { s <- trimws(strsplit(sel, "/", fixed = TRUE)[[1]]); s }
set_p <- function(nms, k) { ids <- sapply(nms, pid); if (anyNA(ids)) return(NA_real_)
  nm <- pname[ids]; ok <- rep(TRUE, N_SIMS); pos <- list(first, second, third, fourth, fifth)
  for (i in seq_len(k)) ok <- ok & pos[[i]] %in% nm; mean(ok) }
exact_p <- function(nms) { ids <- sapply(nms, pid); if (anyNA(ids)) return(NA_real_)
  nm <- pname[ids]; ok <- rep(TRUE, N_SIMS); pos <- list(first, second, third, fourth, fifth)
  for (i in seq_along(nm)) ok <- ok & pos[[i]] == nm[i]; mean(ok) }
hub <- function(sel) {
  s <- sel; num <- function(re) as.numeric(sub(re, "\\1", s))
  if (grepl("poll (\\d+)\\+ votes in the last 8", s)) return(mean(colSums(dv[rounds >= 17, ]) >= num(".*poll (\\d+)\\+ votes in the last 8.*")))
  if (grepl("more votes than Essendon & Richmond", s)) return(mean(dt_tot > tm_tot["Essendon", ] + tm_tot["Richmond", ]))
  if (grepl("^(\\w[\\w ]+) to poll the most votes combined", s, perl = TRUE)) { t <- team_of(sub(" to poll.*$", "", s)); return(unname(p_team[t])) }
  if (grepl("poll in (\\d+)\\+ games", s)) return(mean(colSums(dv > 0) >= num(".*poll in (\\d+)\\+ games.*")))
  if (grepl("win the Brownlow Medal by (\\d+)\\+", s)) return(mean(first == "Nick Daicos" & marg >= num(".*by (\\d+)\\+.*")))
  if (grepl("break record and poll (\\d+)\\+|to poll (\\d+)\\+ votes$", s)) return(mean(dt_tot >= num(".*poll (\\d+)\\+ votes.*")))
  if (grepl("poll the 3 votes in (\\d+)\\+ games", s)) return(mean(colSums(dv == 3) >= num(".*in (\\d+)\\+ games.*")))
  if (grepl("Bailey Smith to poll (\\d+)\\+ votes combined", s)) return(mean(totals[pid("Nick Daicos"),] + totals[pid("Marcus Bontempelli"),] + totals[pid("Bailey Smith"),] >= num(".*poll (\\d+)\\+ votes.*")))
  if (grepl("lead outright after rounds 6, 10 and 15", s)) return(mean(ld(6) & ld(10) & ld(15)))
  if (grepl("Daicos brothers to poll (\\d+)\\+", s)) return(mean(dt_tot + totals[pid("Josh Daicos"),] >= num(".*poll (\\d+)\\+.*")))
  if (grepl("polling (\\d+) or more votes than the next highest for Collingwood", s)) { coll <- totals[pteam == "Collingwood" & pname != "Nick Daicos", ]
    return(mean(dt_tot - apply(coll, 2, max) >= num(".*polling (\\d+) or more.*"))) }
  if (grepl("Top 4 players to all poll (\\d+)\\+", s)) return(mean(apply(totals, 2, function(x) sort(x, decreasing = TRUE)[4]) >= num(".*poll (\\d+)\\+.*")))
  if (grepl("^First 5 \\(any order\\)", s)) return(set_p(c("Nick Daicos","Bailey Smith","Marcus Bontempelli","Patrick Cripps","Will Ashcroft"), 5))
  if (grepl("^First 5 \\(exact order\\)", s)) return(exact_p(c("Nick Daicos","Bailey Smith","Marcus Bontempelli","Patrick Cripps","Will Ashcroft")))
  if (grepl("Bailey Smith or Marcus Bontempelli to Outright Win", s)) return(p_win[pid("Bailey Smith")] + p_win[pid("Marcus Bontempelli")])
  if (grepl("to tie with Bailey Smith or Marcus Bontempelli", s)) return(mean(marg == 0 & first == "Nick Daicos" & second %in% c("Bailey Smith","Marcus Bontempelli")))
  NA_real_
}
price_one <- function(ev, mk, sel, hc) {
  if (grepl("Votes O/U$", ev)) { i <- pid(sub(" Votes O/U$", "", ev)); if (is.na(i)) return(NA)
    return(if (sel == "Over") mean(totals[i, ] > hc) else mean(totals[i, ] < hc)) }
  if (ev == "2026 AFL Brownlow Medal") { i <- pid(sel); return(if (is.na(i)) NA else p_win[i]) }
  if (grepl("^Most Votes - Without Nick Daicos", ev)) { i <- pid(sel); if (is.na(i)) return(NA)
    return(uniq_max(totals[-d, ])[match(i, seq_len(np)[-d])]) }
  if (grepl("^Top (\\d+) Finish", ev)) { k <- as.integer(sub("^Top (\\d+) Finish.*", "\\1", ev)); i <- pid(sel); return(if (is.na(i)) NA else p_topk(k)[i]) }
  if (ev == "Brownlow Medal Quinella") return(set_p(names_in(sel), 2))
  if (ev == "Brownlow Medal Exacta")   return(exact_p(names_in(sel)))
  if (ev == "Brownlow Medal Trifecta") return(exact_p(names_in(sel)))
  if (ev == "Brownlow Medal Boxed Trifecta") return(set_p(names_in(sel), 3))
  if (ev == "Brownlow Medal Pick4")    return(exact_p(names_in(sel)))
  if (ev == "Brownlow Medal Boxed Pick4") return(set_p(names_in(sel), 4))
  if (ev == "Winning Margin") { if (sel == "Tie") return(mean(marg == 0))
    if (grepl("\\+", sel)) return(mean(marg >= as.integer(sub("\\+.*", "", sel))))
    r <-as.integer(regmatches(sel, gregexpr("\\d+", sel))[[1]]); return(mean(marg >= r[1] & marg <= r[2])) }
  if (ev == "Round of Declared Winner") return(mean(!is.na(decl) & decl == as.integer(sub("Round ", "", sel))))  # NA = tied, never declared
  if (grepl("^Leader After Round (\\d+)$", ev)) { r <- as.integer(sub("^Leader After Round ", "", ev)); i <- pid(sel); return(if (is.na(i)) NA else lead_at(r)[i]) }
  if (ev == "Most Votes Last 8 Rounds") { i <- pid(sel); return(if (is.na(i)) NA else p_l8[i]) }
  if (ev == "Most 3 Vote Games") { i <- pid(sel); return(if (is.na(i)) NA else p_most3[i]) }
  if (ev == "Player to Poll in the Most Games") { i <- pid(sel); return(if (is.na(i)) NA else p_mostg[i]) }
  if (ev == "Brownlow Hub") return(hub(sel))
  if (grepl("^Brownlow Group - Group", ev)) return(NA)   # handled per market below (needs the whole field)
  if (grepl("^To poll (\\d+) or more votes$", ev)) { k <- as.integer(sub("^To poll (\\d+).*", "\\1", ev)); i <- pid(sel); return(if (is.na(i)) NA else mean(totals[i, ] >= k)) }
  if (ev == "To Poll a Vote") { i <- pid(sel); return(if (is.na(i)) NA else mean(totals[i, ] >= 1)) }
  NA_real_
}

# ---- run over every line -----------------------------------------------------
L <- fread(LINES)
cat("pricing", nrow(L), "selections pulled at", L$pulled_at[1], "\n")
L[, p := NA_real_]
simple <- !grepl("^Brownlow Group|Most Votes$|Most Votes - Without|^Round \\d+$|^Opening Round$", L$event) | grepl("Without Nick Daicos", L$event)
L[simple, p := mapply(price_one, event, market, selection, handicap)]

# groups: most votes among the listed field
for (m in unique(L[grepl("^Brownlow Group", event), market_id])) { rows <- which(L$market_id == m)
  ids <- sapply(L$selection[rows], pid); if (anyNA(ids)) next
  L$p[rows] <- uniq_max(totals[ids, , drop = FALSE]) }

# team most votes, optionally excluding "X.Surname" players named in the event
for (m in unique(L[grepl(" Most Votes( - Without |$)", event) & !grepl("Nick Daicos \\(", event), market_id])) { rows <- which(L$market_id == m)
  ev <- L$event[rows[1]]; tname <- team_of(sub(" Most Votes.*$", "", ev)); members <- which(pteam == tname)
  ex <- if (grepl("Without", ev)) trimws(strsplit(sub("^.*Without ", "", ev), "/")[[1]]) else character()
  for (x in ex) { ini <- substr(x, 1, 1); sur <- squash(sub("^[A-Z]\\.", "", x))
    hit <- members[substr(pname[members], 1, 1) == ini & endsWith(pkey[members], sur)]
    if (length(hit) != 1) { warning("cannot resolve exclusion ", x, " in ", ev); hit <- integer() }
    members <- setdiff(members, hit) }
  pt <- uniq_max(totals[members, , drop = FALSE]); names(pt) <- pname[members]
  L$p[rows] <- sapply(L$selection[rows], function(s) { i <- pid(s); if (is.na(i)) NA_real_ else unname(pt[pname[i]]) }) }

# 3-vote games: event "Round N" / "Opening Round", market "Home v Away"
for (m in unique(L[grepl("^Round \\d+$|^Opening Round$", event), market_id])) { rows <- which(L$market_id == m)
  r <- if (L$event[rows[1]] == "Opening Round") 0L else as.integer(sub("Round ", "", L$event[rows[1]]))
  tt <- sapply(trimws(strsplit(L$market[rows[1]], " v ", fixed = TRUE)[[1]]), team_of)
  mid <- unique(ep[match_round == r & team %in% tt, match_id]); if (length(mid) != 1) { warning("no unique match for ", L$market[rows[1]], " R", r); next }
  mp <- ep[match_id == mid]
  listed <- L$selection[rows][L$selection[rows] != "Any other player"]
  p3 <- sapply(listed, function(s) { i <- pid(s); if (is.na(i)) NA_real_ else { v <- mp[player_name == pname[i], ensemble_prob_3]; if (length(v) == 1) v else NA_real_ } })
  # sum() without na.rm: an unresolved listed player leaves "Any other player" unpriced too, rather than silently inflating it
  L$p[rows] <- ifelse(L$selection[rows] == "Any other player", 1 - sum(p3), p3[L$selection[rows]])
  L$market[rows] <- paste0("3 votes R", r, " ", L$market[rows]) }

# ---- Kelly + outputs ---------------------------------------------------------
L[, `:=`(implied = 1 / odds, edge = p * odds - 1, kelly = pmax((p * odds - 1) / (odds - 1), 0))]
unpriced <- L[is.na(p)]
cat(sprintf("priced %d of %d selections; %d unpriced\n", sum(!is.na(L$p)), nrow(L), nrow(unpriced)))
if (nrow(unpriced)) print(unpriced[, .N, by = .(event, market)][order(-N)], nrow = 40)
res <- L[!is.na(p), .(market = ifelse(grepl("^3 votes", market), market, event), sel = ifelse(is.na(handicap), selection, paste(selection, handicap)),
                      odds, p = round(p, 4), implied = round(implied, 4), edge = round(edge, 3), kelly = round(kelly, 4), pulled_at)]
setorder(res, -kelly)
fwrite(res, "results/markets/sportsbet_2026_kelly.csv"); fwrite(unpriced, "results/markets/sportsbet_2026_unpriced.csv")
cat("\n== top 25 by full Kelly ==\n"); print(res[edge > 0][1:25])

# ---- artifact inputs ---------------------------------------------------------
parts <- tstrsplit(ep$match_id, "_", fixed = TRUE)
ev <- ep[, .(player_name, team, position = player_position, round = match_round,
             opponent = ifelse(parts[[2]] == team, parts[[3]], parts[[2]]),
             xv = round(ensemble_expected_votes, 3), p3 = round(ensemble_prob_3, 3), p_poll = round(1 - ensemble_prob_0, 3))]
pos <- raw[, .N, by = .(player_name, player_position)][order(-N)][, .SD[1], by = player_name]
summ <- data.table(player_name = pname, team = pteam, sim_mean = rowMeans(totals), sim_sd = apply(totals, 1, sd),
                   win_pct = round(100 * p_win, 2), top3_pct = round(100 * p_topk(3), 1), top10_pct = round(100 * p_topk(10), 1))
summ <- merge(summ, pos[, .(player_name, position = player_position)], by = "player_name", all.x = TRUE)
fwrite(ev, "results/markets/expected_votes_by_round_2026.csv"); fwrite(summ[order(-sim_mean)], "results/markets/player_summary_2026.csv")
cat("wrote", nrow(ev), "player-round rows,", nrow(summ), "players\n")
