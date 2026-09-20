# Pull every Sportsbet Brownlow market as one flat CSV.
#
# Sportsbet's front end calls an unauthenticated JSON API; a browser User-Agent
# is all it needs (verified 2026-09-20). Competition ids for the Brownlow
# classes were read from the `topicLink` of one known event in each class.
#
# Output: results/markets/sportsbet_lines_<YYYYMMDD-HHMM>.csv and a copy at
# results/markets/sportsbet_lines_latest.csv, one row per selection:
#   pulled_at, competition_id, event_id, event, market_id, market, selection,
#   handicap, odds, status
# Raw JSON is kept under results/markets/sportsbet_raw/<stamp>/.
#
# Run: powershell.exe -Command 'Rscript "analysis/sportsbet_pull.R"'
# Reuse for another sport: change COMPS (find ids via any market URL's event ->
# /Events/{id}/Markets -> topicLink "Sports/{sport}/Competitions/{comp}").

suppressMessages({ library(data.table); library(jsonlite); library(curl) })

COMPS <- c(medal_and_hub = 6136, three_vote_games = 16767, team_most_votes = 28057,
           pick_your_own = 24522, player_ou = 27508)
BASE <- "https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports"
h <- new_handle()
handle_setheaders(h, `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/152.0.0.0 Safari/537.36",
                  Accept = "application/json", `Accept-Language` = "en-AU,en;q=0.9")
get_json <- function(url, save_to) {
  r <- curl_fetch_memory(url, handle = h)
  if (r$status_code != 200) stop(sprintf("HTTP %d for %s", r$status_code, url))
  writeBin(r$content, save_to)
  fromJSON(rawToChar(r$content), simplifyVector = FALSE)
}

stamp <- format(Sys.time(), "%Y%m%d-%H%M")
raw_dir <- file.path("results/markets/sportsbet_raw", stamp); dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
rows <- list()
for (cname in names(COMPS)) {
  cid <- COMPS[[cname]]
  events <- get_json(sprintf("%s/Competitions/%d/Events", BASE, cid), file.path(raw_dir, sprintf("comp_%d_events.json", cid)))
  cat(sprintf("%-18s comp %-6d %2d events\n", cname, cid, length(events)))
  for (ev in events) {
    mk <- get_json(sprintf("%s/Events/%s/Markets", BASE, ev$id), file.path(raw_dir, sprintf("event_%s.json", ev$id)))
    for (m in mk) for (s in m$selections) rows[[length(rows) + 1]] <- data.table(
      pulled_at = stamp, competition_id = cid, event_id = ev$id, event = ev$name,
      market_id = m$id, market = m$name, selection = s$name,
      handicap = if (is.null(s$unformattedHandicap)) NA_real_ else as.numeric(s$unformattedHandicap),
      odds = if (is.null(s$price$winPrice)) NA_real_ else s$price$winPrice,
      status = s$statusCode)
  }
}
lines <- rbindlist(rows)
stopifnot(nrow(lines) > 0, !anyNA(lines$odds) || mean(is.na(lines$odds)) < 0.05)
fwrite(lines, sprintf("results/markets/sportsbet_lines_%s.csv", stamp))
fwrite(lines, "results/markets/sportsbet_lines_latest.csv")
cat(sprintf("\n%d selections across %d markets in %d events -> results/markets/sportsbet_lines_%s.csv\n",
            nrow(lines), uniqueN(lines$market_id), uniqueN(lines$event_id), stamp))
print(lines[, .(markets = uniqueN(market_id), selections = .N), by = .(competition_id, event)][order(competition_id)], nrow = 100)
