# Price the Sportsbet Brownlow markets (20 Sep 2026) off a fresh per-round
# simulation of the 2026 ensemble probabilities, and write the per-player
# per-round expected votes used by the interactive artifact.
#
# Why re-simulate: results/brownlow_simulation_results_2026.rds only keeps
# season totals per sim, and half the markets (leader after round N, last-8,
# round of declared winner, most 3-vote games) need the round-by-round path.
#
# Run: powershell.exe -Command 'Rscript "analysis/kelly_markets_2026.R"'

suppressMessages({ library(data.table); devtools::load_all(quiet = TRUE) })
N_SIMS <- 5000

# ---- data --------------------------------------------------------------------
ep  <- fread("results/ensemble_probabilities.csv")[season == 2026]
raw <- fread("data/brownlow_datathon_dataset.csv",
             select = c("match_date","season","match_round","player_first_name",
                        "player_last_name","player_team","player_position"))[season == 2026]
raw[, `:=`(player_name = paste(player_first_name, player_last_name),
           date = as.IDate(match_date, format = "%d/%m/%Y"))]
ep[, date := as.IDate(date)]
raw[, team := sub(" Lions$", "", player_team)]           # raw says "Brisbane Lions", ep "Brisbane"
stopifnot(all(unique(ep$team) %in% unique(raw$team)))
ep <- merge(ep, raw[, .(player_name, team, date, match_round, player_position)],
            by = c("player_name","team","date"), all.x = TRUE)
stopifnot(nrow(ep) == 9522, !anyNA(ep$match_round))
cat("2026 rows", nrow(ep), " matches", uniqueN(ep$match_id), " rounds",
    paste(range(ep$match_round), collapse = "-"), "\n")

prep <- prepare_season_sim(ep)
keys <- unique(prep$player_key); key_id <- match(prep$player_key, keys)
rounds <- sort(unique(ep$match_round)); R <- length(rounds)
round_of_row <- match(ep$match_round, rounds)
np <- length(keys)

# V: one row per (player, round) cell, one column per sim. Flat so every
# aggregate is a rowsum(), never an apply() over a 3-D array (the apply version
# made ~700MB copies per pass and got OOM-killed on 2026-09-20).
cell <- (key_id - 1L) * R + round_of_row           # row index in V for each ep row
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
by_player <- function(M) { storage.mode(M) <- "integer"; rowsum(M, cell_pid, reorder = TRUE) }  # (np*R) x sims -> np x sims
totals <- by_player(V)
rownames(totals) <- keys
pname <- sub("_[^_]+$", "", keys); pteam <- sub("^.*_", "", keys)
alias <- c("Jordan De Goey"="Jordan de Goey","Matthew Carroll"="Matt Carroll","Finn O'Sullivan"="Finn OSullivan",
           "Harrison Petty"="Harry Petty","Connor O'Sullivan"="Connor OSullivan","Massimo D'Ambrosio"="Massimo DAmbrosio")
canon <- function(nm) ifelse(nm %in% names(alias), alias[nm], nm)
id <- function(nm) { i <- which(pname == canon(nm)); stopifnot(length(i) == 1); i }

# ---- market probabilities ----------------------------------------------------
rank_of <- function(tot) apply(-tot, 2, rank, ties.method = "min")
rk <- rank_of(totals)
maxv <- apply(totals, 2, max)
p_win   <- rowMeans(totals == rep(maxv, each = np) & rk == 1 & colSums(totals == rep(maxv, each = np)) == 1)
p_topk  <- function(k) rowMeans(rk <= k)

# ordered finishing positions per sim (ties broken by order — only used for exotics)
ord <- apply(totals, 2, order, decreasing = TRUE)

P <- list()
add <- function(market, sel, odds, p) P[[length(P) + 1]] <<- data.table(market, sel, odds, p)

# Outright
out <- c("Nick Daicos"=1.01,"Bailey Smith"=51,"Marcus Bontempelli"=67,"Patrick Cripps"=81,
         "Will Ashcroft"=101,"Jordan Dawson"=151,"Lachie Neale"=151,"Max Gawn"=151,
         "Harry Sheezel"=251,"Isaac Heeney"=251,"Izak Rankine"=251,"Jai Newcombe"=251,
         "Kysaiah Pickett"=251,"Luke Jackson"=251)
for (n in names(out)) add("Outright", n, out[n], p_win[id(n)])

# Without Daicos
nd <- totals[-id("Nick Daicos"), ]
mx <- apply(nd, 2, max); one <- colSums(nd == rep(mx, each = nrow(nd))) == 1
pw_nd <- rowMeans(nd == rep(mx, each = nrow(nd)) & rep(one, each = nrow(nd)))
names(pw_nd) <- pname[-id("Nick Daicos")]
wo <- c("Bailey Smith"=1.72,"Marcus Bontempelli"=2.5,"Patrick Cripps"=21,"Max Gawn"=41,
        "Will Ashcroft"=41,"Jordan Dawson"=67,"Isaac Heeney"=81,"Jai Newcombe"=81,
        "Lachie Neale"=81,"Harry Sheezel"=101,"Luke Jackson"=101,"Chad Warner"=151)
for (n in names(wo)) add("Most votes w/o Daicos", n, wo[n], pw_nd[n])

# Top 3/5/10/20
t3 <- c("Bailey Smith"=1.07,"Marcus Bontempelli"=1.15,"Patrick Cripps"=6,"Will Ashcroft"=13,
        "Isaac Heeney"=26,"Jai Newcombe"=26,"Jordan Dawson"=26,"Lachie Neale"=31,"Max Gawn"=34,
        "Harry Sheezel"=67,"Jason Horne-Francis"=101,"Kysaiah Pickett"=101,
        "Nasiah Wanganeen-Milera"=101,"Zak Butters"=101)
t5 <- c("Bailey Smith"=1.01,"Marcus Bontempelli"=1.03,"Patrick Cripps"=1.72,"Will Ashcroft"=2,
        "Isaac Heeney"=3.75,"Jordan Dawson"=4.5,"Lachie Neale"=5.5,"Jai Newcombe"=6,"Max Gawn"=6,
        "Harry Sheezel"=10,"Nasiah Wanganeen-Milera"=12,"Izak Rankine"=19,
        "Jason Horne-Francis"=26,"Zak Butters"=34,"Kysaiah Pickett"=51,"Chad Warner"=67,
        "Ed Richards"=101,"Luke Davies-Uniacke"=101,"Luke Jackson"=101,"Max Holmes"=101)
t10 <- c("Patrick Cripps"=1.05,"Will Ashcroft"=1.07,"Jordan Dawson"=1.31,"Lachie Neale"=1.42,
         "Isaac Heeney"=1.43,"Jai Newcombe"=1.61,"Max Gawn"=1.83,"Nasiah Wanganeen-Milera"=2,
         "Izak Rankine"=2.15,"Harry Sheezel"=2.5,"Jason Horne-Francis"=3.5,"Kysaiah Pickett"=4.5,
         "Zak Butters"=4.75,"Chad Warner"=8,"Ed Richards"=10,"Luke Davies-Uniacke"=13,
         "Luke Jackson"=13,"Andrew Brayshaw"=41,"Caleb Serong"=41,"Sam Walsh"=51,
         "Clayton Oliver"=71,"Max Holmes"=101,"Zac Bailey"=101)
t20 <- c("Izak Rankine"=1.01,"Jason Horne-Francis"=1.02,"Kysaiah Pickett"=1.03,"Max Gawn"=1.04,
         "Harry Sheezel"=1.11,"Zak Butters"=1.15,"Ed Richards"=1.34,"Luke Davies-Uniacke"=1.46,
         "Chad Warner"=1.55,"Luke Jackson"=1.9,"Max Holmes"=1.95,"Errol Gulden"=2.8,
         "Caleb Serong"=3.2,"Sam Walsh"=3.5,"Clayton Oliver"=4,"Andrew Brayshaw"=4.4,
         "Matt Rowell"=8.25,"Zac Bailey"=10.5,"Shai Bolton"=12,"Brodie Grundy"=14,
         "Murphy Reid"=16,"Noah Anderson"=19,"Tom Sparrow"=19,"Christian Petracca"=21,
         "Finn Callaghan"=41)
for (k in c(3,5,10,20)) { o <- get(paste0("t",k)); pk <- p_topk(k)
  for (n in names(o)) add(paste0("Top ",k), n, o[n], pk[id(n)]) }

# Exotics
first <- pname[ord[1,]]; second <- pname[ord[2,]]; third <- pname[ord[3,]]
add("Quinella", "Smith/Daicos", 1.67, mean((first=="Nick Daicos"&second=="Bailey Smith")|(first=="Bailey Smith"&second=="Nick Daicos")))
add("Quinella", "Bontempelli/Daicos", 2.4, mean((first=="Nick Daicos"&second=="Marcus Bontempelli")|(first=="Marcus Bontempelli"&second=="Nick Daicos")))
add("Quinella", "Daicos/Cripps", 21, mean((first=="Nick Daicos"&second=="Patrick Cripps")|(first=="Patrick Cripps"&second=="Nick Daicos")))
add("Quinella", "Gawn/Daicos", 41, mean((first=="Nick Daicos"&second=="Max Gawn")|(first=="Max Gawn"&second=="Nick Daicos")))
add("Quinella", "Daicos/Ashcroft", 41, mean((first=="Nick Daicos"&second=="Will Ashcroft")|(first=="Will Ashcroft"&second=="Nick Daicos")))
add("Exacta", "Daicos/Smith", 1.73, mean(first=="Nick Daicos"&second=="Bailey Smith"))
add("Exacta", "Daicos/Bontempelli", 2.5, mean(first=="Nick Daicos"&second=="Marcus Bontempelli"))
add("Exacta", "Daicos/Cripps", 21, mean(first=="Nick Daicos"&second=="Patrick Cripps"))
add("Exacta", "Daicos/Gawn", 41, mean(first=="Nick Daicos"&second=="Max Gawn"))
add("Exacta", "Daicos/Ashcroft", 41, mean(first=="Nick Daicos"&second=="Will Ashcroft"))
add("Trifecta", "Daicos/Smith/Bontempelli", 2.3, mean(first=="Nick Daicos"&second=="Bailey Smith"&third=="Marcus Bontempelli"))
add("Trifecta", "Daicos/Bontempelli/Smith", 3.2, mean(first=="Nick Daicos"&second=="Marcus Bontempelli"&third=="Bailey Smith"))
add("Trifecta", "Daicos/Smith/Cripps", 10, mean(first=="Nick Daicos"&second=="Bailey Smith"&third=="Patrick Cripps"))
add("Trifecta", "Daicos/Smith/Gawn", 56, mean(first=="Nick Daicos"&second=="Bailey Smith"&third=="Max Gawn"))
top3set <- function(a,b,c) mean(first %in% c(a,b,c) & second %in% c(a,b,c) & third %in% c(a,b,c))
add("Boxed trifecta", "Smith/Bontempelli/Daicos", 1.33, top3set("Bailey Smith","Marcus Bontempelli","Nick Daicos"))
add("Boxed trifecta", "Smith/Daicos/Cripps", 9, top3set("Bailey Smith","Nick Daicos","Patrick Cripps"))
add("Boxed trifecta", "Smith/Gawn/Daicos", 51, top3set("Bailey Smith","Max Gawn","Nick Daicos"))
fourth <- pname[ord[4,]]
top4set <- function(...) { s <- c(...); mean(first %in% s & second %in% s & third %in% s & fourth %in% s) }
add("Boxed pick4", "Smith/Bont/Daicos/Cripps", 3.3, top4set("Bailey Smith","Marcus Bontempelli","Nick Daicos","Patrick Cripps"))
add("Boxed pick4", "Smith/Bont/Daicos/Ashcroft", 4.33, top4set("Bailey Smith","Marcus Bontempelli","Nick Daicos","Will Ashcroft"))
add("Boxed pick4", "Smith/Bont/Gawn/Daicos", 15, top4set("Bailey Smith","Marcus Bontempelli","Max Gawn","Nick Daicos"))
add("Pick4 exact", "Daicos/Smith/Bont/Cripps", 7, mean(first=="Nick Daicos"&second=="Bailey Smith"&third=="Marcus Bontempelli"&fourth=="Patrick Cripps"))
add("Pick4 exact", "Daicos/Smith/Bont/Ashcroft", 9, mean(first=="Nick Daicos"&second=="Bailey Smith"&third=="Marcus Bontempelli"&fourth=="Will Ashcroft"))
add("Pick4 exact", "Daicos/Bont/Smith/Ashcroft", 9, mean(first=="Nick Daicos"&second=="Marcus Bontempelli"&third=="Bailey Smith"&fourth=="Will Ashcroft"))

# Winning margin
srt <- apply(totals, 2, function(x) sort(x, decreasing = TRUE)[1:2]); marg <- srt[1,] - srt[2,]
add("Margin", "Tie", 81, mean(marg == 0)); add("Margin", "1-2", 19, mean(marg %in% 1:2))
add("Margin", "3-4", 11, mean(marg %in% 3:4)); add("Margin", "5-6", 7, mean(marg %in% 5:6))
add("Margin", "7-10", 2.65, mean(marg %in% 7:10)); add("Margin", "11+", 2.12, mean(marg >= 11))

# Daicos props
d <- id("Nick Daicos"); dv <- V[cell_pid == d, ]; dt_tot <- totals[d, ]   # dv: R x sims
late <- which(rounds >= 17); late_cells <- cell_rnd >= 17
add("Hub", "Daicos 16+ in R17-24", 1.5, mean(colSums(dv[late, ]) >= 16))
add("Hub", "Daicos 18+ in R17-24", 4, mean(colSums(dv[late, ]) >= 18))
add("Hub", "Daicos polls in 18+ games", 1.75, mean(colSums(dv > 0) >= 18))
add("Hub", "Daicos polls in 19+ games", 4, mean(colSums(dv > 0) >= 19))
add("Hub", "Daicos polls in 20+ games", 13, mean(colSums(dv > 0) >= 20))
add("Hub", "Daicos 3 votes in 12+ games", 2.75, mean(colSums(dv == 3) >= 12))
add("Hub", "Daicos 3 votes in 15+ games", 6, mean(colSums(dv == 3) >= 15))
for (t in c(45,46,47,48,49,50)) add("Hub", paste0("Daicos ", t, "+ votes"),
  c(`45`=2.25,`46`=3,`47`=4,`48`=7,`49`=11,`50`=13)[as.character(t)], mean(dt_tot >= t))
wm <- function(k) mean(first == "Nick Daicos" & marg >= k)
add("Hub", "Daicos wins by 10+", 1.8, wm(10)); add("Hub", "Daicos wins by 12+", 2.75, wm(12)); add("Hub", "Daicos wins by 15+", 6, wm(15))
trio <- totals[id("Nick Daicos"),] + totals[id("Marcus Bontempelli"),] + totals[id("Bailey Smith"),]
add("Hub", "Daicos+Bont+Smith 110+", 3, mean(trio >= 110))
add("Hub", "Daicos brothers 55+", 5, mean(dt_tot + totals[id("Josh Daicos"),] >= 55))
coll <- totals[pteam == "Collingwood" & pname != "Nick Daicos", ]
add("Hub", "Daicos 40+ more than next Collingwood", 6, mean(dt_tot - apply(coll, 2, max) >= 40))
cum <- function(r) { keep <- cell_rnd <= r; rowsum(V[keep, ], cell_pid[keep], reorder = TRUE) }
lead_at <- function(r) { cm <- cum(r); mx <- apply(cm, 2, max); one <- colSums(cm == rep(mx, each = np)) == 1
  rowMeans(cm == rep(mx, each = np) & rep(one, each = np)) }
l6 <- lead_at(6); l10 <- lead_at(10); l15 <- lead_at(15)
ld <- function(r) { cm <- cum(r); mx <- apply(cm, 2, max); cm[d, ] == mx & colSums(cm == rep(mx, each = np)) == 1 }
add("Hub", "Daicos leads after R6, R10 and R15", 3.75, mean(ld(6) & ld(10) & ld(15)))
tm_tot <- rowsum(totals, pteam)
tmx <- apply(tm_tot, 2, max); tone <- colSums(tm_tot == rep(tmx, each = nrow(tm_tot))) == 1
p_team <- rowMeans(tm_tot == rep(tmx, each = nrow(tm_tot)) & rep(tone, each = nrow(tm_tot)))
add("Hub", "Sydney most team votes", 1.72, p_team["Sydney"]); add("Hub", "Fremantle most team votes", 2.1, p_team["Fremantle"])
er <- tm_tot["Essendon", ] + tm_tot["Richmond", ]
add("Hub", "Daicos > Essendon+Richmond", 1.62, mean(dt_tot > er))
add("Hub", "Top 4 all 30+", 6, mean(apply(totals, 2, function(x) sort(x, decreasing = TRUE)[4]) >= 30))
f5 <- c("Nick Daicos","Bailey Smith","Marcus Bontempelli","Patrick Cripps","Will Ashcroft")
fifth <- pname[ord[5,]]
add("Hub", "First 5 any order (Daicos/Smith/Bont/Cripps/Ashcroft)", 10,
    mean(first %in% f5 & second %in% f5 & third %in% f5 & fourth %in% f5 & fifth %in% f5))
add("Hub", "Smith or Bont outright", 26, p_win[id("Bailey Smith")] + p_win[id("Marcus Bontempelli")])

# Leader after round N
for (r in c(6,10,15)) { pl <- get(paste0("l", r))
  o <- list(`6`=c("Nick Daicos"=1.6,"Zak Butters"=5,"Bailey Smith"=5.75,"Harry Sheezel"=17,"Marcus Bontempelli"=19),
            `10`=c("Nick Daicos"=2.5,"Bailey Smith"=3.3,"Marcus Bontempelli"=4.2,"Zak Butters"=7.5,"Lachie Neale"=34),
            `15`=c("Nick Daicos"=1.4,"Marcus Bontempelli"=3.75,"Bailey Smith"=10.5,"Isaac Heeney"=46))[[as.character(r)]]
  for (n in names(o)) add(paste0("Leader after R", r), n, o[n], pl[id(n)]) }

# Most votes last 8 rounds
l8 <- rowsum(V[late_cells, ], cell_pid[late_cells], reorder = TRUE)
mx8 <- apply(l8, 2, max); one8 <- colSums(l8 == rep(mx8, each = np)) == 1
p_l8 <- rowMeans(l8 == rep(mx8, each = np) & rep(one8, each = np))
o8 <- c("Nick Daicos"=2,"Izak Rankine"=2.25,"Will Ashcroft"=10,"Errol Gulden"=15,"Bailey Smith"=26,"Patrick Cripps"=51,"Jordan Dawson"=101)
for (n in names(o8)) add("Most votes R17-24", n, o8[n], p_l8[id(n)])

# Most 3-vote games / most games polled
n3 <- by_player(V == 3L); ng <- by_player(V > 0L)
uniq_max <- function(m) { mx <- apply(m, 2, max); one <- colSums(m == rep(mx, each = np)) == 1
  rowMeans(m == rep(mx, each = np) & rep(one, each = np)) }
p3 <- uniq_max(n3); pg <- uniq_max(ng)
for (n in c("Nick Daicos","Marcus Bontempelli","Bailey Smith")) add("Most 3-vote games", n,
  c("Nick Daicos"=1.03,"Marcus Bontempelli"=23,"Bailey Smith"=26)[n], p3[id(n)])
for (n in c("Nick Daicos","Bailey Smith","Marcus Bontempelli")) add("Most games polled", n,
  c("Nick Daicos"=1.02,"Bailey Smith"=15,"Marcus Bontempelli"=101)[n], pg[id(n)])

# Round of declared winner: first round where winner's cum > every rival's cum + 3 * rival's remaining games
games_left <- sapply(rounds, function(r) tabulate(key_id[ep$match_round > r], np))  # np x R
decl <- rep(NA_integer_, N_SIMS); w <- ord[1, ]; ws <- cbind(w, seq_len(N_SIMS))
cm <- matrix(0L, np, N_SIMS)
for (j in seq_len(R)) {
  cm <- cm + V[cell_rnd == rounds[j], ]
  thr <- cm + 3L * games_left[, j]; wv <- cm[ws]; thr[ws] <- -1L
  hit <- is.na(decl) & wv > apply(thr, 2, max)
  decl[hit] <- rounds[j]
}
for (r in 17:24) add("Round of declared winner", paste0("R", r),
  c(`17`=151,`18`=81,`19`=13,`20`=3.5,`21`=2.5,`22`=3.5,`23`=15,`24`=41)[as.character(r)], mean(decl == r))

# Groups
grp <- list(A=c("Patrick Cripps"=2.75,"Will Ashcroft"=3,"Isaac Heeney"=6.5,"Jordan Dawson"=9,"Lachie Neale"=9,"Jai Newcombe"=10),
            B=c("Max Gawn"=3.75,"Harry Sheezel"=4,"Izak Rankine"=5,"Nasiah Wanganeen-Milera"=5,"Jason Horne-Francis"=9,"Kysaiah Pickett"=9),
            C=c("Jacob van Rooyen"=2.5,"Jack Gunston"=3,"Josh Treacy"=6,"Charlie Curnow"=8,"Jye Amiss"=10,"Logan Morris"=26),
            D=c("Lachie Ash"=3.75,"John Noble"=4,"Jarman Impey"=5,"Wayne Milera"=5,"Josh Daicos"=7,"Nick Blakey"=9))
for (g in names(grp)) { ids <- sapply(names(grp[[g]]), id); sub <- totals[ids, ]
  mx <- apply(sub, 2, max); one <- colSums(sub == rep(mx, each = length(ids))) == 1
  pg <- rowMeans(sub == rep(mx, each = length(ids)) & rep(one, each = length(ids)))
  for (i in seq_along(ids)) add(paste0("Group ", g), names(grp[[g]])[i], grp[[g]][i], pg[i]) }

# ---- 3-vote games (one featured match per round; identified by first-listed player) ----
g3 <- list(
  `0`=c("Justin McInerney"=1.25,"Errol Gulden"=3.75,"Isaac Heeney"=13),
  `1`=c("Patrick Cripps"=1.9,"Jagga Smith"=4,"Sam Walsh"=5,"Jayden Short"=7.5,"Tim Taranto"=16,"Sam Lalor"=23),
  `2`=c("Jai Newcombe"=1.15,"Dylan Moore"=7,"Jack Gunston"=11,"Chad Warner"=16,"Tom Barrass"=19),
  `3`=c("Bailey Smith"=1.01,"Josh Rachele"=15,"Max Holmes"=18),
  `4`=c("Will Ashcroft"=1.06,"Zac Bailey"=9.25,"Lachie Neale"=15,"Logan Morris"=29),
  `5`=c("Josh Rachele"=1.04,"Sam Berry"=12,"Izak Rankine"=17,"Jordan Dawson"=23,"Patrick Cripps"=26),
  `6`=c("Nick Daicos"=1.01,"Harry Perryman"=23),
  `7`=c("Charlie Curnow"=1.07,"Marcus Bontempelli"=11,"Brodie Grundy"=13,"Chad Warner"=14),
  `8`=c("Connor Macdonald"=1.7,"Nick Daicos"=4,"Jai Newcombe"=5,"Dan Houston"=7.5),
  `9`=c("Luke Jackson"=1.5,"Jack Gunston"=5,"Caleb Serong"=5.5,"Michael Frederick"=14,"Jai Newcombe"=19,"Andrew Brayshaw"=23),
  `10`=c("Shaun Mannagh"=1.29,"Bailey Smith"=3.3,"Tom Stewart"=19),
  `11`=c("Blake Hardwick"=2.2,"Jordan Dawson"=3.1,"Jarman Impey"=6,"Wayne Milera"=11,"Josh Weddle"=14,"Sam Berry"=14,"Cam Mackenzie"=19,"Jai Newcombe"=21,"Josh Ward"=26),
  `12`=c("James Sicily"=1.57,"Nick Watson"=6.5,"Cam Mackenzie"=7,"Jack Gunston"=9,"Jai Newcombe"=10,"Connor Macdonald"=18,"Massimo D'Ambrosio"=19),
  `13`=c("Jordan Dawson"=1.08,"Patrick Dangerfield"=7,"Daniel Curtin"=18,"Bailey Smith"=19),
  `14`=c("Jordan Dawson"=1.01,"Sam Berry"=15),
  `15`=c("Luke Jackson"=1.06,"Andrew Brayshaw"=6.5),
  `16`=c("Chad Warner"=1.24,"Will Ashcroft"=4.3,"Josh Dunkley"=17,"Zac Bailey"=21),
  `17`=c("Patrick Dangerfield"=2.3,"Will Ashcroft"=2.3,"Kai Lohmann"=9,"Zac Bailey"=11,"Lachie Neale"=15,"Josh Dunkley"=16,"Bailey Smith"=23,"Max Holmes"=23),
  `18`=c("Josh Treacy"=2.8,"Murphy Reid"=3,"Andrew Brayshaw"=9,"Caleb Serong"=9,"Charlie Curnow"=10,"Shai Bolton"=10,"Alex Pearce"=16,"Jordan Clark"=18),
  `19`=c("Bailey Smith"=1.75,"Rowan Marshall"=2.35,"Gryan Miers"=8,"James Worpel"=26),
  `20`=c("Nick Daicos"=1.01,"Darcy Cameron"=21),
  `21`=c("Oliver Dempsey"=1.3,"Nick Daicos"=7,"Daniel McStay"=8,"Bailey Smith"=10),
  `22`=c("Luke Davies-Uniacke"=1.01,"Paul Curtis"=13),
  `23`=c("Caleb Serong"=1.75,"Izak Rankine"=2.1,"Jye Amiss"=17,"Shai Bolton"=19,"Andrew Brayshaw"=23),
  `24`=c("Touk Miller"=2.25,"Matt Rowell"=3.4,"Noah Anderson"=5,"John Noble"=9,"Max Hall"=10.5,"Bradley Hill"=12.5))
for (r in names(g3)) { o <- g3[[r]]
  mid <- ep[match_round == as.integer(r) & player_name == names(o)[1], match_id]; stopifnot(length(mid) == 1)
  mp <- ep[match_id == mid]; lab <- paste0("3 votes R", r, " (", sub("^[0-9-]+_", "", mid), ")")
  for (n in names(o)) { p <- mp[player_name == canon(n), ensemble_prob_3]; if (length(p) != 1) { cat("MISSING", n, "in", mid, "\n"); next }
    add(lab, n, o[n], p) }
  add(lab, "Any other player", 11, 1 - sum(mp[player_name %in% canon(names(o)), ensemble_prob_3])) }

# ---- team most votes (some markets exclude the favourite) --------------------
team_mkt <- list(
  list(t="Adelaide", ex=NULL, o=c("Jordan Dawson"=1.4,"Izak Rankine"=2.7,"Sam Berry"=151,"Wayne Milera"=151)),
  list(t="Brisbane", ex=NULL, o=c("Will Ashcroft"=1.4,"Lachie Neale"=2.75,"Zac Bailey"=151)),
  list(t="Carlton", ex="Patrick Cripps", o=c("Sam Walsh"=1.2,"Jagga Smith"=6,"George Hewett"=7.5)),
  list(t="Collingwood", ex="Nick Daicos", o=c("Jordan De Goey"=1.62,"Josh Daicos"=3,"Darcy Cameron"=9,"Scott Pendlebury"=26,"Daniel McStay"=51,"Dan Houston"=81,"Jack Crisp"=151)),
  list(t="Essendon", ex=NULL, o=c("Zach Merrett"=1.33,"Sam Durham"=6,"Archie Roberts"=8,"Darcy Parish"=10,"Peter Wright"=151)),
  list(t="Fremantle", ex=NULL, o=c("Luke Jackson"=1.91,"Caleb Serong"=2.88,"Andrew Brayshaw"=7.5,"Shai Bolton"=13,"Murphy Reid"=15,"Josh Treacy"=41,"Jye Amiss"=151)),
  list(t="Geelong", ex=c("Bailey Smith","Max Holmes"), o=c("Shaun Mannagh"=1.2,"Oliver Dempsey"=5.5,"Jeremy Cameron"=15,"Patrick Dangerfield"=67,"Tom Stewart"=151,"Gryan Miers"=151)),
  list(t="Gold Coast", ex=NULL, o=c("Matt Rowell"=1.91,"Christian Petracca"=2.7,"Noah Anderson"=6,"Touk Miller"=15,"John Noble"=81)),
  list(t="Greater Western Sydney", ex=NULL, o=c("Clayton Oliver"=1.72,"Finn Callaghan"=3.75,"Toby Greene"=4.5,"Lachie Ash"=18)),
  list(t="Hawthorn", ex="Jai Newcombe", o=c("Jack Gunston"=2.25,"Nick Watson"=2.75,"Jarman Impey"=6,"Connor Macdonald"=7,"Jack Ginnivan"=81,"James Sicily"=151,"Will Day"=151)),
  list(t="Melbourne", ex=NULL, o=c("Max Gawn"=1.4,"Kysaiah Pickett"=2.63,"Tom Sparrow"=101)),
  list(t="North Melbourne", ex=NULL, o=c("Harry Sheezel"=1.3,"Luke Davies-Uniacke"=3.25)),
  list(t="Port Adelaide", ex=NULL, o=c("Jason Horne-Francis"=1.45,"Zak Butters"=2.63)),
  list(t="Richmond", ex="Tim Taranto", o=c("Jack Ross"=2.75,"Noah Balta"=2.75,"Seth Campbell"=8,"Jayden Short"=10,"Kane McAuliffe"=13,"Sam Lalor"=15)),
  list(t="St Kilda", ex="Nasiah Wanganeen-Milera", o=c("Max Hall"=1.25,"Bradley Hill"=7,"Hugo Garcia"=10,"Jack Sinclair"=21,"Rowan Marshall"=26)),
  list(t="Sydney", ex=NULL, o=c("Isaac Heeney"=1.17,"Chad Warner"=5,"Errol Gulden"=26,"Brodie Grundy"=81)),
  list(t="West Coast", ex="Harley Reid", o=c("Tim Kelly"=1.91,"Jake Waterman"=3,"Elliot Yeo"=18,"Reuben Ginbey"=21)),
  list(t="Western Bulldogs", ex=c("Marcus Bontempelli","Ed Richards"), o=c("Matthew Kennedy"=3.25,"Aaron Naughton"=4,"Joel Freijah"=6,"Ryley Sanders"=6,"Bailey Dale"=8,"Tom Liberatore"=10,"Tim English"=34)))
stopifnot(all(sapply(team_mkt, `[[`, "t") %in% pteam))
for (tm in team_mkt) { rows <- which(pteam == tm$t & !(pname %in% tm$ex)); sub <- totals[rows, ]
  mx <- apply(sub, 2, max); one <- colSums(sub == rep(mx, each = length(rows))) == 1
  pt <- rowMeans(sub == rep(mx, each = length(rows)) & rep(one, each = length(rows))); names(pt) <- pname[rows]
  lab <- paste0(tm$t, " most votes", if (length(tm$ex)) paste0(" w/o ", paste(tm$ex, collapse = "/")) else "")
  for (n in names(tm$o)) { if (!canon(n) %in% names(pt)) { cat("MISSING", n, tm$t, "\n"); next }; add(lab, n, tm$o[n], pt[canon(n)]) } }

# ---- to poll N+ votes ---------------------------------------------------------
pollN <- list(
  `10`=c("Christian Petracca"=1.01,"Harley Reid"=1.01,"Sam Walsh"=1.01,"Andrew Brayshaw"=1.02,"Brodie Grundy"=1.06,"Shai Bolton"=1.06,"Matt Rowell"=1.09,"Clayton Oliver"=1.11,"Zac Bailey"=1.13,"Finn Callaghan"=1.25,"Noah Anderson"=1.29,"Murphy Reid"=1.33,"Jacob van Rooyen"=1.34,"Tom Sparrow"=1.37,"Max Hall"=1.49,"Jack Gunston"=1.54,"Jagga Smith"=1.59,"Shaun Mannagh"=1.76,"Toby Greene"=1.87,"George Hewett"=2.03,"Nick Watson"=2.03,"Tim Taranto"=2.2,"Josh Treacy"=2.35,"Sam Berry"=2.7,"Charlie Curnow"=2.75,"Lachie Ash"=3.2,"Jye Amiss"=3.5,"Touk Miller"=3.5,"Jordan De Goey"=3.9,"Jarman Impey"=4.25,"Wayne Milera"=4.8,"Connor Macdonald"=4.9,"Josh Daicos"=6.25,"Zach Merrett"=7.25,"Bradley Hill"=7.5,"Jack Steele"=7.5,"John Noble"=7.5,"Hugo Garcia"=8.75,"Josh Ward"=13,"Nick Blakey"=15,"Logan Morris"=19),
  `15`=c("Max Gawn"=1.01,"Harry Sheezel"=1.02,"Zak Butters"=1.02,"Ed Richards"=1.08,"Errol Gulden"=1.08,"Luke Davies-Uniacke"=1.08,"Chad Warner"=1.11,"Max Holmes"=1.18,"Luke Jackson"=1.29,"Caleb Serong"=1.66,"Harley Reid"=1.67,"Sam Walsh"=1.72,"Andrew Brayshaw"=2.25,"Clayton Oliver"=3.25,"Matt Rowell"=3.3,"Zac Bailey"=4.1,"Shai Bolton"=4.3,"Brodie Grundy"=4.4,"Murphy Reid"=5.7,"Christian Petracca"=5.8,"Tom Sparrow"=7,"Noah Anderson"=7.25,"Finn Callaghan"=11,"Jack Gunston"=14,"Jagga Smith"=16,"Max Hall"=21),
  `20`=c("Jordan Dawson"=1.03,"Lachie Neale"=1.06,"Isaac Heeney"=1.07,"Jai Newcombe"=1.1,"Nasiah Wanganeen-Milera"=1.15,"Izak Rankine"=1.19,"Max Gawn"=1.27,"Jason Horne-Francis"=1.42,"Harry Sheezel"=1.54,"Kysaiah Pickett"=1.87,"Zak Butters"=1.99,"Ed Richards"=2.85,"Chad Warner"=3.45,"Luke Davies-Uniacke"=3.7,"Luke Jackson"=6,"Harley Reid"=11,"Caleb Serong"=11.5,"Andrew Brayshaw"=12.5,"Max Holmes"=12.5,"Sam Walsh"=13),
  `25`=c("Patrick Cripps"=1.41,"Will Ashcroft"=1.87,"Isaac Heeney"=2.7,"Jordan Dawson"=2.7,"Lachie Neale"=3.15,"Jai Newcombe"=3.35,"Max Gawn"=3.75,"Nasiah Wanganeen-Milera"=5.8,"Harry Sheezel"=6.25,"Izak Rankine"=10.5,"Jason Horne-Francis"=13,"Zak Butters"=16,"Chad Warner"=29,"Kysaiah Pickett"=34),
  `30`=c("Bailey Smith"=1.14,"Marcus Bontempelli"=1.28,"Patrick Cripps"=6,"Will Ashcroft"=21,"Jai Newcombe"=26,"Jordan Dawson"=26,"Lachie Neale"=41,"Max Gawn"=41,"Isaac Heeney"=46))
for (k in names(pollN)) for (n in names(pollN[[k]])) { i <- which(pname == canon(n)); if (length(i) != 1) { cat("MISSING/AMBIG", n, "\n"); next }
  add(paste0("Poll ", k, "+"), n, pollN[[k]][n], mean(totals[i, ] >= as.integer(k))) }

pollA <- c("Stephen Coniglio"=1.09,"Gryan Miers"=1.1,"Tom Liberatore"=1.1,"Jarrod Witts"=1.11,"Bailey Dale"=1.14,"Dylan Moore"=1.14,"Francis Evans"=1.14,"Jake Bowey"=1.14,"Bayley Fritsch"=1.15,"Tim Membrey"=1.15,"Caleb Windsor"=1.16,"Dan Houston"=1.16,"Mitch Owens"=1.16,"Brodie Kemp"=1.18,"Cam Mackenzie"=1.19,"James Peatling"=1.19,"Matthew Carroll"=1.19,"Nick Larkey"=1.21,"Hayden Young"=1.23,"Nick Bryan"=1.23,"Tom Papley"=1.23,"Callum Mills"=1.24,"Matthew Johnson"=1.26,"Dylan Stephens"=1.27,"Mark Keane"=1.27,"Colby McKercher"=1.28,"Jack Higgins"=1.31,"Joe Richards"=1.31,"Ben Ainsworth"=1.35,"Brent Daniels"=1.38,"Daniel Rioli"=1.39,"Darcy Wilson"=1.4,"Jack Darling"=1.42,"Harris Andrews"=1.43,"Luke Ryan"=1.43,"Milan Murdock"=1.43,"Tom McCartin"=1.44,"Beau McCreery"=1.48,"Charlie Cameron"=1.48,"Cooper Sharman"=1.48,"Reuben Ginbey"=1.51,"Rory Laird"=1.51,"Tom McCarthy"=1.51,"Tom Atkins"=1.53,"Joel Fitzgerald"=1.57,"Zeke Uwland"=1.57,"Daniel Curtin"=1.59,"Jack Crisp"=1.64,"Phoenix Gothard"=1.65,"Jayden Short"=1.69,"Levi Ashcroft"=1.69,"Marc Pittonet"=1.71,"Steele Sidebottom"=1.73,"Harvey Thomas"=1.74,"Will Setterfield"=1.74,"Karl Amon"=1.75,"Ollie Wines"=1.75,"Ryan Lester"=1.75,"Oisin Mullin"=1.77,"Mark Blicavs"=1.8,"James Rowbottom"=1.81,"Finn O'Sullivan"=1.84,"Tom De Koning"=1.84,"Harrison Petty"=1.85,"Lachlan Bramble"=1.86,"Oscar McDonald"=1.88,"Elliot Yeo"=1.89,"Josh Worrell"=1.89,"Michael Frederick"=1.89,"Patrick Retschko"=1.89,"Harry Perryman"=1.92,"Jai Serong"=1.94,"Braeden Campbell"=2.05,"Jamie Elliott"=2.05,"Kane Farrell"=2.1,"Jack Macrae"=2.12,"Peter Wright"=2.12,"Talor Byrne"=2.12,"Esava Ratugolea"=2.15,"Charlie Comben"=2.2,"Jordon Sweet"=2.2,"Lloyd Meek"=2.2,"Sam Flanders"=2.3,"George Wardlaw"=2.35,"Patrick Lipinski"=2.35,"Sam Lalor"=2.4,"Kane McAuliffe"=2.5,"Alex Neal-Bullen"=2.55,"Willem Duursma"=2.65,"Alex Pearce"=2.7,"Seth Campbell"=2.8,"Tim English"=2.8,"Marcus Herbert"=2.85,"Riley Bice"=2.85,"Josh Battle"=2.95,"Ben Keays"=3.1,"Ben Long"=3.15,"Edward Allan"=3.15,"Connor O'Sullivan"=3.2,"Cam Rayner"=3.25,"Connor Idun"=3.25,"Daniel Turner"=3.45,"Darcy Byrne-Jones"=3.45,"James Worpel"=3.45,"Aaron Cadman"=3.5,"Lachlan McAndrew"=3.5,"Tom Barrass"=3.5,"Mabior Chol"=3.75,"Massimo D'Ambrosio"=3.9,"Sam Darcy"=3.9,"Cameron Zurhaar"=4,"Callum Brown"=4.2,"Marcus Windhager"=4.25,"Blake Acres"=4.3,"Jeremy Howe"=4.4,"Trent Rivers"=4.4,"Kieren Briggs"=4.5,"Sam Draper"=4.5,"Josh Weddle"=4.7,"Mac Andrew"=4.9,"Ethan Read"=5.1,"Adam Cerra"=5.3,"Harry Sharp"=5.5,"Conor Nash"=5.9,"Dyson Sharp"=6,"Jake Melksham"=6,"Will Hayward"=6,"Josaia Delana"=6.5,"Deven Robertson"=7.25)
for (n in names(pollA)) { i <- which(pname == canon(n)); if (length(i) != 1) { cat("MISSING/AMBIG", n, "\n"); next }
  add("To poll a vote", n, pollA[n], mean(totals[i, ] >= 1)) }

# ---- Kelly -------------------------------------------------------------------
res <- rbindlist(P)
res[, `:=`(implied = 1 / odds, edge = p * odds - 1, kelly = pmax((p * odds - 1) / (odds - 1), 0))]
res[, `:=`(p = round(p, 4), edge = round(edge, 3), kelly = round(kelly, 4), implied = round(implied, 4))]
setorder(res, -kelly)
fwrite(res, "results/markets/sportsbet_2026_kelly.csv")
cat("\n== positive-edge bets (sorted by full Kelly fraction) ==\n")
print(res[edge > 0], nrow = 100)
cat("\n== biggest negative edges (for context) ==\n"); print(res[order(edge)][1:10])

# ---- artifact data: expected votes per player per round ---------------------
parts <- tstrsplit(ep$match_id, "_", fixed = TRUE)
ev <- ep[, .(player_name, team, match_round, player_position, xv = ensemble_expected_votes,
             p3 = ensemble_prob_3, p_poll = 1 - ensemble_prob_0,
             opp = ifelse(parts[[2]] == team, parts[[3]], parts[[2]]))]
pos <- raw[, .N, by = .(player_name, player_team, player_position)][order(-N)][, .SD[1], by = .(player_name, player_team)]
summ <- data.table(player_name = pname, team = pteam, sim_mean = rowMeans(totals), sim_sd = apply(totals, 1, sd),
                   win_pct = round(100 * p_win, 2), top3_pct = round(100 * p_topk(3), 1),
                   top10_pct = round(100 * p_topk(10), 1), p_lead_r6 = round(100*l6,1), p_lead_r15 = round(100*l15,1))
summ <- merge(summ, pos[, .(player_name, position = player_position)][, .SD[1], by = player_name], by = "player_name", all.x = TRUE)
fwrite(ev[, .(player_name, team, position = player_position, round = match_round, opponent = opp,
              xv = round(xv, 3), p3 = round(p3, 3), p_poll = round(p_poll, 3))], "results/markets/expected_votes_by_round_2026.csv")
fwrite(summ[order(-sim_mean)], "results/markets/player_summary_2026.csv")
cat("wrote", nrow(ev), "player-round rows,", nrow(summ), "players\n")
