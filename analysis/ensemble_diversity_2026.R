# Why did the plain average of 22 entrants beat every one of them, and how much
# of that is reachable by widening OUR ensemble?
#
# POSTMORTEM-2026.md Sec 5 flags this as the only mechanism the 2026 data shows
# adding discrimination. Before paying for a walk-forward test of a new model
# family, this measures what there is to win, using predictions already on disk.
#
# The lever is exact for squared error. For an equal-weight ensemble,
#
#   MSE(ensemble) = mean_i MSE(f_i)  -  mean_i E[(f_i - f_ens)^2]
#                   \_ average member quality _/   \_ diversity _/
#
# (the ambiguity decomposition). So the ensemble is ALWAYS at least as good as
# its average member, and the whole of its advantage is the spread between
# members. That makes "is it worth adding a model" a question about how
# DECORRELATED the new model's errors are, not about how good it is alone.
#
# Note what this can and cannot conclude. We cannot blend with other entrants
# for real -- their predictions do not exist before the deadline. They are used
# here only to measure what a given amount of diversity is worth, which is the
# number that decides whether building our own extra member is justified.
#
# Run: powershell.exe -Command 'Rscript "analysis/ensemble_diversity_2026.R"'

suppressMessages({ library(data.table) })

sh <- fread(sprintf("https://docs.google.com/spreadsheets/d/%s/export?format=csv",
                    "1aVDy2j9ljjIjOmXjRAtKJpznM7l5IVxs_sxuciHbxOw"), skip = "matchId")   # the header row, found by content: the sheet gained a summary row above it after 2026-09-21
pm <- fread("results/markets/postmortem_2026_rows.csv",
            select = c("player_name","team","round","pred","actual"))
sq <- function(x) tolower(gsub("[^a-z]", "", tolower(x)))
ABBR <- c("Adelaide"="ADEL","Brisbane"="BL","Carlton"="CARL","Collingwood"="COLL",
          "Essendon"="ESS","Fremantle"="FRE","Gold Coast"="GCFC","Geelong"="GEEL",
          "Greater Western Sydney"="GWS","Hawthorn"="HAW","Melbourne"="MELB",
          "North Melbourne"="NMFC","Port Adelaide"="PORT","Richmond"="RICH","St Kilda"="STK",
          "Sydney"="SYD","Western Bulldogs"="WB","West Coast"="WCE")
sh[, k := paste(rd, sq(playerName), teamAbbreviation)]
pm[, k := paste(round, sq(player_name), unname(ABBR[team]))]
d <- merge(sh, pm[, .(k, InTheGame = pred, actual)], by = "k")

ENTRANTS <- setdiff(names(sh), c("matchId","rd","teamAbbreviation","playerId",
                                 "playerName","gameRank","Actual","k"))
ENTRANTS <- setdiff(ENTRANTS, "consensus")          # consensus is their mean, not a member
MEMBERS <- c(ENTRANTS, "InTheGame")
ok <- Reduce(`&`, lapply(MEMBERS, function(m) !is.na(d[[m]])))
d <- d[ok]

cat(sprintf("=== coverage: %d rows, %d votes, %d members (%d entrants + ours) ===\n",
            nrow(d), sum(d$actual), length(MEMBERS), length(ENTRANTS)))
stopifnot("rows lost their actuals" = !anyNA(d$actual),
          "members should each predict about 1242 votes over a full season" =
            all(abs(vapply(MEMBERS, function(m) sum(d[[m]]), 1) - 1242) < 250))

M <- as.matrix(d[, ..MEMBERS])
y <- d$actual
mse <- function(p) mean((p - y)^2)
rmse <- function(p) sqrt(mse(p))

# ---- 1. the ambiguity decomposition ------------------------------------------
ens <- rowMeans(M)
avg_mse <- mean(apply(M, 2, mse))
diversity <- mean(apply(M, 2, function(p) mean((p - ens)^2)))
cat("\n=== 1. why the average of everyone wins ===\n")
cat("RMSE, lower is better. The identity below is exact, not an approximation.\n")
cat(sprintf("  average member MSE      %.5f  (RMSE %.5f)\n", avg_mse, sqrt(avg_mse)))
cat(sprintf("  minus diversity         %.5f\n", diversity))
cat(sprintf("  = ensemble MSE          %.5f  (RMSE %.5f)\n", mse(ens), sqrt(mse(ens))))
stopifnot("ambiguity decomposition does not close" = abs(avg_mse - diversity - mse(ens)) < 1e-9)
cat(sprintf("  best single member      RMSE %.5f (%s)\n",
            min(apply(M, 2, rmse)), MEMBERS[which.min(apply(M, 2, rmse))]))
cat(sprintf("  ours alone              RMSE %.5f\n", rmse(d$InTheGame)))

# ---- 2. how decorrelated is everyone, really? --------------------------------
E <- M - y                                            # error matrix
ec <- cor(E)
ours <- "InTheGame"
pairs <- ec[upper.tri(ec)]
cat("\n=== 2. error correlation between members ===\n")
cat("1.00 means two models make the same mistakes and blending them gains nothing.\n")
cat(sprintf("  across all pairs: median %.3f, range %.3f to %.3f\n",
            median(pairs), min(pairs), max(pairs)))
oc <- sort(ec[ours, setdiff(MEMBERS, ours)])
cat(sprintf("  ours against the field: median %.3f\n", median(oc)))
cat("  most decorrelated from us (the kind of model worth adding):\n")
print(data.table(model = names(head(oc, 5)), err_cor = round(head(oc, 5), 3),
                 their_rmse = round(apply(M[, names(head(oc, 5)), drop = FALSE], 2, rmse), 5)))

# ---- 3. what is our OWN blend already getting? -------------------------------
ep <- fread("results/ensemble_probabilities.csv")
ep[, `:=`(xg = xgboost_prob_1 + 2*xgboost_prob_2 + 3*xgboost_prob_3,
          gm = gam_prob_1 + 2*gam_prob_2 + 3*gam_prob_3,
          ens = ensemble_expected_votes)]
# 2026's stored votes are zero-filled, so its errors are scored from the feed
# rows instead; prior seasons carry their real votes in the file.
# Join on player, team AND round. On (player, team) alone the 2026 feed rows fan
# out across every round that player played, which silently inflated the 2026
# RMSEs to 0.54 and would have been reported as a finding. The assertion below
# is what catches it.
r26 <- fread("data/brownlow_datathon_dataset.csv",
             select = c("match_date","season","match_round","player_first_name",
                        "player_last_name","player_team"))[season == 2026]
r26[, `:=`(player_name = paste(player_first_name, player_last_name),
           date = as.IDate(match_date, format = "%d/%m/%Y"),
           team = sub(" Lions$", "", player_team))]
r26 <- merge(r26[, .(player_name, team, date, round = match_round)],
             pm[, .(player_name, team, round, feed = actual)],
             by = c("player_name","team","round"))
ep[, date := as.IDate(date)]
ep <- merge(ep, r26[, .(player_name, team, date, feed)],
            by = c("player_name","team","date"), all.x = TRUE)
ep[, act := fifelse(season == 2026, as.numeric(feed), as.numeric(brownlow_votes))]
stopifnot("2026 actuals did not attach cleanly" =
            ep[season == 2026, sum(act) == 1242 && .N == 9522])
cat("\n=== 3. our own two members: how much diversity do we actually have? ===\n")
cat("err_cor 1.00 would mean the two make identical mistakes and blending is pointless.\n")
cat("blend_gain is our ensemble's RMSE improvement over its own BETTER member.\n")
own <- ep[!is.na(act), {
  ex <- xg - act; eg <- gm - act
  .(pred_cor = round(cor(xg, gm), 3), err_cor = round(cor(ex, eg), 3),
    xgb_rmse = round(sqrt(mean(ex^2)), 5), gam_rmse = round(sqrt(mean(eg^2)), 5),
    ens_rmse = round(sqrt(mean((ens - act)^2)), 5),
    blend_gain = round(min(sqrt(mean(ex^2)), sqrt(mean(eg^2))) - sqrt(mean((ens - act)^2)), 5))
}, by = season][order(season)]
print(own)
# The 0.975 in this table is POSTMORTEM Sec 5's headline, so it gets a file.
fwrite(own, "analysis/harness/ensemble_own_members.csv")
stopifnot("2026 ensemble RMSE does not match the known 0.29140" =
            abs(own[season == 2026, ens_rmse] - 0.29140) < 5e-5)
cat("\nNOTE on reading this table: 2015-2025 are seasons the XGBoost was FITTED on,\n")
cat("so its RMSE there is flattered by its own overfit and the negative blend_gain\n")
cat("in those rows is not evidence the GAM hurts. 2026 is the only row the model\n")
cat("never saw, and it is the only one worth drawing a conclusion from.\n")

# ---- 4. what would adding ONE decorrelated member be worth? ------------------
cat("\n=== 4. blending ours with one other member, every option ===\n")
cat("RMSE of a 50/50 blend. Lower is better; gain is against ours alone.\n")
blend <- rbindlist(lapply(setdiff(MEMBERS, ours), function(m) {
  data.table(partner = m, err_cor = ec[ours, m],
             partner_rmse = rmse(d[[m]]),
             blend_rmse = rmse((d[[ours]] + d[[m]]) / 2)) }))
blend[, gain := rmse(d[[ours]]) - blend_rmse]
setorder(blend, -gain)
print(head(blend[, .(partner, err_cor = round(err_cor, 3),
                     partner_rmse = round(partner_rmse, 5),
                     blend_rmse = round(blend_rmse, 5), gain = round(gain, 5))], 8))

# ---- 5. how many members before it stops paying? -----------------------------
cat("\n=== 5. adding members greedily, most decorrelated from us first ===\n")
cat("RMSE of the equal-weight blend as members are added. Lower is better.\n")
order_by_cor <- names(sort(ec[ours, setdiff(MEMBERS, ours)]))
curve <- rbindlist(lapply(0:length(order_by_cor), function(k) {
  set <- c(ours, head(order_by_cor, k))
  data.table(members = length(set), rmse = rmse(rowMeans(M[, set, drop = FALSE]))) }))
curve[, gain := curve$rmse[1] - rmse]
print(curve[members %in% c(1,2,3,4,5,6,8,10,15,22), .(members, rmse = round(rmse, 5),
                                                      gain = round(gain, 5))])
# ---- 6. the target, in closed form -------------------------------------------
# For two members with error sds s1 and s2 and error correlation rho, the
# equal-weight blend has
#     MSE = (s1^2 + s2^2 + 2 rho s1 s2) / 4.
# When the two are equally good this collapses to the tidier
# s * sqrt((1 + rho) / 2), but they rarely are, and using the BETTER member's sd
# for both makes the formula optimistic by about the size of the decidability
# bar -- which is exactly the error an earlier draft of this script made. Keep
# the general form and let the equal-quality case fall out of it.
blend_rmse_at <- function(s1, s2, rho) sqrt((s1^2 + s2^2 + 2 * rho * s1 * s2) / 4)
best_own <- own[season == 2026, pmin(xgb_rmse, gam_rmse)]
pred_blend <- function(rho) blend_rmse_at(best_own, best_own, rho)
cat("\n=== 6. how decorrelated would a second member have to be? ===\n")
cat("Predicted blend RMSE for a second member as good as our best one, at each\n")
cat("error correlation. Lower is better. Ours today sit at 0.975.\n")
tgt <- data.table(rho = c(0.975, 0.94, 0.90, 0.85, 0.82, 0.75, 0.73, 0.70))
tgt[, `:=`(predicted_rmse = round(pred_blend(rho), 5),
           gain = round(best_own - pred_blend(rho), 5))]
print(tgt)
# Check the general form at BOTH points where a real answer exists, using each
# pair's own sds rather than a single assumed one. Two checks, not one, and they
# span the regime the target table extrapolates from.
ours_rmse <- rmse(d[[ours]])
chk <- blend[partner == "bg1"]
own26 <- own[season == 2026]
cat("\nformula checked against every pair where the answer is known:\n")
print(data.table(
  pair = c("ours + bg1 (rho 0.820)", "our own XGBoost + GAM (rho 0.975)"),
  predicted = round(c(blend_rmse_at(ours_rmse, chk$partner_rmse, chk$err_cor),
                      blend_rmse_at(own26$xgb_rmse, own26$gam_rmse, own26$err_cor)), 5),
  measured = round(c(chk$blend_rmse, own26$ens_rmse), 5)))

# Invert the equal-quality case for a partner as good as our best member. The
# target is the best entrant's actual RMSE, a constant read off Sec 1 -- an
# earlier draft wrapped this in a min() against our measured jt blend, which can
# never be the smaller of the two and only made the line look like it was
# comparing something.
BEST_ENTRANT <- 0.27246
rho_star <- 2 * (BEST_ENTRANT / best_own)^2 - 1
cat(sprintf("\nto reach the best entrant's %.5f, a second member as good as our best\n",
            BEST_ENTRANT))
cat(sprintf("would need rho of about %.2f, against the %.3f our XGBoost and GAM\n",
            rho_star, own26$err_cor))
cat("have today.\n")
fwrite(tgt, "analysis/harness/ensemble_diversity_target.csv")
fwrite(blend, "analysis/harness/ensemble_diversity_pairs.csv")
fwrite(curve, "analysis/harness/ensemble_diversity_curve.csv")
cat("\nwritten: analysis/harness/ensemble_diversity_{pairs,curve}.csv\n")
