# Player-name spellings: one table, used everywhere -----------------------------
#
# The datathon data uses short forms ("Harry Petty", "Will Edwards"). The AFL's
# own feed and the community comparison sheet both use full forms ("Harrison
# Petty", "William Edwards"), and add a middle initial where two players share a
# name. Every join between our data and either of those sources trips over the
# same 19 names.
#
# They used to be fixed one at a time, in whichever script hit them: a partial
# list in `analysis/price_markets_2026.R`, a different partial list in
# `analysis/live_count.R`. During the 2026 count that cost a silently dropped
# vote when "Harrison Petty" was read out and only three aliases were loaded.
# The full set is here so a fix lands once.
#
# Derived 2026-09-22 by `analysis/fix_name_joins.R`, which lists every row of
# ours that fails to join and names the club-mate the other source has instead.
# Re-run it after any season rollover: the list grows with debutants.

#' Canonical Player-Name Aliases
#'
#' Maps the squashed (lowercase, letters-only) spelling used by the AFL feed and
#' the community sheet onto the squashed spelling in the datathon data.
#'
#' @return Named character vector: names are the EXTERNAL squashed spelling,
#'   values are OUR squashed spelling.
#' @export
player_name_aliases <- function() {
  a <- c(leonardolombard   = "leolombard",
    harrisonpetty     = "harrypetty",
    baileyjwilliams   = "baileywilliams",
    callummbrown      = "callumbrown",
    thomascole        = "tomcole",
    tomjlynch         = "tomlynch",
    williamedwards    = "willedwards",
    matthewcarroll    = "mattcarroll",
    archermay         = "archiemay",
    williamhayes      = "willhayes",
    christopherscerri = "chrisscerri",
    nicholasmadden    = "nickmadden",
    alixzandertauru   = "alixtauru",
    thomasedwards     = "tomedwards",
    harrisonjones     = "harryjones",
    williammccabe     = "willmccabe",
    olivergreeves     = "olliegreeves",
    williamgreen      = "willgreen",
    mitchellhinge     = "mitchhinge",
    # Found 2026-09-22 by analysis/audit_votes_vs_feed.R on 2021-2025. Note these
    # three run the OTHER way -- the feed uses the short or disambiguated form
    # and the datathon file the long one -- so do not assume a direction.
    matttaberner      = "matthewtaberner",
    # Both Josh Kennedys squash to the same string in our data and are told apart
    # by club in every join, so mapping both feed spellings onto it is correct.
    joshjkennedy      = "joshkennedy",     # West Coast
    joshpkennedy      = "joshkennedy",     # Sydney
    joshuadraper      = "joshdraper")
  # A repeated key would silently resolve to its first value under name lookup.
  if (anyDuplicated(names(a)))
    stop("duplicate alias key(s): ", paste(names(a)[duplicated(names(a))], collapse = ", "))
  a
}

#' Squash a Player Name for Joining
#'
#' Lowercase, letters only. Punctuation and case differ freely between sources
#' and carry no information, so they are removed before any name comparison.
#'
#' @param x Character vector of player names
#' @return Character vector
#' @export
squash_name <- function(x) tolower(gsub("[^a-z]", "", tolower(x)))

#' Translate External Player Names to Our Spelling
#'
#' @param x Character vector of player names as an external source spells them
#' @return Character vector of squashed names in our spelling
#' @export
to_our_name <- function(x) {
  s <- squash_name(x)
  a <- player_name_aliases()
  hit <- s %in% names(a)
  s[hit] <- unname(a[s[hit]])
  s
}
