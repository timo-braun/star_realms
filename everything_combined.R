rm(list=setdiff(ls(), "x"))
setwd("C:/projects/star_realms")
source("./base_game.R")


# game_1 <- game$new(c("Magnum Opus", "Fishstick"))
game_1 <- game$new(c("Player 1", "Player 2"))
# for (i in c(1:10)) {
  cat("\n")
  cat("Turn", i, "\n")
  cat("Current player:",game_1$active_player_id, "\n")
  game_1$play_card(game_1$active_player_id, 1)
  game_1$show_hand(game_1$active_player_id)
  game_1$show_trade_row()
  game_1$buy_card(1)
  game_1$end_turn()
# }

for (i in (1:length(game_1$list_expansion))) {
  print(i)
  # print(game_1$list_expansion[[i]]$object_name)
  if ("Ram" == game_1$list_expansion[[i]]$object_name){
    print(game_1$list_expansion[[i]]$object_name)
  }
}



list_stuff <- c("object_name", "id", "is.ship", "faction", "cost", "money", "attack.condition")

list_stuff[7]
Supply_Bot
Supply_Bot[[list_stuff[4]]]
Space_Station[[list_stuff[1]]]
for (card in all_cards) {
  print(card$id)
  for (i in 1:length(list_stuff)) {
    cat("Getting all relevant information from the card:", list_stuff[i], ":", card[[list_stuff[i]]], "\n")
    print(card[[list_stuff[i]]])
  }
}


# attack = NULL,
# healing = NULL,
# health = NULL,
# draw = NULL,
# discard = NULL,
# destroy.base = NULL,
# two.extra.bases = NULL,
# choose.options = NULL,
# scrap.traderow = NULL,
# has.put.top.deck = NULL,
# put.top.deck.ship = NULL,
# put.top.deck.base = NULL,
# put.top.deck.both = NULL,
# has.condition = NULL,
# money.condition = NULL,
# attack.condition = NULL,
# healing.condition = NULL,
# draw.condition = NULL,
# discard.condition = NULL,
# destroy.base.condition = NULL,
# scrap.traderow.condition = NULL,
# put.top.deck.ship.condition = NULL,
# put.top.deck.base.condition = NULL,
# put.top.deck.both.condition = NULL,
# discard.draw.condition = NULL,
# has.scrap = NULL,
# money.scrap = NULL,
# attack.scrap = NULL,
# healing.scrap = NULL,
# draw.scrap = NULL,
# discard.scrap = NULL,
# scrap.ability = NULL,
# scrap.discard_pile.hand = NULL,
# scrap.discard_pile.hand.condition = NULL,
# is.outpost = NULL,
# n.base.game = NULL,
# n.base.game = NULL,
