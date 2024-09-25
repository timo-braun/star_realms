setwd("C:/projects/star_realms")
source("./gameObject.R")

#### naming conventions ####
# factions:
# 0 = none
# 1 = red
# 2 = yellow
# 3 = green
# 4 = blue

## start with neutral ships
## compute id for each ship
id <- 1

Scout <- gameObject$new(object_name = "Scout", id = id, faction = 0, cost = 0, money = 1)
id <- id + 1

Viper <- gameObject$new(object_name = "Viper", id = id, faction = 0, cost = 0, attack = 1)
id <- id + 1

Explorer <- gameObject$new(object_name = "Explorer", id = id, faction = 0, cost = 2, attack.scrap = 2)
id <- id + 1

## ships from the core set
Blob_Fighter <- gameObject$new(object_name = "Blob_Fighter", id = id, faction = 3, cost = 1, attack = 3, draw.condition = 1, n.base.game = 3)
id <- id + 1

Federation_Shuttle <- gameObject$new(object_name = "Federation_Shuttle", id = id, faction = 4, cost = 1, money = 2, healing.condition = 4, n.base.game = 3)
id <- id + 1

Imperial_Fighter <- gameObject$new(object_name = "Imperial_Fighter", id = id, faction = 2, cost = 1, attack = 2, discard = 1, attack.condition = 2, n.base.game = 3)
id <- id + 1

Trade_Bot <- gameObject$new(object_name = "Trade_Bot", id = id, faction = 1, cost = 1, money = 1, scrap.discard_pile.hand = 1, attack.condition = 2, n.base.game = 3)
id <- id + 1

Battle_Pod <- gameObject$new(object_name = "Battle_Pod", id = id, faction = 3, cost = 2, attack = 4, scrap.traderow = 1, attack.condition = 2, n.base.game = 2)
id <- id + 1

Corvette <- gameObject$new(object_name = "Corvette", id = id, faction = 2, cost = 2, attack = 1, draw = 1, attack.condition = 2, n.base.game = 2)
id <- id + 1

Cutter <- gameObject$new(object_name = "Cutter", id = id, faction = 4, cost = 2, healing = 4, money = 2, attack.condition = 4, n.base.game = 2)
id <- id + 1

Missile_Bot <- gameObject$new(object_name = "Missile_Bot", id = id, faction = 1, cost = 2, scrap.discard_pile.hand = 1, attack = 2, attack.condition = 2, n.base.game = 3)
id <- id + 1

Trade_Pod <- gameObject$new(object_name = "Trade_Pod", id = id, faction = 3, cost = 2, money = 3, attack.condition = 2, n.base.game = 3)
id <- id + 1

Battle_Station <- gameObject$new(object_name = "Battle_Station", id = id, is.ship = FALSE, faction = 1, cost = 3, health = 5, is.outpost = TRUE, attack.scrap = 5, n.base.game = 2)
id <- id + 1

Blob_Wheel <- gameObject$new(object_name = "Blob_Wheel", id = id, is.ship = FALSE, faction = 3, cost = 3, health = 5, is.outpost = FALSE, attack = 1, money.scrap = 3, n.base.game = 3)
id <- id + 1

Embassy_Yacht <- gameObject$new(object_name = "Embassy_Yacht", id = id, faction = 4, cost = 3, money = 2, healing = 3, two.extra.bases = TRUE, draw.condition = 2, n.base.game = 2)
id <- id + 1

Imperial_Frigate <- gameObject$new(object_name = "Imperial_Frigate", id = id, faction = 2, cost = 3, attack = 4, discard = 1, attack.condition = 2, draw.scrap = 1, n.base.game = 3)
id <- id + 1

Ram <- gameObject$new(object_name = "Ram", id = id, faction = 3, cost = 3, attack = 5, attack.condition = 2, money.scrap = 3, n.base.game = 2)
id <- id + 1

Supply_Bot <- gameObject$new(object_name = "Supply_Bot", id = id, faction = 1, cost = 3, money = 2, scrap.discard_pile.hand = 1, attack.condition = 2, n.base.game = 3)
id <- id + 1

Survey_Ship <- gameObject$new(object_name = "Survey_Ship", id = id, faction = 2, cost = 3, money = 1, draw = 1, discard.scrap = 1, n.base.game = 3)
id <- id + 1

Trading_Post <- gameObject$new(object_name = "Trading_Post", id = id, is.ship = FALSE, faction = 4, cost = 3, health = 4, choose.options = 2, healing.condition = 1, money.condition = 1, is.outpost = TRUE, attack.scrap = 3, n.base.game = 2)
id <- id + 1

Barter_World <- gameObject$new(object_name = "Barter_World", id = id, is.ship = FALSE, faction = 4, cost = 4, health = 4, choose.options = 2, healing.condition = 2, money.condition = 2, attack.scrap = 5, n.base.game = 2)
id <- id + 1

Blob_Destroyer <- gameObject$new(object_name = "Blob_Destroyer", id = id, faction = 3, cost = 4, attack = 6, destroy.base.condition = 1, scrap.traderow.condition = 1, n.base.game = 2)
id <- id + 1

Freighter <- gameObject$new(object_name = "Freighter", id = id, faction = 4, cost = 4, money = 4, put.top.deck.ship.condition = 1, n.base.game = 2)
id <- id + 1

Patrol_Mech <- gameObject$new(object_name = "Patrol_Mech", id = id, faction = 1, cost = 4, choose.options = 2, money.condition = 3, attack.condition = 5, scrap.discard_pile.hand.condition = 1, n.base.game = 2) ## should be fixed to avoid interaction between choosing which requires no condition and the scrapping which is actually conditional
id <- id + 1

Recycling_Station <- gameObject$new(object_name = "Recycling_Station", id = id, is.ship = FALSE, faction = 2, cost = 4, health = 4, is.outpost = TRUE, choose.options = 2, money.condition = 1, discard.draw.condition = 2, n.base.game = 2)
id <- id + 1

Space_Station <- gameObject$new(object_name = "Space_Station", id = id, is.ship = FALSE, faction = 2, cost = 4, health = 4, attack = 2, attack.condition = 2, is.outpost = TRUE, money.scrap = 4, n.base.game = 2)
id <- id + 1

## create list of all cards for further analysis
list_cards <- ls()[sapply(mget(ls(), .GlobalEnv), is.R6)]

all_cards <- mget(list_cards)
## removing id & gameObject from all_cards
all_cards$gameObject <- NULL
all_cards$id <- NULL






