library(R6)

gameObject = R6Class("gameObject",
               
               public = list(
                 
                 #### Fields ####
                 
                 object_name = NULL,
                 id = NULL,
                 is.ship = NULL,
                 faction = NULL,
                 cost = NULL,
                 money = NULL,
                 attack = NULL,
                 healing = NULL,
                 health = NULL,
                 draw = NULL,
                 discard = NULL,
                 destroy.base = NULL,
                 two.extra.bases = NULL,
                 choose.options = NULL,
                 scrap.traderow = NULL,
                 has.put.top.deck = NULL,
                 put.top.deck.ship = NULL,
                 put.top.deck.base = NULL,
                 put.top.deck.both = NULL,
                 has.condition = NULL,
                 money.condition = NULL,
                 attack.condition = NULL,
                 healing.condition = NULL,
                 draw.condition = NULL,
                 discard.condition = NULL,
                 destroy.base.condition = NULL,
                 scrap.traderow.condition = NULL,
                 put.top.deck.ship.condition = NULL,
                 put.top.deck.base.condition = NULL,
                 put.top.deck.both.condition = NULL,
                 discard.draw.condition = NULL,
                 has.scrap = NULL,
                 money.scrap = NULL,
                 attack.scrap = NULL,
                 healing.scrap = NULL,
                 draw.scrap = NULL,
                 discard.scrap = NULL,
                 scrap.ability = NULL,
                 scrap.discard_pile.hand = NULL,
                 scrap.discard_pile.hand.condition = NULL,
                 is.outpost = NULL,
                 n.base.game = NULL,
                 initialize = function(object_name, id, is.ship = TRUE, faction, cost, money = NULL, attack = NULL, healing = NULL, health = NULL, draw = NULL, discard = NULL, destroy.base = NULL, two.extra.bases = FALSE, choose.options = NULL, scrap.traderow = NULL, has.put.top.deck = NULL, put.top.deck.ship = NULL, put.top.deck.base = NULL, put.top.deck.both = NULL, has.condition = FALSE, money.condition = NULL, attack.condition = NULL, healing.condition = NULL, draw.condition = NULL, discard.condition = NULL, destroy.base.condition = NULL, scrap.traderow.condition = NULL, put.top.deck.ship.condition = NULL, put.top.deck.base.condition = NULL, put.top.deck.both.condition = NULL, discard.draw.condition = NULL, has.scrap = FALSE, money.scrap = NULL, attack.scrap = NULL, healing.scrap = NULL, draw.scrap = NULL, discard.scrap = NULL, scrap.ability = FALSE, scrap.discard_pile.hand = NULL, scrap.discard_pile.hand.condition = NULL, is.outpost = FALSE, n.base.game = NULL) {
                   self$object_name = object_name
                   self$id = id
                   self$is.ship = is.ship
                   self$faction = faction
                   self$cost = cost
                   self$money = money
                   self$attack = attack
                   self$healing = healing
                   self$health = health
                   self$draw = draw
                   self$discard = discard
                   self$destroy.base = destroy.base
                   self$two.extra.bases = two.extra.bases
                   self$choose.options = choose.options
                   self$scrap.traderow = scrap.traderow
                   self$has.put.top.deck = has.put.top.deck
                   self$put.top.deck.ship = put.top.deck.ship
                   self$put.top.deck.base = put.top.deck.base
                   self$put.top.deck.both = put.top.deck.both
                   self$has.condition = has.condition
                   self$money.condition = money.condition
                   self$attack.condition = attack.condition
                   self$healing.condition = healing.condition
                   self$draw.condition = draw.condition
                   self$discard.condition = discard.condition
                   self$destroy.base.condition = destroy.base.condition
                   self$scrap.traderow.condition = scrap.traderow.condition
                   self$put.top.deck.ship.condition = put.top.deck.ship.condition
                   self$put.top.deck.base.condition = put.top.deck.base.condition
                   self$put.top.deck.both.condition = put.top.deck.both.condition
                   self$discard.draw.condition = discard.draw.condition
                   self$has.scrap = has.scrap
                   self$money.scrap = money.scrap
                   self$attack.scrap = attack.scrap
                   self$healing.scrap = healing.scrap
                   self$draw.scrap = draw.scrap
                   self$discard.scrap = discard.scrap
                   self$scrap.ability = scrap.ability
                   self$scrap.discard_pile.hand = scrap.discard_pile.hand
                   self$scrap.discard_pile.hand.condition = scrap.discard_pile.hand.condition
                   self$is.outpost = is.outpost
                   self$n.base.game = n.base.game
                   
                   ## Automatically adjust properties if scrapping / condition has been declared
                   if (!is.null(money.condition) | !is.null(attack.condition) | !is.null(healing.condition) | !is.null(draw.condition) | !is.null(discard.condition) | !is.null(destroy.base.condition) | !is.null(scrap.traderow.condition) | !is.null(put.top.deck.ship.condition) | !is.null(put.top.deck.base.condition) | !is.null(put.top.deck.both.condition) | !is.null(discard.draw.condition)) {
                     self$has.condition = TRUE
                   }
                   
                   if (!is.null(money.scrap) | !is.null(attack.scrap) | !is.null(healing.scrap) | !is.null(draw.scrap) | !is.null(discard.scrap)) {
                     self$has.scrap = TRUE
                   }
                   
                   if (!is.null(put.top.deck.ship) | !is.null(put.top.deck.base) | !is.null(put.top.deck.both) ) {
                     self$has.put.top.deck = TRUE
                   }
                   
                   if (!is.null(scrap.discard_pile.hand) | !is.null(scrap.discard_pile.hand.condition) ) {
                     self$scrap.ability = TRUE
                   }
                   
                   ## Make sure that an outpost is not marked as ship
                   if ((is.outpost)) {
                     self$is.ship = FALSE
                   }
                   
                 }
                 
                 
               )
)
