library(R6)
source("./ships.R")


game = R6Class("game",
               
               public = list(
                 
                 #### Fields ####
                 
                 player_names = NULL,
                 n_player = NULL,
                 player_decks = NULL,
                 player_hands = NULL,
                 player_discard_pile = NULL,
                 player_1_starts = NULL,
                 trade_row = NULL,
                 turn_counter = NULL,
                 app_rules = NULL,
                 name_expansion = NULL,
                 list_expansion = NULL,
                 player_health = NULL,
                 player_attack = NULL,
                 player_trade = NULL,
                 player_n_cards = NULL,
                 deck_expansion = NULL,
                 active_player_id = NULL,
                 
                 ## initialize
                 initialize = function(player_names, player_1_deck_start = c(8,2), player_2_deck_start = c(8,2), name_expansion = "base", app_rules = FALSE) {
                   self$player_names <- player_names
                   self$n_player <- length(player_names)
                   self$name_expansion <- name_expansion
                   self$list_expansion <- all_cards
                   self$turn_counter <- 0
                   self$app_rules <- app_rules
                   self$player_1_starts <- rbinom(1, 1, 0.5)
                   self$player_health = c(50, 50)
                   self$player_attack = c(0, 0)
                   self$player_trade = c(0, 0)
                   
                   
                   #### GAME SET UP ####
                   ## create player decks ##
                   ## convert numbers in player decks to ships
                   ## first entry n scouts, second m vipers
                   ## first n_player entries are the cards as R6 objects, the next n_player entries
                   ## are just the names of the cards
                   self$player_decks <- list(c(rep("Scout", player_1_deck_start[1]), rep("Viper", player_1_deck_start[2])),
                                             c(rep("Scout", player_2_deck_start[1]), rep("Viper", player_2_deck_start[2])),
                                             c(rep(self$get_card_id("Scout"), player_1_deck_start[1]), rep(self$get_card_id("Viper"), player_1_deck_start[2])),
                                             c(rep(self$get_card_id("Scout"), player_2_deck_start[1]), rep(self$get_card_id("Viper"), player_2_deck_start[2])),
                                             c(rep("Scout", player_1_deck_start[1]), rep("Viper", player_1_deck_start[2])),
                                             c(rep("Scout", player_2_deck_start[1]), rep("Viper", player_2_deck_start[2])) )
                   
                   ## setting the number of total cards in a player's deck
                   self$player_n_cards = c(length(self$player_decks[[self$n_player + 1]]), length(self$player_decks[[self$n_player + 2]]))
                     
                   
                   for (i in (1:length(self$list_expansion))) {
                     # print(i)
                     # print(self$list_expansion[[i]]$object_name)
                     # print(self$list_expansion[[i]]$id)
                     # print(self$list_expansion[[i]]$n.base.game)
                     if (self$list_expansion[[i]]$object_name %in% c("Explorer", "Scout", "Viper")){
                       # print("skipped")
                       next
                     }
                     self$deck_expansion <- c(self$deck_expansion, rep(self$list_expansion[[i]]$id, self$list_expansion[[i]]$n.base.game))
                   }

                   ## replace names in deck with actual cards
                   for (i in (1:self$n_player)){
                     self$player_decks[[i]] <- lapply(self$player_decks[[i]], self$find_card)
                   }
                   
                   ## create each player's first hand
                   self$player_hands <- vector("list", 6)
                   if (self$player_1_starts){
                     self$draw_card(3, "player_1_deck", "player_1_hand")
                     self$draw_card(5, "player_2_deck", "player_2_hand")
                     
                   }
                   if (!self$player_1_starts){
                     self$draw_card(5, "player_1_deck", "player_1_hand")
                     self$draw_card(3, "player_2_deck", "player_2_hand")
                     
                   }
                   
                   ## create each player's discard pile
                   self$player_discard_pile <- vector("list", 6)

                   ## create trade row
                   self$trade_row <- vector(mode = "list", length = 3)
                   self$trade_row <- self$draw_card(5, "trade_deck", "trade_row")
                   
                   ## start turn counter and let the first player begin
                   self$turn_counter <- self$turn_counter + 1
                   self$active_player_id <- self$n_player - as.integer(self$player_1_starts)

                   
                 }, ## finish initialize
                 
                 ## get card id function
                 get_card_id = function(card_name){
                   for (i in (1:length(self$list_expansion))) {
                     # print(i)
                     # print(card_name)
                     # print(self$list_expansion[[i]]$object_name)
                     if (card_name == self$list_expansion[[i]]$object_name){
                       return(self$list_expansion[[i]]$id)
                     }
                   }
                 },
                 
                 ## get n_card function
                 get_n_card = function(card_name){
                   for (i in (1:length(self$list_expansion))) {
                     # print(i)
                     # print(card_name)
                     # print(self$list_expansion[[i]]$object_name)
                     if (card_name == self$list_expansion[[i]]$object_name){
                       return(self$list_expansion[[i]]$n.base.game)
                     }
                   }
                 },
                 
                 ## get card name function based on card_id
                 get_card_name = function(card_id){
                   for (i in (1:length(self$list_expansion))) {
                     # print(i)
                     # print(card_id)
                     # print(self$list_expansion[[i]]$object_name)
                     if (card_id == self$list_expansion[[i]]$id){
                       return(self$list_expansion[[i]]$object_name)
                     }
                   }
                 },
                 
                 ## find card function
                 find_card = function(card_name){
                   for (i in (1:length(self$list_expansion))) {
                     # print(i)
                     # print(card_name)
                     # print(self$list_expansion[[i]]$object_name)
                     if (card_name == self$list_expansion[[i]]$object_name){
                       return(self$list_expansion[[i]])
                     }
                   }
                 },
                 
                 ## add function that adds card into a deck
                 add_card = function(player, card_name){
                   self$player_decks[[player]] <- c(self$player_decks[[player]], self$find_card(card_name))
                 },
                 
                 ## draw card from deck function
                 draw_card = function(n, from, to){
                   
                   if (from == "player_1_deck" && to == "player_1_hand"){
                     
                     ## shuffle discard pile into deck if the deck is empty
                     ## in this case, draw the remaining cards first, and then shuffle the discard pile
                     ## into the deck
                     deck_empty_flag = FALSE
                     player_id = 1
                     if (n > length(self$player_decks[[self$n_player + player_id]])){
                       n_draw = length(self$player_decks[[self$n_player + player_id]])
                       cat("Number of cards drawn before shuffling the discard pile into deck:", n_draw, "\n")
                       
                       ## only draw cards if there are cards in the deck
                       if (length(self$player_decks[[self$n_player + player_id]]) > 0){
                         
                         ## draw the remaining cards
                         cat("Drawing the remaining cards from the deck \n")
                         self$draw_card(n_draw, "player_1_deck", "player_1_hand")
                         deck_empty_flag = TRUE
                       }
                       ## shuffle the discard pile into the deck and draw the remaining cards
                       ## shuffling
                       ## check if there are no cards in hand:
                       if ((length(self$player_hands[[self$n_player + self$active_player_id]])) != 0 && deck_empty_flag == FALSE) {
                         stop("Player ", player_id, " has cards in hand, eventhough they should not. \n")
                       }
                       cat("Shuffling the discard pile and set it up as deck \n")
                       cat("Player 1: Discard pile before shuffling:", self$player_discard_pile[[self$n_player + player_id]], "\n")
                       cat("Player 1: Hand before shuffling:", self$player_hands[[self$n_player + player_id]], "\n")
                       cat("Player 1: Deck before shuffling:", self$player_decks[[self$n_player + player_id]], "\n")
                       if ((length(self$player_discard_pile[[self$n_player + player_id]]) + length(self$player_hands[[self$n_player + player_id]])) != self$player_n_cards[player_id]) {
                         stop("REMOVE AFTER TESTING: The number of cards player ", player_id, " has in deck and in hand does not match what he should have. \n")
                       }
                       self$player_decks[[self$n_player + player_id]] <- self$player_discard_pile[[self$n_player + player_id]]
                       
                       ## resetting discard pile
                       self$player_discard_pile[[self$n_player + player_id]] <- numeric(0)
                       
                       ## sort and clean
                       self$sort_clean("deck", player_id)
                       self$sort_clean("discard", player_id)
                       cat("Player 1: Discard pile after shuffling:", self$player_discard_pile[[self$n_player + player_id]], "\n")
                       cat("Player 1: Deck after shuffling:", self$player_decks[[self$n_player + player_id]], "\n")
                       
                       ## drawing the remaining cards
                       n_draw = n - n_draw
                       self$draw_card(n_draw, "player_1_deck", "player_1_hand")
                     } else {
                       ## Deal cards from deck in the case without needing to adjust deck
                       cat("Dealing", n,  "cards to", self$player_names[player_id], "\n")
                       
                       ## select cards to be drawn, move them to the hand and remove them from deck
                       cards = sample.int(length(self$player_decks[[player_id]]), n)
                       self$player_hands[[self$n_player + player_id]] = c(self$player_hands[[self$n_player + player_id]], self$player_decks[[self$n_player + player_id]][cards])
                       self$player_decks[[self$n_player + player_id]] = self$player_decks[[self$n_player + player_id]][-cards]
                       
                       ## sorting the deck and hands
                       self$sort_clean("deck", player_id)
                       self$sort_clean("hand", player_id)
                       
                       cat("Player 1: Discard pile after drawing:", self$player_discard_pile[[self$n_player + player_id]], "\n")
                       cat("Player 1: Hand after drawing:", self$player_hands[[self$n_player + player_id]], "\n")
                       cat("Player 1: Deck after drawing:", self$player_decks[[self$n_player + player_id]], "\n")
                     }
                     
                   }
                   
                   else if (from == "player_2_deck" && to == "player_2_hand"){
                     
                     ## shuffle discard pile into deck if the deck is empty
                     ## in this case, draw the remaining cards first, and then shuffle the discard pile
                     ## into the deck
                     deck_empty_flag = FALSE
                     player_id = 2
                     if (n > length(self$player_decks[[self$n_player + player_id]])){
                       n_draw = length(self$player_decks[[self$n_player + player_id]])
                       cat("Number of cards drawn before shuffling the discard pile into deck:", n_draw, "\n")
                       
                       ## only draw cards if there are cards in the deck
                       if (length(self$player_decks[[self$n_player + player_id]]) > 0){
                         
                         ## draw the remaining cards
                         cat("Drawing the remaining cards from the deck \n")
                         self$draw_card(n_draw, "player_2_deck", "player_2_hand")
                         deck_empty_flag = TRUE
                       }
                       ## shuffle the discard pile into the deck and draw the remaining cards
                       ## shuffling
                       ## check if there are no cards in hand:
                       if ((length(self$player_hands[[self$n_player + self$active_player_id]])) != 0 && deck_empty_flag == FALSE) {
                         stop("Player ", player_id, " has cards in hand, eventhough they should not. \n")
                       }
                       cat("Shuffling the discard pile and set it up as deck \n")
                       cat("Player 2: Discard pile before shuffling:", self$player_discard_pile[[self$n_player + player_id]], "\n")
                       cat("Player 2: Hand before shuffling:", self$player_hands[[self$n_player + player_id]], "\n")
                       cat("Player 2: Deck before shuffling:", self$player_decks[[self$n_player + player_id]], "\n")
                       if ((length(self$player_discard_pile[[self$n_player + player_id]]) + length(self$player_hands[[self$n_player + player_id]])) != self$player_n_cards[player_id]) {
                         stop("REMOVE AFTER TESTING: The number of cards player ", player_id, " has in deck and in hand does not match what he should have. \n")
                       }
                       self$player_decks[[self$n_player + player_id]] <- self$player_discard_pile[[self$n_player + player_id]]
                       
                       ## resetting discard pile
                       self$player_discard_pile[[self$n_player + player_id]] <- numeric(0)
                       
                       ## sort and clean
                       self$sort_clean("deck", player_id)
                       self$sort_clean("discard", player_id)
                       cat("Player 2: Discard pile after shuffling:", self$player_discard_pile[[self$n_player + player_id]], "\n")
                       cat("Player 2: Deck after shuffling:", self$player_decks[[self$n_player + player_id]], "\n")
                       
                       ## drawing the remaining cards
                       n_draw = n - n_draw
                       self$draw_card(n_draw, "player_2_deck", "player_2_hand")
                     } else {
                       ## Deal cards from deck in the case without needing to adjust deck
                       cat("Dealing", n,  "cards to", self$player_names[player_id], "\n")
                       
                       ## select cards to be drawn, move them to the hand and remove them from deck
                       cards = sample.int(length(self$player_decks[[player_id]]), n)
                       self$player_hands[[self$n_player + player_id]] = c(self$player_hands[[self$n_player + player_id]], self$player_decks[[self$n_player + player_id]][cards])
                       self$player_decks[[self$n_player + player_id]] = self$player_decks[[self$n_player + player_id]][-cards]
                       
                       ## sorting the deck and hands
                       self$sort_clean("deck", player_id)
                       self$sort_clean("hand", player_id)
                       
                       cat("Player 2: Discard pile after drawing:", self$player_discard_pile[[self$n_player + player_id]], "\n")
                       cat("Player 2: Hand after drawing:", self$player_hands[[self$n_player + player_id]], "\n")
                       cat("Player 2: Deck after drawing:", self$player_decks[[self$n_player + player_id]], "\n")
                     }
                     
                   } else if (from == "trade_deck" && to == "trade_row"){
                     cat("Adding", n,  "card(s) to the trade row \n")
                     ## select cards to be drawn, move them to the hand and remove them from deck
                     cards = sample.int(length(self$deck_expansion), n)
                     self$trade_row[[2]] = self$deck_expansion[cards]
                     self$deck_expansion = self$deck_expansion[-cards]
                     ## sorting the deck and hands
                     self$deck_expansion = sort(self$deck_expansion)
                     self$trade_row[[2]] = sort(self$trade_row[[2]])
                     
                     ## clean up the deck & hand so that the cards and card names are correct
                     ## DO THIS FOR THE TRADE ROW, but just for the trade row
                     self$trade_row[[3]] = unlist(lapply(self$trade_row[[2]], self$get_card_name))
                     # print(self$trade_row)
                     self$trade_row[[1]] = (lapply(self$trade_row[[3]], self$find_card))
                     (self$trade_row)
                     # TODO: figure out solution on how to avoid using the line above
                     
                     # self$player_hands[[1]] = lapply(self$player_hands[[self$n_player + 1]], self$find_card)
                     # self$player_hands[[2*self$n_player + 1]] = unlist(lapply(self$player_hands[[self$n_player + 1]], self$get_card_name))
                     
                   } else {
                     stop("Cannot draw a card from this place. Ensure that start and end of draw are correct.")
                   }
                   
                   
                 },
                 
                 ##extract values from R6 object
                 R6_extract_values = function(r6class){
                   tmp <- sapply(r6class, class)
                   slots <- tmp[ !tmp %in% c("environment", "function")]
                   res <- list()
                   for (i in names(slots)) {
                     if ("R6" %in% class(r6class[[i]])) {
                       res[[i]]  <- R6_extract_values(r6class[[i]])
                     }else{
                       res[[i]] <- r6class[[i]]
                     }
                   }
                   return(res)
                 },
                 
                 
                 ## play card function
                 play_card = function(player_id, card_position_hand){
                   ## check if the player may now play
                   ## check that only cards that are currently in hand can be played
                   if (as.integer(card_position_hand) > length(self$player_hands[[player_id]])){
                     cat("Cannot play this card right now")
                   }
                   else if (self$active_player_id == player_id){
                     card_name <- self$player_hands[[2*self$n_player + player_id]][card_position_hand]
                     cat("Playing this card:", card_name, "\n")
                     
                     ## extract relevant fields from the card and execute their effects when the card is played
                     # print(names(self$R6_extract_values(self$player_hands[[player_id]][[card_position_hand]])))
                     for (field in names(self$R6_extract_values(self$player_hands[[player_id]][[card_position_hand]]))) {
                       ## skipping non-relevant fields
                       card_value = self$player_hands[[player_id]][[card_position_hand]][[field]]
                       if ((card_value == FALSE)
                           || (card_value == 0)
                           || field %in% c("id")){
                         next
                       }
                       cat("This card gives you this much", field, ":", card_value, "\n" )
                       
                       ## incorporating effects from playing this card
                       if (field == "money"){
                         cat("Gaining trade:", card_value, "\n")
                         self$player_trade[self$active_player_id] <- self$player_trade[self$active_player_id] + card_value
                       }
                       if (field == "attack"){
                         cat("Gaining attack:", card_value, "\n")
                         self$player_attack[self$active_player_id] <- self$player_attack[self$active_player_id] + card_value
                       }
                       
                       
                     }
                     
                     ## after the card was played, put card from hand into discard pile
                     ## removing the card from the hand
                     self$player_discard_pile[[self$n_player + player_id]] <- c(self$player_discard_pile[[self$n_player + player_id]], self$player_hands[[self$n_player + player_id]][card_position_hand])
                     self$player_hands[[self$n_player + player_id]] <- self$player_hands[[self$n_player + player_id]][-card_position_hand]

                     
                     ## clean up the discard pile & hand so that the cards and card names are correct
                     self$player_discard_pile[[2*self$n_player + player_id]] = unlist(lapply(self$player_discard_pile[[self$n_player + player_id]], self$get_card_name))
                     self$player_discard_pile[[player_id]] = lapply(self$player_discard_pile[[2*self$n_player + player_id]], self$find_card)

                     ## if there are no cards, set everything to numeric(0)
                     if (length(self$player_hands[[self$n_player + player_id]]) > 0){ 
                       # print(unlist(lapply(self$player_hands[[self$n_player + player_id]], self$get_card_name)))
                       ## make sure that the last hand slot does not get deleted
                       if (is.null(unlist(lapply(self$player_hands[[self$n_player + player_id]], self$get_card_name)))){
                         self$player_hands[[2*self$n_player + player_id]] = numeric(0)
                       } else {
                         self$player_hands[[2*self$n_player + player_id]] = unlist(lapply(self$player_hands[[self$n_player + player_id]], self$get_card_name))
                       }
                     self$player_hands[[player_id]] = lapply(self$player_hands[[2*self$n_player + player_id]], self$find_card)
                     } else {
                       self$player_hands[[2*self$n_player + player_id]] = self$player_hands[[player_id]] = numeric(0)
                     }
                     
                     # cat("Check the actual cards in hand of active player:", length(self$player_hands[[player_id]]), "\n")
                     
                   } else {
                     cat("It is not this player's turn right now. \n")
                   }
                 },
                 
                 ## buy card function
                 buy_card = function(card_position_row){
                   player_id <- self$active_player_id
                   ## check if the active player can purchase the card
                   if (self$trade_row[[1]][[card_position_row]]$cost <= self$player_trade[player_id]){
                     cat(self$player_names[player_id], "acquires", self$trade_row[[1]][[card_position_row]]$object_name, "from the trade row. \n")
                     ## adding the card to player discard pile
                     self$player_discard_pile[[self$n_player + player_id]] <- c(self$player_discard_pile[[self$n_player + player_id]], self$trade_row[[2]][card_position_row])
                     self$sort_clean("discard")
                     
                     ## draw new card to trade row
                     self$draw_card(1, from ="trade_deck", to = "trade_row")
                   } else {
                     cat("You don't have sufficient trade to buy this card")
                     return(FALSE)
                   }
                 },
                 
                 ## show hand function
                 show_hand = function(player_id){
                   ## check if the player may now play
                   if (self$active_player_id == player_id){
                     cat("These cards are in your hand:", self$player_hands[[2*self$n_player + player_id]], "\n")
                   } else {
                     cat("You may not look into your opponent's hand. \n")
                   }
                 },
                 
                 ## show hand function
                 show_trade_row = function(){
                     cat("These cards are currently in the trade row:", self$trade_row[[3]], "\n")
                   # TODO: add explorer to trade row
                 },
                 
                 ## sort and clean function
                 sort_clean = function(place, player_id = self$active_player_id){
                   if (place == "deck"){
                     ## sorting the deck
                     self$player_decks[[self$n_player + player_id]] = sort(self$player_decks[[self$n_player + player_id]])
                     
                     ## if there are no cards, set everything to numeric(0)
                     if (length(self$player_decks[[self$n_player + player_id]]) > 0){
                       # cat("More than 0 cards are in the deck \n")
                       # print(unlist(lapply(self$player_decks[[self$n_player + player_id]], self$get_card_name)))
                       ## make sure that the last hand slot does not get deleted
                       if (is.null(unlist(lapply(self$player_decks[[self$n_player + player_id]], self$get_card_name)))){
                         self$player_decks[[2*self$n_player + player_id]] = numeric(0)
                       } else {
                         cat("Updating card names in the deck \n")
                         self$player_decks[[2*self$n_player + player_id]] = unlist(lapply(self$player_decks[[self$n_player + player_id]], self$get_card_name))
                       }
                       cat("Updating cards in the deck \n")
                       self$player_decks[[player_id]] = lapply(self$player_decks[[2*self$n_player + player_id]], self$find_card)
                     } else {
                       self$player_decks[[2*self$n_player + player_id]] = self$player_decks[[player_id]] = numeric(0)
                     }
                   }

                   else if (place == "discard"){
                     ## sorting the deck
                     self$player_discard_pile[[self$n_player + player_id]] = sort(self$player_discard_pile[[self$n_player + player_id]])
                     
                     ## if there are no cards, set everything to numeric(0)
                     cat("Number of cards currently in discard pile:", length(self$player_discard_pile[[self$n_player + player_id]]), "\n")
                     if (length(self$player_discard_pile[[self$n_player + player_id]]) > 0){ 
                       # print(unlist(lapply(self$player_discard_pile[[self$n_player + player_id]], self$get_card_name)))
                       ## make sure that the last hand slot does not get deleted
                       if (is.null(unlist(lapply(self$player_discard_pile[[self$n_player + player_id]], self$get_card_name)))){
                         self$player_discard_pile[[2*self$n_player + player_id]] = numeric(0)
                       } else {
                         self$player_discard_pile[[2*self$n_player + player_id]] = unlist(lapply(self$player_discard_pile[[self$n_player + player_id]], self$get_card_name))
                       }
                       self$player_discard_pile[[player_id]] = lapply(self$player_discard_pile[[2*self$n_player + player_id]], self$find_card)
                     } else {
                       cat("No cards in discard pile. Setting the remaining fields to 0. \n")
                       self$player_discard_pile[[2*self$n_player + player_id]] = self$player_discard_pile[[player_id]] = numeric(0)
                     }
                   }

                   else if (place == "hand"){
                     ## sorting the deck
                     self$player_hands[[self$n_player + player_id]] = sort(self$player_hands[[self$n_player + player_id]])
                     
                     ## if there are no cards, set everything to numeric(0)
                     if (length(self$player_hands[[self$n_player + player_id]]) > 0){ 
                       # print(unlist(lapply(self$player_hands[[self$n_player + player_id]], self$get_card_name)))
                       ## make sure that the last hand slot does not get deleted
                       if (is.null(unlist(lapply(self$player_hands[[self$n_player + player_id]], self$get_card_name)))){
                         self$player_hands[[2*self$n_player + player_id]] = numeric(0)
                       } else {
                         self$player_hands[[2*self$n_player + player_id]] = unlist(lapply(self$player_hands[[self$n_player + player_id]], self$get_card_name))
                       }
                       self$player_hands[[player_id]] = lapply(self$player_hands[[2*self$n_player + player_id]], self$find_card)
                     } else {
                       self$player_hands[[2*self$n_player + player_id]] = self$player_hands[[player_id]] = numeric(0)
                     }
                   }
                   else {
                     cat("The place", place, "cannot be sorted and cleaned. Make sure you are referring to the correct place. \n")
                   }
                   
                 },
                 
                 ## end turn function
                 end_turn = function(){
                   ## resetting all trade and attack for the player
                   cat("Resetting all trade and attack ... \n")
                   self$player_trade = rep(0, self$n_player)
                   self$player_attack = rep(0, self$n_player)
                   
                   ## put all remaining cards in the hand into the discard pile
                   ## check if there are any cards left in the hand
                   if (length(self$player_hands[[self$n_player + self$active_player_id]]) > 0){
                     cat("Putting remaining cards from hand into discard pile ... \n")
                     self$player_discard_pile[[self$n_player + self$active_player_id]] = c(self$player_discard_pile[[self$n_player + self$active_player_id]], self$player_hands[[self$n_player + self$active_player_id]])
                     self$player_hands[[self$n_player + self$active_player_id]] = numeric(0)
                   }
                   
                   ## cleaning up the discard pile and hand
                   self$sort_clean("discard", self$active_player_id)
                   self$sort_clean("hand", self$active_player_id)
                   
                   ## check if there are no cards in hand:
                   if ((length(self$player_hands[[self$n_player + self$active_player_id]])) != 0) {
                     stop("Player has cards in hand, eventhough they should not. \n")
                   }

                   ## draw 5 new cards from hand
                   ## do it case by case
                   ## don't draw cards if it is the second turn
                   if (self$turn_counter != 2 ){
                     if (self$active_player_id == 1){
                       self$draw_card(5, "player_1_deck", "player_1_hand")
                     }
                     
                     if (self$active_player_id == 2){
                       self$draw_card(5, "player_2_deck", "player_2_hand")
                     }
                   }
                   
                   ## increase turn counter by one and set active player to the next person
                   self$turn_counter <- self$turn_counter + 1
                   next_player_id = (self$active_player_id + 1) %% self$n_player
                   if (next_player_id == 0){
                     next_player_id = self$n_player
                   }
                   self$active_player_id = next_player_id
                 }
                 
                 
                 
                 
                 
               )
)


