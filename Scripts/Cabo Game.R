{
  # library(tidyverse)
  
}




{
  
  
  fn_deal <- function(deck, num_cards = 1){
    (deal <- sample(deck, num_cards, replace = FALSE))
    
    (cards_remove <- sapply(deal, \(x){which(deck == x)[1]}))
    
    for(i in seq_along(cards_remove)){
      x <- cards_remove[i] == cards_remove
      cards_remove <- cards_remove + (cumsum(x)-1)*x
    }
    
    deck <<- deck[-cards_remove]
    
    return(deal)
  }
  
  fn_discard <- function(){
    
    
    
  }
  
  fn_beginTurn <- function(action, deck, discard){
    
    # Action is draw from deck or discard
    if(action == "fromDeck"){
      theReturn <- fn_deal(deck, num_cards = 1)
    } else if(action == "fromDiscard"){
      theReturn <- discard
    }
    return(theReturn)
  }
  
  fn_takeTurn <- function(hand, draw, card_num = NA, playAction = NA){
    # card_num = 3
    # hand = hands[[p]]
    # draw
    
    cat(paste("\n\nStart hand: ", paste(hand, collapse = ", ")))
    
    # Take card action
    if(!is.na(playAction)) fn_playCard()
    
    # Exchange move for card in hand location
    if(!is.na(card_num)){
      toDiscard <- hand[card_num]
      hand[card_num] <- draw
    } else{
      toDiscard <- draw
    }
    
    
    cat(paste("\nThe card_num: ", card_num))
    cat(paste("\nThe draw: ", draw))
    cat(paste("\nTo discard: ", toDiscard))
    cat(paste("\nEnd hand: ", paste(hand, collapse = ", ")))
    
    discard <<- append(toDiscard, discard)
    
    list(hand, discard[1])
    
    return(hand)
    
  }
  
  fn_turn <- function(hand, drawAction, playAction, card_num, deck, discard){
    # hand:         Players cards
    # drawAction:   Either 'fromDeck' or 'fromDiscard'
    # playAction:   True or False on playing an action
    # card_num:     Card location in hand to replace with draw
    
    
    draw <- fn_beginTurn(
      action = drawAction,
      deck, discard[1]
    )
    
    hand <- fn_takeTurn(
      hand = hand,
      draw = draw, 
      card_num = card_num,
      playAction = NA
    )
    
    return(hand)
  }
  
  fn_duplicateCards <- function(){}
  
  fn_discard <- function(){}
  
  # fn_kamikazee <- function(){}
  
  # fn_playCard <- function(){}
  
  # fn_peek <- function(){}
  
  # fn_spy <- function(){}
  
  # fn_swap <- function(){}
  
  # fn_peekTwo <- function(){fn_peek()}
  
  fn_callCabo <- function(){}
  
  fn_finalTurn <- function(){}
  
  fn_score <- function(hand){
    
    if(sort(hand) == c(12,12, 13,13)){
      score = 0
      fn_kamikazee()
    } else{
      score = sum(hand)
    }
    
    return(score)
    
    
    
  }
}

