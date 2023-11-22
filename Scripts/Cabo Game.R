{
  # library(tidyverse)
  
}

{
  cards <- rep(
    seq(0,13,1),
    c(2, rep(4, 12), 2)
  )
  
  actions <- c(
    rep(NA, 7),
    rep("peek", 2),
    rep("spy", 2),
    rep("swap", 2),
    rep(NA, 2)
  )
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
  
  fn_takeTurn <- function(hand, draw, move = NA, playAction = NA){
    # move = 3
    # hand = hands[[p]]
    # draw
    
    # Take card action
    if(!is.na(playAction)) fn_playCard()
    
    # Exchange move for card in hand location
    if(!is.na(move)){
      toDiscard <- hand[move]
      hand[move] <- draw
    } else{
      toDiscard <- draw
    }
    
    cat(paste("\n\nThe move: ", move))
    cat(paste("\nThe draw: ", draw))
    cat(paste("\nTo discard: ", toDiscard))
    
    discard <<- append(toDiscard, discard)
    
    return(hand)
    
  }
  
  fn_turn <- function(players){
    
    for(p in 1:players){
      
      draw <- fn_beginTurn(
        action = sample(c("fromDeck", "fromDiscard"), 1), 
        deck, discard[1]
      )
      hands[[p]] <<- fn_takeTurn(
        hands[[p]],
        draw, 
        move = sample(c(1:4, NA), 1),
        playAction = NA
      )
      
    }
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

# Gameplay ----
{
  ## Setup ----
  
  
  
  # Start ----
  {
    deck <- cards
    players = 2
    
    hands <- lapply(1:players, \(p){
      fn_deal(deck, 4)
    })
    discard <- fn_deal(deck, 1)
    
    scores <- lapply(hands, sum)
  }
  
  # Turn 1 ----
  {
    hands
    fn_turn(players)
    hands
    
    scores <- lapply(hands, sum)
  }
  
  
  length(deck)
  
  
  hand <- p1_hand
}