# Gameplay ----
{
  ## Setup ----
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
    
    
    # OOP
    {
      install.packages("R7")
      library(R7) # Load the R7 package
      
      # Define Classes
      {
        # Define the Player class
        Player <- R7::setClass(
          "Player",
          slots = c(
            hand = "numeric", 
            discard = "numeric", 
            drawAction = "character", 
            playAction = "character", 
            card_num = "character"
          )
        )
        
        # Define the Turn class
        Turn <- R7::setClass(
          "Turn",
          slots = c(playerData = "list")
        )
        
        # Define the Game class
        Game <- R7::setClass(
          "Game",
          slots = c(turns = "list")
        )
      }
     
      
      # Constructor functions
      {
        # Constructor for Player
        create_player <- function(hand, discard, drawAction, playAction, card_num) {
          Player$new(
            hand = hand, 
            discard = discard, 
            drawAction = drawAction,
            playAction = playAction,
            card_num = card_num
          )
        }
        
        # Constructor for Turn
        create_turn <- function(playerData) {
          Turn$new(playerData = playerData)
        }
        
        # Constructor for Game
        create_game <- function(turns) {
          Game$new(turns = turns)
        }
        
      }
    }
  }
  
  
  # Start & Setup ----
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
    turn_log <- list(
      turnNum = list(
        player = list(
          hand = list(),
          discard = list()
        )
      )
    )
    turn_log[[1]] = 1
    turn_log[[1]][[1]] = list(
      
    )
    View(turn_log)
    p = 1
    hands
    for(p in 1:players){
      hands[[p]] <- fn_turn(
        hand = hands[[p]],
        drawAction = sample(c("fromDeck", "fromDiscard"), 1),
        playAction = NA,
        card_num = sample(c(1:4, NA), 1),
        deck = deck,
        discard = discard
      )
      
      
    }
    hands
    
    #hand, drawAction, playAction, deck, discard
    
    scores <- lapply(hands, sum)
  }
  
  
  length(deck)
  
  
  hand <- p1_hand
}