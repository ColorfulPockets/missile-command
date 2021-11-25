# missile-command
UCSD CSE 230 Final Project -- creating a Missile Command-like game using the Haskell brick library


## Members
- Bhavani Panda
- Eric Estabaya
- Andrew Nathenson

## Proposal
We will recreate the Atari game called Missile Command but with a twist. The game involves missiles falling from the sky and the player must blow up the missiles in the air before they make contact with the ground. Instead of having the player use the cursor to aim at the missiles, we plan to have each missile correspond to a letter and the player must type that letter to blow up that missile. To make sure they don't just randomly type letters, we plan to incorporate a cool down timer after each letter is typed. In addition, instead of mutliple levels, we plan to make it an endless game with scaling difficulty. We will display various information to the player such as score and their cool down timer. We want to add cool animations and unique game play elements such as different missile types, power ups, and the like.


# 11/25 UPDATE:

## Key Components
The architecture of our game loosely follows that of the Professor's Brick Tac Toe game, with a Board class to represent a 50x50 game grid, a Model class encompassing our Player, Missile, and Score classes, a View class for our UI, a Control class for our game actions, and finally a Main class.  We are modifying these classes extensively, however, to fit the model of a real-time game where the board state is not equivalent to the result of the game, and it is not played in turns.  Instead, Missiles and the Player affect the Board state simultaneously.

## Challenges So Far
Our biggest challenge so far has been trying to understand how to best represent a missile and have it progress in real-time. We want the animation to look like the missile is dropping to the ground. In order to do this, we ended up representing the missile as a trail of pixels colored in blue going from the top of the screen to the bottom, deleting pixels behind it.  

## Deadline Expectation and Goal Modifications
We are making steady progress and believe we will be able to meet our goals for the game. It may be a little tough to add all the extra gameplay elements we listed above so if we are not able to get to all of them, we will modify our goals so that we have a complete game with basic animations, such as the missile blowing up when struck.

## Libraries
- Brick Library
- vty

## Roles
- Bhavani is working on the actions of the game (i.e. collecting user input and making a missile blow up, setting up our scoring system and making the user action reflect in the score).
- Andrew is working on graphics, game design, testing and bug fixing
- Eric is working on 
