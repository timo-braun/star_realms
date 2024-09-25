# star_realms
Re-creating the card game Star Realms in R in order to determine and explain the "fair" value of a card

## Introduction

The initial purpose of the project was to create opponents that are not anymore as easy as the ones provided in the correspoing app. The idea is simple: in order to create better opponents, we need to figure out what makes for a good opponent; someone who understands the game well and makes excellent decisions. 

While there are many options on how to play the cards once in hand, it is typically easy to determine what to do in this case. A more complex and interesting problem is acquiring the right cards at a given point in the game, accounting for the own cards and those of the opponent. Thus, we will focus on this aspect first by explaining why the card costs as much as it does and what we believe to be an appropriate price at a given point in time.

## Approaches to determining the fair price of a card

An intuitive approach when tasked with determining the fair price of a card would be to use a multiple linear regression model where we predict the price using the card effects such as giving $Attack$, $Trade$, or more complex aspects such as conditional "Draw a card", assuming that a blob faction entity is already in play. Also, we would like to make simple statements and therefore assume that the price is independent of other cards and the point in the game.

There are two crucial disadvantages to this approach: Since the number of different card effects is larger than the number of total cards in the game (for the first base deck which we will consider), we would not be able to train such as general linear model. Second, how valuable a card is changes over the course of the game and further depends on which cards the player and opponents already have.

## Current state of development

The main focus at the moment is on implementing the game itself. As of now, the initial game is set up properly. It is possible to buy cards from the trade row, and discard the hand after one is done playing. The next step is to properly include the functionality of the cards that are bought.
