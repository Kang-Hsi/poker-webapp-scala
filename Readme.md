# Team 77

## Team members

```
 - Gramegna Francesco - 378749, francesco.gramegna@epfl.ch
 - Popper Leopold Kang-Hsi - 363077, leopold.popper@epfl.ch
 - Weber William Max Antoine - 355674, william.weber@epfl.ch
 - Wohlers Basile Christophe Pierre - 363824, basile.wohlers@epfl.ch
```
## Proposal

### User stories

As a player, I want to play the standard rules of a game of Texas hold’em poker.
As a player, I want to see the other players' actions.
As a player, when I win a hand, I want to see my balance increase.

### Requirements

Our poker web app will adhere to the standard rules of Texas Hold’em poker with very slight differences.

The UI will display the following: the dealer, small blind, big blind, with the amount of the blinds; all the players’ names with their balance, the current player will be in bold; the action taken by the current player and the remaining hands of the game; the communal cards (aka the cards in the middle) and the pot size; the player’s personal hand and his balance; the actions that the player can take; a description of the result of the last winner.

The server will handle all game logic and state transitions, from “flipping” the communal cards, determining the actions a player can take, updating the balances of the pot, players, blinds, and checking if a player has won. A player loses when he is out of money otherwise the game terminates after N hands.



### Roles

Leopold and Francesco will work on the server-side logic of the app, implementing the algorithms and states of the poker game, whereas William will work on the user interface, while also working closely with Basile who will be taking care of the wires and types for the app. For the tests, each member will write tests for the work done by another team member.

### Mock-ups

![](mockups/app.png)
