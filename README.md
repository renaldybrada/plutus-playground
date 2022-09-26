# Rock Paper Scissor

A simple Rock Paper Scissor game. This script adapted and extended from plutus guess game. 
This simple game only accept string input rock/paper/scissor. If player 1 or player 2 input others, the fund stay locked on utxo.

Just like a typical rock paper scissor game, player or wallet 1 lock RPS word and some amount. 
player or wallet 2 will try to challenge from player 1. 
If player 2 win (for example : player 1 lock 'paper' and player 2 challenge with 'scissor') then the fund goes to challenger (player 2)
If it isn't (either challenger is lose or draw), the fund stay locked on utxo.

## Simulation

## Lose Case
![image](https://user-images.githubusercontent.com/45556134/192297777-13acbab3-5f0a-436c-8fdc-292cc39860c6.png)
![image](https://user-images.githubusercontent.com/45556134/192298145-23e75abf-225b-4f46-9271-6c70bb414639.png)
![image](https://user-images.githubusercontent.com/45556134/192298246-3f435172-56ca-4a17-a40c-839a32d499f9.png)
![image](https://user-images.githubusercontent.com/45556134/192298331-65fa8f71-3eab-4945-9a61-3511d5cd9e57.png)


## Draw Case (Considered as a lose case too)
![image](https://user-images.githubusercontent.com/45556134/192299812-aecedec9-6005-4485-bfb6-08d479a1d719.png)
![image](https://user-images.githubusercontent.com/45556134/192298145-23e75abf-225b-4f46-9271-6c70bb414639.png)
![image](https://user-images.githubusercontent.com/45556134/192298246-3f435172-56ca-4a17-a40c-839a32d499f9.png)
![image](https://user-images.githubusercontent.com/45556134/192300030-6630744b-53a1-4d4d-867a-1bd8edccf27e.png)


## Win Case
![image](https://user-images.githubusercontent.com/45556134/192300279-c96c0762-eb30-4af3-ac88-7ad5eae567b4.png)
![image](https://user-images.githubusercontent.com/45556134/192300426-4e559a90-7a23-4c4a-9f9b-f39c2ce2cd5f.png)
![image](https://user-images.githubusercontent.com/45556134/192300469-2372ac11-771e-41b2-991e-1f1f62355515.png)
![image](https://user-images.githubusercontent.com/45556134/192300635-8afec465-1f7c-4c77-8789-a7d5bd492c6f.png)
