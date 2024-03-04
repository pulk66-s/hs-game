# Functional programming assessement 2

## Introduction

This game is a simple implementation of a RPG game. The game is a dungeon crawler where the player has to navigate through a dungeon and defeat the boss at the end. The game is implemented in Haskell and uses only basic libraries such as `System.IO` or `System.Random`

## Kent Questions

## 1. Items and Helpers

### 1.1 Opposite function

I'm not using this function in my code but you can find in the file `src/Room.hs` the function `opposite`

### 1.2 Item Datatype

You can find the datatype `Item` in the file `src/Item.hs` and some items in the folder `src/Item/`

### 1.3 Show item

You can find the functions `printItem` / `printItems` in the file `src/Item.hs` that does exactly what you asked for. You can also find the function `printPlayerInventory` in the file `src/Player.hs` that prints the player's inventory using the previous function.

### 1.4 EqItem

The `Item` datatype is an instance of the `Eq` typeclass. You can find the instance in the file `src/Item.hs`. So there is no need to implement the function `eqItem` as it is already implemented.

## 2. Creating a Map

the map is represented by a list of indexed rooms in `src/Game.hs`. Each room (in `src/Room.hs`) has a `nextRooms` attribute that contains the list of direction and the index of the next room.

### 2.1 Action

There is nothing like an action function that create a room and limit the actions of the player. You have two states in the game, the `Game` state and the `Fight` state. The `Game` state is used to navigate through the dungeon and the `Fight` state is used to fight the boss. Each state has its own set of actions.The game update the room in the `rooms` list of the `Game` state.

### 2.2 WinRoom

You can find the function in `src/Game.hs`

### 2.3 Room start

You can find the room start in the `src/Game.hs` file.

### 2.4 Default game

You can find the default game in the `src/Game.hs` file.

## 3. Parsing Command

### 3.1 Consume prefix

You can find the function `consumePrefix` in the file `src/Terminal.hs` but it's not used bc considered useless.

### 3.3 Parse Direction

the function `moveCommand` in the file `src/Terminal.hs` is used to parse the direction of the player's command.

### 3.4 Trim function

Not used but you can find the function `trim` in the file `src/Terminal.hs`

### 3.5 Parse Command

You can find the command `parseCommand` or `parseFightCommand` in the file `src/Terminal.hs` that parse the command of the player.

## 4. Game Engine

### 4.1 Delete from

You can find in `src/List.hs` the function `_deleteFrom` function

### 4.2 leaveRoom

There is some update funtion in the game for room, you can find some in the `src/Game.hs` file such as `updateRoom`, `saveCurrentRoom` or `getNextRoom` that are used to update the room when the player leaves it. 

### 4.3 CheckInventory  

There is no `checkInventory` used because there is more specific function such as `hasKey` or `findWeaponByName` that are used instead. But you can find an implementation in `src/Player/Inventory.hs`

### 4.4 Lookup item

There is no lookup item used in the game but you can find an implementation in `src/Item.hs` 


### 4.5 Step

in the `src/Lib.hs` you have a similar function called `evaluateCommand`
