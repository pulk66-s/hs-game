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

## 3. Parsing Command

### 3.1 Consume prefix

You can find the function `consumePrefix` in the file `src/Terminal.hs` but it's not used bc considered useless.

### 3.3 Parse Direction

the function `moveCommand` in the file `src/Terminal.hs` is used to parse the direction of the player's command.

### 3.4 Trim function

Not used but you can find the function `trim` in the file `src/Terminal.hs`

### 3.5 Parse Command

You can find the command `parseCommand` or `parseFightCommand` in the file `src/Terminal.hs` that parse the command of the player.
