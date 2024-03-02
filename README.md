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