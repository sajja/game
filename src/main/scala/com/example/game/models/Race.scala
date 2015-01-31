package com.example.game.models

abstract class Race (val name:String) {
  def speed() = 0
  def attack() = 0
  def armour() = 0
  def hitPoints() = 0
  def defence() = 0

}

class Human(name:String) extends Race(name) {
  override def defence() = 10
  override def armour() = 5
}

class Elf(name:String) extends Race(name)

class Orc(name:String) extends Race(name) {
  override def attack() = 10
}


