package com.example.game.models

import akka.actor.{Props, ActorSystem, Actor}

import scala.util.Try

trait CharacterState {
  def useWeapon(target: Character): Seq[(DamageType.Value, Int)]

  def takeDamage(damage: Seq[(DamageType.Value, Int)])

  def equip(weapon: Weapon): Boolean

  def equip(weapon: Armour): Boolean

  def name: String
}

abstract class Alive(val character: Character) extends CharacterState {
  override def useWeapon(target: Character): Seq[(DamageType.Value, Int)] = {
    character.primaryWeapon match {
      case Some(i) => {
        println(character.name + " attacks " + target.name)
        val damage = i.use().map { case (damageType, amount) => (damageType, amount + character.race.attack())}
        //enhance attack bonus
        println("\tattack bonus " + character.race.attack())
        damage.foreach(i => println("\t" + i._1 + " Damage" + i._2))
        target.takeDamage(damage)
        damage
      }
      case _ => {
        println("How can i fight without weapon")
        Seq((DamageType.NONE, 0))
      }
    }
  }

  override def equip(armour: Armour): Boolean = {
    if (character.canEquip(armour)) {
      character.armour = Some(armour)
      true
    } else {
      false
    }
  }

  override def equip(weapon: Weapon): Boolean = {
    if (character.canEquip(weapon)) {
      character.primaryWeapon = Some(weapon)
      true
    } else {
      false
    }
  }

  override def takeDamage(damage: Seq[(DamageType.Value, Int)]): Unit = {
    damage.foreach {
      case (damageType, amount) => {
        character.hitPoints -= amount
        if (character.hitPoints <= 0) {
          character.state = new Dead(character)
        } else {
          character.state = new Healthy(character)
        }
      }
    }
  }
}

class Healthy(character: Character) extends Alive(character) {
  override def name: String = "Healthy"
}

class Dead(character: Character) extends CharacterState {
  override def useWeapon(target: Character): Seq[(DamageType.Value, Int)] = throw new Exception("Dead " + character.race.name + " fights no more....")

  override def equip(weapon: Weapon): Boolean = throw new Exception("Dead " + character.race.name + " fights no more....")

  override def equip(weapon: Armour): Boolean = throw new Exception("Dead " + character.race.name + " fights no more....")

  override def takeDamage(damage: Seq[(DamageType.Value, Int)]): Unit = throw new Exception("Dead " + character.race.name + " takes no more damage ....")

  override def name: String = "Dead"
}

abstract class Character(val name: String, var hitPoints: Int, val race: Race) {
  var primaryWeapon: Option[Weapon] = None
  var armour: Option[Armour] = None
  var maxHitPoints = hitPoints
  var state: CharacterState = new Healthy(this)

  def useWeapon(target: Character): Try[Seq[(DamageType.Value, Int)]] = {
    Try(state.useWeapon(target))
  }

  def takeDamage(damage: Seq[(DamageType.Value, Int)]) = {
    state.takeDamage(damage)
  }

  def canEquip(weapon: Weapon): Boolean

  def canEquip(armour: Armour): Boolean = true

  def equip(weapon: Weapon): Try[Boolean] = {
    Try(state.equip(weapon))
  }

  def equip(armour: Armour): Try[Boolean] = {
    Try(state.equip(armour))
  }
}

class Ranger(name: String, hitPoints: Int, race: Race) extends Character(name, hitPoints, race) {
  override def canEquip(weapon: Weapon): Boolean = {
    weapon match {
      case i: Bow => i.canWield(this)
      case _ => false
    }
  }
}

class Warrior(name: String, hitPoints: Int, race: Race) extends Character(name, hitPoints, race) {
  override def canEquip(weapon: Weapon): Boolean = {
    weapon match {
      case w: MeeleWeapon => w.canWield(this)
      case _ => false
    }
  }
}