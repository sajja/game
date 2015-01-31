package com.example.game.models

object DamageType extends Enumeration {
  val NORMAL, MAGIC, FIRE, POISON, NONE = Value
}

abstract class Weapon(val name: String, val damage: Damage) {
  def use(): Seq[(DamageType.Value,Int)] = {
    println("\tWeapon: " + name)
    val totalDamage = damage.getDamage
    totalDamage
  }

  def canWield(character: Character): Boolean = true
}

class InflictedDamage(val normal: Map[String, String])

class MeeleWeapon(name: String, damage: Damage) extends Weapon(name, damage)

class RangeWeapon(name: String, damage: Damage) extends Weapon(name, damage)

class MagicWeapon(name: String, damage: Damage) extends Weapon(name, damage)

class Bow(name: String, damage: Damage) extends RangeWeapon(name, damage)

class Sword(name: String, damage: Damage) extends MeeleWeapon(name, damage)

class Axe(name: String, damage: Damage) extends MeeleWeapon(name, damage)

class BattleAxe(name: String, damage: Damage) extends Axe(name, damage) {
  override def canWield(character: Character): Boolean = {
    character.race match {
      case i: Orc => true
      case _ => false
    }
  }
}

abstract class Damage(name: String, amount: Int, damageType: DamageType.Value, enhancement: Option[Damage]) {
  def this(name: String, amount: Int, damageType: DamageType.Value) = {
    this(name, amount, damageType, None)
  }

  def getDamage: Seq[(DamageType.Value,Int)] = {
    println("\t\tDamage type: " + name + ", Damage: " + amount)
    enhancement match {
      case Some(enhancement) => enhancement.getDamage :+(damageType,amount)
      case _ => Seq((damageType,amount))
    }
  }
}

case class Normal(name: String, amount: Int) extends Damage(name, amount, DamageType.NORMAL)

case class FireEnhance(name: String, amount: Int, enhancement: Option[Damage]) extends Damage(name, amount, DamageType.FIRE, enhancement)

case class PoisonEnhance(name: String, amount: Int, enhancement: Option[Damage]) extends Damage(name, amount, DamageType.POISON, enhancement)
