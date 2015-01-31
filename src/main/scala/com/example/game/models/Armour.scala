package com.example.game.models

abstract class Armour(val amount:Int, val baseArmour:Option[Armour]) {
  def this(amount:Int) =  this(amount, None)
}

case class NormalArmour(override val amount:Int) extends Armour(amount)
case class HeavyArmour(override val amount:Int) extends Armour(amount)
case  class MagicDampenArmour(override val amount:Int, override val baseArmour:Option[Armour]) extends Armour(amount)
case  class PoisonDampenArmour(override val amount:Int, override val baseArmour:Option[Armour]) extends Armour(amount)
case  class FireDampenArmour(override val amount:Int, override val baseArmour:Option[Armour]) extends Armour(amount)

