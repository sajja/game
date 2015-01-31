package com.example.game

import akka.actor.ActorSystem
import com.example.game.models._
import org.scalatest.{BeforeAndAfterAll, Matchers, FlatSpec}

import scala.util.{Try, Failure, Success}

class TestGame extends FlatSpec with Matchers with BeforeAndAfterAll {
  implicit val system = ActorSystem("Game")

  val human = new Human("Human")
  val elf = new Elf("Elf")
  val orc = new Orc("Orc")

  val normalArmour = new NormalArmour(5)
  val heavyArmour = new HeavyArmour(10)
  val fireProtectedHeavyArmour = new FireDampenArmour(5, Some(heavyArmour))

  val normalDamage = Normal("Normal", 10)

  val arthur = new Warrior("King arthur", 50, human)
  val hellscreem = new Warrior("Hellscreem", 80, orc)
  val archer = new Ranger("Some archer", 30, human)

  val woodenBow = new Bow("Elvan bow", normalDamage)
  val excaliber = new Sword("Excaliber", normalDamage)
  val battleAxe = new BattleAxe("SoulCarver", normalDamage)

  "Character" should "not be able to fight without a weapon" in {
    getDamage(arthur.useWeapon(hellscreem)) shouldBe 0
  }

  "Archer" should "be able to equip a bow" in {
    archer.canEquip(woodenBow) shouldBe true
  }

  "Archer" should "be not able to equip a sword" in {
    archer.canEquip(excaliber) shouldBe false
  }

  "Arthur" should "be able to equip a sword" in {
    arthur.canEquip(excaliber) shouldBe true
  }

  "Arthur" should "be not able to equip a bow" in {
    arthur.canEquip(woodenBow) shouldBe false
  }

  "Hellscreem" should "be able do more damage than arthur" in {
    arthur.equip(excaliber) shouldBe true
    hellscreem.equip(excaliber) shouldBe true
    val arthurDamage = getDamage(arthur.useWeapon(hellscreem))
    val hellscreemDamage = getDamage(hellscreem.useWeapon(arthur))
    hellscreemDamage should be > arthurDamage
  }

  "Weapons" should "be customized" in {
    val fireEnhanced = new FireEnhance("Fire damage", 20, Some[Normal](normalDamage))
    val dragonBlade = new Sword("Dragon blade", fireEnhanced)

    val faryFire = new PoisonEnhance("Poison damage", 5, Some[FireEnhance](fireEnhanced))
    val soulClaver = new Sword("SoulCalver", faryFire)

    dragonBlade.use().foreach {
      case (DamageType.NORMAL, amount: Int) => amount shouldBe 10
      case (DamageType.FIRE, amount: Int) => amount shouldBe 20
    }
    soulClaver.use().foreach {
      case (DamageType.NORMAL, amount: Int) => amount shouldBe 10
      case (DamageType.FIRE, amount: Int) => amount shouldBe 20
      case (DamageType.POISON, amount: Int) => amount shouldBe 5
    }
  }

  "BattleAxe" should "not be wield by humans" in {
    arthur.canEquip(battleAxe) shouldBe false
  }

  "BattleAxe" should "only be wield by orc" in {
    hellscreem.canEquip(battleAxe) shouldBe true
  }

  "When attacked target" should "take damage" in {
    val grunt = new Warrior("Grunt", 50, orc)
    val longSword = new Sword("Exotic sword", new FireEnhance("Fire", 20, Some[Normal](normalDamage)))
    arthur.equip(longSword)
    arthur.useWeapon(grunt)
    grunt.hitPoints shouldBe 20
  }

  "Dead minion" should "fight no more" in {
    val minion = new Warrior("Minion", 1, human)
    arthur.useWeapon(minion)
    minion.state.name shouldBe "Dead"

    minion.equip(excaliber) match {
      case Success(state) => fail("Dead minion should not fight")
      case Failure(ex) => ex.getMessage should include ("Dead Human fights no more")
    }

    minion.equip(normalArmour) match {
      case Success(state) => fail("Dead minion should not fight")
      case Failure(ex) => ex.getMessage should include ("Dead Human fights no more")
    }

    minion.useWeapon(arthur) match {
      case Success(state) => fail("Dead minion should not fight")
      case Failure(ex) => ex.getMessage should include ("Dead Human fights no more")
    }
  }

  private def getDamage(damage:Try[Seq[(DamageType.Value, Int)]]):Int = {
    damage  match {
      case Success(damage) => damage.reduce((d1, d2) => (d1._1, d1._2 + d2._2))._2
      case Failure(ex) => fail(ex)
    }
  }

  override def beforeAll(): Unit = {
  }
}