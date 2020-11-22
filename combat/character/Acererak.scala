package combat.character

import combat.hexaGrid.{GameBoard, HexaGridPos}
import combat.race.Undead

object Acererak extends Character(
  "Acererak",
  new Undead,
  Vector[Int](13, 16, 20, 27, 21, 20),
  HexaGridPos(13, 7),
  GameBoard,
  285,
  335000
) {
  override val classBranch: String = "Caster"
  override val actions: Int = 1
  override val armorClass: Int = 21
  override val baseStatsProficiency: Vector[String] = Vector[String]("con", "int", "wis")
  override val abilityCheckProficiency: Vector[String] = Vector[String]("arcana", "history")
  //  override val dmgImmunity: Vector[String] = Vector[String]("normal")
  //  override val dmgResistant: Vector[String] = Vector[String]("cold", "lightning")
  private var deathFingers = 2
  private var deathWords = 2
  this.dmgImmunity = this.dmgImmunity ++ Vector[String]("normal")
  this.dmgResistant = this.dmgResistant ++ Vector[String]("cold", "lightning")

  override def proficiencyBonus: Int = 7

  override def abilityProficiency(abilityName: String): Int =
    if ((this.race.abilityCheckProficiency ++ this.abilityCheckProficiency)
      .contains(abilityName)) this.proficiencyBonus * 2
    else 0

  private def spellSave: Int = 8 + this.spellAttack

  private def spellAttack: Int = this.proficiencyBonus + this.stat("int")

  private def spellAttackRoll(mOR: String): (Int, Int) = {
    val atkDice = this.attackRoll(this.hasAdvantage(mOR))
    val atkRoll = atkDice + this.spellAttack
    println(s"Attack roll: $atkRoll")
    (atkDice, atkRoll)
  }

  private def rayOfFrost(target: Character): String = {
    val (atkDice, atkRoll) = this.spellAttackRoll("range")
    if (this.isSucceed(target, atkRoll)) {
      this.inflictDmg(target, this.diceSet.roll(4)(this.diceSet.d8) + this.meleeOrRange("range") + this.isCriticalHit(atkDice, this.diceSet.d8) * 4, "cold")
      this.imposeStatBonus(target, "speed", -10)
      s"${this.name} pointed his bony fingers at ${target.name}'s direction, a cold ray blasted at ${target.name} face. ${target.name} was slowed down due to the cold!\n"
    } else s"A beam of coldness raced toward ${target.name} but it unfortunately missed...\n"
  }

  private def fireball(target: Character): String = {
    val affectedChar = this.board
      .allHexagons
      .filter(_.character.isDefined)
      .map(_.character.get)
      .filter(c => target.distance(c) <= 15)
      .map(c => (c, c.savingThrow("dex")))
    var actionString = s"A tiny fireball shot from ${this.name}'s finger toward ${target.name} but then suddenly expanded on hit, consumed every creature close by...\n"
    val fireDmg = this.diceSet.roll(8)(this.diceSet.d6)
    for ((character, saving) <- affectedChar) {
      if (saving < this.spellSave) {
        this.inflictDmg(character, fireDmg, "fire")
        actionString += s"Moving like a slug, ${character.name} got trapped in the flame!\n"
      } else {
        this.inflictDmg(character, fireDmg / 2, "fire")
        actionString += s"With incredible reflexes, ${character.name} dodged the direct hit from the flame but still got a slight burn!\n"
      }
    }
    actionString
  }

  private def fingerOfDeath(target: Character): String = {
    val saving = target.savingThrow("con")
    if (this.spellSave > saving) {
      this.inflictDmg(target, this.diceSet.roll(7)(this.diceSet.d8) + 30, "necrotic")
      s"${this.name} pointed at ${target.name}, ${target.name} felt a searing pain throughout the body!\n"
    } else {
      this.inflictDmg(target, (this.diceSet.roll(7)(this.diceSet.d8) + 30) / 2, "necrotic")
      s"${this.name} pointed at ${target.name}, ${target.name} felt a searing pain throughout the body but managed to shake it off, still, that was horrible!\n"
    }
  }

  private def powerWordKill(target: Character): String = {
    if (target.isResilient) {
      s"${this.name} uttered an unintelligible phrase, ${target.name} felt a shiver but nothing happened. ${target.name} felt he/she could have died if he/she is just a slit weaker...\n"
    } else {
      this.inflictDmg(target, 999, "death")
      s"${this.name} uttered an unintelligible phrase, ${target.name} dropped dead...\n"
    }
  }

  private def randomAction: Int = {
    val actionRoll = this.diceSet.roll(this.diceSet.d20)
    if (actionRoll == 20 && this.deathWords > 0) 3
    else if (actionRoll == 19 && this.deathFingers > 0) 2
    else actionRoll % 2
  }

  private def randomTarget: Character = {
    val potentialTargets = this.board
      .allHexagons
      .filter(_.character.isDefined)
      .map(_.character.get)
      .filter(_.name != this.name)
      .filter(_.isAlive)
    potentialTargets(this.diceSet.roll(this.diceSet.d100) % potentialTargets.length)
  }

  def takeTurn(): String = {
    this.action(this.randomTarget, this.randomAction)
  }

  override protected def callAction(target: Character, n: Int): (Boolean, String) = {
    n match {
      // Boss is not allowed to drink potion
      //      case 0 =>
      //        this.drinkPotion()
      case 0 =>
        if (this.distance(target) > 120) {
          (false, "The target seems to be too far away to do this, try something else or move closer.\n")
        } else {
          (true, this.rayOfFrost(target))
        }
      case 1 =>
        if (this.distance(target) > 120) {
          (false, "The target seems to be too far away to do this, try something else or move closer.\n")
        } else {
          (true, this.fireball(target))
        }
      case 2 =>
        if (this.distance(target) > 120 || this.deathFingers < 1) {
          (false, "The target seems to be too far away to do this or you can't use this ability anymore, try something else or move closer.\n")
        } else {
          this.deathFingers -= 1
          (true, this.fingerOfDeath(target))
        }
      case 3 =>
        if (this.distance(target) > 120 || this.deathWords < 1) {
          (false, "The target seems to be too far away to do this or you can't use this ability anymore, try something else or move closer.\n")
        } else {
          this.deathWords -= 1
          (true, this.powerWordKill(target))
        }
      case _ => (false, "Unknown action, try something else.\n")
    }
  }
}
