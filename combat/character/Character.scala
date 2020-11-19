package combat.character

import combat.character.constants._
import combat.dice._
import combat.hexaGrid.{HexaGrid, HexaGridPos}
import combat.race._

abstract class Character(
                          val name: String,
                          val race: Race,
                          initialStat: Vector[Int],
                          initialPosition: HexaGridPos,
                          val board: HexaGrid,
                          initialHitPoint: Int = 0,
                          initialExperiencePoint: Int = 85000
                        ) {
  protected val diceSet = new DiceSet
  // TODO: implement random stat character initialization
  //  in the distant future, after many moons
  //  protected def this(name: String) =
  //    this(name, LazyList.continually(diceSet.roll(D20)).take(6).toVector, 0, 0)

  // These stats can be val for the moment
  // However they were kept var because they make sense
  // in a fully developed game
  protected var staticStats: Map[String, Int] = statsRefer.zip(initialStat.take(6)).toMap
  protected var temporaryStatsBonus: Map[String, Int] = initialStatsBonus
  protected var maxHp: Int = initialHitPoint
  protected var temporaryHp: Int = 0
  protected var currentHp: Int = initialHitPoint
  protected var currentExp: Int = initialExperiencePoint
  protected var currentPosition: HexaGridPos = initialPosition
  protected var initiative: Int = 0
  protected var remainingMovementSpeed: Int = 0
  protected var remainingActions: Int = 0
  protected var dmgImmunity: Vector[String] = Vector[String]()
  protected var dmgResistant: Vector[String] = Vector[String]()
  protected var potionVials: Int = 1
  val classBranch: String
  //  val background: String
  //  val alignment: String // not needed at the moment
  val actions: Int
  val armorClass: Int
  // These proficiencies will be affected by class and race,
  // should not implement here.
  val baseStatsProficiency: Vector[String]
  val abilityCheckProficiency: Vector[String]

  def speed: Int = this.race.speed

  def immunities: Vector[String] = this.dmgImmunity ++ this.race.dmgImmunity

  def resistances: Vector[String] = this.dmgResistant ++ this.race.dmgResistant

  def statProficiency(statAbbreviation: String): Int =
    if (this.baseStatsProficiency.contains(statAbbreviation)) this.proficiencyBonus else 0

  def abilityProficiency(abilityName: String): Int =
    if ((this.race.abilityCheckProficiency ++ this.abilityCheckProficiency)
      .contains(abilityName)) this.proficiencyBonus
    else 0

  def isAlive: Boolean = this.remainingHp <= 0

  def isFullHp: Boolean = this.remainingHp == this.maxHp

  protected def currentStat: Map[String, Int] =
    (for (stat <- statsRefer)
      yield stat -> (this.staticStats(stat) + this.temporaryStatsBonus(stat))
      ).toMap
      .withDefaultValue(0)

  protected def currentStatModifier: Map[String, Int] =
    this.currentStat
      .map(stats => stats._1 -> (stats._2 / 2 - 5))
      .withDefaultValue(0)

  protected def status: String = {
    val remainingHpPortion = this.remainingHp.toDouble / this.maxHp
    remainingHpPortion match {
      case n if n <= 0.0 => "death"
      case n if n <= 0.25 => "withering"
      case n if n <= 0.5 => "wounded"
      case n if n <= 0.75 => "angry"
      case _ => "powerful"
    }
  }

  // The following methods are to check the stats of the character
  def stat(statAbbreviation: String): Int = this.currentStatModifier(statAbbreviation)

  def proficiencyBonus: Int = {
    this.level match {
      case x if x < 5 => 2
      case x if x < 9 => 3
      case x if x < 13 => 4
      case x if x < 17 => 5
      case _ => 6
    }
  }

  def statSaving(statAbbreviation: String): Int =
    this.currentStatModifier(statAbbreviation) + this.statProficiency(statAbbreviation)

  def abilityCheck(abilityName: String): Int =
    this.abilityProficiency(abilityName) + this.currentStatModifier(abilitiesToStat(abilityName))

  //  def passivePerception: Int = this.abilityCheck("perception") + 10

  /** Level is determined by the amount of cumulative EXPs. */
  def level: Int = {
    this.currentExp match {
      case x if x < 300 => 1
      case x if x < 900 => 2
      case x if x < 2700 => 3
      case x if x < 6500 => 4
      case x if x < 14000 => 5
      case x if x < 23000 => 6
      case x if x < 34000 => 7
      case x if x < 48000 => 8
      case x if x < 64000 => 9
      case x if x < 85000 => 10
      case x if x < 100000 => 11
      case x if x < 120000 => 12
      case x if x < 140000 => 13
      case x if x < 165000 => 14
      case x if x < 195000 => 15
      case x if x < 225000 => 16
      case x if x < 265000 => 17
      case x if x < 305000 => 18
      case x if x < 335000 => 19
      case _ => 20
    }
  }

  /** Carrying capacity of a character is 15 times
   * the strength stat of that character, in pounds
   * (yes, darn it, it's pounds not kgs). */
  def carryingCapacity: Int = this.currentStat("str") * 15

  /** Returns the current HP of a character.
   *
   * This includes temporary HP gained from spells and items. */
  protected def remainingHp: Int = this.currentHp + this.temporaryHp

  //  /** Total remaining HP, including temporary HP. */
  //  def hp: Int = this.remainingHp

  protected def inflictDmg(target: Character, dmg: Int, dmgType: String): Unit = {
    println(s"\tAttempt to inflict $dmg $dmgType damage(s) to ${target.name}")
    val actualDmg = target.takeDmg(dmg, dmgType)
    println(s"\t${target.name} took $actualDmg damage(s)")
    println(s"\t${target.name} looks ${target.status}")
  }

  protected def takeDmg(dmg: Int, dmgType: String): Int = {
    var actualDmg = dmg
    if (this.immunities.contains(dmgType)) actualDmg *= 0
    if (this.resistances.contains(dmgType)) actualDmg /= 2
    if (this.temporaryHp >= actualDmg) this.temporaryHp -= actualDmg
    else {
      this.currentHp -= (actualDmg - this.temporaryHp)
      this.temporaryHp = 0
    }
    actualDmg
  }

  protected def checkRolls: Int = this.diceSet.roll(this.diceSet.d20)

  protected def checkRolls(withAdvantage: Int): Int = {
    if (withAdvantage == 1) this.checkRollsAdvantage
    else if (withAdvantage == -1) this.checkRollsDisadvantage
    else this.checkRolls
  }

  protected def checkRollsAdvantage: Int = this.diceSet.rollAdvantage(this.diceSet.d20)

  protected def checkRollsDisadvantage: Int = this.diceSet.rollDisadvantage(this.diceSet.d20)

  protected def attackRoll(withAdvantage: Int = 0): Int = this.checkRolls(withAdvantage)

  protected def hasAdvantage(mOR: String): Int = {
    val neighborCharacters: Int = this.currentPosition.neighbors.count(this.board(_).character.isDefined)
    if (mOR == "melee" && neighborCharacters > 1) 1
    else if (mOR == "range" && neighborCharacters > 0) -1
    else 0
  }

  protected def isCriticalHit(atkDice: Int, dmgDice: Dice): Int = {
    if (atkDice == 20) dmgDice.value
    else 0
  }

  def savingThrow(statAbbreviation: String,
                  withAdvantage: Int = 0,
                  withBlessing: Boolean = false): Int = {
    this.checkRolls(withAdvantage) +
      this.statSaving(statAbbreviation) +
      (if (withBlessing) this.diceSet.blessing
      else 0)
  }

  protected def meleeOrRange(mOR: String): Int =
    if (mOR == "range") this.currentStatModifier("dex")
    else this.currentStatModifier("str")

  protected def isSucceed(target: Character, atkRoll: Int): Boolean =
    atkRoll >= target.armorClass

  protected def drinkPotion(): Boolean = {
    if (this.potionVials > 0) {
      this.currentHp += math.min(
        this.maxHp - this.currentHp,
        this.diceSet.roll(4)(this.diceSet.d4) + 4
      )
      println(s"Drank potion, remaining HP is: ${this.remainingHp}")
      true
    } else {
      println("Oh no! There's no more potion left!")
      false
    }
  }

  protected def callAction(target: Character, n: Int): Boolean

  /** Different character has different attack patterns/options.
   *
   * Look for this in specific character classes. */
  def action(target: Character, n: Int): Boolean = {
    if (this.remainingActions > 0) {
      val acted = this.callAction(target, n)
      if (acted) {
        this.remainingActions -= 1
        true
      } else false
    } else {
      println("You have used up your actions in this turn!")
      false
    }
  }

  /** Move toward a desirable destination.
   *
   * Return true if the movement is possible
   * and false otherwise.
   * Character can only move to a certain location
   * if there's sufficient speed remaining,
   * the position exists and not occupied.
   *
   * @param destination a position on the HexaGrid board. */
  def moveToward(destination: HexaGridPos): Boolean = {
    val distance = this.currentPosition.distance(destination) * 5
    if (distance < this.remainingSpeed && this.board.elementAt(destination).isEmpty) {
      if (this.race.bonusSpeed >= distance)
        this.race.bonusSpeed -= distance
      else {
        this.remainingMovementSpeed -= (distance - this.race.bonusSpeed)
      }
      this.board.swap(this.currentPosition, destination)
      this.currentPosition = destination
      true
    } else {
      println("This position is either too far away, not available or not exist in this board.")
      false
    }
  }

  protected def remainingSpeed: Int = this.remainingMovementSpeed + this.race.bonusSpeed

  protected def replenishSpeed(): Unit = {
    this.race.bonusSpeed = 0
    this.remainingMovementSpeed = this.speed + this.currentStatModifier("speed")
  }

  protected def replenishAction(): Unit = this.remainingActions = this.actions

  protected def distance(target: Character): Int = this.currentPosition.distance(target.currentPosition) * 5

  def newTurn(): Unit = {
    this.replenishSpeed()
    this.replenishAction()
  }

  def checkOtherCharacterPosition: String = {
    this.board
      .allHexagons
      .filter(_.character.isDefined)
      .map(_.character.get)
      .filter(_.currentPosition != this.currentPosition)
      .map(c => c.name + ": " + c.currentPosition)
      .mkString("\n")
  }

  def checkStatus(): String = {
    s"$this, level ${this.level}\n" +
      s"You look ${this.status}\n" +
      s"Remaining HP: ${this.remainingHp}\n" +
      s"Remaining speed: ${this.remainingSpeed}\n" +
      s"Remaining action: ${this.remainingActions}\n" +
      s"Current location: ${this.currentPosition}"
  }

  def checkTarget(target: Character): String = target.status

  /** Use at the beginning of a combat to determine the order of turns. */
  def initiativeRoll: Int = {
    this.initiative = this.diceSet.roll(this.diceSet.d20) + this.currentStatModifier("dex")
    println(s"$this: ${this.initiative}")
    this.initiative
  }

  override def toString: String = this.name + " the " + this.classBranch
}
