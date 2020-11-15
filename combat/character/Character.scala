package combat.character

import combat.character.constants._
import combat.dice._
import combat.hexaGrid.{HexaGrid, HexaGridPos}

abstract class Character(
                          val name: String,
                          initialStat: Vector[Int],
                          initialPosition: HexaGridPos,
                          val board: HexaGrid,
                          initialHitPoint: Int = 0,
                          initialExperiencePoint: Int = 0
                        ) {
  private val diceSet = new DiceSet
  // TODO: implement random stat character initialization
  //  in the distant future, after many moons
  //  protected def this(name: String) =
  //    this(name, LazyList.continually(diceSet.roll(D20)).take(6).toVector, 0, 0)

  // These stats can be val for the moment
  // However they were kept var because they make sense
  // in a fully developed game
  private var staticStats: Map[String, Int] = statsRefer.zip(initialStat.take(6)).toMap
  private var temporaryStatsBonus: Map[String, Int] = initialStatsBonus
  private var maxHp: Int = initialHitPoint
  private var temporaryHp: Int = 0
  private var currentHp: Int = initialHitPoint
  private var currentExp: Int = initialExperiencePoint
  private var currentPosition: HexaGridPos = initialPosition
  private var dmgImmunity: Vector[String] = Vector[String]()
  private var dmgResistant: Vector[String] = Vector[String]()
  private var initiative: Int = 0
  val race: String
  val classBranch: String
  val background: String
  val alignment: String
  val speed: Int
  val actions: Int
  val armorClass: Int

  private var remainingMovementSpeed: Int = this.speed
  private var remainingActions: Int = 0

  /** These proficiencies will be affected by class and race,
   * should not implement here. */
  def baseStatsProficiency: Map[String, Int]

  private def currentStat: Map[String, Int] =
    statsRefer
      .zip(
        this.staticStats
          .zip(this.temporaryStatsBonus)
          .map(stats => stats._1._2 + stats._2._2)
      ).toMap
      .withDefaultValue(0)

  private def currentStatModifier: Map[String, Int] =
    this.currentStat
      .map(stats => stats._1 -> (stats._2 / 2 - 5))
      .withDefaultValue(0)

  private def status: String = {
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
  def stat(statAbbreviation: String): Int = this.currentStat(statAbbreviation)

  def proficiencyBonus: Int = {
    this.level match {
      case x if x < 5 => 2
      case x if x < 9 => 3
      case x if x < 13 => 4
      case x if x < 17 => 5
      case _ => 6
    }
  }

  def statSaving(statAbbreviation: String): Int

  def abilityCheck(abilityName: String): Int

  def passivePerception: Int = this.abilityCheck("perception") + 10

  //  def remainingSpeed: Int = this.remainingMovementSpeed

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
  private def remainingHp: Int = this.currentHp + this.temporaryHp

  //  /** Total remaining HP, including temporary HP. */
  //  def hp: Int = this.remainingHp

  private def inflictDmg(target: Character, dmg: Int, dmgType: String): Unit = {
    println(s"Attempt to inflict $dmg $dmgType damage(s) to ${target.name}")
    val actualDmg = target.takeDmg(dmg, dmgType)
    println(s"${target.name} took $actualDmg damage(s)")
    println(s"${target.name} looks ${target.status}")
  }

  private def takeDmg(dmg: Int, dmgType: String): Int = {
    var actualDmg = dmg
    if (this.dmgImmunity.contains(dmgType)) actualDmg *= 0
    if (this.dmgResistant.contains(dmgType)) actualDmg /= 2
    if (this.temporaryHp >= actualDmg) this.temporaryHp -= actualDmg
    else {
      this.currentHp -= (actualDmg - this.temporaryHp)
      this.temporaryHp = 0
    }
    actualDmg
  }

  /** Different character has different attack patterns/options.
   *
   * Look for this in specific character classes. */
  def action(target: Character): Unit = {
    if (this.remainingActions != 0) {
      this.inflictDmg(target, 0, "normal")
      this.remainingActions -= 1
    } else println("All actions have been used up!")
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
    val distance = this.currentPosition.distance(destination)
    if (distance > this.remainingMovementSpeed && this.board.elementAt(destination).isEmpty) {
      this.remainingMovementSpeed -= distance
      this.board.swap(this.currentPosition, destination)
      true
    } else {
      println("This position is either too far away, not available or not exist in this board.")
      false
    }
  }

  private def replenishSpeed(): Unit = this.remainingMovementSpeed = this.speed - this.currentStatModifier("speed")

  private def replenishAction(): Unit = this.remainingActions = this.actions

  def newTurn(): Unit = {
    this.replenishSpeed()
    this.replenishAction()
  }

  def checkStatus(): Unit = {
    println(s"$this, level ${this.level}")
    println(s"You look ${this.status}")
    println(s"Remaining HP: ${this.remainingHp}")
    println(s"Remaining speed: ${this.remainingMovementSpeed}")
    println(s"Current location: ${this.currentPosition}")
  }

  /** Use at the beginning of a combat to determine the order of turns. */
  def initiativeRoll: Int = {
    this.initiative = this.diceSet.roll(this.diceSet.d20) + this.currentStatModifier("dex")
    println(s"$this: ${this.initiative}")
    this.initiative
  }

  override def toString: String = this.name + " the " + this.classBranch
}
