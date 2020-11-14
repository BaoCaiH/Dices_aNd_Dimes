package combat.character

import combat.character.constants._
import combat.dice._

abstract class Character(
                          val name: String,
                          initialStat: Vector[Int],
                          initialHitPoint: Int = 0,
                          initialExperiencePoint: Int = 0
                        ) {
  private val diceSet = new DiceSet
  // TODO: implement random stat character initialization
  //  in the distant future
  //  protected def this(name: String) =
  //    this(name, LazyList.continually(diceSet.roll(D20)).take(6).toVector, 0, 0)

  private var staticStats: Map[String, Int] = statsRefer.zip(initialStat.take(6)).toMap
  private var temporaryStatsBonus: Map[String, Int] = initialStatsBonus
  private var maxHp: Int = initialHitPoint
  private var temporaryHp: Int = 0
  private var currentHp: Int = initialHitPoint
  val race: String
  val classBranch: String
  val background: String
  val alignment: String
  val speed: Int
  val armorClass: Int

  private var remainingMovementSpeed: Int = this.speed

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

  def stat(statAbbreviation: String): Int = this.currentStat(statAbbreviation)

  def proficiencyBonus: Int

  def statSaving(statAbbreviation: String): Int

  def abilityCheck(abilityName: String): Int

  def passivePerception: Int = this.abilityCheck("perception") + 10

  def level: Int

  def carryingCapacity: Int = this.currentStat("str") * 15

  def remainingHp: Int = this.currentHp + this.temporaryHp

  def hp: Int = this.remainingHp

  private def inflictDmg(target: Character, dmg: Int, dmgType: String): Unit = {
    if (target.temporaryHp >= dmg) target.temporaryHp -= dmg
    else {
      target.currentHp -= (dmg - target.temporaryHp)
      target.temporaryHp = 0
    }
  }

  def attack(): Unit

  def moveToward(): Boolean

  override def toString: String = this.name + " the " + this.classBranch
}
