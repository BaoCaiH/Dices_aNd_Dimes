package combat.character

object constants {
  val statsRefer = Vector("str", "dex", "con", "int", "wis", "cha")
  val initialStatsBonus: Map[String, Int] = statsRefer.zip(Vector(0, 0, 0, 0, 0, 0)).toMap
  val abilities = Vector(
    "acrobatic",
    "animal handling",
    "arcana",
    "athletics",
    "deception",
    "history",
    "insight",
    "intimidation",
    "investigation",
    "medicine",
    "nature",
    "perception",
    "performance",
    "persuasion",
    "religion",
    "sleight of hand",
    "stealth",
    "survival"
  )
}
