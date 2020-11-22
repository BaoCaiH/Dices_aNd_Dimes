package combat.character

import combat.hexaGrid.HexaGridPos

object constants {
  val statsRefer = Vector("str", "dex", "con", "int", "wis", "cha")
  val boostedStatsBonus: Map[String, Int] = statsRefer.zip(Vector(99, 99, 99, 99, 99, 99)).toMap
  val boosterTile: HexaGridPos = HexaGridPos(13, 5)
  val abilitiesToStat: Map[String, String] = Map(
    "acrobatic" -> "dex",
    "animal handling" -> "wis",
    "arcana" -> "int",
    "athletics" -> "str",
    "deception" -> "cha",
    "history" -> "int",
    "insight" -> "wis",
    "intimidation" -> "cha",
    "investigation" -> "int",
    "medicine" -> "wis",
    "nature" -> "int",
    "perception" -> "wis",
    "performance" -> "cha",
    "persuasion" -> "cha",
    "religion" -> "int",
    "sleight of hand" -> "dex",
    "stealth" -> "dex",
    "survival" -> "wis"
  ).withDefaultValue("none")
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
