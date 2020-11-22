package combat.game

import combat.character._
import combat.character.constants.{abilities, statsRefer}
import combat.hexaGrid.HexaGrid
import combat.race.Tabaxi

class Action(val input: String) {
  private val command = input.trim.toLowerCase
  private val mainCommand = this.command.takeWhile(_ != ' ')
  private val context = this.command.split("\\s+")

  private def optionStringToNumber(optionString: Option[String], defaultValue: Int = -1): Int = {
    (optionString match {
      case Some(numberString) => numberString.toIntOption
      case None => None
    }) match {
      case Some(number) => number
      case None => defaultValue
    }
  }

  private def getContextString(n: Int): String = {
    this.context.lift(n) match {
      case Some(string) => string
      case None => ""
    }
  }

  private def isCharacterName(board: HexaGrid, name: String): Boolean = {
    board.allCharacters.map(_.name).contains(name)
  }

  def execute(actor: Character, board: HexaGrid): String = {
    if (command == "action surge") {
      actor match {
        case arcane: ArcaneArcher => arcane.actionSurge()
        case _ => "You are not an Arcane Archer"
      }
    } else if (command == "second wind") {
      actor match {
        case arcane: ArcaneArcher => arcane.secondWind()
        case _ => "You are not an Arcane Archer"
      }
    } else if (command == "feline agility") {
      actor.race match {
        case cat: Tabaxi => cat.felineAgility()
        case _ => "You are not a Tabaxi"
      }
    } else if (command == "lay on hands") {
      ""
    } else if (command == "healing word") {
      ""
    } else {
      this.mainCommand match {
        case "action" =>
          val actionString = this.context.lift(1)
          val targetName = this.getContextString(2)
          val actionNumber = this.optionStringToNumber(actionString, 9999)
          if (!isCharacterName(board, targetName)) {
            "Invalid target"
          } else {
            val targetCharacter = board.allCharacters
              .filter(_.name == targetName)
              .apply(0)
            actor.action(targetCharacter, actionNumber)
          }
        case "move" =>
          val x = this.optionStringToNumber(this.context.lift(1))
          val y = this.optionStringToNumber(this.context.lift(2))
          actor.moveToward(x, y)
        case "distance" =>
          val optionX = this.getContextString(1)
          val x = this.optionStringToNumber(this.context.lift(1))
          val y = this.optionStringToNumber(this.context.lift(2))
          if (this.isCharacterName(board, optionX)) {
            val targetCharacter = board.allCharacters
              .filter(_.name == optionX)
              .apply(0)
            actor.distance(targetCharacter).toString
          } else {
            actor.distance(x, y).toString
          }
        case "check" =>
          val what = this.getContextString(1)
          val whatOption = this.getContextString(2)
          what match {
            case "status" => actor.checkStatus
            case "locations" => actor.checkOtherCharacterPosition
            case "saving" => if (statsRefer.contains(whatOption)) {
              actor.statSaving(whatOption).toString
            } else "Wrong stats abbreviation."
            case "ability" => if (abilities.contains(whatOption)) {
              actor.abilityCheck(whatOption).toString
            } else "Wrong ability name."
            case _ =>
              if (this.isCharacterName(board, what)) {
                val targetStatus = actor.checkTarget(board.allCharacters.filter(_.name == what)(0))
                s"$what looks $targetStatus"
              } else {
                if (statsRefer.contains(what)) {
                  s"$what: ${actor.stat(what)}"
                } else "Wrong stats abbreviation."
              }
          }
        case _ => "Invalid command."
      }
    }
  }
}

object Action {
  def apply(input: String) = new Action(input)
}
