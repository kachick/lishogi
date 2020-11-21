package lila.tournament

import org.joda.time.DateTime
import play.api.data._
import play.api.data.Forms._
import play.api.data.validation
import play.api.data.validation.{ Constraint, Constraints }

import chess.Mode
import chess.StartingPosition
import lila.common.Form._
import lila.hub.LightTeam._
import lila.user.User

final class DataForm {

  import DataForm._

  def create(user: User, teamBattleId: Option[TeamID] = None) =
    form(user) fill TournamentSetup(
      name = teamBattleId.isEmpty option user.titleUsername,
      clockTime = clockTimeDefault,
      clockByoyomi = clockByoyomiDefault,
      minutes = minuteDefault,
      waitMinutes = waitMinuteDefault.some,
      startDate = none,
      variant = chess.variant.Standard.id.toString.some,
      position = StartingPosition.initial.fen.some,
      password = None,
      mode = none,
      rated = true.some,
      conditions = Condition.DataForm.AllSetup.default,
      teamBattleByTeam = teamBattleId,
      berserkable = true.some,
      streakable = true.some,
      description = none,
      hasChat = true.some
    )

  def edit(user: User, tour: Tournament) =
    form(user) fill TournamentSetup(
      name = tour.name.some,
      clockTime = tour.clock.limitInMinutes,
      clockByoyomi = tour.clock.byoyomiSeconds,
      minutes = tour.minutes,
      waitMinutes = none,
      startDate = tour.startsAt.some,
      variant = tour.variant.id.toString.some,
      position = tour.position.fen.some,
      mode = none,
      rated = tour.mode.rated.some,
      password = tour.password,
      conditions = Condition.DataForm.AllSetup(tour.conditions),
      teamBattleByTeam = none,
      berserkable = tour.berserkable.some,
      streakable = tour.streakable.some,
      description = tour.description,
      hasChat = tour.hasChat.some
    )

  private val nameType = clean(text).verifying(
    Constraints minLength 2,
    Constraints maxLength 30,
    Constraints.pattern(
      regex = """[\p{L}\p{N}-\s:,;]+""".r,
      error = "error.unknown"
    ),
    Constraint[String] { (t: String) =>
      if (t.toLowerCase contains "lishogi")
        validation.Invalid(validation.ValidationError("Must not contain \"lishogi\""))
      else validation.Valid
    }
  )

  private def form(user: User) =
    Form(
      mapping(
        "name"         -> optional(nameType),
        "clockTime"    -> numberInDouble(clockTimeChoices),
        "clockByoyomi" -> numberIn(clockByoyomiChoices),
        "minutes" -> {
          if (lila.security.Granter(_.ManageTournament)(user)) number
          else numberIn(minuteChoices)
        },
        "waitMinutes"      -> optional(numberIn(waitMinuteChoices)),
        "startDate"        -> optional(inTheFuture(ISODateTimeOrTimestamp.isoDateTimeOrTimestamp)),
        "variant"          -> optional(text.verifying(v => guessVariant(v).isDefined)),
        "position"         -> optional(nonEmptyText),
        "mode"             -> optional(number.verifying(Mode.all map (_.id) contains _)), // deprecated, use rated
        "rated"            -> optional(boolean),
        "password"         -> optional(nonEmptyText),
        "conditions"       -> Condition.DataForm.all,
        "teamBattleByTeam" -> optional(nonEmptyText),
        "berserkable"      -> optional(boolean),
        "streakable"       -> optional(boolean),
        "description"      -> optional(clean(nonEmptyText)),
        "hasChat"          -> optional(boolean)
      )(TournamentSetup.apply)(TournamentSetup.unapply)
        .verifying("Invalid clock", _.validClock)
        .verifying("15s variant games cannot be rated", _.validRatedUltraBulletVariant)
        .verifying("Increase tournament duration, or decrease game clock", _.sufficientDuration)
        .verifying("Reduce tournament duration, or increase game clock", _.excessiveDuration)
    )
}

object DataForm {

  import chess.variant._

  val clockTimes: Seq[Double] = {
    (1 to 9 by 1) ++ (10 to 30 by 5) ++ (40 to 60 by 10)
  }.map(_.toDouble)
  val clockTimeDefault = 5d
  private def formatLimit(l: Double) =
    chess.Clock.Config(l * 60 toInt, 0).limitString + {
      if (l <= 1) " minute" else " minutes"
    }
  val clockTimeChoices = optionsDouble(clockTimes, formatLimit)

  val clockByoyomis       = (0 to 60 by 10)
  val clockByoyomiDefault = 10
  val clockByoyomiChoices = options(clockByoyomis, "%d second{s}")

  val minutes       = (30 to 120 by 10) ++ (150 to 240 by 30) ++ (300 to 360 by 60)
  val minuteDefault = 60
  val minuteChoices = options(minutes, "%d minute{s}")

  val waitMinutes       = Seq(5, 10, 15, 20, 25, 30, 40, 50, 60)
  val waitMinuteChoices = options(waitMinutes, "%d minute{s}")
  val waitMinuteDefault = 5

  val positions = StartingPosition.allWithInitial.map(_.fen)
  val positionChoices = StartingPosition.allWithInitial.map { p =>
    p.fen -> p.fullName
  }
  val positionDefault = StartingPosition.initial.fen

  val validVariants =
    List(Standard)

  def guessVariant(from: String): Option[Variant] =
    validVariants.find { v =>
      v.key == from || from.toIntOption.exists(v.id ==)
    }

  def startingPosition(fen: String, variant: Variant): StartingPosition =
    Thematic.byFen(fen).ifTrue(variant.standard) | StartingPosition.initial
}

private[tournament] case class TournamentSetup(
    name: Option[String],
    clockTime: Double,
    clockByoyomi: Int,
    minutes: Int,
    waitMinutes: Option[Int],
    startDate: Option[DateTime],
    variant: Option[String],
    position: Option[String],
    mode: Option[Int], // deprecated, use rated
    rated: Option[Boolean],
    password: Option[String],
    conditions: Condition.DataForm.AllSetup,
    teamBattleByTeam: Option[String],
    berserkable: Option[Boolean],
    streakable: Option[Boolean],
    description: Option[String],
    hasChat: Option[Boolean]
) {

  def validClock = (clockTime + clockByoyomi) > 0

  def realMode = Mode(rated.orElse(mode.map(Mode.Rated.id ==)) | true)

  def realVariant = variant.flatMap(DataForm.guessVariant) | chess.variant.Standard

  def clockConfig = chess.Clock.Config((clockTime * 60).toInt, clockByoyomi)

  def validRatedUltraBulletVariant =
    realMode == Mode.Casual ||
      lila.game.Game.allowRated(realVariant, clockConfig.some)

  def sufficientDuration = estimateNumberOfGamesOneCanPlay >= 3
  def excessiveDuration  = estimateNumberOfGamesOneCanPlay <= 150

  def isPrivate = password.isDefined || conditions.teamMember.isDefined

  private def estimateNumberOfGamesOneCanPlay: Double = (minutes * 60) / estimatedGameSeconds

  // add 15 seconds for pairing delay
  private def estimatedGameSeconds: Double = {
    60 * (clockTime + clockByoyomi) * 2
  } + 15
}
