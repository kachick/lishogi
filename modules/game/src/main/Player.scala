package lila.game

import cats.implicits._

import shogi.Color
import scala.util.chaining._

import lila.user.User

case class PlayerUser(id: String, rating: Int, ratingDiff: Option[Int])

case class Player(
    id: Player.ID,
    color: Color,
    engineConfig: Option[EngineConfig],
    isWinner: Option[Boolean] = None,
    isOfferingDraw: Boolean = false,
    lastDrawOffer: Option[Int] = None,
    proposeTakebackAt: Int = 0, // ply when takeback was proposed
    userId: Player.UserId = None,
    rating: Option[Int] = None,
    ratingDiff: Option[Int] = None,
    provisional: Boolean = false,
    blurs: Blurs = Blurs.blursZero.zero,
    berserk: Boolean = false,
    name: Option[String] = None
) {

  def playerUser =
    userId flatMap { uid =>
      rating map { PlayerUser(uid, _, ratingDiff) }
    }

  def isAi     = engineConfig.isDefined
  def aiLevel  = engineConfig.map(_.level)
  def aiEngine = engineConfig.map(_.engine)

  def isHuman = !isAi

  def hasUser = userId.isDefined

  def isUser(u: User) = userId.fold(false)(_ == u.id)

  def userInfos: Option[Player.UserInfo] =
    (userId, rating) mapN { (id, ra) =>
      Player.UserInfo(id, ra, provisional)
    }

  def wins = isWinner getOrElse false

  def goBerserk = copy(berserk = true)

  def finish(winner: Boolean) = {
    copy(isWinner = winner option true)
  }

  def offerDraw(turn: Int) =
    copy(
      isOfferingDraw = true,
      lastDrawOffer = Some(turn)
    )

  def removeDrawOffer = copy(isOfferingDraw = false)

  def proposeTakeback(ply: Int) = copy(proposeTakebackAt = ply)

  def removeTakebackProposition = copy(proposeTakebackAt = 0)

  def isProposingTakeback = proposeTakebackAt != 0

  def withName(name: String) = copy(name = name.some)

  def nameSplit: Option[(String, Option[Int])] =
    name map {
      case Player.nameSplitRegex(n, r) => n -> r.toIntOption
      case n                           => n -> none
    }

  def before(other: Player) =
    ((rating, id), (other.rating, other.id)) match {
      case ((Some(a), _), (Some(b), _)) if a != b => a > b
      case ((Some(_), _), (None, _))              => true
      case ((None, _), (Some(_), _))              => false
      case ((_, a), (_, b))                       => a < b
    }

  def ratingAfter = rating map (_ + ~ratingDiff)

  def stableRating = rating ifFalse provisional

  def stableRatingAfter = stableRating map (_ + ~ratingDiff)
}

object Player {

  private val nameSplitRegex = """([^(]++)\((\d++)\)""".r

  def make(
      color: Color,
      engineConfig: Option[EngineConfig] = none
  ): Player =
    Player(
      id = IdGenerator.player(color),
      color = color,
      engineConfig = engineConfig
    )

  def make(
      color: Color,
      userPerf: (User.ID, lila.rating.Perf)
  ): Player =
    make(
      color = color,
      userId = userPerf._1,
      rating = userPerf._2.intRating,
      provisional = userPerf._2.glicko.provisional
    )

  def make(
      color: Color,
      userId: User.ID,
      rating: Int,
      provisional: Boolean
  ): Player =
    Player(
      id = IdGenerator.player(color),
      color = color,
      engineConfig = none,
      userId = userId.some,
      rating = rating.some,
      provisional = provisional
    )

  def make(
      color: Color,
      user: Option[User],
      perfPicker: lila.user.Perfs => lila.rating.Perf
  ): Player =
    user.fold(make(color)) { u =>
      make(color, (u.id, perfPicker(u.perfs)))
    }

  case class HoldAlert(ply: Int, mean: Int, sd: Int) {
    def suspicious = HoldAlert.suspicious(ply)
  }
  object HoldAlert {
    type Map = Color.Map[Option[HoldAlert]]
    val emptyMap: Map                 = Color.Map(none, none)
    def suspicious(ply: Int): Boolean = ply >= 16 && ply <= 40
    def suspicious(m: Map): Boolean   = m exists { _ exists (_.suspicious) }
  }

  case class UserInfo(id: String, rating: Int, provisional: Boolean)

  import reactivemongo.api.bson.Macros
  implicit val holdAlertBSONHandler = Macros.handler[HoldAlert]

  object BSONFields {

    val aiLevel           = "ai"
    val aiEngine          = "et"
    val isOfferingDraw    = "od"
    val lastDrawOffer     = "ld"
    val proposeTakebackAt = "ta"
    val rating            = "e"
    val ratingDiff        = "d"
    val provisional       = "p"
    val blursBits         = "l"
    val holdAlert         = "h"
    val berserk           = "be"
    val name              = "na"
  }

  import reactivemongo.api.bson._
  import lila.db.BSON

  type ID      = String
  type UserId  = Option[String]
  type Win     = Option[Boolean]
  type Builder = Color => ID => UserId => Win => Player

  private def safeRange(range: Range, name: String)(userId: Option[String])(v: Int): Option[Int] =
    if (range contains v) Some(v)
    else {
      logger.warn(s"Player $userId $name=$v (range: ${range.min}-${range.max})")
      None
    }

  private val ratingRange     = safeRange(0 to 4000, "rating") _
  private val ratingDiffRange = safeRange(-1000 to 1000, "ratingDiff") _

  implicit val playerBSONHandler = new BSON[Builder] {

    import BSONFields._
    import Blurs._

    def reads(r: BSON.Reader) =
      color =>
        id =>
          userId =>
            win =>
              Player(
                id = id,
                color = color,
                engineConfig = r
                  .intO(aiLevel)
                  .map(
                    EngineConfig(
                      _,
                      r.strO(aiEngine)
                        .flatMap(EngineConfig.Engine.getByCode)
                        .getOrElse(EngineConfig.Engine.default)
                    )
                  ),
                isWinner = win,
                isOfferingDraw = r boolD isOfferingDraw,
                lastDrawOffer = r intO lastDrawOffer,
                proposeTakebackAt = r intD proposeTakebackAt,
                userId = userId,
                rating = r intO rating flatMap ratingRange(userId),
                ratingDiff = r intO ratingDiff flatMap ratingDiffRange(userId),
                provisional = r boolD provisional,
                blurs = r.getD[Blurs](blursBits, blursZero.zero),
                berserk = r boolD berserk,
                name = r strO name
              )

    def writes(w: BSON.Writer, o: Builder) =
      o(shogi.Sente)("0000")(none)(none) pipe { p =>
        BSONDocument(
          aiLevel           -> p.aiLevel,
          aiEngine          -> p.aiEngine.map(_.code),
          isOfferingDraw    -> w.boolO(p.isOfferingDraw),
          lastDrawOffer     -> p.lastDrawOffer,
          proposeTakebackAt -> w.intO(p.proposeTakebackAt),
          rating            -> p.rating,
          ratingDiff        -> p.ratingDiff,
          provisional       -> w.boolO(p.provisional),
          blursBits         -> p.blurs.nonEmpty.??(BlursBSONHandler writeOpt p.blurs),
          name              -> p.name
        )
      }
  }
}
