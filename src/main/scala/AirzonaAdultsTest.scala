
import Main.logic
import zio.{Schedule, Scope, ZIO, ZIOAppArgs, ZIOAppDefault, ZLayer, durationInt}

object AirzonaAdultsLive extends ZIOAppDefault:
  override def run =
    (logic).provide(
      ZLayer.derive[EmailBuilder.AdultElfRevolt],
      //      EmailService.live, // TODO Make quarantined area that uses this
      ZLayer.derive[EmailServiceTest],
      ZLayer.derive[ParticipantService.Live]
    )
