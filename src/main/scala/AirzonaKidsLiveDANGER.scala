
import Main.logic
import zio.{Schedule, Scope, ZIO, ZIOAppArgs, ZIOAppDefault, ZLayer, durationInt}

object AirzonaKidsLiveDANGER extends ZIOAppDefault:
  override def run =
    (logic).provide(
      ZLayer.derive[EmailBuilder.Kid],
      EmailService.live, // TODO Make quarantined area that uses this
      ZLayer.derive[ParticipantService.Live]
    )
