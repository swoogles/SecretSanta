
import Main.logic
import zio.{Schedule, Scope, ZIO, ZIOAppArgs, ZIOAppDefault, ZLayer, durationInt}

object AirzonaKidsTest extends ZIOAppDefault:
  override def run =
    (logic).provide(
      ZLayer.derive[EmailBuilder.Kid],
      //      EmailService.live, // TODO Make quarantined area that uses this
      ZLayer.derive[EmailServiceTest],
      ZLayer.derive[ParticipantService.Live]
    )
