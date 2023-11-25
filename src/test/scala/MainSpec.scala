import zio.*
import zio.direct.*
import zio.test.*

case class EmailServiceTest() extends EmailService:
  def send(to: String, content: String): ZIO[Any, Throwable, Unit] =
    Console.printLine(s"Sending email to $to with content $content")

case class ParticipantServiceTest() extends ParticipantService:

  val readParticipants: ZIO[Any, Throwable, List[Participant]] =
    ZIO.succeed:
      List(
        Participant(
          "Anna",
          "diaz@email.com"
        ),
        Participant(
          "Paul",
          "diaz@email.com"
        ),
        Participant(
          "James",
          "diaz@email.com"
        ),
        Participant(
          "Charlotte",
          "nicks@email.com"
        ),
        Participant(
          "Spencer",
          "nicks@email.com"
        ),
        Participant(
          "Alice",
          "nicks@email.com"
        ),
        Participant(
          "Morgan",
          "frasures@email.com"
        ),
      )

case class EmailBuilderTest() extends EmailBuilder:

  def buildEmail(pair: GiftPair): String =
    s"${pair.from.name} ==> ${pair.to.name}"

object MainSpec extends ZIOSpecDefault:
  def spec =
    suite("MainSpec")(
      test("test") {
        defer:
          Main.logic.run
          assertCompletes
      }
    ).provide(
      ZLayer.derive[EmailServiceTest],
      ZLayer.derive[EmailBuilderTest],
      ZLayer.derive[ParticipantServiceTest]
    ) @@ TestAspect.withLiveRandom