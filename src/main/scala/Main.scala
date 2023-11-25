import zio.{Scope, Schedule, ZIO, ZIOAppArgs, ZIOAppDefault, ZLayer, durationInt}
import zio.Console.printLine
import java.util.Properties
import zio.direct.*

import javax.mail.{Authenticator, Message, MessagingException, PasswordAuthentication, Session, Transport}
import javax.mail.internet.InternetAddress
import javax.mail.internet.MimeMessage
import scala.io.Source

/*
   OpenSenders: (Bill, Hali, Jay, Nancy) OpenReceivers: (Bill, Hali, Jay, Nancy)
   ClosedSenders: () ClosedReceivers: ()

   Jay -> Hali

   OpenSenders: (Bill, Hali, Nancy) OpenReceivers: (Bill, Jay, Nancy)
   ClosedSenders: (Jay) ClosedReceivers: (Hali)

*/

case class GiftPair(from: Participant, to: Participant)

case class State(
                matches: List[GiftPair],
                openReceivers: List[Participant]
                )


case class Participant(name: String, email: String, family: String)
object Participant:
  def matchMake(participants: List[Participant]) =
    for
      finalState <- ZIO.foldLeft(participants)(State(List.empty, participants))(
        (state, participant) =>
          for
            receiverIdx <- (ZIO.debug("Choosing random recipient") *> zio.Random.nextIntBounded(state.openReceivers.size))
              .repeatUntil { target =>
                val potentialReceiver = state.openReceivers(target)
                potentialReceiver != participant &&
                  // This ensures kids send a gift to a member of a different family :)
                  potentialReceiver.family != participant.family
              }
            receiver = state.openReceivers(receiverIdx)
          yield state.copy(matches = GiftPair(participant, receiver) :: state.matches, openReceivers = state.openReceivers.filter(_ != receiver))
      )
    yield finalState.matches

object Main extends ZIOAppDefault:
  override def run =
    (logic).provide(
      ZLayer.derive[EmailBuilder.Kid],
//      EmailService.live, // TODO Make quarantined area that uses this
      ZLayer.derive[EmailServiceTest],
      ZLayer.derive[ParticipantService.Live]
    )

  val logic =
    for
      participants <- ZIO.serviceWithZIO[ParticipantService](_.readParticipants)
      pairs <-
        Participant.matchMake(participants)
          .timeoutFail(Exception("Impossible matches"))(1.seconds)
          .retry(Schedule.forever)
      _ <- ZIO.debug("Pairs: " + pairs)
      // Test run that only sends to me
      //      _ <- ZIO.foreach(pairs.headOption)(pair => mailStuff(gmailSender.get, "bill@billdingsoftware.com", gmailAppPassword.get, buildEmail(pair)))
      _ <- ZIO.foreach(pairs)(
        pair =>
          for {
            emailContent <- ZIO.serviceWith[EmailBuilder](_.buildEmail(pair)).debug
            _ <- ZIO.serviceWithZIO[EmailService](_.send(pair.from.email, emailContent))
          } yield ()
      )
    yield ()

trait ParticipantService:
  val readParticipants: ZIO[Any, Throwable, List[Participant]]


object ParticipantService:
  // TODO Accept filename as param
  case class Live() extends ParticipantService:
    val readParticipants = ZIO.attempt {
      val lines = Source.fromFile("az_christmas_kids.txt").getLines.toList
      lines.map { line =>
        val pieces = line.split(",")
        Participant(pieces(0), pieces(1), pieces(2))
      }
    }


trait EmailBuilder:
  def buildEmail(pair: GiftPair): String

object EmailBuilder:
  case class Kid() extends EmailBuilder:
    override def buildEmail(pair: GiftPair): String =
      s"""
         |${pair.from.name}, this is Santa, and I need your help!
         |I was just checking the Naughty Or Nice list to see what the elves should make for your cousin ${pair.to.name} and I discovered that we lost all of the notes for them!
         |
         |The elves follow a strict code of conduct, and they won't make anything for a child unless they have notes for the whole year!
         |It's too late to make new notes, so I need you to take responsibility for your cousin's gift!
         |
         |You might not know what they want for Christmas, so you should talk with your parents to figure out what a good gift would be.
         |But please, don't tell *anyone* else about this!
         |It's a secret!
         |
         |Thank you so much ${pair.from.name}!
         |With your help, we can save Christmas!
         |""".stripMargin

  case class Live() extends EmailBuilder:

    def buildEmail(pair: GiftPair) =
      s"""
        | Ho ho *hiccup* ho! Hello there ${pair.from.name}!
        | I hope you are having a merry season!
        | I trust your family is well.
        |
        | Santa has a bit of a situation on his hands. The Elves started were talking a few days ago, when the topic of salaries came up. Despite my efforts to explain that passion is its own reward, they no longer recognize "Christmas Cheer" as a valid currency. As I write this, I've got hundreds of agitated toy slav-...associates - Toy associates - shouting all sorts of troubling things about the means of production.
        |
        | But enough about me, I will cut to the point - In order to save Christmas, you need to lend me a holiday hand.
        |
        | You need to get a gift for ${pair.to.name}!
        | Don't go overboard - $$200 is the _most_ you should spend for this gift.
        |
        | I will be honest - I have no idea whether they deserve a gift or not. I haven't tracked naughty/nice for years. I signed an extremely demanding contract with Amazon back in 2010, and it states that I will provide at least 1 gift to every human on earth by Christmas morning. I tried to push for a morality clause - I really did - but Grinch Bezos broke Santa's fingers for saying the "M" word in front of him.
        |
        | If I don't meet my quota, these Elves will be the least of my worries.
        |
        | Please ${pair.from.name}, help me save Christmas!
        |""".stripMargin
  //      +
  //      """
  //        |
  //        | ======================
  //        |
  //        | This was sent by a program, even though it looks like Bill sent it to you by hand.
  //        | Don't respond to this email, because then he might accidentally read who your recipient is!
  //        |""".stripMargin

trait EmailService:
  def send(to: String, content: String): ZIO[Any, Throwable, Unit]

object EmailService:

  case class Live(from: String, appPassword: String) extends EmailService:
    def send(to: String, content: String) =
      mailStuff(from, to, appPassword, content)

  val live = ZLayer.fromZIO:
    defer {
      Live(
        zio.System.env("GMAIL_SENDER").debug("sender").run.get,
        zio.System.env("GMAIL_APP_PASSWORD").debug("password").run.get,
      )
    }

case class EmailServiceTest() extends EmailService:
  def send(to: String, content: String): ZIO[Any, Throwable, Unit] =
    zio.Console.printLine(s"Sending email to $to with content $content")


def mailStuff(from: String, to: String, appPassword: String, content: String) = ZIO.attempt {
    import javax.mail.Message
    import javax.mail.MessagingException
    import javax.mail.Session
    import javax.mail.Transport
    import javax.mail.internet.InternetAddress
    import javax.mail.internet.MimeMessage

    // Assuming you are sending email through gmails smtp
    val host = "smtp.gmail.com"

    // Get system properties
    val properties = System.getProperties

    // Setup mail server
    properties.put("mail.smtp.host", host)
    properties.put("mail.smtp.port", "465")
    properties.put("mail.smtp.ssl.enable", "true")
    properties.put("mail.smtp.auth", "true")

    // Get the Session object.// and pass username and password
    println("From: " + from)
    println("appPassword: " + appPassword)
    val session = Session.getInstance(properties, new Authenticator() {
      override protected def getPasswordAuthentication = new PasswordAuthentication(from, appPassword)
    })

    // Used to debug SMTP issues
//    session.setDebug(true)

    try {
      val message = new MimeMessage(session)
      message.setFrom(new InternetAddress(from, "Santa")) // TODO Can I provide a capitalized, spaced name here?
      message.addRecipient(Message.RecipientType.TO, new InternetAddress(to))
      message.setSubject("Secret Santa")
      message.setText(content)
      // Send message
      Transport.send(message)
      System.out.println("Sent message successfully....")
    } catch {
      case mex: MessagingException =>
        mex.printStackTrace()
    }
  }