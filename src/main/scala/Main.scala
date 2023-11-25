import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault, ZLayer}
import zio.Console.printLine
import zio.direct.*

import java.util.Properties
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


case class Participant(name: String, email: String)
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
                  potentialReceiver.email != participant.email
              }
            receiver = state.openReceivers(receiverIdx)
          yield state.copy(matches = GiftPair(participant, receiver) :: state.matches, openReceivers = state.openReceivers.filter(_ != receiver))
      )
    yield finalState.matches

object Main extends ZIOAppDefault:
  override def run: ZIO[Environment & ZIOAppArgs & Scope, Any, Any] =
    (logic).provide(ZLayer.derive[EmailBuilder.Live], EmailService.live, ZLayer.derive[ParticipantService.Live])

  val logic =
    for
      participants <- ZIO.serviceWithZIO[ParticipantService](_.readParticipants)
      pairs <- Participant.matchMake(participants)
      // Test run that only sends to me
      //      _ <- ZIO.foreach(pairs.headOption)(pair => mailStuff(gmailSender.get, "bill@billdingsoftware.com", gmailAppPassword.get, buildEmail(pair)))
      _ <- ZIO.foreach(pairs)(
        pair =>
          for {
            emailContent <- ZIO.serviceWith[EmailBuilder](_.buildEmail(pair))
            _ <- ZIO.serviceWithZIO[EmailService](_.send(pair.from.email, emailContent))
          } yield ()
      )
    yield ()

trait ParticipantService:
  val readParticipants: ZIO[Any, Throwable, List[Participant]]


object ParticipantService:
  case class Live() extends ParticipantService:
    val readParticipants = ZIO.attempt {
      val lines = Source.fromFile("names_and_emails.txt").getLines.toList
      lines.map { line =>
        val pieces = line.split(",")
        Participant(pieces(0), pieces(1))
      }
    }


trait EmailBuilder:
  def buildEmail(pair: GiftPair): String

object EmailBuilder:
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
    def send(to: String, content: String) = ZIO.attempt:
      mailStuff(from, to, appPassword, content)

  val live = ZLayer.fromZIO:
    defer {
      Live(
        zio.System.env("GMAIL_APP_PASSWORD").run.get,
        zio.System.env("GMAIL_SENDER").run.get
      )
    }

def mailStuff(from: String, to: String, appPassword: String, content: String) = ZIO.attempt {
    import javax.mail.Message
    import javax.mail.MessagingException
    import javax.mail.Session
    import javax.mail.Transport
    import javax.mail.internet.InternetAddress
    import javax.mail.internet.MimeMessage
    import java.util.Properties

    // Assuming you are sending email from through gmails smtp
    val host = "smtp.gmail.com"

    // Get system properties
    val properties = System.getProperties

    // Setup mail server
    properties.put("mail.smtp.host", host)
    properties.put("mail.smtp.port", "465")
    properties.put("mail.smtp.ssl.enable", "true")
    properties.put("mail.smtp.auth", "true")

    // Get the Session object.// and pass username and password
    val session = Session.getInstance(properties, new Authenticator() {
      override protected def getPasswordAuthentication = new PasswordAuthentication(from, appPassword)
    })

    // Used to debug SMTP issues
//    session.setDebug(true)

    try { // Create a default MimeMessage object.
      val message = new MimeMessage(session)
      // Set From: header field of the header.
      message.setFrom(new InternetAddress(from, "Santa")) // TODO Can I provide a capitalized, spaced name here?
      // Set To: header field of the header.
      message.addRecipient(Message.RecipientType.TO, new InternetAddress(to))
      // Set Subject: header field
      message.setSubject("Secret Santa")
      // Now set the actual message
      message.setText(content)
      System.out.println("About to send")
      // Send message
      Transport.send(message)
      System.out.println("Sent message successfully....")
    } catch {
      case mex: MessagingException =>
        mex.printStackTrace()
    }
  }