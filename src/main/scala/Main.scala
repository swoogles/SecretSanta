import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}
import zio.Console.printLine

import java.util.Properties
import javax.mail.{Authenticator, Message, MessagingException, PasswordAuthentication, Session, Transport}
import javax.mail.internet.InternetAddress
import javax.mail.internet.MimeMessage
import scala.io.Source

case class Participant(name: String, email: String)

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

object Participant:
  def matchMake(participants: List[Participant]) =
    for
      _ <- ZIO.debug("TODO")
      finalState <- ZIO.foldLeft(participants)(State(List.empty, participants))(
        (state, participant) =>
          for
            receiverIdx <- zio.Random.nextIntBounded(state.openReceivers.size).repeatUntil( target => state.openReceivers(target) != participant)
            receiver = state.openReceivers(receiverIdx)
          yield state.copy(matches = GiftPair(participant, receiver) :: state.matches, openReceivers = state.openReceivers.filter(_ != receiver))
      )
//      _ <- participants.foldLeft[State]()
    yield finalState.matches

object Main extends ZIOAppDefault:
  override def run: ZIO[Environment & ZIOAppArgs & Scope, Any, Any] =
    for
      _ <- printLine("Welcome to your first ZIO app!")
      participants <- readParticipants
      pairs <- Participant.matchMake(participants)
      content = pairs.map(pair => "From: " + pair.from.name  + "\t To: " + pair.to.name).mkString("\n")
      _ <- ZIO.debug(content)
      gmailAppPassword <- zio.System.env("GMAIL_APP_PASSWORD").debug("password")
      gmailSender <- zio.System.env("GMAIL_SENDER")
      _ <- ZIO.attempt(mailStuff(gmailSender.get, "bill@billdingsoftware.com", gmailAppPassword.get, content))
    yield ()

  val readParticipants = ZIO.attempt {
    val lines = Source.fromFile("names_and_emails.txt").getLines.toList
    lines.map { line =>
      val pieces = line.split(",")
      Participant(pieces(0), pieces(1))
    }
  }

  def mailStuff(from: String, to: String, appPassword: String, content: String) = {
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
    session.setDebug(true)

    try { // Create a default MimeMessage object.
      val message = new MimeMessage(session)
      // Set From: header field of the header.
      message.setFrom(new InternetAddress(from))
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