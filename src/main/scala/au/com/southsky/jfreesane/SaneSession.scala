package au.com.southsky.jfreesane

import java.awt.image.BufferedImage
import java.io.{BufferedInputStream, Closeable, IOException}
import java.net.{InetAddress, InetSocketAddress, Socket}
import java.util.concurrent.TimeUnit
import java.util.logging.{Level, Logger}

import au.com.southsky.jfreesane.device.{SaneDevice, SaneDeviceHandle}
import au.com.southsky.jfreesane.enums.{FrameType, SaneRpcCode, SaneStatus}
import com.google.common.base.Preconditions

import scala.concurrent.duration._

class SaneSession @throws[IOException] private(val socket: Socket) extends Closeable {
  val outputStream = new SaneOutputStream(socket.getOutputStream)
  val inputStream = new SaneInputStream(this, socket.getInputStream)

  private var _passwordProvider = SanePasswordProvider.usingDotSanePassFile

  /**
    * Returns the current password provider. By default, this password provider
    * will be supplied by {@link SanePasswordProvider#usingDotSanePassFile}, but
    * you may override that with {@link #setPasswordProvider}.
    */
  def passwordProvider: SanePasswordProvider = _passwordProvider

  /**
    * Sets the {@link SanePasswordProvider password provider} to use if the SANE
    * daemon asks for credentials when accessing a resource.
    */
  def passwordProvider_=(passwordProvider: SanePasswordProvider) = _passwordProvider = passwordProvider

  /**
    * Returns the device with the give name. Opening the device will fail if the named device does
    * not exist.
    *
    * @return a new { @link SaneDevice} with the given name associated with the current session, never
    *         { @code null}
    * @throws IOException
    * if an error occurs while communicating with the SANE daemon
    */
  @throws[IOException]
  def device(name: String): SaneDevice = new SaneDevice(this, name, "", "", "")

  /**
    * Lists the devices known to the SANE daemon.
    *
    * @return a list of devices that may be opened, see { @link SaneDevice#open}
    * @throws IOException
    * if an error occurs while communicating with the SANE daemon
    * @throws SaneException
    * if the SANE backend returns an error in response to this request
    */
  @throws[IOException]
  @throws[SaneException]
  def listDevices: List[SaneDevice] = {
    outputStream.write(SaneRpcCode.SANE_NET_GET_DEVICES)
    inputStream.readDeviceList
  }

  /**
    * Closes the connection to the SANE server. This is done immediately by closing the socket.
    *
    * @throws IOException if an error occurred while closing the connection
    */
  @throws[IOException]
  def close = try {
    outputStream.write(SaneRpcCode.SANE_NET_EXIT)
    outputStream.close
  } finally {
    socket.close
  }


  @throws[IOException]
  @throws[SaneException]
  private[jfreesane] def openDevice(device: SaneDevice): SaneDeviceHandle = {
    outputStream.write(SaneRpcCode.SANE_NET_OPEN)
    outputStream.write(device.getName)

    var status: SaneWord = inputStream.readWord

    if (status.intValue != 0)
      throw SaneException(SaneStatus.fromWireValue(status.intValue))

    var handle: SaneWord = inputStream.readWord
    var resource: String = inputStream.readString

    if (!resource.isEmpty) {
      authorize(resource)

      status = inputStream.readWord
      if (status.intValue != 0)
        throw SaneException(SaneStatus.fromWireValue(status.intValue))

      handle = inputStream.readWord
      resource = inputStream.readString
    }

    new SaneDeviceHandle(status, handle, resource)
  }


  @throws[IOException]
  @throws[SaneException]
  private[jfreesane] def acquireImage(device: SaneDevice, listener: ScanListener): BufferedImage = {
    val builder: SaneImage.Builder = new SaneImage.Builder
    var parameters: SaneParameters = null
    listener.scanningStarted(device)
    var currentFrame: Int = 0

    do {
      val handle: SaneDeviceHandle = device.getHandle
      outputStream.write(SaneRpcCode.SANE_NET_START)
      outputStream.write(handle.handle)

      val startStatus: SaneWord = inputStream.readWord

      var port: Int = inputStream.readWord.intValue
      var byteOrder: SaneWord = inputStream.readWord
      var resource: String = inputStream.readString

      if (startStatus.intValue != 0)
        throw SaneException(startStatus)

      if (!resource.isEmpty) {
        authorize(resource)

        val status: Int = inputStream.readWord.intValue
        if (status != 0) {
          throw SaneException(SaneStatus.fromWireValue(status))
        }

        port = inputStream.readWord.intValue
        byteOrder = inputStream.readWord
        resource = inputStream.readString
      }

      // Ask the server for the parameters of this scan
      outputStream.write(SaneRpcCode.SANE_NET_GET_PARAMETERS)
      outputStream.write(handle.handle)

      var imageSocket: Socket = null

      try {
        imageSocket = new Socket(socket.getInetAddress, port)
        val status: Int = inputStream.readWord.intValue

        if (status != 0)
          throw new IOException("Unexpected status (" + status + ") in get_parameters")

        parameters = inputStream.readSaneParameters

        // As a convenience to our listeners, try to figure out how many frames
        // will be read. Usually this will be 1, except in the case of older
        // three-pass color scanners.
        listener.frameAcquisitionStarted(device, parameters, currentFrame, getLikelyTotalFrameCount(parameters))

        val frameStream: FrameReader = new FrameReader(device, parameters, new BufferedInputStream(imageSocket.getInputStream, SaneSession.READ_BUFFER_SIZE), 0x4321 == byteOrder.intValue, listener)
        builder.addFrame(frameStream.readFrame)
      } finally {
        if (imageSocket != null)
          imageSocket.close()
      }
      currentFrame += 1
    } while (!parameters.lastFrame)

    listener.scanningFinished(device)
    val image: SaneImage = builder.build
    image.toBufferedImage
  }


  private def getLikelyTotalFrameCount(parameters: SaneParameters): Int = {
    parameters.frameType match {
      case FrameType.RED | FrameType.GREEN | FrameType.BLUE =>
        3
      case _ =>
        1
    }
  }

  @throws[IOException]
  private[jfreesane] def closeDevice(handle: SaneDeviceHandle) = {
    // RPC code
    outputStream.write(SaneRpcCode.SANE_NET_CLOSE)
    outputStream.write(handle.handle)

    // read the dummy value from the wire, if it doesn't throw an exception
    // we assume the close was successful
    inputStream.readWord
  }

  @throws[IOException]
  private[jfreesane] def cancelDevice(handle: SaneDeviceHandle) = {
    // RPC code
    outputStream.write(SaneRpcCode.SANE_NET_CANCEL)
    outputStream.write(handle.handle)

    // read the dummy value from the wire, if it doesn't throw an exception
    // we assume the cancel was successful
    inputStream.readWord
  }

  @throws[IOException]
  private def initSane = {
    // RPC code
    outputStream.write(SaneRpcCode.SANE_NET_INIT)

    // version number
    outputStream.write(SaneWord.forSaneVersion(1, 0, 3))

    // username
    outputStream.write(System.getProperty("user.name"))

    inputStream.readWord
    inputStream.readWord
  }

  /**
    * Authorize the resource for access.
    *
    * @throws IOException
    * if an error occurs while communicating with the SANE daemon
    */
  @throws[IOException]
  private[jfreesane] def authorize(resource: String) = {
    if (passwordProvider == null)
      throw new IOException("Authorization failed - no password provider present " + "(you must call setPasswordProvider)")

    // RPC code FOR SANE_NET_AUTHORIZE
    outputStream.write(SaneRpcCode.SANE_NET_AUTHORIZE)
    outputStream.write(resource)

    if (!passwordProvider.canAuthenticate(resource))
    // the password provider has indicated that there's no way it can provide
    // credentials for this request.
      throw new IOException("Authorization failed - the password provider is " + "unable to provide a password for the resource [" + resource + "]")

    outputStream.write(passwordProvider.username(resource))
    writePassword(resource, passwordProvider.password(resource))
    // Read reply - from network
    inputStream.readWord
  }

  /**
    * Write password to outputstream depending on resource provided by saned.
    *
    * @param resource as provided by sane in authorization request
    * @param password
    * @throws IOException
    */
  @throws[IOException]
  private def writePassword(resource: String, password: String) = {
    val resourceParts: Array[String] = resource.split("\\$MD5\\$")

    if (resourceParts.length == 1)
    // Write in clean
      outputStream.write(password)
    else
      outputStream.write("$MD5$" + SanePasswordEncoder.derivePassword(resourceParts(1), password))
  }
}

object SaneSession {
  private val READ_BUFFER_SIZE: Int = 1 << 20
  // 1mb
  private val DEFAULT_PORT: Int = 6566

  /**
    * Establishes a connection to the SANE daemon running on the given host on the given port. If the
    * connection cannot be established within the given timeout, {@link SocketTimeoutException} is
    * thrown.
    */
  @throws[IOException]
  def apply(saneAddress: InetAddress,
            port: Int = DEFAULT_PORT,
            timeout: Duration = Duration.Zero): SaneSession = {

    val millis: Long = timeout.toMillis
    Preconditions.checkArgument(millis >= 0 && millis <= Integer.MAX_VALUE, "Timeout must be between 0 and Integer.MAX_VALUE milliseconds": Object)

    // If the user specifies a non-zero timeout that rounds to 0 milliseconds, set the timeout to 1 millisecond instead.
    if (Duration(millis, MILLISECONDS) != timeout)
      Logger.getLogger(classOf[SaneSession].getName).log(Level.WARNING, "Specified timeout of {0} {1} rounds to 0ms and was clamped to 1ms", Array(timeout.toMillis, TimeUnit.MILLISECONDS))

    val socket: Socket = new Socket
    socket.setTcpNoDelay(true)
    socket.connect(new InetSocketAddress(saneAddress, port), millis.toInt)

    val session: SaneSession = new SaneSession(socket)
    session.initSane

    session
  }
}