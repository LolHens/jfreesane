package au.com.southsky.jfreesane.device

import java.awt.image.BufferedImage
import java.io.{Closeable, IOException}

import au.com.southsky.jfreesane.option.{OptionGroup, SaneOption}
import au.com.southsky.jfreesane.{ScanListener, ScanListenerAdapter, _}
import com.google.common.base.Preconditions

/**
  * Represents a SANE device within a session. SANE devices are obtained from a {@link SaneSession}.
  *
  * <p>
  * Definitely not thread-safe. If you're going to use this object from multiple threads, you must do
  * your own synchronization. Even performing read operations (like getting an option's value) must
  * be synchronized.
  *
  * @author James Ring (sjr@jdns.org)
  */
class SaneDevice private[jfreesane](val session: SaneSession,
                                    val name: String,
                                    val vendor: String,
                                    val model: String,
                                    val `type`: String) extends Closeable {

  private var handle: SaneDeviceHandle = null
  private var optionTitleMap: Option[Map[String, SaneOption]] = None
  private var groups = List[OptionGroup]()

  def getName: String = name

  def getVendor: String = vendor

  def getModel: String = model

  def getType: String = `type`

  /**
    * Returns {@code true} if the device is open.
    */
  def isOpen: Boolean = handle != null

  /**
    * Opens the device.
    *
    * @throws IOException           if a problem occurred while talking to SANE
    * @throws au.com.southsky.jfreesane.SaneException
    * @throws IllegalStateException if the device is already open
    */
  @throws[IOException]
  @throws[SaneException]
  def open = {
    Preconditions.checkState(!isOpen, "device is already open": Object)
    handle = session.openDevice(this)
  }

  /**
    * Acquires a single image from the Sane daemon.
    *
    * @return a { @link BufferedImage} representing the image obtained from Sane
    * @throws IOException
    * if an error occurred while talking to the backend
    * @throws SaneException
    * if an application-level error was returned by the Sane daemon
    */
  @throws[IOException]
  @throws[SaneException]
  def acquireImage: BufferedImage = acquireImage(null)

  /**
    * Acquires a single image from the Sane daemon. The given
    * {@link ScanListener} will be notified about updates during the scan.
    *
    * <p>
    * The scanning thread will be used to make calls to {@code listener}.
    * Scanning will not proceed until control returns to JFreeSane, so your
    * implementation should not monopolize this thread for longer than necessary.
    *
    * @param listener
    * if not { @code null}, this object will receive notifications about
    * scan progress
    * @return a { @link BufferedImage} representing the image obtained from Sane
    * @throws IOException
    * if an error occurred while talking to the backend
    * @throws SaneException
    * if an application-level error was returned by the Sane daemon
    */
  @throws[IOException]
  @throws[SaneException]
  def acquireImage(listener: ScanListener): BufferedImage = {
    Preconditions.checkState(isOpen, "device is not open": Object)
    session.acquireImage(this, Option(listener).getOrElse(new ScanListenerAdapter()))
  }

  /**
    * Cancel the current operation of a remote SANE device.
    *
    * @throws IOException           if an error occurs talking to the SANE backend
    * @throws IllegalStateException if the device is not open
    */
  @throws[IOException]
  def cancel = {
    Preconditions.checkState(isOpen, "device is not open": Object)
    session.cancelDevice(handle)
  }

  /**
    * Closes the device.
    *
    * @throws IOException           if an error occurs talking to the SANE backend
    * @throws IllegalStateException if the device is already closed
    */
  @throws[IOException]
  def close = {
    if (!isOpen)
      throw new IOException("device is already closed")

    session.closeDevice(handle)
    handle = null
  }

  override def toString: String = s"SaneDevice [name=$name, vendor=$vendor, model=$model, type=${`type`}]"

  /**
    * Returns the handle by which this device is known to the SANE backend, or {@code null} if
    * if the device is not open (see {@link #isOpen}).
    */
  private[jfreesane] def getHandle: SaneDeviceHandle = handle

  /**
    * Returns the list of options applicable to this device.
    *
    * @return a list of { @link SaneOption} instances
    * @throws IOException if a problem occurred talking to the SANE backend
    */
  @throws[IOException]
  def listOptions: List[SaneOption] = optionTitleMap.getOrElse {
    groups = Nil

    val result = SaneOption.optionsFor(this)
      .map(saneOption => (saneOption.name, saneOption))
      .toMap
    optionTitleMap = Some(result)
    result
  }.values.toList

  private[jfreesane] def addOptionGroup(group: OptionGroup) = groups = groups :+ group

  /**
    * Returns the list of option groups for this device.
    */
  @throws[IOException]
  def optionGroups: List[OptionGroup] = {
    listOptions
    groups
  }

  /**
    * Returns the option with the given name for this device. If the option does
    * not exist, {@code null} is returned. Name matching is case-sensitive.
    */
  @throws[IOException]
  def option(title: String): SaneOption = {
    listOptions
    optionTitleMap.flatMap(_.get(title)).get
  }

  /**
    * Informs this device that its options are stale (e.g. when the server tells us we need to reload
    * options after an option was set).
    */
  private[jfreesane] def invalidateOptions = optionTitleMap = None
}
