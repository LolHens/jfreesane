package au.com.southsky.jfreesane

import java.util.concurrent.TimeUnit

import au.com.southsky.jfreesane.device.SaneDevice

/**
  * A static factory of listeners that limit the rate at which they send
  * {@link ScanListener#recordRead} notifications. The primary purpose of this
  * class is to allow users to easily build user interfaces that provide progress
  * information without having to deal with these potentially very high frequency
  * notifications.
  *
  * <p>
  * For example:
  *
  * <pre>
  * final JProgressBar progressBar = new JProgressBar();
  * // Construct a new ScanAdapter that updates progressBar.
  * ScanListener progressBarUpdater = new ScanAdapter() {
  * {@literal @}Override public recordRead(...) {
  * // Swing updates must be done in the Swing event listener thread.
  * SwingUtilities.invokeLater(new Runnable() {
  * {@literal @}Override public void run() {
  * progressBar.setValue(...);
  * }
  * });
  * }
  * };
  *
  * // Wrap the listener to yield notifications up to 10 times per second and acquire the image.
  * ScanListener rateLimitedListener = RateLimitingScanListeners
  * .noMoreFrequentlyThan(progressBarUpdater, 100, TimeUnit.MILLISECONDS));
  * saneDevice.acquireImage(rateLimitedListener);
  * </pre>
  */
object RateLimitingScanListeners {
  /**
    * Returns {@link ScanListener} that calls the given listener
    * {@link ScanListener#recordRead} method no more frequently than the given
    * time any one device. Record read events from one device that occur more
    * frequently are simply discarded.
    */
  def noMoreFrequentlyThan(listener: ScanListener,
                           time: Long,
                           timeUnit: TimeUnit): ScanListener =
  new ScanListener() {
    private var lastSentTimes = Map[SaneDevice, Long]()

    def scanningStarted(device: SaneDevice) = listener.scanningStarted(device)

    def frameAcquisitionStarted(device: SaneDevice, parameters: SaneParameters, currentFrame: Int, likelyTotalFrames: Int) =
      listener.frameAcquisitionStarted(device, parameters, currentFrame, likelyTotalFrames)

    def recordRead(device: SaneDevice, totalBytesRead: Int, imageSizeBytes: Option[Int]) = {
      val currentTime: Long = System.currentTimeMillis

      val lastSentTime = lastSentTimes.get(device) match {
        case Some(lastSentTime) =>
          lastSentTime

        case None =>
          val initialValue: Long = 0
          lastSentTimes = lastSentTimes + (device -> initialValue)
          initialValue
      }

      if (currentTime - lastSentTime > timeUnit.toMillis(time)) {
        lastSentTimes = lastSentTimes + (device -> currentTime)
        listener.recordRead(device, totalBytesRead, imageSizeBytes)
      }
    }

    def scanningFinished(device: SaneDevice) = listener.scanningFinished(device)
  }
}