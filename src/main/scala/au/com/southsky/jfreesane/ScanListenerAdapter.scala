package au.com.southsky.jfreesane

/**
  * A no-op implementation of {@link ScanListener}. You may extend this subclass
  * and provide implementations of methods corresponding to events of interest to
  * you.
  */
class ScanListenerAdapter extends ScanListener {
  def scanningStarted(device: SaneDevice) = ()

  def frameAcquisitionStarted(device: SaneDevice, parameters: SaneParameters, currentFrame: Int, likelyTotalFrames: Int) = ()

  def recordRead(device: SaneDevice, totalBytesRead: Int, imageSize: Int) = ()

  def scanningFinished(device: SaneDevice) = ()
}
