package au.com.southsky.jfreesane

import java.awt.Color
import java.awt.image.{BufferedImage, Raster}
import java.io.{File, IOException}
import java.net.InetAddress
import java.util
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import java.util.logging.{Level, Logger}
import javax.imageio.ImageIO

import au.com.southsky.jfreesane.device.SaneDevice
import au.com.southsky.jfreesane.enums.{OptionValueConstraintType, OptionValueType, SaneStatus}
import au.com.southsky.jfreesane.option.SaneOption
import com.google.common.base.Charsets
import com.google.common.collect.ImmutableList
import com.google.common.io.{Closeables, Files}
import com.google.common.truth.Truth
import com.google.common.truth.Truth.assertThat
import org.junit.Assert._
import org.junit._

import scala.collection.JavaConversions._

/**
  * Tests JFreeSane's interactions with the backend.
  * <p>
  * <p>
  * This test is ignored right now because it requires a real SANE backend to talk to. If you remove
  * the {@code "@Ignore"} annotation and point this test at a real SANE backend, it should pass.
  *
  * @author James Ring (sjr@jdns.org)
  */
@Ignore class SaneSessionTest {
  private var session: SaneSession = null

  @Before
  @throws[Exception]
  def initSession = this.session = SaneSession.apply(InetAddress.getByName("localhost"))

  @After
  @throws[Exception]
  def closeSession = Closeables.close(session, false)

  @Test
  @throws[Exception]
  def listDevicesSucceeds = {
    val devices: util.List[SaneDevice] = session.listDevices
    SaneSessionTest.log.info("Got " + devices.size + " device(s): " + devices)
    Truth.assertThat(devices).isNotEmpty
  }

  @Test
  @throws[Exception]
  def openDeviceSucceeds = {
    val device: SaneDevice = session.device("test")

    try {
      device.open
    } finally {
      device.close
    }
  }

  @Test
  @throws[Exception]
  def optionGroupsArePopulated = {
    val device: SaneDevice = session.device("test")

    try {
      device.open
      Truth.assertThat(java.lang.Boolean.valueOf(device.optionGroups.nonEmpty)).isTrue()
    } finally {
      device.close
    }
  }

  @Test
  @throws[Exception]
  def imageAcquisitionSucceeds = {
    val device: SaneDevice = session.device("test")

    try {
      device.open
      val image: BufferedImage = device.acquireImage
      val file: File = File.createTempFile("image", ".png")
      ImageIO.write(image, "png", file)
      System.out.println("Successfully wrote " + file)
    } finally {
      device.close
    }
  }

  @Test
  @throws[Exception]
  def listOptionsSucceeds = {
    val device: SaneDevice = session.device("pixma")
    try {
      device.open
      val options: List[SaneOption] = device.listOptions
      Assert.assertTrue("Expect multiple SaneOptions", options.size > 0)
      System.out.println("We found " + options.size + " options")
      for (option <- options) {
        System.out.println(option.toString)

        if (option.`type` != OptionValueType.BUTTON)
          System.out.println(option.valueCount)
      }
    } finally {
      device.close
    }
  }

  @Test
  @throws[Exception]
  def getOptionValueSucceeds = {
    val device: SaneDevice = session.device("test")
    try {
      device.open
      val options: util.List[SaneOption] = device.listOptions
      Assert.assertTrue("Expect multiple SaneOptions", options.size > 0)
      // option 0 is always "Number of options" must be greater than zero

      val optionCount: Int = options.get(0).integerValue
      Assert.assertTrue("Option count must be > 0", optionCount > 0)

      // print out the value of all integer-valued options

      import scala.collection.JavaConversions._
      for (option <- options) {
        System.out.print(option.title)

        if (!option.isActive)
          System.out.print(" [inactive]")
        else if ((option.`type` eq OptionValueType.INT) && option.valueCount == 1 && option.isActive)
          System.out.print("=" + option.integerValue)
        else if (option.`type` eq OptionValueType.STRING)
          System.out.print("=" + option.stringValue(Charsets.US_ASCII))

        System.out.println()
      }
    } finally {
      device.close
    }
  }

  @Test
  @throws[Exception]
  def setOptionValueSucceedsForString = {
    val device: SaneDevice = session.device("test")

    try {
      device.open
      val modeOption: SaneOption = device.option("mode")
      assertThat(modeOption.stringValue = "Gray").isEqualTo("Gray")
    } finally {
      device.close
    }
  }

  @Test
  @throws[Exception]
  def adfAcquisitionSucceeds = {
    val device: SaneDevice = session.device("test")
    device.open
    Truth.assertThat(device.option("source").stringConstraints: util.List[String]).contains("Automatic Document Feeder")
    device.option("source").stringValue = "Automatic Document Feeder"

    def loop: Unit =
      for (i <- 0 until 20)
        try {
          device.acquireImage
        } catch {
          case e: SaneException =>
            if (e.status.orNull == SaneStatus.STATUS_NO_DOCS)
            // out of documents to read, that's fine
              return
            else
              throw e
        }

    loop
  }

  @Test
  @throws[Exception]
  def acquireImageSucceedsAfterOutOfPaperCondition = {
    val device: SaneDevice = session.device("test")
    device.open
    assertThat(device.option("source").stringConstraints: util.List[String]).has.item("Automatic Document Feeder")
    device.option("source").stringValue = "Automatic Document Feeder"

    var thrown: Boolean = false
    for (i <- 0 until 20)
      try {
        device.acquireImage
      } catch {
        case e: SaneException =>
          if (e.status.orNull == SaneStatus.STATUS_NO_DOCS)
          // out of documents to read, that's fine
            thrown = true
          else
            throw e
      }

    Truth.assertThat(java.lang.Boolean.valueOf(thrown)).isTrue
    device.acquireImage
  }

  @Test
  @throws[Exception]
  def acquireMonoImage = {
    val device: SaneDevice = session.device("test")

    try {
      device.open
      val modeOption: SaneOption = device.option("mode")
      assertEquals("Gray", modeOption.stringValue = "Gray")
      val image: BufferedImage = device.acquireImage

      val file: File = File.createTempFile("mono-image", ".png")
      ImageIO.write(image, "png", file)
      System.out.println("Successfully wrote " + file)
    } finally {
      device.close
    }
  }

  /**
    * Tests that this SANE client produces images that match
    * {@link "http://www.meier-geinitz.de/sane/test-backend/test-pictures.html"} .
    */
  @Test
  @throws[Exception]
  def producesCorrectImages = {
    val device: SaneDevice = session.device("test")
    // Solid black and white
    try {
      device.open
      device.option("br-x").fixedValue = 200
      device.option("br-y").fixedValue = 200

      /*
       * assertProducesCorrectImage(device, "Gray", 1, "Solid white");
       * assertProducesCorrectImage(device, "Gray", 8, "Solid white");
       * assertProducesCorrectImage(device, "Gray", 16, "Solid white");
       * assertProducesCorrectImage(device, "Gray", 1, "Solid black");
       * assertProducesCorrectImage(device, "Gray", 8, "Solid black");
       * assertProducesCorrectImage(device, "Gray", 16, "Solid black");
       *
       * assertProducesCorrectImage(device, "Color", 1, "Solid white");
       * assertProducesCorrectImage(device, "Color", 8, "Solid white");
       * assertProducesCorrectImage(device, "Color", 16, "Solid white");
       * assertProducesCorrectImage(device, "Color", 1, "Solid black");
       * assertProducesCorrectImage(device, "Color", 8, "Solid black");
       * assertProducesCorrectImage(device, "Color", 16, "Solid black");
       *
       * assertProducesCorrectImage(device, "Gray", 1, "Color pattern");
       * assertProducesCorrectImage(device, "Color", 1, "Color pattern");
       *
       * assertProducesCorrectImage(device, "Gray", 8, "Color pattern");
       * assertProducesCorrectImage(device, "Color", 8, "Color pattern");
       */

      assertProducesCorrectImage(device, "Gray", 1, "Grid")
      assertProducesCorrectImage(device, "Color", 1, "Color pattern")

      //      assertProducesCorrectImage(device, "Color", 8, "Color pattern");
      //      assertProducesCorrectImage(device, "Color", 16, "Color pattern");
    } finally {
      device.close
    }
  }

  @Test
  @throws[Exception]
  def readsAndSetsStringsCorrectly = {
    val device: SaneDevice = session.device("test")

    try {
      device.open
      assertThat(device.option("mode").stringValue(Charsets.US_ASCII)).matches("Gray|Color")
      assertThat(device.option("mode").stringValue = "Gray").isEqualTo("Gray")
      assertThat(device.option("mode").stringValue(Charsets.US_ASCII)).isEqualTo("Gray")
      assertThat(device.option("read-return-value").stringValue(Charsets.US_ASCII)).isEqualTo("Default")
    } finally {
      device.close
    }
  }

  @Test
  @throws[Exception]
  def readsFixedPrecisionCorrectly = {
    val device: SaneDevice = session.device("test")

    try {
      device.open

      // this option gets rounded to the nearest whole number by the backend
      assertEquals(123, device.option("br-x").fixedValue = 123.456, 0.0001)
      assertEquals(123, device.option("br-x").fixedValue, 0.0001)
    } finally {
      device.close
    }
  }

  @Test
  @throws[Exception]
  def readsBooleanOptionsCorrectly = {
    val device: SaneDevice = session.device("test")

    try {
      device.open

      val option: SaneOption = device.option("hand-scanner")
      assertThat(java.lang.Boolean.valueOf(option.booleanValue = true)).isTrue
      assertThat(java.lang.Boolean.valueOf(option.booleanValue)).isTrue
      assertThat(java.lang.Boolean.valueOf(option.booleanValue = false)).isFalse
      assertThat(java.lang.Boolean.valueOf(option.booleanValue)).isFalse
    } finally {
      device.close
    }
  }

  @Test
  @throws[Exception]
  def readsStringListConstraintsCorrectly = {
    val device: SaneDevice = session.device("test")

    try {
      device.open

      val option: SaneOption = device.option("string-constraint-string-list")
      Truth2.assertThat(option).isNotNull
      Truth2.assertThat(option.constraintType).isEqualTo(OptionValueConstraintType.STRING_LIST_CONSTRAINT)
      assertThat(option.stringConstraints: util.List[String]).has.exactly("First entry", "Second entry", "This is the very long third entry. Maybe the frontend has an idea how to display it")
    } finally {
      device.close
    }
  }

  @Test
  @throws[Exception]
  def readIntegerValueListConstraintsCorrectly = {
    val device: SaneDevice = session.device("test")

    try {
      device.open

      val option: SaneOption = device.option("int-constraint-word-list")
      assertNotNull(option)
      assertEquals(OptionValueConstraintType.VALUE_LIST_CONSTRAINT, option.constraintType)
      assertEquals(ImmutableList.of(-42, -8, 0, 17, 42, 256, 65536, 16777216, 1073741824), option.integerValueListConstraint)
    } finally {
      device.close
    }
  }

  @Test
  @throws[Exception]
  def readFixedValueListConstraintsCorrectly = {
    val device: SaneDevice = session.device("test")

    try {
      device.open

      val option: SaneOption = device.option("fixed-constraint-word-list")
      assertNotNull(option)
      assertEquals(OptionValueConstraintType.VALUE_LIST_CONSTRAINT, option.constraintType)
      val expected: List[Double] = List(-32.7d, 12.1d, 42d, 129.5d)
      val actual: List[Double] = option.fixedValueListConstraint
      assertEquals(expected.size, actual.size)

      for (i <- expected.indices)
        assertEquals(expected(i), actual(i), 0.00001)
    } finally {
      device.close
    }
  }

  @Test
  @throws[Exception]
  def readIntegerConstraintRangeCorrectly = {
    val device: SaneDevice = session.device("test")

    try {
      device.open

      val option: SaneOption = device.option("int-constraint-range")
      assertNotNull(option)
      assertEquals(OptionValueConstraintType.RANGE_CONSTRAINT, option.constraintType)
      assertEquals(4, option.rangeConstraints.minInt)
      assertEquals(192, option.rangeConstraints.maxInt)
      assertEquals(2, option.rangeConstraints.quantizationInt)
    } finally {
      device.close
    }
  }

  @Test
  @throws[Exception]
  def readFixedConstraintRangeCorrectly = {
    val device: SaneDevice = session.device("test")

    try {
      device.open

      val option: SaneOption = device.option("fixed-constraint-range")
      assertNotNull(option)
      assertEquals(OptionValueConstraintType.RANGE_CONSTRAINT, option.constraintType)
      assertEquals(-42.17, option.rangeConstraints.minFixed, 0.00001)
      assertEquals(32767.9999, option.rangeConstraints.maxFixed, 0.00001)
      assertEquals(2.0, option.rangeConstraints.quantizationFixed, 0.00001)
    } finally {
      device.close
    }
  }

  @Test
  @throws[Exception]
  def arrayOption = {
    val device: SaneDevice = session.device("pixma")

    try {
      device.open

      val option: SaneOption = device.option("gamma-table")
      assertNotNull(option)
      //      assertFalse(option.isConstrained());
      assertEquals(OptionValueType.INT, option.`type`)
      val values =
        (0 until option.valueCount)
          .map(i => i % 256)
          .toList

      assertEquals(values, option.integerValue = values)
      assertEquals(values, option.integerArrayValue)
    } finally {
      device.close
    }
  }

  @Test
  @throws[Exception]
  def pixmaConstraints = {
    val device = session.device("pixma")

    try {
      device.open

      val option = device.option("tl-x")
      assertNotNull(option)
      assertEquals(OptionValueConstraintType.RANGE_CONSTRAINT, option.constraintType)
      assertEquals(OptionValueType.FIXED, option.`type`)
      val constraint = option.rangeConstraints

      System.out.println(constraint.minFixed)
      System.out.println(constraint.maxFixed)
      System.out.println(option.units)
      System.out.println(option.fixedValue = -4)
      System.out.println(option.fixedValue = 97.5)
    } finally {
      device.close
    }
  }

  @Test
  @throws[Exception]
  def multipleListDevicesCalls = {
    session.listDevices
    session.listDevices
  }

  @Test
  @throws[Exception]
  def multipleGetDeviceCalls = {
    session.device("test")
    session.device("test")
  }

  @Test
  @throws[Exception]
  def multipleOpenDeviceCalls = {
    {
      val device: SaneDevice = session.device("test")
      openAndCloseDevice(device)
    }
    {
      val device: SaneDevice = session.device("test")
      openAndCloseDevice(device)
    }
  }

  @Test
  @throws[Exception]
  def canSetButtonOption = {
    val device: SaneDevice = session.device("pixma")

    try {
      device.open

      device.option("button-update").setButtonValue
      assertEquals("Gray", device.option("mode").stringValue = "Gray")
      assertEquals("Gray", device.option("mode").stringValue)
    } finally {
      device.close
    }
  }

  @Test
  @throws[Exception]
  def handScanning = {
    val device: SaneDevice = session.device("test")

    try {
      device.open

      device.option("hand-scanner").booleanValue = true
      device.acquireImage
    } finally {
      device.close
    }
  }

  @Test
  @throws[Exception]
  def threePassScanning = {
    val device: SaneDevice = session.device("test")

    try {
      device.open

      assertEquals("Color pattern", device.option("test-picture").stringValue = "Color pattern")
      assertEquals("Color", device.option("mode").stringValue = "Color")
      assertEquals(true, device.option("three-pass").booleanValue = true)

      for (i <- 0 until 5) {
        val file = File.createTempFile("three-pass", ".png")
        ImageIO.write(device.acquireImage, "png", file)
        System.out.println("Wrote three-pass test to " + file)
      }
    } finally {
      device.close
    }
  }

  @Test
  @throws[Exception]
  def reducedArea = {
    val device: SaneDevice = session.device("test")

    try {
      device.open

      device.option("mode").stringValue = "Color"
      device.option("resolution").integerValue = 200
      device.option("tl-x").fixedValue = 0.0
      device.option("tl-y").fixedValue = 0.0
      device.option("br-x").fixedValue = 105.0
      device.option("br-y").fixedValue = 149.0
      device.acquireImage
    } finally {
      device.close
    }
  }

  @Test
  @throws[Exception]
  def passwordAuthentication = {
    // assumes that test is a password-authenticated device
    session.passwordProvider = SanePasswordProvider.forUsernameAndPassword("sjr", "password")
    val device: SaneDevice = session.device("test")
    device.open
    device.acquireImage
  }

  /**
    * This test assumes that you have protected the "test" device with a username
    * of "sjr" and a password other than "badpassword".
    */
  @Test
  @throws[Exception]
  def invalidPasswordCausesAccessDeniedError = {
    session.passwordProvider = SanePasswordProvider.forUsernameAndPassword("sjr", "badpassword")
    val device: SaneDevice = session.device("test")

    try {
      device.open
      fail("Expected a SaneException, didn't get one")
    } catch {
      case e: SaneException =>
        if (e.status.orNull != SaneStatus.STATUS_ACCESS_DENIED)
          throw e

      // if we got here, we got the expected exception
    }
  }

  @Test
  @throws[Exception]
  def highResolutionScan = {
    val device: SaneDevice = session.device("pixma")
    device.open
    device.option("resolution").integerValue = 1200
    device.option("mode").stringValue = "Color"
    device.acquireImage
  }

  @Test
  @throws[Exception]
  def passwordAuthenticationFromLocalFileSpecified = {
    val passwordFile: File = File.createTempFile("sane", ".pass")
    try {
      Files.write("sjr:password:test", passwordFile, Charsets.ISO_8859_1)
      session.passwordProvider = SanePasswordProvider.usingSanePassFile(passwordFile.getAbsolutePath)
      val device: SaneDevice = session.device("test")
      device.open
      device.acquireImage
    } finally {
      passwordFile.delete
    }
  }

  @Test
  @throws[Exception]
  def listenerReceivesScanStartedEvent = {
    val notifiedDevice: AtomicReference[SaneDevice] = new AtomicReference[SaneDevice]
    val frameCount: AtomicInteger = new AtomicInteger
    val listener: ScanListener = new ScanListenerAdapter() {
      override def scanningStarted(device: SaneDevice) =
        notifiedDevice.set(device)

      override def frameAcquisitionStarted(device: SaneDevice, parameters: SaneParameters, currentFrame: Int, likelyTotalFrames: Int) =
        frameCount.incrementAndGet
    }

    val device: SaneDevice = session.device("test")
    device.open
    device.option("resolution").fixedValue = 1200
    device.option("mode").stringValue = "Color"
    device.option("three-pass").booleanValue = true
    device.acquireImage(listener)
    Truth2.assertThat(notifiedDevice.get()).isSameAs(device)
    Truth2.assertThat(frameCount.get()).isEqualTo(3)
  }

  @throws[Exception]
  private def openAndCloseDevice(device: SaneDevice) = {
    try {
      device.open
      device.listOptions
    } finally {
      device.close
    }
  }

  @throws[IOException]
  @throws[SaneException]
  private def assertProducesCorrectImage(device: SaneDevice, mode: String, sampleDepth: Int, testPicture: String) = {
    val actualImage: BufferedImage = acquireImage(device, mode, sampleDepth, testPicture)

    writeImage(mode, sampleDepth, testPicture, actualImage)

    if (testPicture.startsWith("Solid"))
      assertImageSolidColor(if (testPicture.endsWith("black"))
        Color.black
      else
        Color.white, actualImage)
    else {
      // compare with sample images
    }
  }

  @throws[IOException]
  private def writeImage(mode: String, sampleDepth: Int, testPicture: String, actualImage: BufferedImage) = {
    val file: File = File.createTempFile(s"image-$mode-$sampleDepth-${testPicture.replace(' ', '_')}", ".png")
    ImageIO.write(actualImage, "png", file)
    System.out.println("Successfully wrote " + file)
  }

  private def assertImageSolidColor(color: Color, image: BufferedImage) =
    for (x <- 0 until image.getWidth)
      for (y <- 0 until image.getHeight)
        assertEquals(color.getRGB(), image.getRGB(x, y))

  @throws[IOException]
  @throws[SaneException]
  private def acquireImage(device: SaneDevice, mode: String, sampleDepth: Int, testPicture: String): BufferedImage = {
    device.option("mode").stringValue = mode
    device.option("depth").integerValue = sampleDepth
    device.option("test-picture").stringValue = testPicture
    return device.acquireImage
  }

  private def assertImagesEqual(expected: BufferedImage, actual: BufferedImage) = {
    assertEquals("image widths differ", expected.getWidth, actual.getWidth)
    assertEquals("image heights differ", expected.getHeight, actual.getHeight)

    val expectedRaster: Raster = expected.getRaster
    val actualRaster: Raster = actual.getRaster

    for (x <- 0 until expected.getWidth())
      for (y <- 0 until expected.getHeight()) {
        val expectedPixels = expectedRaster.getPixel(x, y, null: Array[Int])
        val actualPixels = actualRaster.getPixel(x, y, null: Array[Int])

        // assert that all the samples are the same for the given pixel
        Assert.assertArrayEquals(expectedPixels, actualPixels);
      }
  }
}

@Ignore object SaneSessionTest {
  private val log: Logger = Logger.getLogger(classOf[SaneSessionTest].getName)
  private val jfreesaneLogger: Logger = Logger.getLogger("au.com.southsky.jfreesane")

  @BeforeClass def setupLogging = {
    for (handler <- Logger.getLogger("").getHandlers)
      handler.setLevel(Level.FINE)

    jfreesaneLogger.setLevel(Level.FINE)
  }
}