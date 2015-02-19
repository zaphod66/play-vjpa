package controllers

import play.api.mvc.{Action, Controller}

object Barcodes extends Controller {
  val imageResolution = 200
  
  def barcode(ean: Long) = Action {
    import java.lang.IllegalArgumentException
    
    val mimeType = "image/png"
    
    try {
      val imageData = ean13BarCode(ean, mimeType)
      
      Ok(imageData).as(mimeType)
    } catch {
      case e: IllegalArgumentException => BadRequest("Couldn't generate bar code. Error: " + e.getMessage)
    }
  }
  
  def ean13BarCode(ean: Long, mimeType: String): Array[Byte] = {
    import java.io.ByteArrayOutputStream
    import java.awt.image.BufferedImage
    import org.krysalis.barcode4j.output.bitmap.BitmapCanvasProvider
    import org.krysalis.barcode4j.impl.upcean.EAN13Bean
    
    val output: ByteArrayOutputStream = new ByteArrayOutputStream
    val canvas: BitmapCanvasProvider  = new BitmapCanvasProvider(
        output, mimeType, imageResolution, BufferedImage.TYPE_BYTE_BINARY, false, 0)
    
    val barcode = new EAN13Bean()
    barcode.generateBarcode(canvas, String valueOf ean)
    canvas.finish
    
    output.toByteArray
  }
}