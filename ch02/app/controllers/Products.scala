package controllers

import play.api.mvc.{Action, Controller, Flash}
import play.api.data.Form
import play.api.data.Forms.{mapping, longNumber, nonEmptyText}
import play.api.i18n.Messages
import models.Product

object Products extends Controller {
  
  def list = Action { implicit request =>
    val products = Product.findAll
    
    Ok(views.html.products.list(products))
  }
  
  def show(ean: Long) = Action { implicit request =>
    Product.findByEan(ean).map { product =>
      Ok(views.html.products.details(product))
    }.getOrElse(NotFound)
  }
  
  def newProduct = Action { implicit request =>
    val form = if (request2flash.get("error").isDefined) {
      productForm.bind(request2flash.data)
    } else {
      productForm
    }
    
    Ok(views.html.products.editProduct(form))
  }
  
  def save = Action { implicit request =>
    val newProductForm = productForm.bindFromRequest()
    
    newProductForm.fold(
        hasErrors = { form =>
          Redirect(routes.Products.newProduct).
            flashing(Flash(form.data) + ("error" -> Messages("validation.errors")))
        },
        success = { newProduct =>
          Product.add(newProduct)
          val message = Messages("product.new.success", newProduct.name)
          Redirect(routes.Products.show(newProduct.ean)).flashing("success" -> message)
        }
    )
  }
  
  def edit(ean: Long) = Action { implicit request =>
    val form = if (request2flash.get("error").isDefined) {
      updateForm.bind(request2flash.data)
    } else {
      updateForm.fill(Product.findByEan(ean).getOrElse(Product(0L,"","")))
    }
    
    Ok(views.html.products.editProduct(form, Some(ean)))
  }
  
  def update(ean: Long) = Action { implicit request =>
    if (Product.findByEan(ean).isEmpty)
      NotFound
    else {
      val form = updateForm.bindFromRequest()
      
      form.fold(
        hasErrors = { form =>
          Redirect(routes.Products.edit(ean)).
            flashing(Flash(form.data) + ("error" -> Messages("validation.errors")))
        },
        success = { product =>
          Product.update(product)
          val message = Messages("product.edit.success", product.name)
          Redirect(routes.Products.show(product.ean)).flashing("success" -> message)
        }
      )
    }
  }

  def delete(ean: Long) = Action { implicit request =>
    val p = Product.findByEan(ean)
    
    Ok(views.html.products.deleteProduct(p.get))
  }
  
  def deleted(ean: Long) = Action { implicit request =>
    val p = Product.findByEan(ean)

    p.map { x => Product.delete(x) }

    p match {
      case Some(p) => Ok(views.html.products.deletedProduct(p.name))
      case None    => Redirect(routes.Application.index)
    }
    
  }
  
  private def eanDoesntExist(ean: Long): Boolean = Product.findByEan(ean).isEmpty
  
  private def makeProductForm(error: String, constraint: (Long) => Boolean) = Form(
    mapping(
      "ean" -> longNumber.verifying("validation.ean.checksum", eanCheck _).verifying(error, constraint),
      "name" -> nonEmptyText,
      "description" -> nonEmptyText
    )(Product.apply)(Product.unapply)
  )

  private val productForm = makeProductForm("validation.ean.duplicate", eanDoesntExist(_))

  private val updateForm = makeProductForm("validation.ean.notexisting", !eanDoesntExist(_))
  
  private def updateProductForm(ean: Long) =
    makeProductForm("validation.ean.duplicate", { newEan =>
      newEan == ean || eanDoesntExist(newEan)
    })
  
  private def eanCheck(ean: Long) = {
    def sumDigits(digits: IndexedSeq[(Char, Int)]): Int = {
      digits.map { _._1 }.map { _.toInt }.sum
    }

    val (singles, triples) = ean.toString.reverse.zipWithIndex.partition {
      _._2 % 2 == 0
    }

    (sumDigits(singles) + sumDigits(triples) * 3) % 10 == 0
  }
}
