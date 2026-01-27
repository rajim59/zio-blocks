package zio.blocks.schema.migration // Package ISOLATED in migration folder

import zio.blocks.schema.{DynamicValue, PrimitiveValue, Schema} // Importing Global dependencies
import scala.annotation.unused

/**
 * SchemaExpr: Isolated Pure Data AST for Migration.
 * Location: inside 'migration' package explicitly.
 */
sealed trait SchemaExpr[A] extends Product with Serializable

object SchemaExpr {

  case class DefaultValue[A](value: DynamicValue) extends SchemaExpr[A]

  case class Constant[A](value: DynamicValue) extends SchemaExpr[A]

  case class Identity[A]() extends SchemaExpr[A]

  /**
   * Converted Operation:
   * Uses 'ConversionOp' enum for strict serializability.
   */
  case class Converted[A, B](
    operand: SchemaExpr[A],
    op: ConversionOp 
  ) extends SchemaExpr[B]

  // =================================================================================
  // Conversion Operations (Pure Data Enum)
  // =================================================================================
  sealed trait ConversionOp extends Product with Serializable
  object ConversionOp {
    case object ToString extends ConversionOp
    case object ToInt extends ConversionOp
    case object ParseJson extends ConversionOp
    // Add more as needed
  }

  // =================================================================================
  // SMART CONSTRUCTORS
  // =================================================================================

  def default[A](@unused schema: Schema[A]): SchemaExpr[A] = {
    // Isolated logic for migration defaults
    DefaultValue(DynamicValue.Primitive(PrimitiveValue.Unit))
  }

  def constant[A](value: A, schema: Schema[A]): SchemaExpr[A] =
    Constant(schema.toDynamicValue(value))
}