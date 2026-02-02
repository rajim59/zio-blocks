<<<<<<< HEAD
package zio.blocks.schema.migration

import zio.blocks.schema.DynamicOptic

/**
 * MigrationAction: Pure Data Definition.
 * Uses the local SchemaExpr defined in this package.
 */
sealed trait MigrationAction extends Product with Serializable {
  def at: DynamicOptic
}

object MigrationAction {

  case class AddField(
    at: DynamicOptic,
    default: SchemaExpr[_]
  ) extends MigrationAction

  case class DropField(
    at: DynamicOptic,
    defaultForReverse: SchemaExpr[_]
  ) extends MigrationAction

  case class Rename(
    at: DynamicOptic,
    to: String
  ) extends MigrationAction

  case class TransformValue(
    at: DynamicOptic,
    transform: SchemaExpr[_]
  ) extends MigrationAction

  case class Mandate(
    at: DynamicOptic,
    default: SchemaExpr[_]
  ) extends MigrationAction

  case class Optionalize(
    at: DynamicOptic
  ) extends MigrationAction

  case class ChangeType(
    at: DynamicOptic,
    converter: SchemaExpr[_]
  ) extends MigrationAction

  case class Join(
    at: DynamicOptic,
    sourcePaths: List[DynamicOptic],
    combiner: SchemaExpr[_]
  ) extends MigrationAction

  case class Split(
    at: DynamicOptic,
    targetPaths: List[DynamicOptic],
    splitter: SchemaExpr[_]
  ) extends MigrationAction

  case class RenameCase(
    at: DynamicOptic,
    from: String,
    to: String
  ) extends MigrationAction

  case class TransformCase(
    at: DynamicOptic,
    actions: List[MigrationAction]
  ) extends MigrationAction

  case class TransformElements(
    at: DynamicOptic,
    transform: SchemaExpr[_]
  ) extends MigrationAction

  case class TransformKeys(
    at: DynamicOptic,
    transform: SchemaExpr[_]
  ) extends MigrationAction

  case class TransformValues(
    at: DynamicOptic,
    transform: SchemaExpr[_]
  ) extends MigrationAction
=======
package zio.blocks.schema.migration

// 🔥 FIX: এখান থেকে 'SchemaExpr' সরিয়ে দেওয়া হয়েছে।
// আগে ছিল: import zio.blocks.schema.{DynamicOptic, DynamicValue, PrimitiveValue, SchemaExpr}
// এখন:
import zio.blocks.schema.{DynamicOptic, DynamicValue, PrimitiveValue}

sealed trait MigrationAction extends java.io.Serializable {
  def at: DynamicOptic
  def reverse: MigrationAction
}

object MigrationAction {

  case class AddField(
    at: DynamicOptic,
    default: SchemaExpr[_]
  ) extends MigrationAction {
    override def reverse: MigrationAction = DropField(at, default)
  }

  case class DropField(
    at: DynamicOptic,
    defaultForReverse: SchemaExpr[_]
  ) extends MigrationAction {
    override def reverse: MigrationAction = AddField(at, defaultForReverse)
  }

  case class Rename(
    at: DynamicOptic,
    to: String
  ) extends MigrationAction {
    override def reverse: MigrationAction = {
      val oldName = at.nodes.lastOption match {
        case Some(DynamicOptic.Node.Field(n)) => n
        case _                                => "unknown"
      }
      val newPath = DynamicOptic(at.nodes.dropRight(1) :+ DynamicOptic.Node.Field(to))
      Rename(newPath, oldName)
    }
  }

  case class TransformValue(
    at: DynamicOptic,
    transform: SchemaExpr[_]
  ) extends MigrationAction {
    override def reverse: MigrationAction = TransformValue(at, transform)
  }

  case class Mandate(
    at: DynamicOptic,
    default: SchemaExpr[_]
  ) extends MigrationAction {
    override def reverse: MigrationAction = Optionalize(at)
  }

  case class Optionalize(
    at: DynamicOptic
  ) extends MigrationAction {
    override def reverse: MigrationAction =
      Mandate(at, SchemaExpr.Constant(DynamicValue.Primitive(PrimitiveValue.Unit)))
  }

  case class ChangeType(
    at: DynamicOptic,
    converter: SchemaExpr[_]
  ) extends MigrationAction {
    override def reverse: MigrationAction = ChangeType(at, converter)
  }

  case class Join(
    at: DynamicOptic,
    sourcePaths: Vector[DynamicOptic],
    combiner: SchemaExpr[_]
  ) extends MigrationAction {
    override def reverse: MigrationAction = Split(at, sourcePaths, combiner)
  }

  case class Split(
    at: DynamicOptic,
    targetPaths: Vector[DynamicOptic],
    splitter: SchemaExpr[_]
  ) extends MigrationAction {
    override def reverse: MigrationAction = Join(at, targetPaths, splitter)
  }

  case class RenameCase(
    at: DynamicOptic,
    from: String,
    to: String
  ) extends MigrationAction {
    override def reverse: MigrationAction = RenameCase(at, to, from)
  }

  case class TransformCase(
    at: DynamicOptic,
    actions: Vector[MigrationAction]
  ) extends MigrationAction {
    override def reverse: MigrationAction = TransformCase(at, actions.map(_.reverse).reverse)
  }
>>>>>>> 81fc9d21 (fix: resolve schema migration engine compilation errors)
}