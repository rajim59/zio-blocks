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
}