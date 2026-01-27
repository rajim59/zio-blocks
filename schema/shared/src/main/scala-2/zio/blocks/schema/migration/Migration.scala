package zio.blocks.schema.migration

import zio.blocks.schema.Schema

/**
 * Migration (Scala 2 Exclusive).
 * 
 * ARCHITECTURE:
 * This follows the standard runtime model due to Scala 2's limitations 
 * (without using heavy libraries like Shapeless).
 * The types of the internal steps are erased, but the data is preserved.
 * 
 * @tparam A The source type.
 * @tparam B The target type.
 */
final case class Migration[A, B](
  dynamicMigration: DynamicMigration, // Using the Untyped/List-based DynamicMigration
  sourceSchema: Schema[A],
  targetSchema: Schema[B]
) {

  /**
   * Standard runtime composition.
   * Returns a new Migration with merged lists of actions.
   */
  def ++[C](that: Migration[B, C]): Migration[A, C] =
    Migration(
      this.dynamicMigration ++ that.dynamicMigration,
      this.sourceSchema,
      that.targetSchema
    )

  def andThen[C](that: Migration[B, C]): Migration[A, C] = this ++ that
}

object Migration {

  /**
   * Identity Migration.
   * The list of actions is empty.
   */
  def identity[A](implicit schema: Schema[A]): Migration[A, A] =
    Migration(
      DynamicMigration.empty,
      schema,
      schema
    )
    
  // Builder pattern entry point
  // def newBuilder...
}