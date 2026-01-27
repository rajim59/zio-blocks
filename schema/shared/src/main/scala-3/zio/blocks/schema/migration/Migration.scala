package zio.blocks.schema.migration

import zio.blocks.schema.Schema


/**
 * Migration (Scala 3 Exclusive).
 * 
 * ARCHITECTURE:
 * This is a "Type-Preserving Data Structure".
 * Unlike the runtime version, this keeps track of every single migration step
 * in the type system (via the 'Steps' tuple).
 * 
 * @tparam A The source type.
 * @tparam B The target type.
 * @tparam Steps A Tuple type representing the sequence of actions (e.g., Rename *: Delete *: EmptyTuple).
 */
final case class Migration[A, B, Steps <: Tuple](
  dynamicMigration: DynamicMigration[Steps], // Using the Typed DynamicMigration
  sourceSchema: Schema[A],
  targetSchema: Schema[B]
) {

  /**
   * Pure Compile-Time Composition.
   * Merges the steps of two migrations into a single, longer Tuple type.
   */
  def ++[C, NextSteps <: Tuple](that: Migration[B, C, NextSteps]): Migration[A, C, Tuple.Concat[Steps, NextSteps]] =
    Migration(
      this.dynamicMigration ++ that.dynamicMigration,
      this.sourceSchema,
      that.targetSchema
    )

  def andThen[C, NextSteps <: Tuple](that: Migration[B, C, NextSteps]): Migration[A, C, Tuple.Concat[Steps, NextSteps]] = 
    this ++ that
}

object Migration {

  /**
   * Identity Migration.
   * Represents "No Operation". The 'Steps' type is 'EmptyTuple'.
   */
  def identity[A](implicit schema: Schema[A]): Migration[A, A, EmptyTuple] =
    Migration(
      DynamicMigration.empty,
      schema,
      schema
    )

  // Builder pattern entry point (Assuming MigrationBuilder is also split or handles types appropriately)
  // def newBuilder...
}