// FILE: schema/src/main/scala-3/zio/blocks/schema/migration/DynamicMigration.scala
package zio.blocks.schema.migration



/**
 * DynamicMigration (Scala 3 Exclusive).
 * 
 * Features:
 * 1. Preserves Exact Types: Uses a Tuple 'Steps' to track every action type.
 * 2. Compile-Time Composition: '++' creates a new Tuple type at compile time.
 * 3. Pure Data: No runtime logic inside.
 */
final case class DynamicMigration[Steps <: Tuple](actions: Steps) {

  /**
   * Concatenates two migrations preserving their types in the Tuple.
   * Example: (A, B) ++ (C) => (A, B, C)
   */
  def ++[OtherSteps <: Tuple](that: DynamicMigration[OtherSteps]): DynamicMigration[Tuple.Concat[Steps, OtherSteps]] =
    DynamicMigration(actions ++ that.actions)
}

object DynamicMigration {
  
  val empty: DynamicMigration[EmptyTuple] = DynamicMigration(EmptyTuple)

  /**
   * Helper to wrap a single action into a Tuple based Migration.
   */
  def single[A <: MigrationAction](action: A): DynamicMigration[A *: EmptyTuple] = 
    DynamicMigration(action *: EmptyTuple)
    
  // Note: We can add a varargs apply using inline macros if needed, 
  // but 'single' and '++' are sufficient for a builder pattern.
}