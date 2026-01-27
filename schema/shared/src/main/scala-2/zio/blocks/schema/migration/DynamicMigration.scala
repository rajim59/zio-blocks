// FILE: schema/src/main/scala-2/zio/blocks/schema/migration/DynamicMigration.scala
package zio.blocks.schema.migration

/**
 * DynamicMigration (Scala 2 Exclusive).
 * 
 * Note: In Scala 2, without heavy Shapeless dependency, we often trade 
 * strict compile-time step tracking for runtime lists in the core structure,
 * and handle safety via builders or implicit evidence elsewhere.
 */
final case class DynamicMigration(actions: List[MigrationAction]) {

  def ++(that: DynamicMigration): DynamicMigration =
    DynamicMigration(this.actions ++ that.actions)
}

object DynamicMigration {
  val empty: DynamicMigration = DynamicMigration(List.empty)
  
  def single(action: MigrationAction): DynamicMigration = 
    DynamicMigration(List(action))
}