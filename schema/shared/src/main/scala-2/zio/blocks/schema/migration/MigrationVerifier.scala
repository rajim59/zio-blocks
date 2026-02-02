package zio.blocks.schema.migration

import zio.blocks.schema.Schema
import scala.language.experimental.macros

/**
 * MigrationVerifier for Scala 2
 * Proves validity at compile-time using Macros.
 */
trait MigrationVerifier[From, To, Ops <: MigrationOp] {
  def verify(source: Schema[From], target: Schema[To]): Migration[From, To]
}

object MigrationVerifier {
  // Scala 2 Macro Call
  implicit def derive[From, To, Ops <: MigrationOp]: MigrationVerifier[From, To, Ops] = 
    macro zio.blocks.schema.migration.macros.MigrationMacros.verifyImpl[From, To, Ops]
}