package zio.blocks.schema.migration

/**
 * MigrationOps defines the "Algebra" of migration at the Type Level.
 * Compatible with Scala 2 and Scala 3.
 */

// 1. Base Trait
sealed trait MigrationOp

// 2. Type-Level List (HList style)
sealed trait MNil extends MigrationOp

// FIX: 'infix type' (Scala 3) replaced with 'sealed trait' (Scala 2 compatible)
// This represents the Cons (::) of the list
sealed trait ::[H, T <: MigrationOp] extends MigrationOp

// 3. Operation Types

// Rename: K1 = Old Name, K2 = New Name
sealed trait RenameOp[K1 <: Singleton, K2 <: Singleton] extends MigrationOp

// Add: K = Name, V = Type
sealed trait AddFieldOp[K <: Singleton, V] extends MigrationOp

// Drop: K = Name
sealed trait DropFieldOp[K <: Singleton] extends MigrationOp

// Change Type: K = Name, OldV, NewV
sealed trait ChangeTypeOp[K <: Singleton, OldV, NewV] extends MigrationOp

// Mandate: K = Name, V = Type
sealed trait MandateFieldOp[K <: Singleton, V] extends MigrationOp

// Optionalize: K = Name, V = Type
sealed trait OptionalizeFieldOp[K <: Singleton, V] extends MigrationOp

// Transform: K = Name, V = Type
sealed trait TransformFieldOp[K <: Singleton, V] extends MigrationOp