package zio.blocks.schema.migration

import zio.blocks.schema.{Schema, DynamicOptic}
import zio.blocks.schema.migration.MigrationAction._
import scala.annotation.unused
import scala.annotation.implicitNotFound

// --- Type Level Linked List Definition ---
sealed trait MList
sealed trait MNil extends MList
sealed trait ::[H, T <: MList] extends MList

/**
 * MigrationValid Witness (Same as before)
 */
@implicitNotFound("COMPILE ERROR: Cannot build migration from ${A} to ${B}. If types are different, you must provide migration steps.")
trait MigrationValid[A, B, Steps <: MList]

object MigrationValid {
  implicit def validIfStepsExist[A, B, H, T <: MList]: MigrationValid[A, B, H :: T] = 
    new MigrationValid[A, B, H :: T] {}

  implicit def validIfIdentity[A, B](implicit ev: A =:= B): MigrationValid[A, B, MNil] = 
    new MigrationValid[A, B, MNil] {}
}

/**
 * MigrationBuilder (Scala 2 - Compile-Time Type Tracking).
 */
class MigrationBuilder[A, B, Steps <: MList](
  private val sourceSchema: Schema[A],
  private val targetSchema: Schema[B],
  // Changed to private[migration] to allow access from transformCase
  private[migration] val actionsList: List[MigrationAction]
) {

  private def append[NewAction](action: MigrationAction): MigrationBuilder[A, B, NewAction :: Steps] = {
    new MigrationBuilder(sourceSchema, targetSchema, actionsList :+ action)
  }

  private def toOptic(nodes: IndexedSeq[DynamicOptic.Node]): DynamicOptic =
    DynamicOptic(nodes.toVector)

  private def extractFieldName(path: DynamicOptic): String =
    path.nodes.lastOption match {
      case Some(DynamicOptic.Node.Field(name)) => name
      case _                                   => "unknown"
    }

  private def toConstant[T](value: T)(implicit schema: Schema[T]): SchemaExpr[T] = {
    SchemaExpr.constant(value, schema)
  }

  // --- Operations ---

  def addField[T](target: ToDynamicOptic[B, T], default: SchemaExpr[_]): MigrationBuilder[A, B, AddField :: Steps] = {
    val path = target.apply()
    append[AddField](AddField(toOptic(path.nodes), default))
  }

  def addFieldConstant[T](target: ToDynamicOptic[B, T], value: T)(implicit schema: Schema[T]): MigrationBuilder[A, B, AddField :: Steps] = {
    val path = target.apply()
    append[AddField](AddField(toOptic(path.nodes), toConstant(value)))
  }

  def dropField[T](source: ToDynamicOptic[A, T], defaultExpr: Option[SchemaExpr[_]] = None)(implicit
    schema: Schema[T]
  ): MigrationBuilder[A, B, DropField :: Steps] = {
    val path = source.apply()
    val actualDefault = defaultExpr.getOrElse(SchemaExpr.default(schema))
    append[DropField](DropField(toOptic(path.nodes), actualDefault))
  }

  def renameField[T1, T2](from: ToDynamicOptic[A, T1], to: ToDynamicOptic[B, T2]): MigrationBuilder[A, B, Rename :: Steps] = {
    val fromPath = from.apply()
    val toPath   = to.apply()
    append[Rename](Rename(toOptic(fromPath.nodes), extractFieldName(toPath)))
  }

  def transformField[T1, T2](
    from: ToDynamicOptic[A, T1],
    @unused target: ToDynamicOptic[B, T2],
    transform: SchemaExpr[_]
  ): MigrationBuilder[A, B, TransformValue :: Steps] = {
    val fromPath = from.apply()
    append[TransformValue](TransformValue(toOptic(fromPath.nodes), transform))
  }

  def mandateField[T](
    from: ToDynamicOptic[A, Option[T]],
    @unused target: ToDynamicOptic[B, T],
    default: SchemaExpr[_]
  ): MigrationBuilder[A, B, Mandate :: Steps] = {
    val path = from.apply()
    append[Mandate](Mandate(toOptic(path.nodes), default))
  }

  def optionalizeField[T](
    source: ToDynamicOptic[A, T],
    @unused target: ToDynamicOptic[B, Option[T]]
  ): MigrationBuilder[A, B, Optionalize :: Steps] = {
    val path = source.apply()
    append[Optionalize](Optionalize(toOptic(path.nodes)))
  }

  def changeFieldType[T1, T2](
    from: ToDynamicOptic[A, T1],
    @unused target: ToDynamicOptic[B, T2],
    converter: SchemaExpr[_]
  ): MigrationBuilder[A, B, ChangeType :: Steps] = {
    val path = from.apply()
    append[ChangeType](ChangeType(toOptic(path.nodes), converter))
  }

  def joinFields[T](
    sources: Vector[ToDynamicOptic[A, Any]],
    target: ToDynamicOptic[B, T],
    combiner: SchemaExpr[_]
  ): MigrationBuilder[A, B, Join :: Steps] = {
    val targetPath  = target.apply()
    val sourcePaths = sources.map(_.apply())
    val action = Join(toOptic(targetPath.nodes), sourcePaths.map(p => toOptic(p.nodes)).toList, combiner)
    append[Join](action)
  }

  def splitField[T](
    source: ToDynamicOptic[A, T],
    targets: Vector[ToDynamicOptic[B, Any]],
    splitter: SchemaExpr[_]
  ): MigrationBuilder[A, B, Split :: Steps] = {
    val sourcePath  = source.apply()
    val targetPaths = targets.map(_.apply())
    val action = Split(toOptic(sourcePath.nodes), targetPaths.map(p => toOptic(p.nodes)).toList, splitter)
    append[Split](action)
  }

  def transformElements[T](at: ToDynamicOptic[A, Vector[T]], transform: SchemaExpr[_]): MigrationBuilder[A, B, TransformElements :: Steps] = {
    val path  = at.apply()
    val optic = toOptic(path.nodes :+ DynamicOptic.Node.Elements)
    append[TransformElements](TransformElements(optic, transform))
  }

  def transformKeys[K, V](at: ToDynamicOptic[A, Map[K, V]], transform: SchemaExpr[_]): MigrationBuilder[A, B, TransformKeys :: Steps] = {
    val path = at.apply()
    append[TransformKeys](TransformKeys(toOptic(path.nodes), transform))
  }

  def transformValues[K, V](at: ToDynamicOptic[A, Map[K, V]], transform: SchemaExpr[_]): MigrationBuilder[A, B, TransformValues :: Steps] = {
    val path = at.apply()
    append[TransformValues](TransformValues(toOptic(path.nodes), transform))
  }

  def renameCase(from: String, to: String): MigrationBuilder[A, B, RenameCase :: Steps] =
    append[RenameCase](RenameCase(DynamicOptic.root, from, to))

  // NEW METHOD: transformCase implementation for Scala 2
  def transformCase[CaseA, CaseB](
    @unused at: String,
    @unused caseMigration: MigrationBuilder[CaseA, CaseB, MNil] => MigrationBuilder[CaseA, CaseB, _]
  ): MigrationBuilder[A, B, TransformCase :: Steps] = {
    // We create a builder with empty list (MNil) for the lambda
    val initialBuilder = MigrationBuilder.make[CaseA, CaseB](using null.asInstanceOf[Schema[CaseA]], null.asInstanceOf[Schema[CaseB]])
    val resultBuilder = caseMigration(initialBuilder)
    
    // We access the private actionsList directly from the result builder
    val innerActions = resultBuilder.actionsList
    
    append[TransformCase](TransformCase(DynamicOptic.root, innerActions))
  }

  // --- Build Methods ---

  // Standard build with validation
  def build(implicit validity: MigrationValid[A, B, Steps]): Migration[A, B] = {
    Migration(DynamicMigration(actionsList), sourceSchema, targetSchema)
  }

  // NEW METHOD: buildPartial (No validation)
  def buildPartial: Migration[A, B] = {
    Migration(DynamicMigration(actionsList), sourceSchema, targetSchema)
  }
}

object MigrationBuilder {
  def make[A, B](implicit source: Schema[A], target: Schema[B]): MigrationBuilder[A, B, MNil] =
    new MigrationBuilder(source, target, List.empty)
}