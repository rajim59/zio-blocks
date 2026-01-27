package zio.blocks.schema.migration

import zio.blocks.schema.{Schema, DynamicOptic, DynamicValue, PrimitiveValue}
import zio.blocks.schema.migration.macros.AccessorMacros
import zio.blocks.schema.migration.MigrationAction._
import scala.annotation.unused
import scala.compiletime.*

class MigrationBuilder[A, B, Steps <: Tuple](
  private val sourceSchema: Schema[A],
  private val targetSchema: Schema[B],
  private val steps: DynamicMigration[Steps] 
) {

  private def append[NewAction <: MigrationAction](action: NewAction): MigrationBuilder[A, B, Tuple.Concat[Steps, NewAction *: EmptyTuple]] = {
    val single = DynamicMigration.single(action)
    new MigrationBuilder(sourceSchema, targetSchema, steps ++ single)
  }

  private def toOptic(nodes: IndexedSeq[DynamicOptic.Node]): DynamicOptic =
    DynamicOptic(nodes.toVector)

  private def extractFieldName(path: DynamicOptic): String =
    path.nodes.lastOption match {
      case Some(DynamicOptic.Node.Field(name)) => name
      case _                                   => "unknown"
    }

  private def toConstantExpr(value: Any): SchemaExpr[_] = {
    val dv = value match {
      case i: Int     => DynamicValue.Primitive(PrimitiveValue.Int(i))
      case s: String  => DynamicValue.Primitive(PrimitiveValue.String(s))
      case b: Boolean => DynamicValue.Primitive(PrimitiveValue.Boolean(b))
      case d: Double  => DynamicValue.Primitive(PrimitiveValue.BigDecimal(java.math.BigDecimal.valueOf(d)))
      case l: Long    => DynamicValue.Primitive(PrimitiveValue.BigDecimal(java.math.BigDecimal.valueOf(l)))
      case other      => DynamicValue.Primitive(PrimitiveValue.String(other.toString))
    }
    SchemaExpr.Constant(dv)
  }

  inline def addField[T](inline target: B => T, default: SchemaExpr[_]): MigrationBuilder[A, B, Tuple.Concat[Steps, AddField *: EmptyTuple]] = {
    val path = AccessorMacros.derive(target).apply()
    append(AddField(toOptic(path.nodes), default))
  }

  inline def addFieldConstant[T](inline target: B => T, value: Any): MigrationBuilder[A, B, Tuple.Concat[Steps, AddField *: EmptyTuple]] = {
    val path         = AccessorMacros.derive(target).apply()
    val constantExpr = toConstantExpr(value)
    append(AddField(toOptic(path.nodes), constantExpr))
  }

  inline def dropField[T](
    inline source: A => T,
    defaultExpr: Option[SchemaExpr[_]] = None
  )(implicit schema: Schema[T]): MigrationBuilder[A, B, Tuple.Concat[Steps, DropField *: EmptyTuple]] = {
    val path          = AccessorMacros.derive(source).apply()
    val actualDefault = defaultExpr.getOrElse(SchemaExpr.default(schema))
    append(DropField(toOptic(path.nodes), actualDefault))
  }

  inline def renameField[T1, T2](inline from: A => T1, inline to: B => T2): MigrationBuilder[A, B, Tuple.Concat[Steps, Rename *: EmptyTuple]] = {
    val fromPath = AccessorMacros.derive(from).apply()
    val toPath   = AccessorMacros.derive(to).apply()
    append(Rename(toOptic(fromPath.nodes), extractFieldName(toPath)))
  }

  inline def transformField[T1, T2](
    inline from: A => T1,
    @unused inline to: B => T2,
    transform: SchemaExpr[_]
  ): MigrationBuilder[A, B, Tuple.Concat[Steps, TransformValue *: EmptyTuple]] = {
    val fromPath = AccessorMacros.derive(from).apply()
    append(TransformValue(toOptic(fromPath.nodes), transform))
  }

  inline def mandateField[T](
    inline from: A => Option[T],
    @unused inline to: B => T,
    default: SchemaExpr[_]
  ): MigrationBuilder[A, B, Tuple.Concat[Steps, Mandate *: EmptyTuple]] = {
    val path = AccessorMacros.derive(from).apply()
    append(Mandate(toOptic(path.nodes), default))
  }

  inline def optionalizeField[T](
    inline source: A => T,
    @unused inline target: B => Option[T]
  ): MigrationBuilder[A, B, Tuple.Concat[Steps, Optionalize *: EmptyTuple]] = {
    val path = AccessorMacros.derive(source).apply()
    append(Optionalize(toOptic(path.nodes)))
  }

  inline def changeFieldType[T1, T2](
    inline from: A => T1,
    @unused inline to: B => T2,
    converter: SchemaExpr[_]
  ): MigrationBuilder[A, B, Tuple.Concat[Steps, ChangeType *: EmptyTuple]] = {
    val path = AccessorMacros.derive(from).apply()
    append(ChangeType(toOptic(path.nodes), converter))
  }

  inline def joinFields[T](
    inline sources: Vector[A => Any], 
    inline target: B => T,
    combiner: SchemaExpr[_]
  ): MigrationBuilder[A, B, Tuple.Concat[Steps, Join *: EmptyTuple]] = {
    val targetPath  = AccessorMacros.derive(target).apply()
    val sourcePaths = sources.map(s => AccessorMacros.derive(s.asInstanceOf[A => Any]).apply())
    val joinAction = Join(
      toOptic(targetPath.nodes), 
      sourcePaths.map(p => toOptic(p.nodes)).toList, 
      combiner
    )
    append(joinAction)
  }

  inline def splitField[T](
    inline source: A => T,
    inline targets: Vector[B => Any],
    splitter: SchemaExpr[_]
  ): MigrationBuilder[A, B, Tuple.Concat[Steps, Split *: EmptyTuple]] = {
    val sourcePath  = AccessorMacros.derive(source).apply()
    val targetPaths = targets.map(t => AccessorMacros.derive(t.asInstanceOf[B => Any]).apply())
    val splitAction = Split(
      toOptic(sourcePath.nodes), 
      targetPaths.map(p => toOptic(p.nodes)).toList, 
      splitter
    )
    append(splitAction)
  }

  inline def transformElements[T](inline at: A => Vector[T], transform: SchemaExpr[_]): MigrationBuilder[A, B, Tuple.Concat[Steps, TransformElements *: EmptyTuple]] = {
    val path  = AccessorMacros.derive(at).apply()
    val optic = toOptic(path.nodes :+ DynamicOptic.Node.Elements)
    append(TransformElements(optic, transform))
  }

  inline def transformKeys[K, V](inline at: A => Map[K, V], transform: SchemaExpr[_]): MigrationBuilder[A, B, Tuple.Concat[Steps, TransformKeys *: EmptyTuple]] = {
    val path = AccessorMacros.derive(at).apply()
    append(TransformKeys(toOptic(path.nodes), transform))
  }

  inline def transformValues[K, V](inline at: A => Map[K, V], transform: SchemaExpr[_]): MigrationBuilder[A, B, Tuple.Concat[Steps, TransformValues *: EmptyTuple]] = {
    val path = AccessorMacros.derive(at).apply()
    append(TransformValues(toOptic(path.nodes), transform))
  }

  def renameCase(from: String, to: String): MigrationBuilder[A, B, Tuple.Concat[Steps, RenameCase *: EmptyTuple]] =
    append(RenameCase(DynamicOptic.root, from, to))

  def transformCase[CaseA, CaseB](
    @unused at: String,
    @unused caseMigration: MigrationBuilder[CaseA, CaseB, EmptyTuple] => MigrationBuilder[CaseA, CaseB, _] 
  ): MigrationBuilder[A, B, Tuple.Concat[Steps, TransformCase *: EmptyTuple]] = {
    val initialBuilder = MigrationBuilder.make[CaseA, CaseB](using null.asInstanceOf[Schema[CaseA]], null.asInstanceOf[Schema[CaseB]])
    val resultBuilder = caseMigration(initialBuilder)
    val innerActions = resultBuilder.steps.actions.toList.asInstanceOf[List[MigrationAction]]
    append(TransformCase(DynamicOptic.root, innerActions))
  }

  inline def build: Migration[A, B, Steps] = {
    inline if (constValue[Tuple.Size[Steps]] == 0) {
       summonFrom {
         case _: (A =:= B) => ()
         case _ => error("COMPILE ERROR: Cannot build an empty migration because Source and Target types are different! You must add migration steps (e.g., .renameField, .addField).")
       }
    }
    Migration(steps, sourceSchema, targetSchema)
  }

  def buildPartial: Migration[A, B, Steps] =
    Migration(steps, sourceSchema, targetSchema)
}

object MigrationBuilder {
  def make[A, B](implicit source: Schema[A], target: Schema[B]): MigrationBuilder[A, B, EmptyTuple] =
    new MigrationBuilder(source, target, DynamicMigration.empty)
}