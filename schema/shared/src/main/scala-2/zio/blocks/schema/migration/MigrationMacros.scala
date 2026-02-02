package zio.blocks.schema.migration.macros

import zio.blocks.schema.migration.{MigrationBuilder, MigrationOp}
import zio.blocks.schema.SchemaExpr
import scala.reflect.macros.whitebox

class MigrationMacros(val c: whitebox.Context) {
  import c.universe._

  // ১. পাথ এক্সট্রাকশন (Path Extraction)
  private def extractPath(tree: Tree): String = {
    def loop(t: Tree): String = t match {
      case Function(_, body) => loop(body)
      case Select(_, name)   => name.toString.trim
      case Ident(name)       => name.toString.trim 
      case Block(_, expr)    => loop(expr)
      case Typed(expr, _)    => loop(expr)
      case _ =>
        c.abort(c.enclosingPosition, s"Invalid selector path. Found: ${showRaw(t)}")
    }
    val path = loop(tree)
    if (path.isEmpty || path.contains("<init>")) 
      c.abort(c.enclosingPosition, "Selector path cannot be empty or invalid")
    path
  }

  // ২. টাইপ কনস্ট্রাকশন হেল্পার
  private def mkSingletonType(s: String): Type =
    c.internal.constantType(Constant(s))

  private def mkBuilder(newOpsType: Tree): c.Tree = {
    val A = c.weakTypeOf[zio.blocks.schema.migration.MigrationBuilder[_, _, _]].typeArgs(0)
    val B = c.weakTypeOf[zio.blocks.schema.migration.MigrationBuilder[_, _, _]].typeArgs(1)
    
    q"""
      new zio.blocks.schema.migration.MigrationBuilder[$A, $B, $newOpsType](
        ${c.prefix}.sourceSchema,
        ${c.prefix}.targetSchema
      )
    """
  }

  // ৩. STRICT VALIDATION HELPER (Fail Fast Logic)
  private def validateFieldExists(tpe: Type, name: String): Unit = {
    // টাইপ এলিয়াস ভেঙে মূল টাইপ বের করা
    val realType = tpe.dealias.widen
    val symbol = realType.member(TermName(name))
    
    if (symbol == NoSymbol) {
      c.abort(c.enclosingPosition, s"❌ CRITICAL MIGRATION ERROR: Field '$name' does not exist in type '$realType'. You cannot rename or drop a field that doesn't exist!")
    }
  }

  // ------------------------------------------------------------------
  // Macro Implementations (Logic -> Type Generation)
  // ------------------------------------------------------------------

  def renameFieldImpl[A: WeakTypeTag, B: WeakTypeTag, Ops: WeakTypeTag, T1: WeakTypeTag, T2: WeakTypeTag](
    from: c.Expr[A => T1],
    to: c.Expr[B => T2]
  ): c.Tree = {
    val fromStr = extractPath(from.tree)
    val toStr   = extractPath(to.tree)

    validateFieldExists(weakTypeOf[A], fromStr)

    val fromName = mkSingletonType(fromStr)
    val toName   = mkSingletonType(toStr)
    val opsType  = weakTypeOf[Ops]
    val newOpsType = tq"zio.blocks.schema.migration.::[zio.blocks.schema.migration.RenameOp[$fromName, $toName], $opsType]"
    mkBuilder(newOpsType)
  }

  def addFieldImpl[A: WeakTypeTag, B: WeakTypeTag, Ops: WeakTypeTag, T: WeakTypeTag](
    target: c.Expr[B => T],
    default: c.Expr[SchemaExpr[_, _]]
  ): c.Tree = {
    val targetName = mkSingletonType(extractPath(target.tree))
    val fieldType  = weakTypeOf[T]
    val opsType    = weakTypeOf[Ops]
    val newOpsType = tq"zio.blocks.schema.migration.::[zio.blocks.schema.migration.AddFieldOp[$targetName, $fieldType], $opsType]"
    mkBuilder(newOpsType)
  }

  def dropFieldImpl[A: WeakTypeTag, B: WeakTypeTag, Ops: WeakTypeTag, T: WeakTypeTag](
    source: c.Expr[A => T],
    defaultExpr: c.Expr[Option[SchemaExpr[_, _]]]
  )(schema: c.Expr[zio.blocks.schema.Schema[T]]): c.Tree = {
    val sourceStr = extractPath(source.tree)
    
    validateFieldExists(weakTypeOf[A], sourceStr)

    val sourceName = mkSingletonType(sourceStr)
    val opsType    = weakTypeOf[Ops]
    val newOpsType = tq"zio.blocks.schema.migration.::[zio.blocks.schema.migration.DropFieldOp[$sourceName], $opsType]"
    mkBuilder(newOpsType)
  }

  def changeTypeImpl[A: WeakTypeTag, B: WeakTypeTag, Ops: WeakTypeTag, T: WeakTypeTag, U: WeakTypeTag](
    from: c.Expr[A => T],
    to: c.Expr[B => U],
    converter: c.Expr[SchemaExpr[_, _]]
  ): c.Tree = {
    val fromStr = extractPath(from.tree)
    validateFieldExists(weakTypeOf[A], fromStr)

    val fromName = mkSingletonType(fromStr)
    val oldType  = weakTypeOf[T]
    val newType  = weakTypeOf[U]
    val opsType  = weakTypeOf[Ops]
    val newOpsType = tq"zio.blocks.schema.migration.::[zio.blocks.schema.migration.ChangeTypeOp[$fromName, $oldType, $newType], $opsType]"
    mkBuilder(newOpsType)
  }

  def mandateFieldImpl[A: WeakTypeTag, B: WeakTypeTag, Ops: WeakTypeTag, T: WeakTypeTag](
    from: c.Expr[A => Option[T]],
    target: c.Expr[B => T],
    default: c.Expr[SchemaExpr[_, _]]
  ): c.Tree = {
    val fromStr = extractPath(from.tree)
    validateFieldExists(weakTypeOf[A], fromStr)

    val fromName = mkSingletonType(fromStr)
    val valueType = weakTypeOf[T]
    val opsType  = weakTypeOf[Ops]
    val newOpsType = tq"zio.blocks.schema.migration.::[zio.blocks.schema.migration.MandateFieldOp[$fromName, $valueType], $opsType]"
    mkBuilder(newOpsType)
  }

  def optionalizeFieldImpl[A: WeakTypeTag, B: WeakTypeTag, Ops: WeakTypeTag, T: WeakTypeTag](
    source: c.Expr[A => T],
    target: c.Expr[B => Option[T]]
  ): c.Tree = {
    val sourceStr = extractPath(source.tree)
    validateFieldExists(weakTypeOf[A], sourceStr)

    val sourceName = mkSingletonType(sourceStr)
    val valueType  = weakTypeOf[T]
    val opsType    = weakTypeOf[Ops]
    val newOpsType = tq"zio.blocks.schema.migration.::[zio.blocks.schema.migration.OptionalizeFieldOp[$sourceName, $valueType], $opsType]"
    mkBuilder(newOpsType)
  }

  def transformFieldImpl[A: WeakTypeTag, B: WeakTypeTag, Ops: WeakTypeTag, T: WeakTypeTag, U: WeakTypeTag](
    from: c.Expr[A => T],
    to: c.Expr[B => U],
    transform: c.Expr[SchemaExpr[_, _]]
  ): c.Tree = {
    val fromStr = extractPath(from.tree)
    validateFieldExists(weakTypeOf[A], fromStr)

    val fromName = mkSingletonType(fromStr)
    val valueType = weakTypeOf[T]
    val opsType  = weakTypeOf[Ops]
    val newOpsType = tq"zio.blocks.schema.migration.::[zio.blocks.schema.migration.TransformFieldOp[$fromName, $valueType], $opsType]"
    mkBuilder(newOpsType)
  }

  // ------------------------------------------------------------------
  // 4. Verification Logic (Type -> Code Generation)
  // ------------------------------------------------------------------

  def verifyImpl[A: WeakTypeTag, B: WeakTypeTag, Ops: WeakTypeTag]: c.Tree = {
    val A = weakTypeOf[A]
    val B = weakTypeOf[B]
    val opsType = weakTypeOf[Ops]

    def loop(tpe: Type): List[Tree] = {
      if (tpe <:< typeOf[zio.blocks.schema.migration.MNil]) {
        Nil
      } 
      else if (tpe.typeSymbol == symbolOf[zio.blocks.schema.migration.::[_, _]]) {
        val typeArgs = tpe.typeArgs
        val head = typeArgs(0)
        val tail = typeArgs(1)
        val headAction = processOp(head, A)
        headAction :: loop(tail)
      } 
      else {
        c.abort(c.enclosingPosition, s"Unexpected Ops type structure: $tpe")
      }
    }

    val actions = loop(opsType).reverse

    q"""
      new zio.blocks.schema.migration.MigrationVerifier[$A, $B, $opsType] {
        def verify(source: zio.blocks.schema.Schema[$A], target: zio.blocks.schema.Schema[$B]): zio.blocks.schema.migration.Migration[$A, $B] = {
          val act = Vector(..$actions)
          zio.blocks.schema.migration.Migration(
            zio.blocks.schema.migration.DynamicMigration(act),
            source,
            target
          )
        }
      }
    """
  }

  // 🔥 IMPORTANT: Names are now fully synced with Client's MigrationAction
  private def processOp(opType: Type, Source: Type): Tree = {
    val sym = opType.typeSymbol

    // 1. Rename
    if (sym == symbolOf[zio.blocks.schema.migration.RenameOp[_, _]]) {
      val args = opType.typeArgs
      val k1 = extractLiteral(args(0))
      val k2 = extractLiteral(args(1))
      validateFieldExists(Source, k1) 
      val optic = mkFieldOptic(k1)
      // FIX: RenameField -> Rename
      q"zio.blocks.schema.migration.MigrationAction.Rename($optic, $k2)"
    }
    // 2. AddField
    else if (sym == symbolOf[zio.blocks.schema.migration.AddFieldOp[_, _]]) {
      val args = opType.typeArgs
      val k = extractLiteral(args(0))
      val optic = mkFieldOptic(k)
      // FIX: SchemaExpr.Literal -> SchemaExpr.Constant
      q"zio.blocks.schema.migration.MigrationAction.AddField($optic, zio.blocks.schema.SchemaExpr.Constant(zio.blocks.schema.DynamicValue.Primitive(zio.blocks.schema.PrimitiveValue.Unit)))"
    }
    // 3. DropField
    else if (sym == symbolOf[zio.blocks.schema.migration.DropFieldOp[_]]) {
      val args = opType.typeArgs
      val k = extractLiteral(args(0))
      validateFieldExists(Source, k)
      val optic = mkFieldOptic(k)
      // FIX: DeleteField -> DropField, Literal -> Constant
      q"zio.blocks.schema.migration.MigrationAction.DropField($optic, zio.blocks.schema.SchemaExpr.Constant(zio.blocks.schema.DynamicValue.Primitive(zio.blocks.schema.PrimitiveValue.Unit)))"
    }
    // 4. ChangeType
    else if (sym == symbolOf[zio.blocks.schema.migration.ChangeTypeOp[_, _, _]]) {
      val args = opType.typeArgs
      val k = extractLiteral(args(0))
      validateFieldExists(Source, k)
      val optic = mkFieldOptic(k)
      q"zio.blocks.schema.migration.MigrationAction.ChangeType($optic, zio.blocks.schema.SchemaExpr.Constant(zio.blocks.schema.DynamicValue.Primitive(zio.blocks.schema.PrimitiveValue.Unit)))"
    }
    // 5. Mandate
    else if (sym == symbolOf[zio.blocks.schema.migration.MandateFieldOp[_, _]]) {
      val args = opType.typeArgs
      val k = extractLiteral(args(0))
      validateFieldExists(Source, k)
      val optic = mkFieldOptic(k)
      // FIX: MandateField -> Mandate
      q"zio.blocks.schema.migration.MigrationAction.Mandate($optic, zio.blocks.schema.SchemaExpr.Constant(zio.blocks.schema.DynamicValue.Primitive(zio.blocks.schema.PrimitiveValue.Unit)))"
    }
    // 6. Optionalize
    else if (sym == symbolOf[zio.blocks.schema.migration.OptionalizeFieldOp[_, _]]) {
      val args = opType.typeArgs
      val k = extractLiteral(args(0))
      validateFieldExists(Source, k)
      val optic = mkFieldOptic(k)
      // FIX: OptionalizeField -> Optionalize
      q"zio.blocks.schema.migration.MigrationAction.Optionalize($optic)"
    }
    // 7. TransformValue
    else if (sym == symbolOf[zio.blocks.schema.migration.TransformFieldOp[_, _]]) {
      val args = opType.typeArgs
      val k = extractLiteral(args(0))
      validateFieldExists(Source, k)
      val optic = mkFieldOptic(k)
      q"zio.blocks.schema.migration.MigrationAction.TransformValue($optic, zio.blocks.schema.SchemaExpr.Constant(zio.blocks.schema.DynamicValue.Primitive(zio.blocks.schema.PrimitiveValue.Unit)))"
    }
    else {
      c.abort(c.enclosingPosition, s"Unknown migration operation: $opType")
    }
  }

  private def mkFieldOptic(fieldName: String): Tree = {
    q"zio.blocks.schema.DynamicOptic(Vector(zio.blocks.schema.DynamicOptic.Node.Field($fieldName)))"
  }

  private def extractLiteral(tpe: Type): String = {
    tpe match {
      case ConstantType(Constant(s: String)) => s
      case _ => c.abort(c.enclosingPosition, s"Expected a string literal type, got: $tpe")
    }
  }
}