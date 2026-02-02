package zio.blocks.schema.migration.macros

import zio.blocks.schema.migration.{MigrationAction, MigrationBuilder}
import zio.blocks.schema.{DynamicOptic, SchemaExpr}
import scala.reflect.macros.blackbox

class MigrationMacros(val c: blackbox.Context) {
  import c.universe._

  // হেল্পার মেথড: ফাংশন (x => x.field) থেকে প্লেইন পাথ স্ট্রিং বের করা
  // যদি পাথ ভ্যালিড না হয়, তবে কম্পাইলার এখানে Error দেবে (c.abort)
  private def extractPath(tree: Tree): Seq[String] = {
    def loop(t: Tree, acc: List[String]): List[String] = t match {
      case Function(_, body) => loop(body, acc)
      case Select(qual, name) => loop(qual, name.toString.trim :: acc)
      case Ident(_) => acc // বেস অবজেক্টে পৌঁছে গেছি
      case _ =>
        c.abort(c.enclosingPosition, s"Invalid selector path. Expected simple field access (e.g., _.field), found: $t")
    }
    loop(tree, Nil)
  }

  // পাথগুলোকে DynamicOptic কোডে কনভার্ট করার মেথড
  private def toOpticExpr(path: Seq[String]): c.Expr[DynamicOptic] = {
    val nodes = path.map(n => q"zio.blocks.schema.DynamicOptic.Node.Field($n)")
    c.Expr[DynamicOptic](q"zio.blocks.schema.DynamicOptic(Vector(..$nodes))")
  }

  // ------------------------------------------------------------------
  // Macro Implementations
  // ------------------------------------------------------------------

  def addFieldImpl[A: WeakTypeTag, B: WeakTypeTag, T: WeakTypeTag](
    target: c.Expr[B => T],
    default: c.Expr[SchemaExpr[_]]
  ): c.Expr[MigrationBuilder[A, B]] = {
    val targetPath = extractPath(target.tree)
    val opticCode = toOpticExpr(targetPath)

    q"${c.prefix}.internalAppend(zio.blocks.schema.migration.MigrationAction.AddField($opticCode, $default))"
  }

  def renameFieldImpl[A: WeakTypeTag, B: WeakTypeTag, T: WeakTypeTag, U: WeakTypeTag](
    from: c.Expr[A => T],
    to: c.Expr[B => U]
  ): c.Expr[MigrationBuilder[A, B]] = {
    val fromPath = extractPath(from.tree)
    val toPath = extractPath(to.tree)
    val toName = toPath.lastOption.getOrElse(c.abort(c.enclosingPosition, "Target path is empty"))

    val opticCode = toOpticExpr(fromPath)

    q"${c.prefix}.internalAppend(zio.blocks.schema.migration.MigrationAction.Rename($opticCode, $toName))"
  }

  def dropFieldImpl[A: WeakTypeTag, B: WeakTypeTag, T: WeakTypeTag](
    source: c.Expr[A => T],
    defaultExpr: c.Expr[Option[SchemaExpr[_]]]
  )(schema: c.Expr[zio.blocks.schema.Schema[T]]): c.Expr[MigrationBuilder[A, B]] = {
    val sourcePath = extractPath(source.tree)
    val opticCode = toOpticExpr(sourcePath)

    // ডিফল্ট ভ্যালু হ্যান্ডলিং লজিক
    q"""
      val actualDefault = $defaultExpr.getOrElse(zio.blocks.schema.SchemaExpr.DefaultValue($schema))
      ${c.prefix}.internalAppend(zio.blocks.schema.migration.MigrationAction.DropField($opticCode, actualDefault))
    """
  }
}